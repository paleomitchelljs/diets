// Capture every build of the talk site as a PNG, or as a GIF where the slide marks the
// build as animated (slide.anim = { build: ms }). No npm packages: drives headless Chrome
// over the DevTools protocol with Node's built-in fetch and WebSocket.
//
// Usage: node tools/export/capture.mjs [--site static/index.html] [--out export] [--width 1600]
// Output: <out>/media/s<slide>_b<build>.(png|gif) and <out>/manifest.json
//         (title, notes and media path for every build, in order).
import { spawn, execFileSync } from 'node:child_process';
import { mkdirSync, writeFileSync, rmSync, mkdtempSync, existsSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { tmpdir } from 'node:os';

const arg = (k, d) => { const i = process.argv.indexOf('--' + k); return i > 0 ? process.argv[i + 1] : d; };
const SITE = resolve(arg('site', 'static/index.html'));
const OUT = resolve(arg('out', 'export'));
const W = +arg('width', 1600), H = Math.round(W * 9 / 16);
const GIF_W = +arg('gif-width', 1280), FPS = +arg('fps', 15), HOLD = +arg('hold', 1500);
const CHROME = arg('chrome', process.platform === 'darwin'
  ? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' : 'google-chrome');
const PORT = 9300 + Math.floor(Math.random() * 500);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

mkdirSync(join(OUT, 'media'), { recursive: true });
const profile = mkdtempSync(join(tmpdir(), 'talk-cap-'));
const chrome = spawn(CHROME, ['--headless=new', `--remote-debugging-port=${PORT}`, `--user-data-dir=${profile}`,
  '--hide-scrollbars', '--disable-gpu', '--force-color-profile=srgb', `--window-size=${W},${H}`, 'about:blank'],
  { stdio: 'ignore' });

let ws, nextId = 1; const pending = new Map(), handlers = [];
const send = (method, params = {}) => new Promise((res, rej) => {
  const id = nextId++; pending.set(id, { res, rej }); ws.send(JSON.stringify({ id, method, params }));
});
async function connect() {
  for (let i = 0; i < 400; i++) {                 // up to 60 s: Chrome starts slowly when the CPU is busy
    try {
      const list = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json();
      const page = list.find((t) => t.type === 'page');
      if (page) {
        ws = new WebSocket(page.webSocketDebuggerUrl);
        await new Promise((r, j) => { ws.onopen = r; ws.onerror = j; });
        ws.onmessage = (m) => {
          const msg = JSON.parse(m.data);
          if (msg.id && pending.has(msg.id)) { const p = pending.get(msg.id); pending.delete(msg.id); msg.error ? p.rej(new Error(msg.error.message)) : p.res(msg.result); }
          else if (msg.method) handlers.forEach((h) => h(msg));
        };
        return;
      }
    } catch (e) { /* not up yet */ }
    await sleep(150);
  }
  throw new Error('Chrome did not start');
}
const evaluate = async (expr) => (await send('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true })).result.value;
const shot = async (file) => {
  const { data } = await send('Page.captureScreenshot', { format: 'png', clip: { x: 0, y: 0, width: W, height: H, scale: 1 } });
  writeFileSync(file, Buffer.from(data, 'base64'));
};

async function record(file, startFn, ms) {
  // screencast frames with their timestamps, then ffmpeg concat -> palette GIF
  const dir = mkdtempSync(join(tmpdir(), 'talk-gif-'));
  const frames = [];
  const h = async (msg) => {
    if (msg.method !== 'Page.screencastFrame') return;
    const { data, metadata, sessionId } = msg.params;
    const f = join(dir, `f${String(frames.length).padStart(5, '0')}.png`);
    writeFileSync(f, Buffer.from(data, 'base64')); frames.push({ f, t: metadata.timestamp });
    send('Page.screencastFrameAck', { sessionId }).catch(() => {});
  };
  handlers.push(h);
  await send('Page.startScreencast', { format: 'png', maxWidth: W, maxHeight: H, everyNthFrame: 1 });
  await sleep(150);
  const t0 = Date.now() / 1000;
  await evaluate(startFn);
  await sleep(ms + 300);
  await send('Page.stopScreencast');
  handlers.splice(handlers.indexOf(h), 1);
  // make sure the settled end state is the last frame
  const last = join(dir, 'zz_last.png'); await shot(last); frames.push({ f: last, t: Date.now() / 1000 });
  const kept = frames.filter((fr, i) => i === 0 || fr.t >= t0 - 0.05 || i === frames.length - 1);
  let list = '';
  kept.forEach((fr, i) => {
    const dur = i < kept.length - 1 ? Math.max(0.02, kept[i + 1].t - fr.t) : HOLD / 1000;
    list += `file '${fr.f}'\nduration ${dur.toFixed(3)}\n`;
  });
  list += `file '${kept[kept.length - 1].f}'\n`;
  writeFileSync(join(dir, 'list.txt'), list);
  execFileSync('ffmpeg', ['-y', '-loglevel', 'error', '-f', 'concat', '-safe', '0', '-i', join(dir, 'list.txt'),
    '-vf', `fps=${FPS},scale=${GIF_W}:-1:flags=lanczos,split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=4:diff_mode=rectangle`,
    '-loop', '0', file]);
  rmSync(dir, { recursive: true, force: true });
  return kept.length;
}

try {
  await connect();
  await send('Page.enable'); await send('Runtime.enable');
  await send('Emulation.setDeviceMetricsOverride', { width: W, height: H, deviceScaleFactor: 1, mobile: false });
  await send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-reduced-motion', value: 'no-preference' }] });
  const url = 'file://' + SITE + '?capture#0';
  await send('Page.navigate', { url });
  for (let i = 0; i < 100 && !(await evaluate('!!(window.Deck && window.TALK && document.fonts && document.fonts.status === "loaded")')); i++) await sleep(150);
  await sleep(600);
  const talk = await evaluate(`TALK.map((s) => ({ n: Math.max(1, (s.builds || []).length), anim: s.anim || {}, notes: s.builds || [] }))`);
  const manifest = { site: SITE, width: W, height: H, built: new Date().toISOString(), slides: [] };
  for (let s = 0; s < talk.length; s++) {
    const sl = { builds: [] };
    for (let b = 0; b < talk[s].n; b++) {
      const ms = talk[s].anim[b];
      let media;
      if (ms && b > 0) {
        await evaluate(`Deck.go(${s}, ${b - 1}, 'jump')`); await sleep(700);
        media = `media/s${s}_b${b}.gif`;
        const n = await record(join(OUT, media), `Deck.go(${s}, ${b}, 'step')`, ms);
        process.stdout.write(`slide ${s} build ${b}: gif (${n} frames)\n`);
      } else {
        await evaluate(`Deck.go(${s}, ${b}, 'jump')`); await sleep(700);
        media = `media/s${s}_b${b}.png`;
        await shot(join(OUT, media));
        process.stdout.write(`slide ${s} build ${b}: png\n`);
      }
      const title = await evaluate(`(() => { const f = document.querySelector('.frame-wrap.here .frame'); const h = f && (f.querySelector('h2') || f.querySelector('h1')); return h ? h.innerText.trim() : ''; })()`);
      sl.builds.push({ media, title, notes: talk[s].notes[b] || '' });
    }
    manifest.slides.push(sl);
  }
  writeFileSync(join(OUT, 'manifest.json'), JSON.stringify(manifest, null, 2));
  process.stdout.write(`wrote ${join(OUT, 'manifest.json')}\n`);
} finally {
  try { ws && ws.close(); } catch (e) { /* ignore */ }
  chrome.kill();
  await sleep(300);
  rmSync(profile, { recursive: true, force: true });
}
