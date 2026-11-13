/* The talk engine. Each slide is a 1600 x 900 frame scaled to the window; a slide has
 * builds (one press each) and a make(frame) that returns show(build, how), with how one of
 * 'step' (forward one), 'back' or 'jump'. Keys: right / space / page down next, left /
 * page up back, S scroll mode (every slide stacked at its last build), T theme, P presenter
 * window, ? keys. Main window and presenter stay in step over BroadcastChannel. Position
 * lives in the address (#slide.build); only the theme is remembered. */
(function (root) {
  'use strict';
  const W = 1600, H = 900, NS = 'http://www.w3.org/2000/svg', CH = 'ppmr-talk';
  const Deck = root.Deck = { W, H };
  const reduced = matchMedia('(prefers-reduced-motion: reduce)');

  Deck.el = (tag, cls, parent, html) => {
    const e = document.createElement(tag); if (cls) e.className = cls;
    if (html != null) e.innerHTML = html; if (parent) parent.appendChild(e); return e;
  };
  Deck.svg = (tag, attrs, parent) => {
    const e = document.createElementNS(NS, tag);
    for (const k in attrs || {}) if (attrs[k] != null) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e); return e;
  };
  Deck.tween = (ms, fn, done) => {
    if (reduced.matches || ms <= 0) { fn(1); if (done) done(); return () => {}; }
    let stop = false; const t0 = performance.now();
    const ease = (k) => k < 0.5 ? 4 * k * k * k : 1 - Math.pow(-2 * k + 2, 3) / 2;
    const step = (t) => { if (stop) return; const k = Math.min(1, (t - t0) / ms); fn(ease(k)); if (k < 1) requestAnimationFrame(step); else if (done) done(); };
    requestAnimationFrame(step); return () => { stop = true; };
  };

  /* ---------- tooltip ---------- */
  let tipEl;
  Deck.tip = (ev, html) => {
    if (!tipEl) tipEl = Deck.el('div', 'tip', document.body);
    tipEl.innerHTML = html; tipEl.hidden = false;
    const x = Math.min(ev.clientX + 14, innerWidth - 340), y = Math.min(ev.clientY + 14, innerHeight - 90);
    tipEl.style.left = x + 'px'; tipEl.style.top = y + 'px';
  };
  Deck.untip = () => { if (tipEl) tipEl.hidden = true; };

  /* ---------- theme ---------- */
  const THEME = 'ppmr-theme';
  let theme = 'dark';
  try { theme = localStorage.getItem(THEME) || 'dark'; } catch (e) { /* no storage */ }
  const applyTheme = () => { document.documentElement.dataset.theme = theme; };
  applyTheme();
  Deck.toggleTheme = () => {
    theme = theme === 'dark' ? 'light' : 'dark'; applyTheme();
    try { localStorage.setItem(THEME, theme); } catch (e) { /* ignore */ }
    if (chan) chan.postMessage({ theme });
  };

  /* ---------- channel ---------- */
  let chan = null;
  try { chan = new BroadcastChannel(CH); } catch (e) { /* old browser: presenter will not follow */ }

  /* ---------- make a frame for a slide ---------- */
  function makeFrame(slide, parent) {
    const wrap = Deck.el('div', 'frame-wrap', parent);
    const fr = Deck.el('div', 'frame', wrap);
    if (slide.title) Deck.el('h2', '', fr, slide.title);
    const inst = slide.make ? slide.make(fr) || {} : {};
    if (slide.draft) { const t = Deck.el('div', 'draft-tag', fr, 'draft'); t.title = slide.draft; }
    return { wrap, fr, inst };
  }
  const nb = (s) => Math.max(1, (S[s].builds || []).length);

  /* ---------- main ---------- */
  let S, frames = [], pos = { s: 0, b: 0 }, scroll = false;
  function readHash() {
    const m = location.hash.match(/^#(\d+)(?:\.(\d+))?/);
    if (!m) return { s: 0, b: 0 };
    const s = Math.min(+m[1], S.length - 1); return { s, b: Math.min(+(m[2] || 0), nb(s) - 1) };
  }
  function fit() {
    if (scroll) {
      const k = Math.min(1, (innerWidth - 40) / W);
      frames.forEach((f) => { f.wrap.style.width = W * k + 'px'; f.wrap.style.height = H * k + 'px'; f.fr.style.transform = `scale(${k})`; f.wrap.style.left = ''; f.wrap.style.top = ''; });
    } else {
      const k = Math.min(innerWidth / W, innerHeight / H);
      frames.forEach((f) => {
        f.wrap.style.width = W * k + 'px'; f.wrap.style.height = H * k + 'px';
        f.wrap.style.left = (innerWidth - W * k) / 2 + 'px'; f.wrap.style.top = (innerHeight - H * k) / 2 + 'px';
        f.fr.style.transform = `scale(${k})`;
      });
    }
  }
  function go(s, b, how, quiet) {
    s = Math.max(0, Math.min(S.length - 1, s)); b = Math.max(0, Math.min(nb(s) - 1, b));
    frames.forEach((f, i) => f.wrap.classList.toggle('here', i === s));
    if (s !== pos.s && how === 'step') how = 'jump';
    pos = { s, b };
    const f = frames[s]; if (f.inst.show) f.inst.show(b, how);
    count.textContent = `${s + 1}/${S.length}` + (nb(s) > 1 ? ` · ${b + 1}` : '');
    try { history.replaceState(null, '', '#' + s + (b ? '.' + b : '')); } catch (e) { /* file:// */ }
    if (chan && !quiet) chan.postMessage({ s, b });
  }
  function next() { if (pos.b < nb(pos.s) - 1) go(pos.s, pos.b + 1, 'step'); else if (pos.s < S.length - 1) go(pos.s + 1, 0, 'jump'); }
  function prev() { if (pos.b > 0) go(pos.s, pos.b - 1, 'back'); else if (pos.s > 0) go(pos.s - 1, nb(pos.s - 1) - 1, 'jump'); }
  Deck.next = next; Deck.prev = prev;

  function setScroll(on) {
    scroll = on; document.body.classList.toggle('scroll', on); fit();
    if (on) frames.forEach((f, i) => f.inst.show && f.inst.show(nb(i) - 1, 'jump'));
    else { go(pos.s, pos.b, 'jump'); scrollTo(0, 0); }
  }

  let count;
  function controls() {
    const c = Deck.el('nav', 'ctl', document.body);
    const btn = (title, svg, fn) => { const b = Deck.el('button', '', c, svg); b.title = title; b.setAttribute('aria-label', title); b.onclick = fn; return b; };
    btn('Back (←)', '<svg viewBox="0 0 24 24"><path d="M15 5l-7 7 7 7"/></svg>', prev);
    count = Deck.el('span', 'count', c);
    btn('Next (→)', '<svg viewBox="0 0 24 24"><path d="M9 5l7 7-7 7"/></svg>', next);
    btn('Scroll all slides (S)', '<svg viewBox="0 0 24 24"><rect x="5" y="3" width="14" height="7" rx="1"/><rect x="5" y="14" width="14" height="7" rx="1"/></svg>', () => setScroll(!scroll));
    btn('Presenter window (P)', '<svg viewBox="0 0 24 24"><rect x="3" y="4" width="12" height="9" rx="1"/><path d="M18 7h3M18 11h3M3 17h18M3 20h12"/></svg>', openPresenter);
    btn('Theme (T)', '<svg viewBox="0 0 24 24"><circle cx="12" cy="12" r="8"/><path d="M12 4a8 8 0 0 1 0 16z" fill="currentColor"/></svg>', Deck.toggleTheme);
    const help = Deck.el('div', 'helpbox', document.body, '<dl><dt>→ space</dt><dd>next</dd><dt>←</dt><dd>back</dd><dt>S</dt><dd>scroll mode</dd><dt>P</dt><dd>presenter window</dd><dt>T</dt><dd>dark / light</dd><dt>B</dt><dd>blank</dd></dl>');
    help.hidden = true;
    btn('Keys (?)', '<svg viewBox="0 0 24 24"><circle cx="12" cy="12" r="9"/><path d="M9.5 9.5a2.5 2.5 0 1 1 3.5 2.3c-.7.3-1 .9-1 1.7M12 17h.01"/></svg>', () => { help.hidden = !help.hidden; });
    let idle; const wake = () => { c.classList.remove('idle'); clearTimeout(idle); idle = setTimeout(() => c.classList.add('idle'), 2500); };
    addEventListener('pointermove', wake); wake();
    Deck.helpbox = help;
  }
  function openPresenter() {
    const u = location.href.split('#')[0].split('?')[0] + '?presenter#' + pos.s + '.' + pos.b;
    window.open(u, 'ppmr-presenter', 'width=1400,height=900');
  }
  function keys(onNext, onPrev) {
    addEventListener('keydown', (e) => {
      if (e.metaKey || e.ctrlKey || e.altKey) return;
      const k = e.key;
      if (k === 'ArrowRight' || k === ' ' || k === 'PageDown') { e.preventDefault(); onNext(); }
      else if (k === 'ArrowLeft' || k === 'PageUp') { e.preventDefault(); onPrev(); }
      else if (k === 't' || k === 'T') Deck.toggleTheme();
      else if ((k === 's' || k === 'S') && !Deck.isPresenter) setScroll(!scroll);
      else if ((k === 'p' || k === 'P') && !Deck.isPresenter) openPresenter();
      else if ((k === 'b' || k === 'B') && !Deck.isPresenter) document.getElementById('deck').style.visibility = document.getElementById('deck').style.visibility ? '' : 'hidden';
      else if (k === '?' && Deck.helpbox) Deck.helpbox.hidden = !Deck.helpbox.hidden;
    });
  }

  /* ---------- presenter ---------- */
  function presenter() {
    Deck.isPresenter = true;
    document.body.classList.add('presenter');
    const P = Deck.el('div', 'pres', document.body);
    const curBox = Deck.el('div', 'cur', P), side = Deck.el('div', 'side', P);
    Deck.el('div', 'cap', curBox, 'Now');
    const curHost = Deck.el('div', '', curBox);
    Deck.el('div', 'cap', side, 'Next');
    const nxtHost = Deck.el('div', 'nxt', side);
    const timer = Deck.el('div', 'timer', side, '<span>0:00</span><button type="button">reset</button>');
    const posEl = Deck.el('div', 'pos', side);
    const notes = Deck.el('div', 'notes', P);
    let t0 = Date.now();
    timer.querySelector('button').onclick = () => { t0 = Date.now(); };
    setInterval(() => { const s = Math.floor((Date.now() - t0) / 1000); timer.firstChild.textContent = Math.floor(s / 60) + ':' + String(s % 60).padStart(2, '0'); }, 500);
    let cur = null, nxt = null, here = { s: 0, b: 0 };
    const mount = (host, s, b, wpx) => {
      host.innerHTML = '';
      const f = makeFrame(S[s], host);
      const k = wpx / W; f.wrap.classList.add('here'); f.wrap.style.position = 'relative';
      f.wrap.style.width = W * k + 'px'; f.wrap.style.height = H * k + 'px'; f.fr.style.transform = `scale(${k})`;
      if (f.inst.show) f.inst.show(b, 'jump'); return { s, f };
    };
    const show = (s, b, how) => {
      here = { s, b };
      const wc = curBox.clientWidth || 800, wn = side.clientWidth || 500;
      if (!cur || cur.s !== s) cur = mount(curHost, s, b, wc); else if (cur.f.inst.show) cur.f.inst.show(b, how || 'jump');
      const n = b < nb(s) - 1 ? { s, b: b + 1 } : (s < S.length - 1 ? { s: s + 1, b: 0 } : null);
      if (n) { if (!nxt || nxt.s !== n.s) nxt = mount(nxtHost, n.s, n.b, wn); else if (nxt.f.inst.show) nxt.f.inst.show(n.b, 'jump'); }
      else { nxtHost.innerHTML = '<p>End</p>'; nxt = null; }
      const note = (S[s].builds || [])[b] || '';
      notes.innerHTML = '<p>' + note.split('\n\n').join('</p><p>') + '</p>';
      posEl.textContent = `slide ${s + 1} of ${S.length}` + (nb(s) > 1 ? `, build ${b + 1} of ${nb(s)}` : '');
    };
    const send = (s, b) => { if (chan) chan.postMessage({ s, b }); };
    const step = (d) => {
      let { s, b } = here;
      if (d > 0) { if (b < nb(s) - 1) b++; else if (s < S.length - 1) { s++; b = 0; } }
      else { if (b > 0) b--; else if (s > 0) { s--; b = nb(s) - 1; } }
      show(s, b, d > 0 ? 'step' : 'back'); send(s, b);
    };
    keys(() => step(1), () => step(-1));
    if (chan) chan.onmessage = (m) => { if (m.data.theme) { theme = m.data.theme; applyTheme(); } else show(m.data.s, m.data.b, 'jump'); };
    const h = readHash(); show(h.s, h.b);
    addEventListener('resize', () => { cur = nxt = null; show(here.s, here.b); });
  }

  Deck.start = function (slides) {
    S = slides;
    if (/[?&]presenter\b/.test(location.search)) return presenter();
    const deck = Deck.el('div', '', document.body); deck.id = 'deck';
    frames = S.map((sl) => makeFrame(sl, deck));
    controls(); keys(next, prev);
    if (chan) chan.onmessage = (m) => { if (m.data.theme) { theme = m.data.theme; applyTheme(); } else go(m.data.s, m.data.b, 'jump', true); };
    addEventListener('resize', fit);
    addEventListener('hashchange', () => { const h = readHash(); if (h.s !== pos.s || h.b !== pos.b) go(h.s, h.b, 'jump'); });
    fit(); const h = readHash(); pos = { s: -1, b: 0 }; go(h.s, h.b, 'jump');
  };
})(window);
