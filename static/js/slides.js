/* The talk: one entry per slide. builds[] are the speaker notes, one per press.
 * anim: { build: ms } marks builds whose arrival animates; tools/export records them as GIFs.
 * All numbers come from data/data.js (code/site_export.R). DRAFT: see `draft` on each slide. */
(function (root) {
  'use strict';
  const D = root.Deck, P = root.Plot, X = root.DATA;
  const L10 = Math.LN10;
  const counts = Object.fromEntries(X.counts.map((c) => [c.minor, c]));
  const byMinor = (fit) => fit ? Object.fromEntries(fit.clades.map((c) => [c.minor, c])) : {};
  const F = { primary: byMinor(X.fits.primary), lambda0: byMinor(X.fits.lambda0), lambda1: byMinor(X.fits.lambda1), unpooled: byMinor(X.fits.unpooled) };
  const G0 = X.fits.primary.global, G = Array.isArray(G0) ? G0[0] : G0;
  const global = { int: G.alpha0, slope: G.beta0, sigma: Math.exp(G.gamma0) };
  const NAME = {
    Accipitriformes: 'Accipitriforms', Strigiformes: 'Strigiforms', Falconiformes: 'Falconiforms', Passeriformes: 'Passeriforms',
    Charadriiformes: 'Charadriiforms', Pelecaniformes: 'Pelecaniforms', 'other Aves': 'Other birds', Crocodylia: 'Crocodylians',
    Caniformia: 'Caniforms', Feliformia: 'Feliforms', Dasyuromorphia: 'Dasyuromorphs', 'other Mammalia': 'Other mammals',
    Serpentes: 'Serpentes', Varanidae: 'Varanids', Teiioidea: 'Teiioids', Iguania: 'Iguanians', 'other Squamata': 'Other squamates'
  };
  const nm = (k) => NAME[k] || k;
  const mx = (m) => (m + Math.log(1000)) / L10;           // model m -> log10 g
  const lg = Math.log10;
  const fossils = X.fossils;
  const ziphodont = new Set(['Varanidae']);               // PLACEHOLDER: JM to supply tooth-form coding

  /* a fossil record as marks: interval cross + dot, or a bracket bar */
  function fossilMark(Pl, r, parent, opts = {}) {
    const g = D.svg('g', { class: 'fade' }, parent);
    const col = opts.color || 'var(--fossil)';
    const x = lg(r.pred), xl = r.pred_lo ? lg(r.pred_lo) : x, xh = r.pred_hi ? lg(r.pred_hi) : x;
    const xc = r.pred ? x : (xl + xh) / 2;
    if (r.pred_lo && r.pred_hi) D.svg('line', { x1: Pl.sx(xl), x2: Pl.sx(xh), y1: 0, y2: 0, stroke: col, 'stroke-width': 2, opacity: 0.7, class: 'hx' }, g);
    let yc;
    if (r.prey_g) {
      yc = lg(r.prey_g);
      if (r.prey_lo && r.prey_hi) D.svg('line', { x1: Pl.sx(xc), x2: Pl.sx(xc), y1: Pl.sy(lg(r.prey_lo)), y2: Pl.sy(lg(r.prey_hi)), stroke: col, 'stroke-width': 2, opacity: 0.7 }, g);
      D.svg('circle', { cx: Pl.sx(xc), cy: Pl.sy(yc), r: opts.r || 9, fill: col, class: 'ring' }, g);
    } else {
      const yl = lg(r.prey_lo), yh = lg(r.prey_hi); yc = (yl + yh) / 2;
      D.svg('rect', { x: Pl.sx(xc) - 5, y: Pl.sy(yh), width: 10, height: Math.max(4, Pl.sy(yl) - Pl.sy(yh)), rx: 4, fill: col, opacity: 0.55 }, g);
    }
    g.querySelectorAll('.hx').forEach((l) => { l.setAttribute('y1', Pl.sy(yc)); l.setAttribute('y2', Pl.sy(yc)); });
    const hit = D.svg('circle', { cx: Pl.sx(xc), cy: Pl.sy(yc), r: 18, fill: 'transparent' }, g);
    hit.addEventListener('pointermove', (e) => D.tip(e, `<b>${r.taxon}</b> ate <b>${r.prey}</b><br>predator ${P.fmt(r.pred)} (${P.fmt(r.pred_lo)}–${P.fmt(r.pred_hi)})<br>prey ${r.prey_g ? P.fmt(r.prey_g) + ' (' : 'between '}${P.fmt(r.prey_lo)}–${P.fmt(r.prey_hi)}${r.prey_g ? ')' : ''}<br>${r.obs.replace(/_/g, ' ')} · ${r.pair_id}`));
    hit.addEventListener('pointerleave', D.untip);
    g.dataset.x = xc; g.dataset.y = yc;
    return g;
  }
  const plotBox = (fr, x = 60, y = 150, w = 1480, h = 710) => { const b = D.el('div', 'plot', fr); Object.assign(b.style, { left: x + 'px', top: y + 'px', width: w + 'px', height: h + 'px' }); return b; };
  const label = (Pl, x, y, txt, cls = 'lbl', anchor = 'start', parent) => { const t = D.svg('text', { x, y, class: cls, 'text-anchor': anchor }, parent || Pl.over); t.textContent = txt; return t; };

  const S = [];

  /* ---------- 0 title ---------- */
  S.push({
    builds: ['Title. Fossil gut contents are the only direct record of what extinct predators ate. Tonight: how to compare them to living animals without pretending one stomach is a diet.'],
    make(fr) { D.el('div', 'titlecard', fr, '<h1>What did theropods <em>eat like</em>?</h1><p>Jonathan Mitchell · Coe College</p>'); }
  });

  /* ---------- 1 one specimen is not a species ---------- */
  // spindles arrive one per click, worst fit to the Compsognathus meal first, best last
  const spinOrder = X.spindles.slice().sort((a, b) => a.comp_fit - b.comp_fit);
  S.push({
    title: 'One gut is <em>one meal</em>',
    anim: Object.fromEntries(spinOrder.map((_, i) => [i + 1, 900])),
    builds: ['Compsognathus with Bavarisaurus in its gut. One predator, one prey, one point: a meal about half a percent of its own mass.']
      .concat(spinOrder.map((sp, i) => `${sp.species}: every recorded meal, ${sp.n} records from ${sp.sources} studies. ` +
        (i === 0 ? 'Of these living predators, this is the worst match to the Compsognathus meal as a share of body mass.'
         : i === spinOrder.length - 1 ? 'The best match of the six. Even so, the fossil is one draw from a spindle like this, and we do not know where in it the draw fell.'
         : `Match to the Compsognathus meal, ${i + 1} of ${spinOrder.length} from worst.`))),
    make(fr) {
      const Pl = P.loglog(plotBox(fr), { x: [1, 5], y: [-2, 6], w: 1480, h: 710 });
      const spin = (sp, half) => {
        const g = D.svg('g', { class: 'fade hidden' }, Pl.g), lab = D.svg('g', { class: 'fade hidden' }, Pl.over);
        const hx = lg(sp.pred_g), col = P.color(sp.major);
        const kd = P.kde(sp.y, 0.14, -2, 6, 160), mxd = Math.max(...kd.map((d) => d[1]));
        const keep = kd.filter((d) => d[1] > mxd * 0.02);
        const R = keep.map(([y, d]) => [hx + half * d / mxd, y]), Lf = keep.map(([y, d]) => [hx - half * d / mxd, y]);
        D.svg('path', { d: P.path(Pl, R) + P.path(Pl, Lf.reverse()).replace(/^M/, 'L') + 'Z', fill: col, opacity: 0.45, stroke: col, 'stroke-width': 2.5 }, g);
        const top = keep[keep.length - 1][0];
        const t = D.svg('text', { x: Pl.sx(hx), y: Pl.sy(top) - 16, 'text-anchor': 'middle', class: 'lbl', 'font-style': 'italic' }, lab); t.textContent = sp.label;
        const hit = D.svg('rect', { x: Pl.sx(hx - half), y: Pl.sy(top), width: Pl.sx(hx + half) - Pl.sx(hx - half), height: Pl.sy(keep[0][0]) - Pl.sy(top), fill: 'transparent' }, g);
        hit.addEventListener('pointermove', (e) => D.tip(e, `<b><i>${sp.species}</i></b><br>${sp.n} prey records, ${sp.sources} studies<br>predator ${P.fmt(sp.pred_g)}`));
        hit.addEventListener('pointerleave', D.untip);
        return [g, lab];
      };
      const layers = spinOrder.map((x) => spin(x, 0.17));
      const comp = fossils.find((r) => r.pair_id === 'Conrad2017_01');
      fossilMark(Pl, comp, Pl.over, { r: 13 });
      label(Pl, Pl.sx(lg(comp.pred)) + 24, Pl.sy(lg(comp.prey_g)) + 46, 'Compsognathus', 'lbl', 'start');
      return { show(b) {
        layers.forEach((els, i) => els.forEach((e) => e.classList.toggle('hidden', b < i + 1)));
      } };
    }
  });

  /* ---------- 2 borrowing strength ---------- */
  S.push({
    title: 'Borrowing from relatives is <em>pooling</em>',
    anim: { 1: 2600 },
    builds: [
      'Three clades on their own. Crocodylians: six species. Iguanians: 47 records. Strigiforms: 29,448 records. Each line is what the clade says about itself alone. The dashed line is all tetrapods together.',
      'Now let each clade borrow from the rest. Thin clades slide toward the shared line, because their own data cannot hold them far from it. Strigiforms barely move: 29,000 records do not need help. Nobody estimates Scipionyx from one hatchling alone; this is that reasoning written down.'
    ],
    make(fr) {
      const Pl = P.loglog(plotBox(fr), { x: [1, 6], y: [-1, 5], w: 1480, h: 710 });
      const xr = [1, 6];
      D.svg('path', { d: P.path(Pl, P.linePts(global, ...xr)), class: 'guide', 'stroke-width': 3 }, Pl.g);
      label(Pl, Pl.sx(5.9), Pl.sy(P.lineY(global, 5.9)) + 34, 'All tetrapods', 'lbl muted', 'end');
      const show = ['Crocodylia', 'Iguania', 'Strigiformes'];
      const items = show.map((k) => {
        const c = counts[k], xa = mx(c.m_lo), xb = mx(c.m_hi);
        X.points.filter((p) => p.k === k).forEach((p) => D.svg('circle', { cx: Pl.sx(p.x), cy: Pl.sy(p.y), r: 5, fill: P.color(c.major), opacity: 0.3 }, Pl.g));
        const path = D.svg('path', { fill: 'none', stroke: P.color(c.major), 'stroke-width': 5, 'stroke-linecap': 'round' }, Pl.g);
        const n = k === 'Crocodylia' ? `${c.species} species` : `${c.rows.toLocaleString()} records`;
        const t = label(Pl, 0, 0, `${nm(k)} · ${n}`);
        return { k, xa, xb, path, t, a: F.unpooled[k], z: F.primary[k] };
      });
      let stop = [];
      const draw = (k) => items.forEach((it) => {
        const c = { int: it.a.int + (it.z.int - it.a.int) * k, slope: it.a.slope + (it.z.slope - it.a.slope) * k };
        it.path.setAttribute('d', P.path(Pl, P.linePts(c, it.xa, it.xb)));
        it.t.setAttribute('x', Pl.sx(it.xb) + 12); it.t.setAttribute('y', Pl.sy(P.lineY(c, it.xb)) + 8);
      });
      return { show(b, how) {
        stop.forEach((f) => f()); stop = [];
        if (b === 0) draw(0);
        else if (how === 'step') stop.push(D.tween(2200, draw)); else draw(1);
      } };
    }
  });

  /* ---------- 3 three rules ---------- */
  S.push({
    title: 'The names are <em>rules for whom to borrow from</em>',
    anim: { 1: 1800, 2: 1800 },
        builds: [
      'Iguanians on the right: 22 species, 47 records. On the left, the clades we can borrow from. Rule one: borrow from nobody. The line is the iguanians alone. (No pooling.)',
      'Rule two: borrow from everybody equally. Every clade counts the same, birds and snakes and cats alike. In comparative-methods language this is white noise around a shared mean, or an Ornstein-Uhlenbeck model with one optimum and strong pull.',
      'Rule three: borrow from relatives, more from closer ones. Other squamates count most for an iguanian, archosaurs less, mammals not at all. That is Brownian motion.'
    ],
    make(fr) {
      // tree
      const tbox = plotBox(fr, 60, 150, 600, 700);
      const svg = D.svg('svg', { width: 600, height: 700 }, tbox);
      const tree = P.newick(X.skeleton.newick), tips = P.tips(tree), H = 640, top = 30;
      const maxd = Math.max(...tips.map((t) => t.depth)), sx = (d) => 10 + d / maxd * 330;
      tips.forEach((t, i) => { t.y = top + i * H / (tips.length - 1); });
      const setY = (n) => { if (n.kids.length) { n.kids.forEach(setY); n.y = (n.kids[0].y + n.kids[n.kids.length - 1].y) / 2; } };
      setY(tree);
      const edges = D.svg('g', {}, svg);
      const drawN = (n, pd) => {
        D.svg('path', { d: `M${sx(pd)},${n.y}H${sx(n.depth)}`, stroke: 'var(--rule)', 'stroke-width': 4, fill: 'none' }, edges);
        if (n.kids.length) { D.svg('path', { d: `M${sx(n.depth)},${n.kids[0].y}V${n.kids[n.kids.length - 1].y}`, stroke: 'var(--rule)', 'stroke-width': 4 }, edges); n.kids.forEach((k) => drawN(k, n.depth)); }
      };
      drawN(tree, 0);
      const corr = Object.fromEntries(X.skeleton.corr.map((r) => [r.minor_clade, r]));
      const tipEls = tips.map((t) => {
        const c = counts[t.name] || {};
        const g = D.svg('g', { class: 'fade' }, svg);
        D.svg('circle', { cx: sx(t.depth), cy: t.y, r: 11, fill: P.color(c.major) }, g);
        const tx = D.svg('text', { x: sx(t.depth) + 18, y: t.y + 10, class: 'lbl small' }, g); tx.textContent = nm(t.name);
        return { t, g };
      });
      const ruleT = D.svg('text', { x: 10, y: 10, class: 'lbl' }, svg);
      // croc panel
      const THIN = 'Iguania';
      const Pl = P.loglog(plotBox(fr, 700, 150, 840, 710), { x: [0.5, 3.5], y: [-2, 3], w: 840, h: 710 });
      const c = counts[THIN], xa = mx(c.m_lo), xb = mx(c.m_hi);
      D.svg('path', { d: P.path(Pl, P.linePts(global, 0.5, 3.5)), class: 'guide', 'stroke-width': 3 }, Pl.g);
      X.points.filter((p) => p.k === THIN).forEach((p) => D.svg('circle', { cx: Pl.sx(p.x), cy: Pl.sy(p.y), r: 8, fill: 'var(--squamata)', opacity: 0.5 }, Pl.g));
      const line = D.svg('path', { fill: 'none', stroke: 'var(--squamata)', 'stroke-width': 6, 'stroke-linecap': 'round' }, Pl.g);
      const tag = label(Pl, Pl.left + Pl.pw - 20, Pl.top + Pl.ph - 28, '', 'lbl', 'end');
      const rules = [
        { name: 'Nobody', model: 'no pooling', fit: F.unpooled, w: (k) => k === THIN ? 1 : 0.12 },
        { name: 'Everybody, equally', model: 'λ = 0 · white noise / strong OU', fit: F.lambda0, w: () => 1 },
        { name: 'Relatives, closer more', model: 'λ = 1 · Brownian motion', fit: F.lambda1,
          w: (k) => k === THIN ? 1 : 0.12 + 0.88 * Math.min(1, (corr[THIN][k] || 0) / 0.5) }
      ];
      let cur = null, stop = () => {};
      const setLine = (cc) => line.setAttribute('d', P.path(Pl, P.linePts(cc, xa, xb)));
      return { show(b, how) {
        const r = rules[b];
        ruleT.textContent = r.name; tag.textContent = r.model;
        tipEls.forEach((e) => e.g.style.opacity = r.w(e.t.name));
        const to = r.fit[THIN]; stop();
        if (cur && how === 'step') { const from = cur; stop = D.tween(1400, (k) => setLine({ int: from.int + (to.int - from.int) * k, slope: from.slope + (to.slope - from.slope) * k })); }
        else setLine(to);
        cur = to;
      } };
    }
  });

  /* ---------- 4 tree prediction for theropods ---------- */
  const theroRecs = fossils.filter((r) => r.group === 'Theropoda');
  const treeLines = (cand) => X.tree.lines.filter((l) => l.candidate === cand);
  S.push({
    title: 'The tree <em>predicts</em> first',
    anim: { 1: 900, 2: 900 },
    builds: [
      'Before any fossil: theropods sit on the bird stem, between crocodylians and birds. The tree turns that position into a prediction.',
      'This band is where a new theropod lineage should fall, borrowing from birds and crocodylians by shared history. It is wide, and that is the honest answer: close relatives in the living data scale differently, so the tree cannot narrow it much.',
      'Now the fossils: every non-avian theropod gut with a prey mass or bracket. Bars are prey known only to within a bracket.'
    ],
    make(fr) {
      const Pl = P.loglog(plotBox(fr), { x: [1.5, 7], y: [-3, 6], w: 1480, h: 710 });
      P.ratioGuides(Pl, [1, 0.1, 0.01, 0.001]);
      const tl = treeLines(X.tree.theropod_tip);
      const band = D.svg('g', { class: 'fade hidden' }, Pl.g);
      if (tl.length) {
        const lo = tl.map((l) => [lg(l.pred_g), lg(l.pred_q10)]), hi = tl.map((l) => [lg(l.pred_g), lg(l.pred_q90)]);
        D.svg('path', { d: P.band(Pl, lo, hi), fill: 'var(--accent-soft)', stroke: 'var(--accent-ink)', 'stroke-width': 1.5, 'stroke-dasharray': '4 6' }, band);
        D.svg('path', { d: P.path(Pl, tl.map((l) => [lg(l.pred_g), lg(l.line_med)])), fill: 'none', stroke: 'var(--accent-ink)', 'stroke-width': 4 }, band);
      }
      const pts = D.svg('g', {}, Pl.g);
      const marks = theroRecs.map((r) => fossilMark(Pl, r, pts));
      marks.forEach((m) => m.classList.add('hidden'));
      return { show(b) {
        band.classList.toggle('hidden', b < 1);
        marks.forEach((m) => m.classList.toggle('hidden', b < 2));       // all at once; specimens get names on the next slide
      } };
    }
  });

  /* ---------- 5 specific animals ---------- */
  const cases = [
    { title: '<em>Microraptor</em>: one genus, four kinds of prey', ids: ['OConnor2011_01', 'Xing2013_01', 'Hone2022_01', 'OConnor2019_01'], labels: ['bird', 'fish', 'mammal', 'lizard'],
      note: 'Microraptor: four gut records from one genus, a bird, a fish, a mammal and a lizard. The best fossil case for diet breadth: the spread of these four is the closest thing we have to a fossil spindle.' },
    { title: 'Juvenile <em>Gorgosaurus</em>: prey about 3% of its mass', ids: ['Therrien2023_01', 'Therrien2023_02'], labels: ['Citipes', 'Citipes'], guide: 0.03,
      note: 'A juvenile Gorgosaurus with two young Citipes in its gut. Each is about 3% of the predator’s mass. Juvenile tyrannosaurs were not eating like adults.' },
    { title: '<em>Sinocalliopteryx</em>: about 8%', ids: ['Xing2012_01'], labels: ['Sinornithosaurus'], guide: 0.08,
      note: 'Sinocalliopteryx with a Sinornithosaurus leg in its gut: a dromaeosaur about 8% of its mass.' },
    { title: '<em>Velociraptor</em>: a fight, not a meal', ids: ['Carpenter1998_01'], labels: ['Protoceratops'], guide: 1,
      note: 'Velociraptor locked with Protoceratops. The prey is heavier than the predator. This is a fight, not a meal, and it goes in the model as a direct observation, not a stomach.' }
  ];
  S.push({
    title: cases[0].title,
    builds: cases.map((c) => c.note),
    make(fr) {
      const h2 = fr.querySelector('h2');
      const Pl = P.loglog(plotBox(fr), { x: [1.5, 7], y: [-3, 6], w: 1480, h: 710 });
      P.ratioGuides(Pl, [1, 0.1, 0.01, 0.001]);
      const tl = treeLines(X.tree.theropod_tip);
      if (tl.length) D.svg('path', { d: P.band(Pl, tl.map((l) => [lg(l.pred_g), lg(l.pred_q10)]), tl.map((l) => [lg(l.pred_g), lg(l.pred_q90)])), fill: 'var(--accent-soft)' }, Pl.g);
      const bg = D.svg('g', { opacity: 0.25 }, Pl.g);
      theroRecs.forEach((r) => fossilMark(Pl, r, bg, { color: 'var(--muted)', r: 6 }));
      const layers = cases.map((c) => {
        const g = D.svg('g', { class: 'fade hidden' }, Pl.g), lab = D.svg('g', { class: 'fade hidden' }, Pl.over);
        if (c.guide) {
          const lr = lg(c.guide);
          D.svg('path', { d: P.path(Pl, [[1.5, 1.5 + lr], [7, 7 + lr]]), stroke: 'var(--accent-ink)', 'stroke-width': 2.5, fill: 'none', 'stroke-dasharray': '10 6' }, g);
        }
        c.ids.forEach((id, i) => {
          const r = fossils.find((f) => f.pair_id === id); if (!r) return;
          const m = fossilMark(Pl, r, g, { r: 12 });
          label(Pl, Pl.sx(+m.dataset.x) + 20, Pl.sy(+m.dataset.y) + (i % 2 ? 34 : -14), c.labels[i], 'lbl', 'start', lab);
        });
        return [g, lab];
      });
      return { show(b) {
        h2.innerHTML = cases[b].title;
        layers.forEach(([g, l], i) => { g.classList.toggle('hidden', i !== b); l.classList.toggle('hidden', i !== b); });
      } };
    }
  });

  /* ---------- 5b every living spindle, theropods on top ---------- */
  S.push({
    title: 'Theropods against <em>every living diet</em>',
    anim: { 1: 900 },
    builds: [
      `Every living predator species with at least ${X.allspin.min_records} vertebrate prey records: ${X.allspin.species.length} spindles. Colour is the major clade: birds, mammals, squamates, and the one crocodylian with enough records. Snakes sit low and flat; cats climb steeply; owls are narrow; canids are wide.`,
      'And the theropod gut records on top. They fall inside the living cloud, mostly in its lower half for their size, and no single colour owns them.'
    ],
    make(fr) {
      const Pl = P.loglog(plotBox(fr, 60, 150, 1130, 710), { x: [1, 7], y: [-3, 6], w: 1130, h: 710 });
      P.ratioGuides(Pl, [1, 0.01]);
      const shade = {}; X.allspin.groups.forEach((g) => { shade[g.group] = P.color(g.major); });   // one colour per major clade
      const half = 0.11, step = X.allspin.step;
      const cloud = D.svg('g', {}, Pl.g);
      X.allspin.species.forEach((sp) => {
        const R = sp.d.map((d, i) => [sp.x + half * d, sp.y0 + i * step]), L = sp.d.map((d, i) => [sp.x - half * d, sp.y0 + i * step]);
        const path = D.svg('path', { d: P.path(Pl, R) + P.path(Pl, L.reverse()).replace(/^M/, 'L') + 'Z',
          fill: shade[sp.group], 'fill-opacity': 0.28, stroke: shade[sp.group], 'stroke-opacity': 0.7, 'stroke-width': 1.5 }, cloud);
        path.addEventListener('pointermove', (e) => D.tip(e, `<b><i>${sp.sp.replace(/_/g, ' ')}</i></b><br>${nm(sp.group.replace(/^other (.*)$/, 'other $1'))} · ${sp.n} records`));
        path.addEventListener('pointerleave', D.untip);
      });
      const pts = D.svg('g', { class: 'fade hidden' }, Pl.g);
      theroRecs.forEach((r) => fossilMark(Pl, r, pts, { r: 10 }));
      // legend: major clades
      const lg_ = D.svg('svg', { width: 330, height: 710 }, plotBox(fr, 1215, 160, 330, 710));
      let y = 40;
      [['Aves', 'Birds'], ['Mammalia', 'Mammals'], ['Squamata', 'Squamates'], ['Crocodylia', 'Crocodylians']].forEach(([maj, lab]) => {
        D.svg('rect', { x: 0, y: y - 22, width: 28, height: 28, rx: 5, fill: P.color(maj), 'fill-opacity': 0.7 }, lg_);
        const t = D.svg('text', { x: 42, y: y, class: 'lbl small' }, lg_); t.textContent = lab;
        y += 50;
      });
      y += 10;
      D.svg('circle', { cx: 13, cy: y - 7, r: 10, fill: 'var(--fossil)', class: 'ring' }, lg_);
      const ft = D.svg('text', { x: 38, y: y, class: 'lbl small' }, lg_); ft.textContent = 'Theropods';
      return { show(b) { pts.classList.toggle('hidden', b < 1); } };
    }
  });

  /* ---------- 5c fits along the trees ---------- */
  function drawTree(svg, newick, x0, w, top, h, opts = {}) {
    const tree = P.newick(newick), tips = P.tips(tree);
    if (opts.clado) {                                     // equal steps per node level, tips aligned
      const ht = (n) => (n.h = n.kids.length ? 1 + Math.max(...n.kids.map(ht)) : 0);
      ht(tree); const H = tree.h, set = (n) => { n.depth = H - n.h; n.kids.forEach(set); }; set(tree);
      tips.forEach((t) => { t.depth = H; });
    }
    const maxd = Math.max(...tips.map((t) => t.depth)), sx = (d) => x0 + d / maxd * w;
    tips.forEach((t, i) => { t.y = top + (tips.length === 1 ? h / 2 : i * h / (tips.length - 1)); });
    const setY = (n) => { if (n.kids.length) { n.kids.forEach(setY); n.y = (n.kids[0].y + n.kids[n.kids.length - 1].y) / 2; } };
    setY(tree);
    const g = D.svg('g', {}, svg);
    const drawN = (n, pd) => {
      D.svg('path', { d: `M${sx(pd)},${n.y}H${sx(n.depth)}`, stroke: opts.stroke || 'var(--rule)', 'stroke-width': 4, fill: 'none' }, g);
      if (n.kids.length) { D.svg('path', { d: `M${sx(n.depth)},${n.kids[0].y}V${n.kids[n.kids.length - 1].y}`, stroke: opts.stroke || 'var(--rule)', 'stroke-width': 4 }, g); n.kids.forEach((k) => drawN(k, n.depth)); }
    };
    drawN(tree, 0);
    tips.forEach((t) => { t.x = sx(t.depth); });
    return tips;
  }
  S.push({
    title: 'Fits along the tree, <em>theropods</em> beside them',
    anim: { 1: 900 },
    builds: [
      'Each living group at its typical size: the dot is the median meal as a share of predator mass, the bar is where 80 percent of a new species\' meals would fall. Colour is the major clade. Pale with a triangle: a group whose scaling clearly differs from its clade. Hawks and cats steeper, owls and snakes shallower.',
      'The theropods on their own dated tree, every gut record as a share of its predator. Tyrannosaurids, compsognathids, paravians. They spread across the same range as the living groups, and the spread within one genus, Microraptor, is as wide as a whole living clade.'
    ],
    make(fr) {
      const TF = X.treefits, ext = Object.fromEntries(TF.extant.map((r) => [r.minor, r]));
      const rx = [-2, 3], tick = [-2, -1, 0, 1, 2, 3], pct = (v) => { const x = Math.pow(10, v); return x >= 1 ? x.toLocaleString() + '%' : (+x.toPrecision(1)) + '%'; };
      const panel = (x, w) => { const box = plotBox(fr, x, 150, w, 720); return D.svg('svg', { width: w, height: 720 }, box); };
      const axis = (svg, x0, x1, bottom, title) => {
        const sx = (v) => x0 + (v - rx[0]) / (rx[1] - rx[0]) * (x1 - x0);
        tick.forEach((t) => {
          D.svg('line', { x1: sx(t), x2: sx(t), y1: 10, y2: bottom, class: 'gridline' }, svg);
          D.svg('line', { x1: sx(t), x2: sx(t), y1: bottom, y2: bottom + 10, class: 'tick', stroke: 'var(--muted)', 'stroke-width': 2.5 }, svg);
          if (t % 2 === 0) { const tx = D.svg('text', { x: sx(t), y: bottom + 40, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = pct(t); }
        });
        D.svg('line', { x1: x0, x2: x1, y1: bottom, y2: bottom, stroke: 'var(--muted)', 'stroke-width': 2.5 }, svg);
        const at = D.svg('text', { x: (x0 + x1) / 2, y: bottom + 82, 'text-anchor': 'middle', class: 'lbl' }, svg); at.textContent = title;
        return sx;
      };
      // extant
      const L = panel(30, 790), bottom = 600;
      const tipsL = drawTree(L, X.skeleton.newick, 0, 150, 20, bottom - 40);
      const sxL = axis(L, 450, 780, bottom, 'Meal as % of predator');
      tipsL.forEach((t) => {
        const r = ext[t.name]; if (!r) return;
        const base = P.color(r.major), col = r.differs ? `color-mix(in oklab, ${base} 50%, white)` : base;
        const lab = D.svg('text', { x: t.x + 14, y: t.y + 10, class: 'lbl small' }, L);
        lab.textContent = nm(t.name) + (r.differs === 'steeper' ? ' ▲' : r.differs === 'shallower' ? ' ▼' : '');
        if (r.differs) lab.setAttribute('fill', 'var(--ink)');
        const g = D.svg('g', {}, L);
        D.svg('line', { x1: sxL(r.r10), x2: sxL(r.r90), y1: t.y, y2: t.y, stroke: col, 'stroke-width': 7, 'stroke-linecap': 'round', opacity: 0.85 }, g);
        D.svg('circle', { cx: sxL(r.r50), cy: t.y, r: 10, fill: col, stroke: r.differs ? 'var(--ink)' : 'var(--board)', 'stroke-width': r.differs ? 2.5 : 2 }, g);
        g.addEventListener('pointermove', (e) => D.tip(e, `<b>${nm(t.name)}</b> (${r.species} species)<br>at ${P.fmt(r.pred_g)}: median meal ${pct(r.r50)} of predator<br>slope ${r.slope} vs ${r.major} ${r.major_slope}${r.differs ? ' · ' + r.differs : ''}`));
        g.addEventListener('pointerleave', D.untip);
      });
      // theropods
      const Rg = D.svg('g', { class: 'fade hidden' }, panel(830, 740));
      const R = Rg.ownerSVGElement;
      const tipsR = drawTree(Rg, TF.theropod_tree.newick, 0, 140, 20, bottom - 40, { stroke: 'var(--fossil)', clado: true });
      const genusCount = {}; tipsR.forEach((t) => { const g = t.name.split(' ')[0]; genusCount[g] = (genusCount[g] || 0) + 1; });
      const sxR = axis(Rg, 410, 730, bottom, 'Meal as % of predator');
      tipsR.forEach((t) => {
        const id = t.name.replace(/ /g, '_');
        const lab = D.svg('text', { x: t.x + 14, y: t.y + 10, class: 'lbl small', 'font-style': 'italic' }, Rg);
        const genus = t.name.split(' ')[0];
        lab.textContent = genusCount[genus] > 1 ? t.name.replace(/^(\w)\w+ (\w+)$/, '$1. $2') : genus;   // genus unless it has several species here
        fossils.filter((f) => f.group === 'Theropoda' && f.taxon === t.name).forEach((f, k) => {
          const pr = f.pred || Math.sqrt(f.pred_lo * f.pred_hi);
          const yy = t.y + (k % 2 ? 7 : -7) * (k > 0 ? 1 : 0);
          if (f.prey_g) {
            const c = D.svg('circle', { cx: sxR(Math.log10(f.prey_g / pr * 100)), cy: yy, r: 9, fill: 'var(--fossil)', class: 'ring' }, Rg);
            c.addEventListener('pointermove', (e) => D.tip(e, `<b>${f.taxon}</b> ate <b>${f.prey}</b><br>${(f.prey_g / pr * 100).toPrecision(2)}% of predator mass`));
            c.addEventListener('pointerleave', D.untip);
          } else {
            D.svg('line', { x1: sxR(Math.log10(f.prey_lo / pr * 100)), x2: sxR(Math.log10(f.prey_hi / pr * 100)), y1: yy, y2: yy,
              stroke: 'var(--fossil)', 'stroke-width': 8, 'stroke-linecap': 'round', opacity: 0.6 }, Rg);
          }
        });
      });
      return { show(b) { Rg.classList.toggle('hidden', b < 1); } };
    }
  });

  /* ---------- 6 which living animals ---------- */
  const sup = (unit, cmp) => (X.support ? X.support.rows : []).filter((r) => r.unit === unit && r.comparison === cmp);
  S.push({
    title: 'Which living animals do they <em>eat like</em>?',
    builds: [
      'For all theropods, then each theropod clade: support for each living group as the source of the records, and for the dated dinosaur tree, all given equal odds beforehand. The dashed line is "cannot tell": every option equal. Paravians lean toward the tree; compsognathids toward crocodylians and squamates; tyrannosaurids, four records, are flat. None of these is far from the line. The model is telling us how little these records can say, which a regression line through eleven taxa never would.'
    ],
    make(fr) {
      const units = [['Theropoda', 'All theropods'], ['Theropoda:Tyrannosauridae', 'Tyrannosaurids'],
                     ['Theropoda:Compsognathidae', 'Compsognathids'], ['Theropoda:Paraves', 'Paravians']];
      const order = [['major:Aves', 'Aves', 'aves'], ['major:Mammalia', 'Mamm.', 'mammalia'], ['major:Squamata', 'Squam.', 'squamata'],
                     ['major:Crocodylia', 'Croc.', 'crocodylia'], ['none', 'None', null], ['tree:dated', 'Tree', 'fossil']];
      const softmax = (v) => { const m = Math.max(...v), e = v.map((x) => Math.exp(x - m)), t = e.reduce((a, b) => a + b, 0); return e.map((x) => x / t); };
      const panels = units.map(([u, name], ui) => {
        const rows = sup(u, 'dated_tree_vs_major_vs_none');
        const lp = order.map(([c]) => { const r = rows.find((x) => x.candidate === c); return r ? r.lpd : -Infinity; });
        const p = softmax(lp);
        const n = (rows[0] || {}).n_records;
        return { name, p };
      });
      const top = Math.max(0.5, ...panels.flatMap((x) => x.p));                // shared, honest scale
      panels.forEach(({ name, p }, ui) => {
        const box = plotBox(fr, 110 + ui * 362, 160, 350, 700), svg = D.svg('svg', { width: 350, height: 700 }, box);
        for (let v = 0; v <= top + 1e-9; v += 0.25) {
          const yy = 600 - v / top * 500;
          D.svg('line', { x1: 0, x2: 350, y1: yy, y2: yy, class: 'gridline' }, svg);
          if (ui === 0) { const tl = D.svg('text', { x: -12, y: yy + 10, 'text-anchor': 'end', class: 'lbl small' }, svg); tl.textContent = Math.round(v * 100) + '%'; }
        }
        const t = D.svg('text', { x: 175, y: 30, 'text-anchor': 'middle', class: 'lbl' }, svg); t.textContent = name;
        const base = 600, sh = 500, bw = 46, gap = 10, x0 = 175 - (order.length * (bw + gap) - gap) / 2;
        const yeq = base - (1 / order.length) / top * sh;
        const ord = order.map((o, i) => [o, p[i]]).sort((a, b) => b[1] - a[1]);      // most to least supported
        ord.forEach(([[c, lab, col], pv], i) => {
          const x = x0 + i * (bw + gap), h = pv / top * sh;
          const bar = D.svg('rect', { x, y: base - h, width: bw, height: Math.max(2, h), rx: 4, fill: col ? `var(--${col})` : 'var(--muted)' }, svg);
          bar.addEventListener('pointermove', (e) => D.tip(e, `<b>${name}</b> · ${lab}<br>support ${(pv * 100).toFixed(0)}%`));
          bar.addEventListener('pointerleave', D.untip);
          const tx = D.svg('text', { x: x + bw / 2, y: base + 16, class: 'lbl small', 'text-anchor': 'end',
            transform: `rotate(-50 ${x + bw / 2} ${base + 16})` }, svg); tx.textContent = lab;
        });
        D.svg('line', { x1: x0 - 8, x2: x0 + order.length * (bw + gap), y1: yeq, y2: yeq, class: 'guide', 'stroke-width': 2.5 }, svg);
        if (ui === 3) { const ct = D.svg('text', { x: x0 + order.length * (bw + gap), y: yeq - 10, 'text-anchor': 'end', class: 'guide-lbl' }, svg); ct.textContent = 'cannot tell'; }
      });
    }
  });

  /* ---------- 7 relatives disagree ---------- */
  S.push({
    title: 'Close relatives <em>scale differently</em>',
    builds: [
      'Slope of prey mass on predator mass for each living group. Feliforms: prey mass rises faster than predator mass. Caniforms: about in step. Sister groups, different answers.',
      'Accipitriforms against strigiforms: the same split inside birds. The model found this from data. It did not assume the tree was right, and it is why the tree cannot predict theropods tightly.'
    ],
    make(fr) {
      const box = plotBox(fr), W = 1480, Hh = 710, svg = D.svg('svg', { width: W, height: Hh }, box);
      const rows = X.fits.primary.clades.filter((c) => counts[c.minor]).map((c) => Object.assign({ major: counts[c.minor].major }, c))
        .sort((a, b) => a.major.localeCompare(b.major) || b.slope - a.slope);
      const left = 300, pw = W - left - 80, x0 = 0, x1 = 1.6, sx = (v) => left + (v - x0) / (x1 - x0) * pw, bh = (Hh - 100) / rows.length;
      for (let t = 0; t <= 1.6; t += 0.4) { D.svg('line', { x1: sx(t), x2: sx(t), y1: 0, y2: rows.length * bh, class: 'gridline' }, svg); const tx = D.svg('text', { x: sx(t), y: rows.length * bh + 40, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = t.toFixed(1); }
      D.svg('line', { x1: sx(1), x2: sx(1), y1: 0, y2: rows.length * bh, class: 'guide', 'stroke-width': 2.5 }, svg);
      const xt = D.svg('text', { x: left + pw / 2, y: Hh - 4, 'text-anchor': 'middle', class: 'lbl small' }, svg); xt.textContent = 'How fast meals grow as predators grow';
      const st = D.svg('text', { x: sx(1), y: -8, 'text-anchor': 'middle', class: 'guide-lbl' }, svg); st.textContent = 'in step';
      const els = rows.map((c, i) => {
        const y = i * bh + bh / 2, g = D.svg('g', { class: 'fade' }, svg);
        D.svg('line', { x1: sx(c.slope_lo), x2: sx(c.slope_hi), y1: y, y2: y, stroke: P.color(c.major), 'stroke-width': 4, 'stroke-linecap': 'round' }, g);
        D.svg('circle', { cx: sx(c.slope), cy: y, r: 9, fill: P.color(c.major), class: 'ring' }, g);
        const t = D.svg('text', { x: left - 14, y: y + 10, 'text-anchor': 'end', class: 'lbl small' }, g); t.textContent = nm(c.minor);
        const v = D.svg('text', { x: sx(c.slope_hi) + 12, y: y + 10, class: 'lbl small' }, g); v.textContent = c.slope.toFixed(2);
        return { c, g, v };
      });
      const hi = [['Feliformia', 'Caniformia'], ['Accipitriformes', 'Strigiformes']];
      return { show(b) { els.forEach((e) => { const on = hi[b].includes(e.c.minor); e.g.style.opacity = on ? 1 : 0.25; e.v.style.display = on ? '' : 'none'; }); } };
    }
  });

  /* ---------- 8 known answers ---------- */
  S.push({
    title: 'Does it get <em>known answers</em> right?',
    builds: [
      'Positive controls: fossils whose living group we know. Repenomamus should look like a mammal, the two crocodyliforms like crocodylians, Sanajeh like a squamate. Outlined bar is the right answer. With one or two records each, if these come out flat, the method is honest about thin data; if they come out wrong, I say so.'
    ],
    make(fr) {
      const units = [['Repenomamus_robustus', 'Repenomamus', 'Mammalia'], ['Crocodyliformes', 'Crocodyliforms', 'Crocodylia'], ['Sanajeh_indicus', 'Sanajeh', 'Squamata']];
      const order = ['Aves', 'Mammalia', 'Squamata', 'Crocodylia', 'none'];
      units.forEach(([u, title, right], ui) => {
        const box = plotBox(fr, 60 + ui * 500, 170, 460, 680), svg = D.svg('svg', { width: 460, height: 680 }, box);
        const t = D.svg('text', { x: 230, y: 30, 'text-anchor': 'middle', class: 'lbl' }, svg); t.textContent = title;
        const rows = sup(u, 'major_vs_none'), base = 600, sh = 520, top = Math.max(0.4, ...rows.map((r) => r.rel));
        const yeq = base - (1 / 5) / top * sh;
        D.svg('line', { x1: 20, x2: 440, y1: yeq, y2: yeq, class: 'guide', 'stroke-width': 2.5 }, svg);
        if (ui === 2) {
          D.svg('line', { x1: 300, x2: 340, y1: base + 76, y2: base + 76, class: 'guide', 'stroke-width': 2.5 }, svg);
          const ct = D.svg('text', { x: 350, y: base + 83, class: 'guide-lbl' }, svg); ct.textContent = 'cannot tell';
        }
        order.forEach((o, i) => {
          const r = rows.find((x) => x.candidate === (o === 'none' ? 'none' : 'major:' + o)); if (!r) return;
          const x = 30 + i * 82, h = r.rel / top * sh;
          D.svg('rect', { x, y: base - h, width: 66, height: h, rx: 4, fill: o === 'none' ? 'var(--muted)' : P.color(o),
            stroke: o === right ? 'var(--ink)' : 'none', 'stroke-width': 4 }, svg);
          const tx = D.svg('text', { x: x + 33, y: base + 40, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = { Aves: 'Aves', Mammalia: 'Mamm.', Squamata: 'Squam.', Crocodylia: 'Croc.', none: 'None' }[o];
          const v = D.svg('text', { x: x + 33, y: base - h - 10, 'text-anchor': 'middle', class: 'lbl small' }, svg); v.textContent = Math.round(r.rel * 100) + '%';
        });
      });
    }
  });

  /* ---------- 9 thanks ---------- */
  S.push({
    title: 'Thanks',
    builds: ['Acknowledgements.'],
    make(fr) {
      D.el('div', 'thanks', fr, '<p><b>NSF</b></p><p><b>Coe College</b></p><p>Faith Hysell</p><p>Mike Grundler · SquamataBase</p><p>OS-Prey · CarniDIET · GATEWAy · AVONET</p><p>PhyloPic</p>');
    }
  });

  root.TALK = S;
})(window);
