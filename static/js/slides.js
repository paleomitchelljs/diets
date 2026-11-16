/* The talk: one entry per slide. builds[] are the speaker notes, one per press.
 * titles[] (optional) swaps the heading per build.
 * anim: { build: ms } marks builds whose arrival animates; tools/export records them as GIFs.
 * All numbers come from data/data.js (code/site_export.R). */
(function (root) {
  'use strict';
  const D = root.Deck, P = root.Plot, X = root.DATA;
  const L10 = Math.LN10;
  const counts = Object.fromEntries(X.counts.map((c) => [c.minor, c]));
  const byMinor = (fit) => fit ? Object.fromEntries(fit.clades.map((c) => [c.minor, c])) : {};
  const F = { primary: byMinor(X.fits.primary), lambda0: byMinor(X.fits.lambda0), lambda1: byMinor(X.fits.lambda1), unpooled: byMinor(X.fits.unpooled) };
  const glob = (fit) => { const g0 = fit.global, g = Array.isArray(g0) ? g0[0] : g0; return { int: g.alpha0, slope: g.beta0, sigma: Math.exp(g.gamma0), raw: g }; };
  const global = glob(X.fits.primary);
  const NAME = {
    Accipitriformes: 'Accipitriforms', Strigiformes: 'Strigiforms', Falconiformes: 'Falconiforms', Passeriformes: 'Passeriforms',
    Charadriiformes: 'Charadriiforms', Pelecaniformes: 'Pelecaniforms', 'other Aves': 'Other Aves', Crocodylia: 'Crocodylians',
    Caniformia: 'Caniforms', Feliformia: 'Feliforms', Dasyuromorphia: 'Dasyuromorphs', 'other Mammalia': 'Other Mammalia',
    Serpentes: 'Serpentes', Varanidae: 'Varanids', Teiioidea: 'Teiioids', Iguania: 'Iguanians', 'other Squamata': 'Other Squamata'
  };
  const nm = (k) => NAME[k] || k;
  const mx = (m) => (m + Math.log(1000)) / L10;           // model m -> log10 g
  const lg = Math.log10;
  const fossils = X.fossils;
  const theroRecs = fossils.filter((r) => r.group === 'Theropoda');
  const nSpecies = X.counts.reduce((a, c) => a + c.species, 0);
  const ext = Object.fromEntries(X.treefits.extant.map((r) => [r.minor, r]));

  /* ---------- colour ---------- */
  // every minor clade its own colour: a hue family per major clade, lightness alternating down the tree
  const tipOrder = P.tips(P.newick(X.skeleton.newick)).map((t) => t.name);
  const HUE = { Aves: [250, 90], Mammalia: [70, 60], Squamata: [155, 70], Crocodylia: [335, 0] };
  const minorCol = {};
  Object.keys(HUE).forEach((maj) => {
    const ks = tipOrder.filter((k) => counts[k] && counts[k].major === maj);
    ks.forEach((k, i) => {
      const [h, s] = HUE[maj], hh = ks.length > 1 ? h + s * (i / (ks.length - 1) - 0.5) : h;
      minorCol[k] = `oklch(${i % 2 ? 0.84 : 0.64} 0.15 ${hh.toFixed(0)})`;
    });
  });
  // minor clades whose slope clearly differs from their major clade's line get a colour of their own
  const OWN = ['var(--own1)', 'var(--own2)', 'var(--own3)', 'var(--own4)', 'var(--own5)', 'var(--own6)'];
  const ownCol = {};
  tipOrder.filter((k) => ext[k] && ext[k].differs).forEach((k, i) => { ownCol[k] = OWN[i % OWN.length]; });
  const hierCol = (k) => ownCol[k] || P.color(counts[k].major);
  // diverging scale for deviation from the global line (log10 units): blue below, grey at zero, orange above
  const DIV = { lo: [59, 143, 232], mid: [154, 163, 159], hi: [240, 138, 36], lim: 1.2 };
  const divCol = (v) => {
    const k = Math.max(-1, Math.min(1, v / DIV.lim)), a = DIV.mid, b = k < 0 ? DIV.lo : DIV.hi, t = Math.abs(k);
    return 'rgb(' + a.map((c, i) => Math.round(c + (b[i] - c) * t)).join(',') + ')';
  };

  /* ---------- shared marks ---------- */
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
  /* the living records as a cloud of points; returns the circles with their clade */
  const cloud = (Pl, parent, r = 4, opacity = 0.3) => X.points.map((p) => ({ k: p.k, el: D.svg('circle', { cx: Pl.sx(p.x), cy: Pl.sy(p.y), r, fill: 'var(--muted)', opacity }, parent) }));
  const oneToOne = (Pl) => D.svg('path', { d: P.path(Pl, [[Pl.o.x[0], Pl.o.x[0]], [Pl.o.x[1], Pl.o.x[1]]]), class: 'guide' }, Pl.g);
  const cutout = (fr, src, credit, cls = '') => D.el('figure', 'cutout fade hidden ' + cls, fr, `<img src="img/${src}" alt=""><figcaption>${credit}</figcaption>`);

  /* a PhyloPic silhouette (data/silhouettes.js, keyed by minor clade), drawn h tall (narrower if it is long)
   * in colour col, centred on y; ax says which edge x is: 'start', 'middle' or 'end'. Null if there is none. */
  const SIL = root.SIL || {};
  function sil(parent, key, x, y, h, col, ax = 'start') {
    const s = SIL[key]; if (!s) return null;
    const k = Math.min(h / s.h, 1.9 * h / s.w), w = s.w * k, hh = s.h * k;
    const el = D.svg('svg', { x: ax === 'end' ? x - w : ax === 'middle' ? x - w / 2 : x, y: y - hh / 2, width: w, height: hh, viewBox: s.vb, class: 'sil fade' }, parent);
    el.innerHTML = s.body; el.style.fill = col;
    return el;
  }

  /* tree layout: sets x, y on every node; cladogram = equal steps per node level, tips aligned */
  function layoutTree(newick, x0, w, top, h, clado) {
    const tree = typeof newick === 'string' ? P.newick(newick) : newick, tips = P.tips(tree);
    if (clado) {
      // a zero-length internal branch is a polytomy (the 'other' bins hang from their major clade's crown node): draw it as one
      const collapse = (n) => { n.kids.forEach(collapse); n.kids = n.kids.flatMap((k) => (k.kids.length && k.len === 0 ? k.kids : [k])); }; collapse(tree);
      const ht = (n) => (n.h = n.kids.length ? 1 + Math.max(...n.kids.map(ht)) : 0);
      ht(tree); const H = tree.h, set = (n) => { n.depth = H - n.h; n.kids.forEach(set); }; set(tree);
      tips.forEach((t) => { t.depth = H; });
    }
    const maxd = Math.max(...tips.map((t) => t.depth));
    // tips take one row each, or t.rows rows (a tip that carries several marks)
    const wt = tips.map((t) => t.rows || 1), tot = wt.reduce((a, b) => a + b, 0) - (wt[0] + wt[wt.length - 1]) / 2;
    let acc = -wt[0] / 2;
    tips.forEach((t, i) => { t.y = top + (tips.length === 1 ? h / 2 : (acc + wt[i] / 2) * h / tot); acc += wt[i]; });
    const setY = (n) => { if (n.kids.length) { n.kids.forEach(setY); n.y = (n.kids[0].y + n.kids[n.kids.length - 1].y) / 2; } n.x = x0 + n.depth / maxd * w; };
    setY(tree);
    return { tree, tips, x0 };
  }
  function drawTree(svg, newick, x0, w, top, h, opts = {}) {
    const { tree, tips } = layoutTree(newick, x0, w, top, h, opts.clado);
    const g = D.svg('g', {}, svg), stroke = opts.stroke || 'var(--rule)';
    const drawN = (n, px) => {
      D.svg('path', { d: `M${px},${n.y}H${n.x}`, stroke, 'stroke-width': 4, fill: 'none' }, g);
      if (n.kids.length) { D.svg('path', { d: `M${n.x},${n.kids[0].y}V${n.kids[n.kids.length - 1].y}`, stroke, 'stroke-width': 4 }, g); n.kids.forEach((k) => drawN(k, n.x)); }
    };
    drawN(tree, x0);
    return tips;
  }

  const S = [];

  /* ---------- 0 title ---------- */
  S.push({
    builds: [`Title. Fossil gut contents are the only direct record of what an extinct predator ate. The comparison set is ${nSpecies.toLocaleString()} living predator species. The question for the next few minutes: what can a handful of fossil meals say against that?`],
    make(fr) {
      D.el('div', 'titlecard', fr, `<h1>Preferred prey size for <em>theropod dinosaurs</em></h1>` +
        `<p class="sub">Evidence from ${nSpecies.toLocaleString()} extant species</p>` +
        `<p class="who">Jonathan Mitchell</p><div class="plate"><img src="img/logo_coe.png" alt="Coe College"></div>`);
    }
  });

  /* ---------- 1 a specimen preserves one meal ---------- */
  // spindles arrive one per click, worst fit to the Compsognathus meal first, best last
  const spinOrder = X.spindles.slice().sort((a, b) => a.comp_fit - b.comp_fit);
  S.push({
    title: 'Some specimens preserve <em>a single meal</em>',
    anim: Object.fromEntries(spinOrder.map((_, i) => [i + 1, 900])),
    builds: ['Compsognathus. This is the inside of the ribcage, and the small bones are a lizard. One predator, one prey, one point. The lizard is about half a percent of the predator’s mass.']
      .concat(spinOrder.map((sp, i) => `${sp.species}: every recorded meal, ${sp.n} records from ${sp.sources} studies. ` +
        (i === 0 ? 'Of these six living predators, this is the worst match to the Compsognathus meal as a share of body mass.'
         : i === spinOrder.length - 1 ? 'The best match of the six. Even so, the fossil is one draw from a spindle like this. We do not know where in the spindle that draw fell.'
         : `Match to the Compsognathus meal: ${i + 1} of ${spinOrder.length}, counting from the worst.`))),
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
        // above the name, or beside it when the spindle reaches the top of the plot
        if (Pl.sy(top) > 60) sil(lab, sp.minor, Pl.sx(hx), Pl.sy(top) - 82, 46, col, 'middle'); else sil(lab, sp.minor, Pl.sx(hx) + sp.label.length * 9 + 16, Pl.sy(top) - 28, 46, col);
        const hit = D.svg('rect', { x: Pl.sx(hx - half), y: Pl.sy(top), width: Pl.sx(hx + half) - Pl.sx(hx - half), height: Pl.sy(keep[0][0]) - Pl.sy(top), fill: 'transparent' }, g);
        hit.addEventListener('pointermove', (e) => D.tip(e, `<b><i>${sp.species}</i></b><br>${sp.n} prey records, ${sp.sources} studies<br>predator ${P.fmt(sp.pred_g)}`));
        hit.addEventListener('pointerleave', D.untip);
        return [g, lab];
      };
      const layers = spinOrder.map((x) => spin(x, 0.17));
      const comp = fossils.find((r) => r.pair_id === 'Conrad2017_01');
      fossilMark(Pl, comp, Pl.over, { r: 13 });
      label(Pl, Pl.sx(lg(comp.pred)) + 24, Pl.sy(lg(comp.prey_g)) + 46, 'Compsognathus', 'lbl', 'start');
      const gut = D.el('figure', 'gut', fr, '<img src="img/compsognathus_gut.jpg" alt=""><figcaption>Compsognathus gut contents · Conrad 2017</figcaption>');
      return { show(b) {
        gut.classList.toggle('small', b > 0);
        layers.forEach((els, i) => els.forEach((e) => e.classList.toggle('hidden', b < i + 1)));
      } };
    }
  });

  /* ---------- 2 lumping (pooling), shown on one number per clade: schematic histograms ---------- */
  S.push({
    titles: ['Lump nothing: a mean from three records is <em>hard to trust</em>', 'Lump everything: one mean <em>hides real differences</em>', 'Lump partly: small samples <em>lean on large ones</em>'],
    anim: { 1: 900, 2: 900 },
    builds: [
      'A schematic, not data. The question on this slide and the next is how much to lump. Start with one number per clade: the typical prey size as a share of predator size. Three clades, with four hundred records, twenty-five, and three. Lump nothing: take each clade’s own mean. The dot is the mean and the bar is how well we know it. With four hundred records the mean is pinned down. With three it could be almost anywhere.',
      'The other extreme: lump everything. Ignore the clades and take one mean for all the records. Every clade gets the same answer, and the clade with four hundred records writes it. The middle clade sits well below that line, and we have enough records there to know it.',
      'In between: lump partly, which statisticians call partial pooling. Each clade keeps its own mean, but the means are treated as draws from a shared distribution. The big clade does not move. The middle one barely moves. The clade with three records is lumped most of the way in with the others, and its bar narrows. How much a clade is lumped depends on how little it has. Next: the same three options with lines in place of means.'
    ],
    make(fr) {
      // deterministic made-up clades; x is log10 (prey size / predator size)
      let seed = 7; const rnd = () => { seed = (seed * 1664525 + 1013904223) % 4294967296; return seed / 4294967296; };
      const norm = () => Math.sqrt(-2 * Math.log(rnd() + 1e-9)) * Math.cos(2 * Math.PI * rnd());
      const SD = 0.5, K = 3;                                   // within-clade sd; K = within / between variance
      const cl = [{ name: 'Clade A', n: 400, mu: -1.0, col: 'var(--aves)' }, { name: 'Clade B', n: 25, mu: -1.7, col: 'var(--squamata)' }, { name: 'Clade C', n: 3, col: 'var(--mammalia)' }];
      cl.forEach((c) => { c.v = c.mu == null ? [-0.55, -0.2, 0.05] : Array.from({ length: c.n }, () => c.mu + norm() * SD); });
      const mean = (a) => a.reduce((s, v) => s + v, 0) / a.length;
      const all = cl.flatMap((c) => c.v), grand = mean(all);
      cl.forEach((c) => {
        const w = c.n / (c.n + K);
        c.est = [{ m: mean(c.v), se: SD / Math.sqrt(c.n) }, { m: grand, se: SD / Math.sqrt(all.length) }, { m: grand + (mean(c.v) - grand) * w, se: SD / Math.sqrt(c.n + K) }];
      });
      const box = plotBox(fr, 0, 0, 1600, 900), svg = D.svg('svg', { width: 1600, height: 900 }, box);
      const X0 = 440, X1 = 1500, LO = -3.2, HI = 0.5, BW = 0.1, sx = (v) => X0 + (v - LO) / (HI - LO) * (X1 - X0);
      const AX = 740, base = [310, 490, 670], HH = 112;
      // shared axis
      const ax = D.svg('g', { class: 'axis' }, svg);
      D.svg('line', { x1: X0, x2: X1, y1: AX, y2: AX }, ax);
      [[-3, '0.1%'], [-2, '1%'], [-1, '10%'], [0, '100%']].forEach(([t, s]) => {
        D.svg('line', { x1: sx(t), x2: sx(t), y1: 190, y2: AX, class: 'gridline' }, ax);
        D.svg('line', { x1: sx(t), x2: sx(t), y1: AX, y2: AX + 12, class: 'tick' }, ax);
        D.svg('text', { x: sx(t), y: AX + 46, 'text-anchor': 'middle' }, ax).textContent = s;
      });
      D.svg('text', { x: (X0 + X1) / 2, y: AX + 98, 'text-anchor': 'middle', class: 'title' }, ax).textContent = 'Prey size as a share of predator size';
      // the shared mean: solid when it is the answer, dashed when it is only the centre the clades lean toward
      const gSolid = D.svg('line', { x1: sx(grand), x2: sx(grand), y1: 180, y2: AX, stroke: 'var(--ink)', 'stroke-width': 5, 'stroke-linecap': 'round', class: 'fade hidden' }, svg);
      const gDash = D.svg('line', { x1: sx(grand), x2: sx(grand), y1: 180, y2: AX, class: 'guide fade hidden' }, svg);
      const gLbl = D.svg('text', { x: sx(grand) + 16, y: 204, class: 'lbl small halo fade hidden' }, svg); gLbl.textContent = 'mean of all records';
      const rows = cl.map((c, i) => {
        const y0 = base[i];
        // histogram: the big clade fills the row; smaller clades are drawn one block per record
        const bins = {}; c.v.forEach((v) => { const b = Math.floor((v - LO) / BW); bins[b] = (bins[b] || 0) + 1; });
        const top = Math.max(...Object.values(bins)), unit = Math.min(22, HH / top);
        const hist = D.svg('g', { class: 'fade', opacity: 0.6 }, svg);
        Object.entries(bins).forEach(([b, k]) => D.svg('rect', { x: sx(LO + b * BW) + 1.5, y: y0 - k * unit, width: sx(LO + BW) - sx(LO) - 3, height: k * unit, rx: 2 }, hist));
        D.svg('line', { x1: X0, x2: X1, y1: y0, y2: y0, stroke: 'var(--rule)', 'stroke-width': 2 }, svg);
        const t1 = D.svg('text', { x: 100, y: y0 - 48, class: 'lbl', fill: c.col }, svg); t1.textContent = c.name; t1.style.fill = c.col;
        const t2 = D.svg('text', { x: 100, y: y0 - 8, class: 'lbl small' }, svg); t2.textContent = c.n + ' records';
        // estimate lane under the histogram: the clade's own mean as a ghost ring, then the current estimate
        const ey = y0 + 30;
        const ghost = D.svg('circle', { cx: sx(c.est[0].m), cy: ey, r: 10, fill: 'none', stroke: c.col, 'stroke-width': 3, class: 'fade hidden' }, svg);
        const bar = D.svg('rect', { x: -1, y: -4, width: 2, height: 8, fill: c.col }, svg);
        const dot = D.svg('circle', { cx: 0, cy: 0, r: 12, fill: c.col, class: 'ring' }, svg);
        [bar, dot].forEach((e) => { e.style.transition = 'transform 0.8s ease'; });
        return { c, hist, ghost, bar, dot, ey };
      });
      const tag = D.svg('text', { x: 100, y: AX + 98, class: 'guide-lbl' }, svg);
      const tags = ['no lumping', 'complete lumping', 'partial lumping'];
      return { show(b) {
        tag.textContent = tags[b];
        gSolid.classList.toggle('hidden', b !== 1); gDash.classList.toggle('hidden', b !== 2); gLbl.classList.toggle('hidden', b === 0);
        rows.forEach((r) => {
          const e = r.c.est[b], px = sx(e.m), half = Math.max(2, 2 * e.se * (X1 - X0) / (HI - LO));
          r.hist.setAttribute('fill', b === 1 ? 'var(--muted)' : r.c.col);
          r.ghost.classList.toggle('hidden', b === 0);
          r.dot.style.transform = `translate(${px}px, ${r.ey}px)`;
          r.bar.style.transform = `translate(${px}px, ${r.ey}px) scaleX(${half})`;
        });
      } };
    }
  });

  /* ---------- 3 evolutionary models: who is lumped with whom ---------- */
  const lam = global.raw;
  S.push({
    kicker: 'Evolutionary models describe who is lumped with whom',
    titles: ['Lump <em>nobody</em>', 'Lump <em>everyone equally</em>', 'Lump <em>close relatives</em>', 'Hierarchical models lump <em>variably across the tree</em>'],
    builds: [
      `Every record from the living predators: ${X.counts.reduce((a, c) => a + c.rows, 0).toLocaleString()} meals, ${X.counts.length} clades, the tree on the left. Rule one: lump nobody. Each clade is its own problem. No shared line at all.`,
      'Rule two: lump everyone equally. One line, and every clade is scatter around it. Birds, snakes, cats, all the same distance from each other. In comparative-methods language that is white noise, or an Ornstein-Uhlenbeck model with one optimum and a strong pull. Lambda near zero. (A strong OU is not strictly lambda zero, which is why it says approximately.)',
      'Rule three: Brownian motion. Lump relatives together, and the close ones most. Now every clade has its own colour and its own line, and a clade’s line is pulled toward its neighbours on the tree. Lambda of one.',
      `The model we fit does not pick a rule ahead of time. It estimates how much the tree matters: lambda comes out near ${lam.lambda_a.toFixed(1)} for the height of the line and ${lam.lambda_b.toFixed(1)} for the slope. Colour here is the major clade. A clade gets a colour of its own only where its slope clearly differs from the rest of its major clade: ${Object.keys(ownCol).map(nm).join(', ')}.`
    ],
    make(fr) {
      // tree with clade names
      const tbox = plotBox(fr, 60, 180, 560, 680);
      const svg = D.svg('svg', { width: 560, height: 680 }, tbox);
      const tips = drawTree(svg, X.skeleton.newick, 10, 270, 20, 600, { clado: true });
      const tipEls = tips.map((t) => {
        const dot = D.svg('circle', { cx: t.x + 16, cy: t.y, r: 11, class: 'fade', opacity: 0 }, svg);
        const tx = D.svg('text', { x: t.x + 36, y: t.y + 10, class: 'lbl small' }, svg); tx.textContent = nm(t.name);
        return { k: t.name, dot, pic: ownCol[t.name] ? sil(svg, t.name, 500, t.y, 30, ownCol[t.name]) : null };
      });
      // all the points
      const Pl = P.loglog(plotBox(fr, 640, 180, 900, 680), { x: [0, 6], y: [-3, 6], w: 900, h: 680 });
      const pts = cloud(Pl, Pl.g, 4, 0.32);
      const lines = D.svg('g', {}, Pl.g);
      const line = (c, xa, xb, col, w = 4) => D.svg('path', { d: P.path(Pl, P.linePts(c, xa, xb)), fill: 'none', stroke: col, 'stroke-width': w, 'stroke-linecap': 'round', class: 'fade', opacity: 0 }, lines);
      const rng = (k) => [mx(counts[k].m_lo), mx(counts[k].m_hi)];
      const L1 = [line(glob(X.fits.lambda0), 0, 6, 'var(--ink)', 5)];
      const L2 = tipOrder.filter((k) => F.lambda1[k]).map((k) => line(F.lambda1[k], ...rng(k), minorCol[k]));
      const L3 = X.majors.map((m) => {
        const ks = X.counts.filter((c) => c.major === m.major);
        return line(m, mx(Math.min(...ks.map((c) => c.m_lo))), mx(Math.max(...ks.map((c) => c.m_hi))), P.color(m.major), 5);
      }).concat(Object.keys(ownCol).map((k) => line(F.primary[k], ...rng(k), ownCol[k], 5)));
      const tag = label(Pl, Pl.left + Pl.pw - 18, Pl.top + Pl.ph - 24, '', 'lbl halo', 'end');
      const tags = ['no lumping', 'white noise · strong OU (λ ≈ 0)', 'Brownian motion (λ = 1)', `λ estimated (${lam.lambda_a.toFixed(1)}, ${lam.lambda_b.toFixed(1)})`];
      const colour = [() => 'var(--muted)', () => 'var(--muted)', (k) => minorCol[k], hierCol];
      return { show(b) {
        tag.textContent = tags[b];
        pts.forEach((p) => { p.el.setAttribute('fill', colour[b](p.k)); p.el.setAttribute('opacity', b < 2 ? 0.32 : 0.42); });
        tipEls.forEach((e) => { e.dot.setAttribute('fill', colour[b](e.k)); e.dot.setAttribute('opacity', b < 2 ? 0 : 1); if (e.pic) e.pic.style.opacity = b === 3 ? 1 : 0; });
        [L1, L2, L3].forEach((set, i) => set.forEach((l) => l.setAttribute('opacity', b === i + 1 ? 1 : 0)));
      } };
    }
  });

  /* ---------- 4 the global line, then theropods ---------- */
  S.push({
    titles: ['A <em>global regression</em> line', 'Where <em>theropods</em> fall relative to extant diet data'],
    anim: { 1: 900 },
    builds: [
      `Start with the simplest version. Every living record in grey, and one line through all of them: prey mass goes up with predator mass with a slope of ${global.slope.toFixed(2)}. The dashed line is one to one, a meal as heavy as the animal eating it. Nearly everything sits below it.`,
      `Now the fossils, in red: every non-avian theropod gut with a prey mass or a bracket, ${theroRecs.length} records. Bars are prey known only to within a bracket. They fall inside the living cloud, and most of them sit under the line.`
    ],
    make(fr) {
      const Pl = P.loglog(plotBox(fr), { x: [0, 7], y: [-3, 6], w: 1480, h: 710 });
      cloud(Pl, Pl.g, 4, 0.3);
      oneToOne(Pl);
      D.svg('path', { d: P.path(Pl, P.linePts(global, 0, 7)), fill: 'none', stroke: 'var(--ink)', 'stroke-width': 5, 'stroke-linecap': 'round' }, Pl.g);
      const marks = theroRecs.map((r) => fossilMark(Pl, r, Pl.g, { r: 10 }));
      return { show(b) { marks.forEach((m) => m.classList.toggle('hidden', b < 1)); } };
    }
  });

  /* ---------- 5 specific animals ---------- */
  const preyLab = (r) => /^[A-Z][a-z]+( [a-z]+)?$/.test(r.prey) && !/idae$|ia$|ei$|es$/.test(r.prey) ? r.prey.split(' ')[0] : r.prey;
  const cases = [
    { title: '<i>Microraptor</i>: four observations', ids: ['OConnor2011_01', 'Xing2013_01', 'Hone2022_01', 'OConnor2019_01'], labels: ['bird', 'fish', 'mammal', 'lizard'],
      img: 'microraptor.jpg', credit: 'IVPP V17972 · O’Connor et al. 2011',
      note: 'Microraptor. Four gut records from one genus: a bird, a fish, a mammal, a lizard. This is the best fossil case for diet breadth we have. The spread of these four is the closest thing to a fossil spindle. The fish is a bar because all that is preserved is fin rays; more on that bar in a minute.' },
    { title: 'Juvenile <i>Gorgosaurus</i>: two observations', ids: ['Therrien2023_01', 'Therrien2023_02'], labels: ['Citipes', 'Citipes'],
      img: 'gorgosaurus.jpg', credit: 'TMP 2009.12.14 · Therrien et al. 2023',
      note: 'A juvenile Gorgosaurus with two young Citipes in its gut. Each is about 3% of the predator’s mass. A juvenile tyrannosaur was not eating like an adult.' },
    { title: '<i>Sinocalliopteryx</i>: three observations', ids: ['Xing2012_01', 'Xing2012_04', 'Xing2012_05'], labels: ['Sinornithosaurus (prey)', 'Confuciusornis ×2 (prey)', ''],
      img: 'sinocalliopteryx.jpg', credit: 'JMP-V-05-8-01 · Xing et al. 2012',
      note: 'Sinocalliopteryx, two specimens. One has a Sinornithosaurus leg in its gut, a dromaeosaur about 8% of its mass. The other has two Confuciusornis, each about 1%. The labels on the plot are the prey.' },
    { title: '<i>Velociraptor</i>: battling <i>Protoceratops</i>', ids: ['Carpenter1998_01'], labels: ['Protoceratops (prey)'],
      img: 'velociraptor.jpg', credit: 'Fighting dinosaurs (cast) · Y. Tamai, CC BY 2.0',
      note: 'Velociraptor locked with Protoceratops. The prey is heavier than the predator: this one sits above the one-to-one line. It is a fight, not a meal, and it goes into the model as a direct observation, not a stomach.' }
  ];
  S.push({
    titles: cases.map((c) => c.title),
    builds: cases.map((c) => c.note),
    make(fr) {
      const Pl = P.loglog(plotBox(fr), { x: [1.5, 7], y: [-3, 6], w: 1480, h: 710 });
      cloud(Pl, Pl.g, 4, 0.16);
      oneToOne(Pl);
      D.svg('path', { d: P.path(Pl, P.linePts(global, 1.5, 7)), fill: 'none', stroke: 'var(--ink-2)', 'stroke-width': 4, 'stroke-linecap': 'round' }, Pl.g);
      const bg = D.svg('g', { opacity: 0.3 }, Pl.g);
      theroRecs.forEach((r) => fossilMark(Pl, r, bg, { color: 'var(--muted)', r: 6 }));
      const layers = cases.map((c) => {
        const g = D.svg('g', { class: 'fade hidden' }, Pl.g), lab = D.svg('g', { class: 'fade hidden' }, Pl.over);
        c.ids.forEach((id, i) => {
          const r = fossils.find((f) => f.pair_id === id); if (!r) return;
          const m = fossilMark(Pl, r, g, { r: 12 });
          if (c.labels[i]) label(Pl, Pl.sx(+m.dataset.x) + 20, Pl.sy(+m.dataset.y) + (i % 2 ? 34 : -14), c.labels[i], 'lbl halo', 'start', lab);
        });
        return [g, lab, cutout(fr, c.img, c.credit)];
      });
      return { show(b) {
        layers.forEach((els, i) => els.forEach((e) => e.classList.toggle('hidden', i !== b)));
      } };
    }
  });

  /* ---------- 6 every living spindle, theropods on top ---------- */
  S.push({
    title: 'Theropods compared to <em>extant predators</em>',
    anim: { 1: 900 },
    builds: [
      `Every living predator species with at least ${X.allspin.min_records} vertebrate prey records: ${X.allspin.species.length} spindles, each one a species’ whole recorded diet. Colour is the major clade. Snakes sit low and flat. Cats climb steeply. Owls are narrow, canids are wide.`,
      'And the theropod gut records on top, in red. They fall inside the living cloud, mostly in its lower half for their size. No single colour owns them.'
    ],
    make(fr) {
      const Pl = P.loglog(plotBox(fr, 60, 150, 1130, 710), { x: [1, 7], y: [-3, 6], w: 1130, h: 710 });
      oneToOne(Pl);
      const shade = {}; X.allspin.groups.forEach((g) => { shade[g.group] = P.color(g.major); });   // one colour per major clade
      const half = 0.11, step = X.allspin.step;
      const cl = D.svg('g', {}, Pl.g);
      X.allspin.species.forEach((sp) => {
        const R = sp.d.map((d, i) => [sp.x + half * d, sp.y0 + i * step]), L = sp.d.map((d, i) => [sp.x - half * d, sp.y0 + i * step]);
        const path = D.svg('path', { d: P.path(Pl, R) + P.path(Pl, L.reverse()).replace(/^M/, 'L') + 'Z',
          fill: shade[sp.group], 'fill-opacity': 0.28, stroke: shade[sp.group], 'stroke-opacity': 0.7, 'stroke-width': 1.5 }, cl);
        path.addEventListener('pointermove', (e) => D.tip(e, `<b><i>${sp.sp.replace(/_/g, ' ')}</i></b><br>${nm(sp.group)} · ${sp.n} records`));
        path.addEventListener('pointerleave', D.untip);
      });
      const pts = D.svg('g', { class: 'fade hidden' }, Pl.g);
      theroRecs.forEach((r) => fossilMark(Pl, r, pts, { r: 10 }));
      const lg_ = D.svg('svg', { width: 330, height: 710 }, plotBox(fr, 1215, 160, 330, 710));
      let y = 40;
      ['Aves', 'Mammalia', 'Squamata', 'Crocodylia'].forEach((maj) => {
        D.svg('rect', { x: 0, y: y - 22, width: 28, height: 28, rx: 5, fill: P.color(maj), 'fill-opacity': 0.7 }, lg_);
        const t = D.svg('text', { x: 42, y: y, class: 'lbl small' }, lg_); t.textContent = maj;
        y += 50;
      });
      y += 10;
      D.svg('circle', { cx: 13, cy: y - 7, r: 10, fill: 'var(--fossil)', class: 'ring' }, lg_);
      const ft = D.svg('text', { x: 42, y: y, class: 'lbl small' }, lg_); ft.textContent = 'Theropoda';
      return { show(b) { pts.classList.toggle('hidden', b < 1); } };
    }
  });

  /* ---------- 7 by kind of record ---------- */
  const OBS = { stomach_content: 'Stomach contents', pellet_scat: 'Pellets and scats', prey_remains: 'Prey remains', direct_observation: 'Direct observation',
    regurgitate: 'Regurgitates', dna_detection: 'DNA', unknown: 'Not stated' };
  S.push({
    title: 'Prey size by <em>type of record</em>',
    builds: [
      (() => { const o = Object.fromEntries(X.obs.map((r) => [r.obs, r])), x = (k) => Math.exp(o[k].d).toFixed(2);
        return `The living data are not one kind of record. Most of it, ${o.pellet_scat.rows.toLocaleString()} rows, is pellets and scats. Stomachs are ${o.stomach_content.rows.toLocaleString()}. The model estimates a shift for each kind, measured against stomach contents, with everything else held fixed. Pellets and scats: prey ${x('pellet_scat')} times as heavy as a stomach would give. Prey remains: ${x('prey_remains')}. Watching the animal hunt: ${x('direct_observation')}. Big prey is what you see, and what is left at the nest. Fossil guts are stomachs, so every comparison in this talk is made at the stomach line.`; })()
    ],
    make(fr) {
      const W = 1480, Hh = 710, svg = D.svg('svg', { width: W, height: Hh }, plotBox(fr));
      const rows = X.obs.filter((r) => OBS[r.obs]).sort((a, b) => (a.obs === 'stomach_content' ? -1 : b.obs === 'stomach_content' ? 1 : b.rows - a.rows));
      const left = 420, nx = 640, x0 = 700, x1 = W - 60, rx = [-0.3, 1.2], sx = (v) => x0 + (v - rx[0]) / (rx[1] - rx[0]) * (x1 - x0), bh = 78, top = 40;
      const tk = [1, 1.5, 2, 3];
      tk.forEach((t) => {
        const x = sx(Math.log(t));
        D.svg('line', { x1: x, x2: x, y1: top - 10, y2: top + rows.length * bh, class: t === 1 ? 'guide' : 'gridline' }, svg);
        const tx = D.svg('text', { x, y: top + rows.length * bh + 44, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = t + ' ×';
      });
      const hn = D.svg('text', { x: nx, y: top - 14, 'text-anchor': 'end', class: 'lbl small muted' }, svg); hn.textContent = 'records';
      const xt = D.svg('text', { x: (x0 + x1) / 2, y: top + rows.length * bh + 104, 'text-anchor': 'middle', class: 'lbl' }, svg); xt.textContent = 'Prey size relative to stomach contents';
      rows.forEach((r, i) => {
        const y = top + i * bh + bh / 2, ref = r.obs === 'stomach_content', g = D.svg('g', {}, svg);
        const off = !ref && (r.d_lo > 0 || r.d_hi < 0), col = ref ? 'var(--fossil)' : off ? 'var(--accent-ink)' : 'var(--muted)';
        const t = D.svg('text', { x: left, y: y + 11, 'text-anchor': 'end', class: 'lbl' + (ref ? '' : ' small') }, g); t.textContent = OBS[r.obs];
        const n = D.svg('text', { x: nx, y: y + 10, 'text-anchor': 'end', class: 'lbl small muted' }, g); n.textContent = r.rows.toLocaleString();
        if (!ref) D.svg('line', { x1: sx(Math.max(rx[0], r.d_lo)), x2: sx(Math.min(rx[1], r.d_hi)), y1: y, y2: y, stroke: col, 'stroke-width': 6, 'stroke-linecap': 'round' }, g);
        D.svg('circle', { cx: sx(r.d), cy: y, r: ref ? 13 : 11, fill: col, class: 'ring' }, g);
        if (!ref) { const v = D.svg('text', { x: sx(Math.min(rx[1], r.d_hi)) + 16, y: y + 10, class: 'lbl small' }, g); v.textContent = Math.exp(r.d).toFixed(2) + ' ×'; }
        D.svg('rect', { x: 0, y: y - bh / 2, width: W, height: bh, fill: 'transparent' }, g);
        g.addEventListener('pointermove', (e) => D.tip(e, `<b>${OBS[r.obs]}</b><br>${r.rows.toLocaleString()} records · ${r.species} species · ${r.sources} studies` + (ref ? '<br>reference; fossil guts are compared here' : `<br>${Math.exp(r.d).toFixed(2)} × stomach contents (90% ${Math.exp(r.d_lo).toFixed(2)}–${Math.exp(r.d_hi).toFixed(2)})`)));
        g.addEventListener('pointerleave', D.untip);
      });
    }
  });

  /* ---------- 8 prey : predator mass ratio along the trees ---------- */
  const pct = (v) => { const x = Math.pow(10, v); return x >= 1 ? x.toLocaleString() + '%' : (+x.toPrecision(1)) + '%'; };
  S.push({
    titles: ['Prey to predator mass ratio for <em>extant clades</em>', 'Prey to predator mass ratio for <em>theropods</em>'],
    anim: { 1: 900 },
    builds: [
      'Each living clade at its typical body size. The dot is the median meal as a share of predator mass, and the bar is the 90% interval on that median, not the range of meals. The number is how many species are behind it. The grey tick is where the global line says a predator of that size should be. Coloured rows are the clades that clearly sit off the global line: hawks, both carnivoran groups and dasyuromorphs above it, crocodylians and teiioids below.',
      'The theropods on their own tree, every gut record as a share of its predator. Dots are measured prey. Thin bars are brackets: prey known only between two bounds. The Microraptor fish runs off the axis because its lower bound comes from a 6.7 mm fin ray and nothing else. The spread inside one genus, Microraptor, is as wide as a whole living clade.'
    ],
    make(fr) {
      const TF = X.treefits;
      const panel = (x, w) => { const box = plotBox(fr, x, 150, w, 720); return D.svg('svg', { width: w, height: 720 }, box); };
      const axis = (svg, x0, x1, bottom, title, rx, every) => {
        const sx = (v) => x0 + (v - rx[0]) / (rx[1] - rx[0]) * (x1 - x0);
        for (let t = rx[0]; t <= rx[1]; t++) {
          D.svg('line', { x1: sx(t), x2: sx(t), y1: 10, y2: bottom, class: 'gridline' }, svg);
          D.svg('line', { x1: sx(t), x2: sx(t), y1: bottom, y2: bottom + 10, class: 'tick', stroke: 'var(--muted)', 'stroke-width': 2.5 }, svg);
          if (t % every === 0) { const tx = D.svg('text', { x: sx(t), y: bottom + 40, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = pct(t); }
        }
        D.svg('line', { x1: x0, x2: x1, y1: bottom, y2: bottom, stroke: 'var(--muted)', 'stroke-width': 2.5 }, svg);
        const at = D.svg('text', { x: (x0 + x1) / 2, y: bottom + 82, 'text-anchor': 'middle', class: 'lbl' }, svg); at.textContent = title;
        return sx;
      };
      // extant
      const L = panel(30, 790), bottom = 600;
      const tipsL = drawTree(L, X.skeleton.newick, 0, 130, 20, bottom - 40, { clado: true });
      const sxL = axis(L, 470, 770, bottom, 'Prey as % of predator', [-1, 2], 1);
      const nh = D.svg('text', { x: 440, y: 2, 'text-anchor': 'end', class: 'lbl small muted' }, L); nh.textContent = 'species';
      tipsL.forEach((t) => {
        const r = ext[t.name]; if (!r) return;
        const off = r.dev_differs, col = off ? P.color(r.major) : 'var(--muted)';
        const lab = D.svg('text', { x: t.x + 14, y: t.y + 10, class: off ? 'lbl small strong' : 'lbl small' }, L); lab.textContent = nm(t.name);
        const n = D.svg('text', { x: 440, y: t.y + 10, 'text-anchor': 'end', class: 'lbl small muted' }, L); n.textContent = r.species.toLocaleString();
        const g = D.svg('g', {}, L);
        const exp = r.r50 - r.dev;                         // the global line's answer at this clade's size
        D.svg('line', { x1: sxL(exp), x2: sxL(exp), y1: t.y - 11, y2: t.y + 11, stroke: 'var(--ink-2)', 'stroke-width': 3 }, g);
        D.svg('line', { x1: sxL(r.c5), x2: sxL(r.c95), y1: t.y, y2: t.y, stroke: col, 'stroke-width': 7, 'stroke-linecap': 'round' }, g);
        D.svg('circle', { cx: sxL(r.r50), cy: t.y, r: 10, fill: col, class: 'ring' }, g);
        D.svg('rect', { x: 0, y: t.y - 16, width: 790, height: 32, fill: 'transparent' }, g);
        g.addEventListener('pointermove', (e) => D.tip(e, `<b>${nm(t.name)}</b> · ${r.species} species, ${r.rows.toLocaleString()} records<br>at ${P.fmt(r.pred_g)}: median meal ${pct(r.r50)} of predator (90% ${pct(r.c5)}–${pct(r.c95)})<br>global line: ${pct(exp)}${off ? ' · clearly ' + off : ''}`));
        g.addEventListener('pointerleave', D.untip);
      });
      // theropods
      const Rg = D.svg('g', { class: 'fade hidden' }, panel(830, 740));
      const tipsR = drawTree(Rg, TF.theropod_tree.newick, 0, 140, 20, bottom - 40, { stroke: 'var(--fossil)', clado: true });
      const genusCount = {}; tipsR.forEach((t) => { const g = t.name.split(' ')[0]; genusCount[g] = (genusCount[g] || 0) + 1; });
      const rx = [-2, 3], sxR = axis(Rg, 410, 730, bottom, 'Prey as % of predator', rx, 2);
      tipsR.forEach((t) => {
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
          } else {                                          // bracket: thin, capped, clamped to the axis with an arrow
            const lo = Math.log10(f.prey_lo / pr * 100), hi = Math.log10(f.prey_hi / pr * 100), a = Math.max(lo, rx[0]), z = Math.min(hi, rx[1]);
            const g = D.svg('g', { stroke: 'var(--fossil)', 'stroke-width': 3, opacity: 0.75, fill: 'none' }, Rg);
            D.svg('line', { x1: sxR(a), x2: sxR(z), y1: yy, y2: yy }, g);
            if (lo < rx[0]) D.svg('path', { d: `M${sxR(a) + 10},${yy - 7}L${sxR(a)},${yy}L${sxR(a) + 10},${yy + 7}` }, g); else D.svg('line', { x1: sxR(a), x2: sxR(a), y1: yy - 6, y2: yy + 6 }, g);
            D.svg('line', { x1: sxR(z), x2: sxR(z), y1: yy - 6, y2: yy + 6 }, g);
            const hit = D.svg('rect', { x: sxR(a), y: yy - 8, width: sxR(z) - sxR(a), height: 16, fill: 'transparent', stroke: 'none' }, g);
            hit.addEventListener('pointermove', (e) => D.tip(e, `<b>${f.taxon}</b> ate <b>${f.prey}</b><br>bracket only: ${P.fmt(f.prey_lo)}–${P.fmt(f.prey_hi)}<br>${(f.prey_lo / pr * 100).toPrecision(1)}%–${(f.prey_hi / pr * 100).toPrecision(2)}% of predator mass`));
            hit.addEventListener('pointerleave', D.untip);
          }
        });
      });
      return { show(b) { Rg.classList.toggle('hidden', b < 1); } };
    }
  });

  /* ---------- 8 relatives disagree: slopes ---------- */
  S.push({
    title: 'Close relatives <em>scale differently</em>',
    builds: [
      `Slope of prey mass on predator mass for each living clade, with its 90% interval. The dashed line is the global slope, ${global.slope.toFixed(2)}. Feliforms: ${F.primary.Feliformia.slope.toFixed(2)}, prey mass rises faster than predator mass. Caniforms: ${F.primary.Caniformia.slope.toFixed(2)}. Sister groups, different answers.`,
      `Accipitriforms against strigiforms: ${F.primary.Accipitriformes.slope.toFixed(2)} against ${F.primary.Strigiformes.slope.toFixed(2)}. The same split, inside birds. The model found this from the data. It did not assume the tree was right, and this is why the tree cannot predict a theropod tightly. Next: one line through everything, and where the theropods fall against it.`
    ],
    make(fr) {
      const box = plotBox(fr), W = 1480, Hh = 710, svg = D.svg('svg', { width: W, height: Hh }, box);
      const rows = X.fits.primary.clades.filter((c) => counts[c.minor]).map((c) => Object.assign({ major: counts[c.minor].major }, c))
        .sort((a, b) => a.major.localeCompare(b.major) || b.slope - a.slope);
      const left = 300, pw = W - left - 80, x0 = 0, x1 = 1.6, sx = (v) => left + (v - x0) / (x1 - x0) * pw, bh = (Hh - 100) / rows.length;
      for (let t = 0; t <= 1.6; t += 0.4) { D.svg('line', { x1: sx(t), x2: sx(t), y1: 0, y2: rows.length * bh, class: 'gridline' }, svg); const tx = D.svg('text', { x: sx(t), y: rows.length * bh + 40, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = t.toFixed(1); }
      D.svg('line', { x1: sx(global.slope), x2: sx(global.slope), y1: 0, y2: rows.length * bh, class: 'guide', 'stroke-width': 2.5 }, svg);
      const xt = D.svg('text', { x: left + pw / 2, y: Hh - 4, 'text-anchor': 'middle', class: 'lbl' }, svg); xt.textContent = 'Predator–prey mass scaling (slope)';
      const els = rows.map((c, i) => {
        const y = i * bh + bh / 2, g = D.svg('g', { class: 'fade' }, svg);
        D.svg('line', { x1: sx(Math.max(x0, c.slope_lo)), x2: sx(Math.min(x1, c.slope_hi)), y1: y, y2: y, stroke: P.color(c.major), 'stroke-width': 4, 'stroke-linecap': 'round' }, g);
        D.svg('circle', { cx: sx(c.slope), cy: y, r: 9, fill: P.color(c.major), class: 'ring' }, g);
        const t = D.svg('text', { x: left - 14, y: y + 10, 'text-anchor': 'end', class: 'lbl small' }, g); t.textContent = nm(c.minor);
        const v = D.svg('text', { x: sx(Math.min(x1, c.slope_hi)) + 12, y: y + 10, class: 'lbl small' }, g); v.textContent = c.slope.toFixed(2);
        return { c, g, v, pic: sil(svg, c.minor, 72, y, 30, P.color(c.major), 'end') };
      });
      const hi = [['Feliformia', 'Caniformia'], ['Accipitriformes', 'Strigiformes']];
      return { show(b) { els.forEach((e) => { const on = hi[b].includes(e.c.minor); e.g.style.opacity = on ? 1 : 0.45; e.v.style.display = on ? '' : 'none'; if (e.pic) e.pic.style.opacity = on ? 1 : 0; }); } };
    }
  });

  /* ---------- 9 deviation from the global line, mapped on the tree ---------- */
  const THERO = [
    { name: 'Tyrannosauridae', re: /^(Daspletosaurus|Gorgosaurus)/ },
    { name: 'Compsognathidae', re: /^(Huadanosaurus|Sinocalliopteryx|Sinosauropteryx|Compsognathus|Scipionyx)/ },
    { name: 'Paraves (non-avian)', re: /^(Anchiornis|Velociraptor|Microraptor)/ }
  ];
  const resid = (pred, prey) => lg(prey) - P.lineY(global, lg(pred));
  THERO.forEach((t) => {
    t.recs = theroRecs.filter((f) => t.re.test(f.taxon)).map((f) => {
      const pr = f.pred || Math.sqrt(f.pred_lo * f.pred_hi);
      return f.prey_g ? { f, v: resid(pr, f.prey_g) } : { f, lo: resid(pr, f.prey_lo), hi: resid(pr, f.prey_hi) };
    });
    const v = t.recs.filter((r) => r.v != null).map((r) => r.v).sort((a, b) => a - b);
    t.dev = v.length % 2 ? v[(v.length - 1) / 2] : (v[v.length / 2 - 1] + v[v.length / 2]) / 2;      // median of the measured records
  });
  S.push({
    title: 'Prey size relative to the <em>global regression</em>',
    anim: { 1: 900 },
    builds: [
      'Every living clade on the tree. Colour is how far a clade sits from the global line at its own typical size: orange is larger prey than the line predicts, blue is smaller, grey is on the line. Branch colours between the tips are interpolated, the way a phylogenetic trait map does it. On the right, the same number for each tip with its 90% interval. The axis is its own colour key. Mammalian carnivores and hawks run orange. Crocodylians and teiioids run blue.',
      `Now the theropods, grafted onto the bird stem. Each dot is one gut record against the global line; thin bars are brackets. The medians of the measured records: ${THERO.map((t) => t.name.replace(' (non-avian)', '') + ' ' + Math.pow(10, t.dev).toPrecision(2) + ' times the line').join(', ')}. Compsognathids sit on the blue side, with the crocodylians and teiioids, and away from the birds they are closest to on the tree. The paravians cover the whole axis, from a lizard in a Microraptor to the Protoceratops fight. ${theroRecs.length} records cannot say more than that.`
    ],
    make(fr) {
      const W = 1480, Hh = 720, svg = D.svg('svg', { width: W, height: Hh }, plotBox(fr, 60, 150, W, Hh));
      const defs = D.svg('defs', {}, svg), th = Object.fromEntries(THERO.map((t) => [t.name, t]));
      const bottom = 590, cx0 = 760, cx1 = 1440, rx = [-2.5, 2], sx = (v) => cx0 + (v - rx[0]) / (rx[1] - rx[0]) * (cx1 - cx0);
      // tip chart frame; the axis is also the colour key
      const times = { '-2': '1/100 ×', '-1': '1/10 ×', 0: '1 ×', 1: '10 ×', 2: '100 ×' };
      [-2, -1, 0, 1, 2].forEach((t) => {
        D.svg('line', { x1: sx(t), x2: sx(t), y1: 0, y2: bottom, class: t === 0 ? 'guide' : 'gridline' }, svg);
        const tx = D.svg('text', { x: sx(t), y: bottom + 52, 'text-anchor': 'middle', class: 'lbl small' }, svg); tx.textContent = times[t];
      });
      const bar = D.svg('linearGradient', { id: 'cmbar' + Math.random().toString(36).slice(2), gradientUnits: 'userSpaceOnUse', x1: cx0, x2: cx1, y1: 0, y2: 0 }, defs);
      for (let i = 0; i <= 20; i++) D.svg('stop', { offset: i / 20, 'stop-color': divCol(rx[0] + (rx[1] - rx[0]) * i / 20) }, bar);
      D.svg('rect', { x: cx0, y: bottom + 2, width: cx1 - cx0, height: 12, rx: 6, fill: `url(#${bar.id})` }, svg);
      const at = D.svg('text', { x: (cx0 + cx1) / 2, y: bottom + 100, 'text-anchor': 'middle', class: 'lbl' }, svg); at.textContent = 'Prey size relative to the global regression';
      // one drawing without the theropods, one with them grafted onto the stem of Aves
      const draw = (withThero) => {
        const G = D.svg('g', { class: 'fade' }, svg), tree = P.newick(X.skeleton.newick);
        const isAves = (n) => P.tips(n).every((t) => counts[t.name] && counts[t.name].major === 'Aves');
        const graft = (n) => n.kids.forEach((k, i) => {
          if (!isAves(k)) return graft(k);
          const tip = (name) => ({ name, kids: [], len: 1, rows: 3 }), node = (kids) => ({ name: '', len: 1, kids });
          n.kids[i] = node([tip(THERO[0].name), node([tip(THERO[1].name), node([tip(THERO[2].name), k])])]);
        });
        if (withThero) graft(tree);
        const { tips } = layoutTree(tree, 6, 300, 16, bottom - 30, true);
        // tip values from the fit or the fossil records; internal nodes by least squares on the drawn cladogram
        tips.forEach((t) => { t.v = th[t.name] ? th[t.name].dev : ext[t.name].dev; });
        const inner = []; const link = (n, par) => { n.par = par; if (n.kids.length) { inner.push(n); n.v = 0; n.kids.forEach((k) => link(k, n)); } }; link(tree, null);
        for (let it = 0; it < 500; it++) inner.forEach((n) => { const nb = n.kids.concat(n.par ? [n.par] : []); n.v = nb.reduce((a, k) => a + k.v, 0) / nb.length; });
        const drawN = (n, par) => {
          const px = par ? par.x : 6, id = 'cm' + Math.random().toString(36).slice(2);
          const gr = D.svg('linearGradient', { id, gradientUnits: 'userSpaceOnUse', x1: px, x2: n.x, y1: 0, y2: 0 }, defs);
          D.svg('stop', { offset: 0, 'stop-color': divCol(par ? par.v : n.v) }, gr); D.svg('stop', { offset: 1, 'stop-color': divCol(n.v) }, gr);
          D.svg('path', { d: `M${px},${n.y}H${n.x}`, stroke: `url(#${id})`, 'stroke-width': 9, 'stroke-linecap': 'round', fill: 'none' }, G);
          if (n.kids.length) D.svg('path', { d: `M${n.x},${n.kids[0].y}V${n.kids[n.kids.length - 1].y}`, stroke: divCol(n.v), 'stroke-width': 9, 'stroke-linecap': 'round', fill: 'none' }, G);
          n.kids.forEach((k) => drawN(k, n));
        };
        drawN(tree, null);
        tips.forEach((t) => {
          const T = th[t.name], g = D.svg('g', {}, G);
          const lab = D.svg('text', { x: t.x + 16, y: t.y + 9, class: 'lbl tiny' + (T ? ' strong' : '') }, g); lab.textContent = T ? t.name : nm(t.name);
          if (T) {
            D.svg('rect', { x: cx0, y: t.y - 36, width: cx1 - cx0, height: 72, rx: 8, fill: 'var(--fossil)', opacity: 0.08 }, g);
            const pts = T.recs.filter((r) => r.v != null), brs = T.recs.filter((r) => r.v == null);
            brs.forEach((r, k) => {
              const yy = t.y + 14 + k * 9 - (brs.length - 1) * 4.5, a = Math.max(r.lo, rx[0]), z = Math.min(r.hi, rx[1]);
              const b = D.svg('g', { stroke: 'var(--fossil)', 'stroke-width': 3, opacity: 0.8, fill: 'none' }, g);
              D.svg('line', { x1: sx(a), x2: sx(z), y1: yy, y2: yy }, b);
              if (r.lo < rx[0]) D.svg('path', { d: `M${sx(a) + 10},${yy - 6}L${sx(a)},${yy}L${sx(a) + 10},${yy + 6}` }, b);
            });
            pts.forEach((r) => {
              const c = D.svg('circle', { cx: sx(Math.max(rx[0], Math.min(rx[1], r.v))), cy: t.y - (brs.length ? 14 : 0), r: 10, fill: divCol(r.v), stroke: 'var(--fossil)', 'stroke-width': 3.5 }, g);
              c.addEventListener('pointermove', (e) => D.tip(e, `<b>${r.f.taxon}</b> ate <b>${r.f.prey}</b><br>${Math.pow(10, r.v).toPrecision(2)} × the global line`));
              c.addEventListener('pointerleave', D.untip);
            });
          } else {
            const r = ext[t.name];
            D.svg('line', { x1: sx(r.dev_lo), x2: sx(r.dev_hi), y1: t.y, y2: t.y, stroke: divCol(r.dev), 'stroke-width': 6, 'stroke-linecap': 'round' }, g);
            D.svg('circle', { cx: sx(r.dev), cy: t.y, r: 9, fill: divCol(r.dev), class: 'ring' }, g);
            D.svg('rect', { x: cx0, y: t.y - 13, width: cx1 - cx0, height: 26, fill: 'transparent' }, g);
            g.addEventListener('pointermove', (e) => D.tip(e, `<b>${nm(t.name)}</b> at ${P.fmt(r.pred_g)}<br>${Math.pow(10, r.dev).toPrecision(2)} × the global line (90% ${Math.pow(10, r.dev_lo).toPrecision(2)}–${Math.pow(10, r.dev_hi).toPrecision(2)})`));
            g.addEventListener('pointerleave', D.untip);
          }
        });
        return G;
      };
      const views = [draw(false), draw(true)];
      return { show(b) { views.forEach((g, i) => { g.classList.toggle('hidden', i !== b); g.style.pointerEvents = i === b ? '' : 'none'; }); } };
    }
  });

  /* ---------- 11 closing: three bullets, each with a small figure on one shared axis ---------- */
  const CL = (() => {
    const C = X.closing, ty = Object.fromEntries(C.types.map((t) => [t.obs, t]));
    const devs = X.treefits.extant.slice().sort((a, b) => a.dev - b.dev), lo = devs[0], hi = devs[devs.length - 1];
    const mid = theroRecs.map((f) => { const pr = f.pred || Math.sqrt(f.pred_lo * f.pred_hi); return resid(pr, f.prey_g || Math.sqrt(f.prey_lo * f.prey_hi)); });
    return { C, ty, devs, lo, hi, fold: Math.round(Math.pow(10, hi.dev - lo.dev)),
      nIn: mid.filter((v) => v >= C.all.q5 && v <= C.all.q95).length, nLow: mid.filter((v) => v < C.all.q5).length, nHigh: mid.filter((v) => v > C.all.q95).length,
      pc: (k) => Math.round(ty[k].below * 100) };
  })();
  S.push({
    title: 'Conclusions',
    anim: { 1: 900, 2: 900 },
    builds: (() => {
      const C = CL.C, o = Object.fromEntries(X.obs.map((r) => [r.obs, r]));
      return [
        `Three points, each on the same axis: prey size against the global line, so predator size is taken out. First, the ${CL.devs.length} living clades. From ${nm(CL.lo.minor)} to ${nm(CL.hi.minor)} is a ${CL.fold}-fold difference in prey size at the same predator size. There is no single living predator to compare a fossil to.`,
        `Second, the theropods. Grey is every living record, ${C.all.rows.toLocaleString()} of them. Red dots are theropod records with a measured prey, thin bars are brackets, and the open dot is the Velociraptor fight. ${CL.nIn} of ${theroRecs.length} sit inside the middle 90% of living records, ${CL.nLow} below it and ${CL.nHigh} above (brackets counted at their midpoint). Nothing here needs a kind of predator that is not alive today.`,
        `Third, the kind of record. Fossil guts are stomachs. The dashed line is the median theropod stomach record with a measured prey, ${C.thero_n} records. Against living stomachs, ${CL.pc('stomach_content')}% of records sit below that line: theropods look ordinary. Against prey remains it is ${CL.pc('prey_remains')}% (pellets and scats ${CL.pc('pellet_scat')}%, observed kills ${CL.pc('direct_observation')}%): the same theropods start to look like small-prey specialists. These are raw records, so part of the gap is which species each method samples. The model’s shift for method alone is smaller, ${Math.exp(o.pellet_scat.d).toFixed(1)} to ${Math.exp(o.direct_observation.d).toFixed(1)} times. Either way: compare stomachs to stomachs.`
      ];
    })(),
    make(fr) {
      const C = CL.C, W = 1480, Hh = 710, svg = D.svg('svg', { width: W, height: Hh }, plotBox(fr));
      const x0 = 900, x1 = 1370, rx = [-3, 3], sx = (v) => x0 + (Math.max(rx[0], Math.min(rx[1], v)) - rx[0]) / (rx[1] - rx[0]) * (x1 - x0);
      const bottom = 600, TX = 76, rowY = [80, 270, 480];
      // shared axis under the three small figures
      const times = { '-2': '1/100 ×', 0: '1 ×', 2: '100 ×' };
      [-2, -1, 0, 1, 2].forEach((t) => {
        D.svg('line', { x1: sx(t), x2: sx(t), y1: 10, y2: bottom, class: t === 0 ? 'guide' : 'gridline' }, svg);
        if (times[t]) { const tx = D.svg('text', { x: sx(t), y: bottom + 36, 'text-anchor': 'middle', class: 'lbl tiny' }, svg); tx.textContent = times[t]; }
      });
      D.svg('line', { x1: x0, x2: x1, y1: bottom, y2: bottom, stroke: 'var(--muted)', 'stroke-width': 2.5 }, svg);
      const at = D.svg('text', { x: (x0 + x1) / 2, y: bottom + 84, 'text-anchor': 'middle', class: 'lbl small' }, svg); at.textContent = 'Prey size relative to the global regression';
      const bullet = (g, y, head, sub) => {
        D.svg('circle', { cx: 44, cy: y - 22, r: 9, fill: 'var(--accent-ink)' }, g);
        const h = D.svg('text', { x: TX, y: y - 8, class: 'lbl' }, g); h.textContent = head; h.style.fontSize = '42px';
        const t = D.svg('text', { x: TX, y: y + 36, class: 'lbl small muted' }, g); t.textContent = sub;
      };
      const violin = (t, y, hh, col, parent) => {
        const pts = t.d.map((d, i) => [C.grid0 + i * C.step, d]).filter((p) => p[0] >= rx[0] && p[0] <= rx[1]);
        const up = pts.map((p) => `${sx(p[0]).toFixed(1)},${(y - hh * p[1]).toFixed(1)}`), dn = pts.slice().reverse().map((p) => `${sx(p[0]).toFixed(1)},${(y + hh * p[1]).toFixed(1)}`);
        D.svg('path', { d: 'M' + up.concat(dn).join('L') + 'Z', fill: col, 'fill-opacity': 0.4, stroke: col, 'stroke-width': 2.5 }, parent);
        D.svg('line', { x1: sx(t.q50), x2: sx(t.q50), y1: y - hh * 0.8, y2: y + hh * 0.8, stroke: 'var(--ink)', 'stroke-width': 3.5, 'stroke-linecap': 'round' }, parent);
      };
      // 1 heterogeneity: one dot per clade, swarmed, the two ends named
      const g1 = D.svg('g', { class: 'fade' }, svg);
      bullet(g1, rowY[0], 'Extant predators vary widely', `${CL.fold}-fold among ${CL.devs.length} clades at one predator size`);
      const placed = [];
      CL.devs.forEach((r) => {
        const px = sx(r.dev), oy = [0, -21, 21, -42, 42].find((o) => !placed.some((q) => q.o === o && Math.abs(q.x - px) < 21)) || 0;
        placed.push({ x: px, o: oy }); r.cy = rowY[0] + oy;
        const c = D.svg('circle', { cx: px, cy: r.cy, r: 9.5, fill: P.color(r.major), class: 'ring' }, g1);
        c.addEventListener('pointermove', (e) => D.tip(e, `<b>${nm(r.minor)}</b> · ${r.species} species<br>${Math.pow(10, r.dev).toPrecision(2)} × the global line (90% ${Math.pow(10, r.dev_lo).toPrecision(2)}–${Math.pow(10, r.dev_hi).toPrecision(2)})`));
        c.addEventListener('pointerleave', D.untip);
      });
      const l0 = D.svg('text', { x: sx(CL.lo.dev) - 18, y: CL.lo.cy + 8, 'text-anchor': 'end', class: 'lbl tiny' }, g1); l0.textContent = nm(CL.lo.minor);
      const l1 = D.svg('text', { x: sx(CL.hi.dev) + 18, y: CL.hi.cy + 8, class: 'lbl tiny' }, g1); l1.textContent = nm(CL.hi.minor);
      sil(g1, CL.lo.minor, sx(CL.lo.dev) - 30 - nm(CL.lo.minor).length * 13, CL.lo.cy, 30, P.color(CL.lo.major), 'end');
      sil(g1, CL.hi.minor, sx(CL.hi.dev) + 30 + nm(CL.hi.minor).length * 13, CL.hi.cy, 30, P.color(CL.hi.major));
      // 2 theropods against every living record
      const g2 = D.svg('g', { class: 'fade hidden' }, svg), y2 = rowY[1];
      bullet(g2, y2, 'Theropods fall within that range', `${CL.nIn} of ${theroRecs.length} records inside the middle 90%`);
      violin(C.all, y2 - 26, 50, 'var(--muted)', g2);
      let k = 0;
      theroRecs.filter((f) => !f.prey_g).forEach((f) => {
        const pr = f.pred || Math.sqrt(f.pred_lo * f.pred_hi), lo = resid(pr, f.prey_lo), hi = resid(pr, f.prey_hi), yy = y2 + 62 + (k++) * 3.6;
        D.svg('line', { x1: sx(lo), x2: sx(hi), y1: yy, y2: yy, stroke: 'var(--fossil)', 'stroke-width': 2, opacity: 0.7 }, g2);
      });
      theroRecs.filter((f) => f.prey_g).forEach((f) => {
        const pr = f.pred || Math.sqrt(f.pred_lo * f.pred_hi), v = resid(pr, f.prey_g), gut = f.obs === 'stomach_content';
        const c = D.svg('circle', { cx: sx(v), cy: y2 + 44, r: 8, fill: gut ? 'var(--fossil)' : 'var(--board)', stroke: gut ? 'var(--board)' : 'var(--fossil)', 'stroke-width': gut ? 1.5 : 3.5 }, g2);
        c.addEventListener('pointermove', (e) => D.tip(e, `<b>${f.taxon}</b> ate <b>${f.prey}</b><br>${Math.pow(10, v).toPrecision(2)} × the global line<br>${f.obs.replace(/_/g, ' ')}`));
        c.addEventListener('pointerleave', D.untip);
      });
      // 3 record type: living stomachs against living prey remains, with the theropod median
      const g3 = D.svg('g', { class: 'fade hidden' }, svg), y3 = rowY[2];
      bullet(g3, y3, 'Record type matters', 'compare stomachs with stomachs');
      [['stomach_content', y3 - 34], ['prey_remains', y3 + 44]].forEach(([key, y]) => {
        const t = CL.ty[key], ref = key === 'stomach_content';
        violin(t, y, 32, ref ? 'var(--fossil)' : 'var(--muted)', g3);
        const a = D.svg('text', { x: x0 - 16, y: y + 8, 'text-anchor': 'end', class: 'lbl tiny' + (ref ? ' strong' : '') }, g3); a.textContent = OBS[key];
        const p = D.svg('text', { x: x1 + 18, y: y + 9, class: 'lbl small' + (ref ? ' strong' : '') }, g3); p.textContent = CL.pc(key) + '%';
      });
      D.svg('line', { x1: sx(C.thero_med), x2: sx(C.thero_med), y1: y3 - 78, y2: y3 + 86, stroke: 'var(--fossil)', 'stroke-width': 3.5, 'stroke-dasharray': '8 7' }, g3);
      const tm = D.svg('text', { x: x1 + 18, y: y3 - 66, class: 'lbl tiny muted' }, g3); tm.textContent = 'below';
      const tm2 = D.svg('text', { x: sx(C.thero_med) - 10, y: y3 + 104, 'text-anchor': 'end', class: 'lbl tiny', fill: 'var(--fossil)' }, g3); tm2.textContent = 'theropod median'; tm2.style.fill = 'var(--fossil)';
      return { show(b) { g2.classList.toggle('hidden', b < 1); g3.classList.toggle('hidden', b < 2); } };
    }
  });

  /* ---------- 12 acknowledgments ---------- */
  S.push({
    title: 'Acknowledgments',
    builds: ['Acknowledgments. NSF and Coe College for support. Faith Hysell. Mike Grundler and SquamataBase. The people who built OS-Prey, CarniDIET, GATEWAy and AVONET, and every author who weighed a stomach. PhyloPic.'],
    make(fr) {
      D.el('figure', 'ackfig', fr, '<img src="img/python_sunbear.png" alt=""><figcaption><i>Malayopython reticulatus</i> after swallowing <i>Helarctos malayanus</i> · Fredriksson 2005</figcaption>');
      D.el('div', 'ack with-fig', fr,
        '<div class="logos"><div class="plate"><img src="img/logo_nsf.svg" alt="U.S. National Science Foundation"></div>' +
        '<div class="plate wide"><img src="img/logo_coe.png" alt="Coe College"></div></div>' +
        '<div class="names"><p>Faith Hysell</p><p>Mike Grundler · SquamataBase</p><p>OS-Prey · CarniDIET · GATEWAy · AVONET</p><p>PhyloPic</p></div>');
    }
  });

  /* Running order (JM 2026-11-13). S is in the order the slides are written above:
   * 0 title, 1 single meal, 2 lumping, 3 hierarchical, 4 global line, 5 specimens, 6 spindles, 7 record type,
   * 8 ratio by clade, 9 slopes, 10 tree map, 11 conclusions, 12 acknowledgments.
   * Slopes now follow the hierarchical slide; the tree map closes the results; the ratio slide (8) is not shown. */
  root.TALK = [0, 1, 2, 3, 9, 4, 5, 6, 7, 10, 11, 12].map((i) => S[i]);
})(window);
