/* Plot helpers: log-log axes in grams, model lines, bands, a newick parser. No library. */
(function (root) {
  'use strict';
  const D = root.Deck, L10 = Math.LN10, LN1000 = Math.log(1000);
  const Plot = root.Plot = {};

  Plot.mass = (l) => {                       // log10 grams -> label
    const g = Math.pow(10, l);
    if (g >= 1e6) return (g / 1e6).toLocaleString() + ' t';
    if (g >= 1e3) return (g / 1e3).toLocaleString() + ' kg';
    if (g >= 1) return g.toLocaleString() + ' g';
    return (+g.toPrecision(1)).toString() + ' g';
  };
  Plot.fmt = (g) => g == null ? '–' : g >= 1e6 ? (g / 1e6).toPrecision(2) + ' t' : g >= 1e3 ? (g / 1e3).toPrecision(2) + ' kg' : g >= 1 ? (+g.toPrecision(2)) + ' g' : (+g.toPrecision(2)) + ' g';
  Plot.color = (major) => `var(--${String(major || 'fossil').toLowerCase()})`;

  /* opts: x [lo, hi] and y [lo, hi] in log10 g, w, h, xlab, ylab, left, bottom */
  Plot.loglog = function (host, o) {
    const left = o.left || 170, bottom = o.bottom || 104, top = 16, right = 40;
    const w = o.w, h = o.h, pw = w - left - right, ph = h - top - bottom;
    const svg = D.svg('svg', { width: w, height: h, viewBox: `0 0 ${w} ${h}` }, host);
    const sx = (v) => left + (v - o.x[0]) / (o.x[1] - o.x[0]) * pw;
    const sy = (v) => top + ph - (v - o.y[0]) / (o.y[1] - o.y[0]) * ph;
    const ax = D.svg('g', { class: 'axis' }, svg);
    for (let t = Math.ceil(o.x[0]); t <= o.x[1]; t++) {
      D.svg('line', { x1: sx(t), x2: sx(t), y1: top, y2: top + ph, class: 'gridline' }, ax);
      D.svg('line', { x1: sx(t), x2: sx(t), y1: top + ph, y2: top + ph + 12, class: 'tick' }, ax);
      D.svg('text', { x: sx(t), y: top + ph + 46, 'text-anchor': 'middle' }, ax).textContent = Plot.mass(t);
    }
    for (let t = Math.ceil(o.y[0]); t <= o.y[1]; t++) {
      D.svg('line', { x1: left, x2: left + pw, y1: sy(t), y2: sy(t), class: 'gridline' }, ax);
      D.svg('line', { x1: left - 12, x2: left, y1: sy(t), y2: sy(t), class: 'tick' }, ax);
      D.svg('text', { x: left - 20, y: sy(t) + 10, 'text-anchor': 'end' }, ax).textContent = Plot.mass(t);
    }
    D.svg('line', { x1: left, x2: left + pw, y1: top + ph, y2: top + ph }, ax);
    D.svg('line', { x1: left, x2: left, y1: top, y2: top + ph }, ax);
    D.svg('text', { x: left + pw / 2, y: h - 8, 'text-anchor': 'middle', class: 'title' }, ax).textContent = o.xlab || 'Predator size';
    D.svg('text', { x: -(top + ph / 2), y: 32, transform: 'rotate(-90)', 'text-anchor': 'middle', class: 'title' }, ax).textContent = o.ylab || 'Meal size';
    const id = 'c' + Math.random().toString(36).slice(2);
    const cp = D.svg('clipPath', { id }, D.svg('defs', {}, svg));
    D.svg('rect', { x: left, y: top, width: pw, height: ph }, cp);
    const g = D.svg('g', { 'clip-path': `url(#${id})` }, svg);
    const over = D.svg('g', {}, svg);                // labels, unclipped
    return { svg, g, over, sx, sy, o, left, top, pw, ph };
  };

  Plot.path = (P, pts) => pts.map((p, i) => (i ? 'L' : 'M') + P.sx(p[0]).toFixed(1) + ',' + P.sy(p[1]).toFixed(1)).join('');
  Plot.ratioGuides = function (P, ratios) {
    for (const r of ratios) {
      const lr = Math.log10(r), x0 = P.o.x[0], x1 = P.o.x[1];
      D.svg('path', { d: Plot.path(P, [[x0, x0 + lr], [x1, x1 + lr]]), class: 'guide' }, P.g);
      const xl = Math.min(x1 - 0.15, P.o.y[1] - lr - 0.25);
      const t = D.svg('text', { x: P.sx(xl), y: P.sy(xl + lr) - 8, class: 'guide-lbl', 'text-anchor': 'end' }, P.over);
      t.textContent = r >= 1 ? '1 : 1' : (r * 100 >= 1 ? (r * 100) + '%' : (r * 100).toPrecision(1) + '%');
    }
  };

  /* Model line in log10 space from a clade row (int, slope on ln scale, m = ln pred - ln 1000). */
  Plot.lineY = (c, x10) => (c.int + c.slope * (x10 * L10 - LN1000)) / L10;
  Plot.predSd = (c, glob, x10) => {
    const m = x10 * L10 - LN1000, s = c.sigma * Math.exp((glob.gamma1 || 0) * m);
    return Math.sqrt(s * s + (glob.tau_u || 0) ** 2 + (glob.tau_w || 0) ** 2) / L10;
  };
  Plot.linePts = (c, xa, xb, n = 40) => Array.from({ length: n + 1 }, (_, i) => { const x = xa + (xb - xa) * i / n; return [x, Plot.lineY(c, x)]; });

  Plot.band = (P, lower, upper) => Plot.path(P, lower) + Plot.path(P, upper.slice().reverse()).replace(/^M/, 'L') + 'Z';

  /* Gaussian kernel density of values v (log10), bandwidth bw, at n points. */
  Plot.kde = (v, bw, lo, hi, n = 80) => Array.from({ length: n + 1 }, (_, i) => {
    const y = lo + (hi - lo) * i / n; let s = 0;
    for (const x of v) s += Math.exp(-0.5 * ((y - x) / bw) ** 2);
    return [y, s / (v.length * bw * Math.sqrt(2 * Math.PI))];
  });

  /* newick -> {name, len, kids, depth} */
  Plot.newick = function (s) {
    let i = 0;
    const node = () => {
      const n = { kids: [], name: '', len: 0 };
      if (s[i] === '(') { i++; n.kids.push(node()); while (s[i] === ',') { i++; n.kids.push(node()); } i++; }
      let nm = ''; while (i < s.length && !':,);'.includes(s[i])) nm += s[i++];
      n.name = nm.replace(/_/g, ' ');
      if (s[i] === ':') { i++; let l = ''; while (i < s.length && !',);'.includes(s[i])) l += s[i++]; n.len = +l; }
      return n;
    };
    const root = node();
    const walk = (n, d) => { n.depth = d + n.len; n.kids.forEach((k) => walk(k, n.depth)); };
    root.len = 0; walk(root, 0);
    return root;
  };
  Plot.tips = (n, out = []) => { if (!n.kids.length) out.push(n); else n.kids.forEach((k) => Plot.tips(k, out)); return out; };
})(window);
