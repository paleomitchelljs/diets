# Talk site (draft)

A linear talk of twelve screens on the fs110 deck style: a 1600 × 900 stage scaled to the window, chalkboard (dark) by default. No build step and no libraries; `index.html` opens straight from the file.

## Keys

→ / space / page down next · ← back · **S** scroll mode (every slide stacked at its last build) · **P** presenter window (current and next screen, notes, timer; kept in step over `BroadcastChannel`) · **T** dark / light · **B** blank · **?** keys.

## Data

The site reads only `data/data.js` (same content as `data/data.json`), written by `code/site_export.R` from `results/`. Rebuild after any fit changes with
`FIT=results/6_work/terr/fit_free LAM0=results/6_work/terr/fit_lambda0 LAM1=results/6_work/terr/fit_lambda1 UNPOOLED=results/6_work/terr/fit_unpooled Rscript code/site_export.R`
(the environment variables choose which fit folders feed the site).

## Slides

| # | Title | Builds | Source |
|---|---|---|---|
| 0 | Preferred prey size for theropod dinosaurs | 1 | species count from `counts`; Coe wordmark |
| 1 | Some specimens preserve a single meal | 7 | *Compsognathus* gut close-up (Conrad 2017 fig. 2), then six living spindles |
| 2 | A mean from three records is hard to trust / One mean for everyone hides real differences / Small samples borrow from large ones | 3 | schematic (made-up records, three clades of 400, 25 and 3): per-clade histograms with each clade's mean, under no, complete and partial pooling |
| 3 | Borrow from nobody / everyone equally / close relatives / Hierarchical models borrow variably across the tree | 4 | all points; λ = 0 global line; λ = 1 clade lines; free-λ major lines plus minor clades whose slope differs from their major clade |
| 4 | A global regression line / Where theropods fall relative to extant diet data | 2 | global line of the free-λ fit, 1:1 line, theropod records |
| 5 | Specimens | 4 | *Microraptor*, *Gorgosaurus*, *Sinocalliopteryx*, *Velociraptor*, each with an image |
| 6 | Theropods compared to extant predators | 2 | species spindles, theropods on top |
| 7 | Prey size by type of record | 1 | `obs`: the fit's shift per observation type against stomach contents |
| 8 | Prey to predator mass ratio for extant clades / theropods | 2 | `treefits.extant` (median and 90% interval at the clade's median size; tick = global line), theropod records |
| 9 | Close relatives scale differently | 2 | clade slopes; dashed line = global slope |
| 10 | Prey size relative to the global regression | 2 | deviation from the global line mapped on the tree, theropods grafted on the stem of Aves |
| 11 | Acknowledgments | 1 | NSF and Coe logos |

A slide's heading can change per build (`titles: [...]` in `js/slides.js`); `kicker` adds a small line above it.

Notes on the figures:
- "Global regression" is the root line of the free-λ fit (alpha0, beta0) at the stomach-content reference. It is not an OLS through the plotted points (OLS on all rows: slope 1.06; `fit_pooled`: 0.47; this line: 0.81).
- A clade is marked as off the global line when the 90% interval of (clade line minus global line), evaluated at the clade's median predator mass from the joint draws, excludes zero.
- Slide 10 branch colours are interpolated by least squares on the drawn cladogram (unit branch lengths). They are for display, not an ancestral-state estimate. Theropod tip colours are the median of the records with a measured prey mass; brackets are drawn but not used.
- Brackets that run past an axis are clamped and drawn with an arrow.

## Images

`img/` holds the figures used on slides. Sources: `compsognathus_gut.jpg` Conrad 2017 fig. 2 (© Linnean Society); `microraptor.jpg` O'Connor et al. 2011 (IVPP V17972); `gorgosaurus.jpg` Therrien et al. 2023 fig. 1A (CC BY); `sinocalliopteryx.jpg` Xing et al. 2012 (CC BY); `velociraptor.jpg` Wikimedia Commons, "Fighting dinosaurs (1)", Yuya Tamai, CC BY 2.0; `logo_nsf.svg` and `logo_coe.png` Wikimedia Commons. The site is public on GitHub Pages: check the two non-CC figures before pushing.

## Deploy

`.github/workflows/pages.yml` publishes `static/` to GitHub Pages on a push to `master` that touches `static/`. One-time setup: Settings → Pages → Source: GitHub Actions.
