# Talk site (draft)

A linear talk of twelve screens on the fs110 deck style: a 1600 × 900 stage scaled to the window, chalkboard (dark) by default. No build step and no libraries; `index.html` opens straight from the file.

## Keys

→ / space / page down next · ← back · **S** scroll mode (every slide stacked at its last build) · **P** presenter window (current and next screen, notes, timer; kept in step over `BroadcastChannel`) · **T** dark / light · **B** blank · **I** silhouettes on / off · **?** keys.

## Data

The site reads only `data/data.js` (same content as `data/data.json`), written by `code/site_export.R` from `results/`. Rebuild after any fit changes with
`FIT=results/6_work/terr/fit_free LAM0=results/6_work/terr/fit_lambda0 LAM1=results/6_work/terr/fit_lambda1 UNPOOLED=results/6_work/terr/fit_unpooled Rscript code/site_export.R`
(the environment variables choose which fit folders feed the site).

## Slides

| # | Title | Builds | Source |
|---|---|---|---|
| 0 | Preferred prey size for theropod dinosaurs | 1 | species count from `counts`; Coe wordmark |
| 1 | Some specimens preserve a single meal | 7 | *Compsognathus* gut close-up (Conrad 2017 fig. 2), then six living spindles |
| 2 | Lump nothing: a mean from three records is hard to trust / Lump everything: one mean hides real differences / Lump partly: small samples lean on large ones | 3 | schematic (made-up records, three clades of 400, 25 and 3) under no, complete and partial lumping (pooling) |
| 3 | Lump nobody / everyone equally / close relatives / Hierarchical models lump variably across the tree | 4 | all points; λ = 0 global line; λ = 1 clade lines; free-λ major lines plus minor clades whose slope differs from their major clade |
| 4 | Close relatives scale differently | 2 | clade slopes; dashed line = global slope |
| 5 | A global regression line / Where theropods fall relative to extant diet data | 2 | global line of the free-λ fit, 1:1 line, theropod records |
| 6 | Specimens | 4 | *Microraptor*, *Gorgosaurus*, *Sinocalliopteryx*, *Velociraptor*, each with an image |
| 7 | Theropods compared to extant predators | 2 | species spindles, theropods on top |
| 8 | Prey size by type of record | 1 | `obs`: the fit's shift per observation type against stomach contents |
| 9 | Prey size relative to the global regression | 2 | deviation from the global line mapped on the tree, theropods grafted on the stem of Aves |
| 10 | Conclusions | 3 | three bullets, each with a small figure on one axis (`closing`) |
| 11 | Acknowledgments | 1 | NSF and Coe logos; python and sun bear photo (Fredriksson 2005) |

Order changed 2026-11-13 (JM's run-through notes): slopes moved up to follow the hierarchical slide; the tree map closes the results; the "prey to predator mass ratio" slide is written in `js/slides.js` but left out of the running order (last line of the file).

A slide's heading can change per build (`titles: [...]` in `js/slides.js`); `kicker` adds a small line above it.

Notes on the figures:
- "Global regression" is the root line of the free-λ fit (alpha0, beta0) at the stomach-content reference. It is not an OLS through the plotted points (OLS on all rows: slope 1.06; `fit_pooled`: 0.47; this line: 0.81).
- A clade is marked as off the global line when the 90% interval of (clade line minus global line), evaluated at the clade's median predator mass from the joint draws, excludes zero.
- Slide 9 branch colours are interpolated by least squares on the drawn cladogram (unit branch lengths). They are for display, not an ancestral-state estimate. Theropod tip colours are the median of the records with a measured prey mass; brackets are drawn but not used.
- Brackets that run past an axis are clamped and drawn with an arrow.
- Slides 2 and 3 say "lumping" for pooling on screen (JM, 2026-11-13); "partial pooling" is named once in the notes.
- Slide 10 is the one slide with text bullets (JM, 2026-11-13). Its densities are raw residuals from the global line. The fit's observation shift is not removed, so the record types differ by method and by which species each method samples (prey remains are mostly Accipitriformes and Feliformia). The dashed line is the median of the 14 theropod stomach records with a measured prey mass (brackets not used); the percentages are the share of living records of that type below it.

## Images

`img/` holds the figures used on slides. Sources: `compsognathus_gut.jpg` Conrad 2017 fig. 2 (© Linnean Society); `microraptor.jpg` O'Connor et al. 2011 (IVPP V17972); `gorgosaurus.jpg` Therrien et al. 2023 fig. 1A (CC BY); `sinocalliopteryx.jpg` Xing et al. 2012 (CC BY); `velociraptor.jpg` Wikimedia Commons, "Fighting dinosaurs (1)", Yuya Tamai, CC BY 2.0; `logo_nsf.svg` and `logo_coe.png` Wikimedia Commons. The site is public on GitHub Pages: check the two non-CC figures before pushing.

## Outlier records (not shown since 2026-11-13)

The title-slide sidebar was removed (JM); the `highlights` block is still exported and the crops are still in `img/highlights/`, unused. Titles no longer colour their emphasised words. The acknowledgments slide shows `img/python_sunbear.png` (Fredriksson 2005, fig. 1: a reticulated python that had swallowed an adult female sun bear), cropped from a screenshot JM supplied.

Four living records where the recorded prey is far heavier than the predator. Three are database rows, chosen from the largest prey : predator ratios among sources with a PDF in `archive_old_pipeline/compiled/Compiled/`, and each read in that PDF. The table (pair id, image, what the paper says) is in `code/site_export.R` (`highlights`); the ratio is prey mass / predator mass from the analysis rows, so it uses taxon-average masses. `img/highlights/*.png` are crops of each paper's title block (220 dpi): Elmeros 2006 (weasel, roe deer: "probably represent opportunistic scavenging of carrion"), Marr & Knight 1983 (golden eagle, "Cattle carrion (Bos taurus)", 1 record), Pemberton et al. 2008 (Tasmanian devil, cattle in scats), The fourth is not a database row: Joubert 2006 (lions killing elephant calves at Savute; not ingested), with the prey taken as the 6-year-old male calf in the text. Its mass (about 930 kg) is shoulder height at age 6 from Lee & Moss 1995 Table I put through the height-mass equations of Laws et al. 1975 and Larramendi 2016 (886-974 kg); the lion mass is the species average in the rows. A *Nerodia taxispilota* / *Ictalurus punctatus* card (Camp et al. 1980) was dropped for it; `img/highlights/camp1980.png` is unused. These are crops of copyrighted pages: check before pushing to the public site.

## Silhouettes

`tools/phylopic/fetch_silhouettes.py` downloads one PhyloPic silhouette per minor clade (and per theropod group) and writes `img/phylopic/*.svg`, `img/phylopic/credits.csv` (taxon, contributor, licence, PhyloPic page) and `data/silhouettes.js`. Only CC0 / public-domain images are taken, so none needs attribution; no such image exists for a compsognathid. The pick is the first hit for a list of names in the script (e.g. Feliformia = *Lynx pardinus*, Serpentes = *Crotalus aquilus*, Teiioidea = *Aspidoscelis sonorae*); edit the list and rerun to change one. They are drawn beside clade names only where a slide picks out a few examples: slide 1 (above each genus), slide 3 last build (clades with their own colour), slide 4 (the highlighted pair), slide 10 (the two end clades). **I** hides them.

## Deploy

`.github/workflows/pages.yml` publishes `static/` to GitHub Pages on a push to `master` that touches `static/`. One-time setup: Settings → Pages → Source: GitHub Actions.
