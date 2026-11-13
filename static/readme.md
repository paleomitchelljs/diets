# Talk site (draft)

A linear talk of ten screens on the fs110 deck style: a 1600 × 900 stage scaled to the window, chalkboard (dark) by default. No build step and no libraries; `index.html` opens straight from the file.

## Keys

→ / space / page down next · ← back · **S** scroll mode (every slide stacked at its last build) · **P** presenter window (current and next screen, notes, timer; kept in step over `BroadcastChannel`) · **T** dark / light · **B** blank · **?** keys.

## Data

The site reads only `data/data.js` (same content as `data/data.json`), written by `code/site_export.R` from `results/`. Rebuild with `Rscript code/site_export.R` after any fit changes. Environment variables in that script choose which fit folders feed the site.

## Slides

| # | Title | Builds | Source |
|---|---|---|---|
| 0 | Title | 1 | |
| 1 | One gut is one meal | 2 | *Compsognathus* (Conrad 2017) and every *Buteo jamaicensis* record |
| 2 | Borrowing from relatives is pooling | 2 | solo lines slide to partially pooled lines |
| 3 | The names are rules for whom to borrow from | 3 | skeleton tree; Crocodylia under no pooling, λ = 0, λ = 1 |
| 4 | The tree predicts first | 3 | theropod tip on the bird stem (b10), then the fossils |
| 5 | Specimens | 4 | *Microraptor*, *Gorgosaurus*, *Sinocalliopteryx*, *Velociraptor* |
| 6 | Which living animals do they eat like? | 2 | similarity test support (b08) |
| 7 | Close relatives scale differently | 2 | clade slopes |
| 8 | Does it get known answers right? | 1 | positive controls (b08) |
| 9 | Thanks | 1 | |

## Draft status

Slides marked "draft" use stand-in numbers:
- Solo lines (slides 2, 3) are per-clade OLS until `fit_unpooled` finishes; the λ = 1 panel uses the λ-free fit until `fit_lambda1` finishes.
- The tree band and the support bars come from the current fit with marine clades and a 20-draw debug run of the fossil test. They will change.
- Ziphodont marks (slide 6) are a placeholder: `db/` has no tooth-form field.
- No PhyloPic silhouettes yet (needs network at build time), no static fallback images yet.

## Deploy

`.github/workflows/pages.yml` publishes `static/` to GitHub Pages on a push to `master` that touches `static/`. One-time setup: Settings → Pages → Source: GitHub Actions.
