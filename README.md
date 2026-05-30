# DietProject

**Predator–prey body-mass ratios in living and extinct vertebrates.**

A database and modelling pipeline for paired predator–prey body masses, built to:

1. **Map** the predator–prey mass ratio — `PPMR = ln(prey mass) − ln(predator mass)` —
   across living vertebrates, and how it varies by clade, predator size, and feeding mode.
2. **Place extinct taxa** (dinosaur gut contents, coprolites, healed bite marks) against
   that extant reference — asking whether a preserved fossil diet falls along the bird,
   crocodylian, squamate, or mammal line, and how PPMR has evolved.

## The organizing problem: preservation / observation bias

Fossil diet evidence is overwhelmingly **gut contents**, which carry a strong size-dependent
preservation bias (large/robust prey survive and are identifiable; small/soft prey vanish).
Extant diet data, by contrast, come from a mix of **stomach contents, scat, pellets, and
direct observation** — and each mode is size-biased differently. So a naive comparison of a
fossil gut to an extant "bird line" (which is mostly scat/pellet data) is apples-to-oranges.

Two findings shape the methodology (see `data/` outputs and the notes below):

- **Observation mode and clade are nearly aliased** in the extant data (crocodilians are
  sampled almost only by stomach, birds almost only by scat/pellets), and the within-taxon
  overlap needed to *calibrate* a mode correction barely exists. So we **don't try to convert
  scat to gut**; instead we compare **gut-to-gut** (fossil gut vs extant stomach contents) and
  treat observation mode as an explicit **observation process** in the hierarchical model.
- **The "macropredator band."** Vertebrate-eating predators — snakes (−1.4 to −2.5),
  terrestrial mammalian carnivores (−1.99), vertebrate-eating birds (−1.89), and **fossil
  theropods (−1.7 to −5.5) and the fossil crocodyliform *Confractosuchus* (≈ −3.5)** — all
  cluster at relatively *large* prey (ln-PPMR ≈ −1.5 to −3.5). Small-prey specialists
  (crocodilians, lizards, marine mammals) sit far lower (≈ −7 to −8). Preserved fossil diets
  fall in the macropredator band, *not* the small-prey regime.

## Pipeline (`code/`, run from repo root)

| script | what it does |
|---|---|
| `x00_reformatData.R` | wide `raw_data.csv` → long `longform.csv` (one row per prey item); applies `mass_overrides.csv` |
| `x01_simpleAnalysis.R` | lme4 baseline + simple brms (centered predictor, Student-t; `RUN_BRMS=TRUE`) |
| `x02_phyloModel.R` | phylogenetic partial-pooling brms (phylo + non-phylo species, clade-varying slope). Placeholder taxonomy tree; drop a real Newick at `data/tree.tre` |
| `x03_massData.R` | classify taxa by clade; fill **mammal** masses (PanTHERIA) + **bird** masses (AVONET); write `mass_overrides.csv` + `mass_gaps.csv` |
| `x04_summariseFit.R` | summarize a `fit_*.rds`: effects, Rhat/ESS, phylo-signal λ |
| `x05_cladeSlopes.R` | per-clade & Aves-subclade PPMR-vs-size slopes (stomach-only) + plots |
| `x06_modeReframe.R` | observation-mode / preservation-bias reframe (the obstype confound analysis) |
| `x07_gutComparison.R` | **gut-to-gut**: extant stomach PPMR by clade vs fossil gut points (Squamata split into Serpentes/Lacertilia) |
| `x08_intake.R` | builds/refreshes `intake_manifest.csv` — the source-material registry (see `pdfs/INTAKE.md`) |
| `x09…x16` | structured-data ingests: gator stomach (`x09`), RaptorDiets (`x10`), Tucker mammals (`x13`), Avian Diet DB (`x14`), bird-gut PPMR (`x15`), FishBase coefficients (`x16`); plus the sources-doc generator (`x11`) and the PPMR-through-time plot (`x12`) |

```bash
Rscript code/x00_reformatData.R          # rebuild longform.csv
Rscript code/x03_massData.R              # regenerate mass_overrides + gaps (needs data/AVONET, data/pantheria)
Rscript code/x06_modeReframe.R           # mode/bias reframe (lme4, fast)
Rscript code/x07_gutComparison.R         # gut-to-gut comparison + figures
Rscript code/x12_ppmrThroughTime.R       # PPMR-through-time visual
# Bayesian fits (run on a real machine; cores=4. Sandbox: STAN_CORES=1, but slow.)
RUN_BRMS=TRUE Rscript code/x01_simpleAnalysis.R   # data/fit_simple.rds
RUN_BRMS=TRUE Rscript code/x02_phyloModel.R       # data/fit_phylo.rds
Rscript code/x04_summariseFit.R data/fit_phylo.rds
```

## Data (`data/`)

**Tracked (curated / small / project's own):**
- `raw_data.csv` — the source predator-records × prey-species matrix (the project's compilation).
- `fossil_predprey.csv`, `fossil_diet_extractions.csv` — fossil gut/coprolite/bite-mark cases.
- `extant_new_observations.csv` — long-format staging for hand-extracted papers (with `source_file` provenance).
- `mass_overrides.csv`, `mass_gaps.csv`, `intake_manifest.csv`.
- `reconciliations.md` (data-quality log), `observation_bias_notes.md` (methodological notes),
  `external_data_wanted.md` (what to fetch), `INTEGRATED_SOURCES.md` (auto-generated).
- `structured/` small lookups: `tucker2016_mammal_ppmr.csv`, `gator_jekyll_stomach.csv`,
  `aviandiet_gut_ppmr.csv`, `raptor_diets_gut.csv`, `fishbase_lw_coef.csv`.
- `mass_references/theropoda_benson.csv` (dinosaur femur→mass calibration).

**Not tracked (gitignored — copyrighted, large, or third-party; refetch per `external_data_wanted.md`):**
- `pdfs/` (copyrighted journal PDFs + raw data drops; only `pdfs/INTAKE.md` is tracked), `images/`.
- `data/AVONET/`, `data/pantheria`, `data/mass_references/*.xls*`.
- large `data/structured/*` (FishBase / aviandietdb / aviandiet_long+gut / raptor_diets_long raw exports).
- regenerable outputs (`longform.csv`, `fit_*.rds`, `phylo_*`, `*.log`) and `archive_*/`.

## Source-material intake

`pdfs/INTAKE.md` documents the system: `intake_manifest.csv` tracks every source file
(queued / done / structured / superseded); processed files move to `pdfs/_processed/`; every
extracted observation carries a `source_file` linking back to its origin.

## Conventions

- **No silent edits to `raw_data.csv`.** New extant observations → `extant_new_observations.csv`;
  mass fills → `mass_overrides.csv` (applied to NAs only; delete to revert).
- **Data-quality issues → `data/reconciliations.md`** (comparison table, candidate explanations,
  resolution). See the NIGP 127587 femur entry for the template.
- **Carrion/scavenging is signal, not noise** — hunt-vs-scavenge is unobservable in fossils;
  the comparison is observed gut-ratio (modern) vs observed gut-ratio (extinct).
- **Archives are write-once** (`archive_owl/`, `archive_old_pipeline/`).

## Status / roadmap

- **Done:** model hygiene (centered predictor, Student-t); the mode/bias reframe; the gut-to-gut
  comparison; the intake system; ingestion of the Avian Diet DB (→ a 697-species bird gut
  reference), RaptorDiets, the Tucker mammal-carnivore compilation, and AVONET/FishBase mass sources.
- **Next:** (P2) a fossil-placement bridge with full mass-uncertainty propagation; (P3) a real
  time-calibrated phylogeny to replace the placeholder tree and let fossils sit *inside* the tree;
  an observation-process model with a size-dependent preservation filter `g_mode(size)` — which
  digestion / feeding-trial data could **empirically calibrate** (see `observation_bias_notes.md`).
