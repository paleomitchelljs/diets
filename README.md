# DietProject

**Predator–prey body-mass ratios in living and extinct vertebrates** — a database + modelling
pipeline that builds an extant predator–prey mass reference and places fossil gut contents (dinosaur
diets, etc.) against it.

This README is the map. For the scientific narrative see the memory files (linked at the bottom) and
`docs/history/` for how earlier model phases were decided.

---

## 1. The question and the core idea

Two goals:
1. **Map** how prey mass scales with predator mass across living clades (birds, crocodilians, lizards,
   snakes, mammals), and how it varies by predator size and observation mode.
2. **Place extinct taxa** — theropod gut contents and other fossil diets — against that extant reference.

**The organizing problem is taphonomic/observational bias.** Fossil diet is almost all *gut contents*,
which preferentially preserve **vertebrate** prey, biased toward **larger** items (small/soft prey
digest away). Extant diet data is a mix of stomach, scat, pellet, and direct-observation, each biased
differently. A naive fossil-gut-vs-extant comparison is apples-to-oranges.

**Current approach = matched filter, not bias-correction.** Rather than model the preservation filter and
"correct" fossils back to an unbiased reference (poorly identified), we **subset the extant data to share
the fossil filter** and compare like-to-like:
- **Vertebrate prey only** (invertebrate prey excluded — matches "bone preserves, cuticle doesn't").
- **Observation mode modelled, fossils predicted at `obstype = Stomach`** — extant stomach contents carry
  the same digestion filter as a fossil gut (small prey under-represented), so Stomach is the matched
  analog. All stomach data is retained in full; other modes inform the offset only.

(Full rationale: memory `diet-project-findings.md` → "THE MATCHED-FILTER REFRAME".)

---

## 2. Current state (2026-06)

**Model:** `data/fit_gutslopes_cfix.rds` (+ `gut_model_meta.rds`) — extant-only, clade-FIXED,
vertebrate-prey-only. Built by `code/run_vertonly.sh`. Converged (R-hat ≈ 1.00, 0 divergences).

```
preyMass | weights(certainty) ~ predmass_c*clade + predmass_c*obstype + (1|species) + (1|pred_specimen)
sigma ~ 0 + clade + predmass_c + obstype          # gaussian; response = ln(prey mass), NOT PPMR
```
15,260 rows (8,699 = Stomach gut contents). Per-clade prey~predator slopes at Stomach:
Aves 1.08 · Mammalia 0.89 · Lacertilia 0.89 · Serpentes 0.70 · Crocodylia 0.17 (wide CrI; 3-species data limit).

**Headline fossil result** (`code/x07_fossilPlacement.R`): controlling for the gut filter (predicting at
Stomach) pulls apparent theropod relative prey size *down* vs same-size birds, and on a vertebrate-to-
vertebrate, size-matched basis the theropod gut sample most resembles **saurophagous lizards (Lacertilia)**
— the speculation being shared *ziphodont* prey-processing, not shared ecology (untestable with current
data; see the conference abstract `abstract_svp26.docx`).

**Open caveats:** Crocodylia is the thin cell (recently extended from a near-point at 925 g to a 2.8 kg–100 kg
range via new croc records, ref 206–226). The ziphodont mechanism is a hypothesis the current model cannot
adjudicate (processing ⟂ ecology are confounded in the one clade that matches). See "roadmap" below.

---

## 3. Pipeline (`code/`, run from repo root; each script finds ROOT from its own path)

### Core current pipeline (extant reference → fossil placement)
| step | script | in → out |
|---|---|---|
| 1 | `x00_reformatData.R` | `raw_data.csv` → `longform.csv` (one row per prey item; adds `prey_type` invert/vert; applies `mass_overrides.csv` to NA masses only) |
| 2 | `x03_massData.R` | fill mammal (PanTHERIA) + bird (AVONET) masses → `mass_overrides.csv`, `mass_gaps.csv` |
| 3 | `x20_gutSlopesStan.R` | the model. Assembles longform + ADB + `extant_new_observations.csv`, vert-only matched filter, clade-fixed brms fit → `fit_gutslopes_cfix.rds`, `gut_model_meta.rds`, `gut_slopes_posterior.csv`, `gut_sigma_by_clade.csv`. **Driver: `run_vertonly.sh`** (sets cores/iter; `GUT_FULL=1` also fits an obstype:clade variant + LOO) |
| 4 | `x22_buildFossilPoints.R` | curated fossil cases → `fossil_ppmr_points.csv` (theropod gut + predation events, tier-tagged) |
| 5 | `x07_fossilPlacement.R` | posterior-predict each fossil vs the extant clades at Stomach → `fossil_placement.csv`, `theropod_best_match.csv`, `theropod_clade_similarity.csv` + figures |

```bash
Rscript code/x00_reformatData.R           # rebuild longform.csv (only if raw_data/overrides changed)
Rscript code/x03_massData.R               # rebuild mass fills (needs data/AVONET, data/pantheria)
bash    code/run_vertonly.sh              # fit the model + place fossils (STAN_CORES=4; ~30 min if machine free)
# x07 is invoked by run_vertonly.sh; run it alone to re-place fossils on an existing fit:
Rscript code/x07_fossilPlacement.R
```

### Data ingestion (build the structured tables x20 consumes)
`x08_intake.R` (intake registry → `intake_manifest.csv`) · `x09_ingestGatorJekyll.R` · `x10_ingestRaptorDiets.R` ·
`x11_sourcesDoc.R` (→ `INTEGRATED_SOURCES.md`) · `x13_ingestTuckerMammals.R` · `x14_ingestAvianDietDB.R` ·
`x15_avianGutPPMR.R` (→ `aviandiet_gut_ppmr.csv`, the Aves Stomach source) · `x16_fishbaseCoef.R` ·
`x18_predatorTraits.R` (→ `predator_traits.csv`) · `x19_fossilMassEstimates.R` (femur→mass → `fossil_mass_estimates.csv`) ·
`x23_teguDiet.R` (tegu structured tables).

### Earlier-phase / exploratory (kept; not on the current fossil-placement path)
`x01_simpleAnalysis.R`, `x02_phyloModel.R` (phylogenetic fit — revisit under roadmap P3) ·
`x05_cladeSlopes.R`, `x06_modeReframe.R` (lme4 clade-slope + obstype-confound quick-looks) ·
`x17_predmassScaling.R` (partial-pooled scaling) · `x12_ppmrThroughTime.R` · `x21_slopeForests.R` ·
`x04_summariseFit.R` (summarize any `fit_*.rds`) · `x_validate.R` (data-validation checks).
Superseded scripts live in `code/_legacy/` (old `x07_gutComparison`, the prey_type-variant `x22_compareVariants` + its driver).

---

## 4. Where things live

```
code/              analysis pipeline (above). code/_legacy/ = superseded scripts.
data/              raw_data.csv (source) + curated tables + the current fit + regenerable outputs.
  data/structured/   ingested third-party tables (aviandiet, fishbase, raptor, tucker, gator, tegu, varanus…)
  data/_archive/     superseded fits/outputs, parked (gitignored, regenerable)
  data/AVONET/, data/pantheria, data/mass_references/  third-party mass sources (gitignored)
figures/           project plots (fossil placement, model facets, PPMR-vs-time, similarity…)
docs/history/      retired decision docs (how earlier model phases were chosen)
pdfs/              source PDFs (gitignored, copyrighted). pdfs/extant_to_ingest/toadd/ = clean DROP ZONE for new papers.
                   pdfs/_processed/ = filed after extraction (organized by batch/disposition, each with a README).
logs/              run logs (gitignored).
abstract_svp26.docx   the SVP 2026 conference abstract.
```

### Key data files (tracked, curated)
- `raw_data.csv` — the source predator-records × prey-species matrix. **Never silently edited.** Field names
  are in ROW 3; prey matrix starts col 31 (row1 prey genus, row2 prey class, row3 prey mass).
- `extant_new_observations.csv` — hand-extracted records (229 rows; has `source_file` provenance + `prey_stage`).
  New paper extractions go here, not into raw_data.
- `longform_refs.csv` — RefNo → citation map for the raw_data studies.
- `fossil_predprey.csv`, `fossil_diet_extractions.csv` — curated fossil cases (feed x22).
- `mass_overrides.csv` (NA-only fills), `mass_gaps.csv`, `predator_traits.csv`, `prey_habitat_by_taxon.csv`,
  `actualistic_studies.csv` (digestion/feeding-trial registry for calibrating the preservation filter).
- Docs: `DATA_AUDIT.md`, `observation_bias_notes.md`, `reconciliations.md` (data-quality log),
  `external_data_wanted.md` (fetch list), `INTEGRATED_SOURCES.md`, `key_cites.md`,
  `theropod_gut_mass_estimates.md`, `data/structured/ai_found_2026-06-02_intake.md` (the running intake ledger).

---

## 5. Conventions

- **No silent edits to `raw_data.csv`.** New observations → `extant_new_observations.csv`; mass fills →
  `mass_overrides.csv` (NA-only; delete to revert).
- **Records need prey to species/genus (+ stage).** Prey mass is inferred from the taxon (PanTHERIA/AVONET/
  FishBase/squamate-SVL), so category-only diet studies aren't usable. Record genus + species + stage
  ("juvenile"/"egg"/etc.) — stage scales the inferred mass.
- **Carrion is signal, not noise** — hunt-vs-scavenge is unobservable in fossils; the comparison is
  observed gut-ratio (modern) vs observed gut-ratio (fossil). No scavenging/hunting flags.
- **Verify before flagging data** — read the source before calling a record pathological (memory
  `verify-before-flagging-data.md`).
- **Data-quality issues → `reconciliations.md`**; archives (`archive_*/`, `code/_legacy/`, `data/_archive/`) are write-once.
- **PanTHERIA is malformed** — read mammal mass by column POSITION 5 (memory `pantheria-file-malformed.md`).

## 6. Running Bayesian fits
`STAN_CORES=4` when the machine is free (lower `uptime` load); the fit takes ~30 min. The model is launched
detached for long runs (`nohup caffeinate -dimsu bash code/run_vertonly.sh &`); the fit is saved immediately
after sampling, so a sleep/interruption can't lose it. (Repo is SHARED — github.com/paleomitchelljs/diets,
collaborator branches `annika`/`cassie`; do not touch them. pdfs/ must never be pushed.)

## 7. Roadmap
- **Densify Crocodylia** — the thin/uncertain cell; mine more croc stomach data with prey-to-species.
- **Bound the within-vertebrate size filter** — the one taphonomic axis subsetting can't match; calibrate
  with actualistic digestion data (`actualistic_studies.csv`: Andrews corrosion, Fisher croc digestion).
- **Test the ziphodont/processing hypothesis** — needs a dentition/oral-processing covariate with within-clade
  variation (felid-slicing vs canid-crushing) to break the processing⟂clade confound.
- **Real time-calibrated phylogeny** (P3) — replace the placeholder tree in `x02` so fossils sit inside the tree.

---
*Memory (auto-loaded each session): `diet-project-pipeline.md` (this map, in brief), `diet-project-findings.md`
(scientific findings + the matched-filter reframe), `x20-model-redesign.md` (the model), plus
`pantheria-file-malformed.md` and `verify-before-flagging-data.md`.*
