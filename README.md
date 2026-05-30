# DietProject

A database and modelling pipeline for **paired predator–prey body masses in living and extinct vertebrates**.

The goal: fit hierarchical predator–prey mass-ratio (PPMR) models from extant
diet records (scat, gut contents, observation studies), and use the fitted
clade- and strategy-level posteriors as a reference against which the diets of
extinct taxa (dinosaur gut contents, coprolites) can be placed — to ask whether
preserved fossil diets fall along the bird, crocodylian, squamate, or mammal
line.

## Layout

```
DietProject/
├── README.md                     ← this file
├── relaunch.md                   ← live runbook for the Stan fits
│
├── code/                         ← active analysis pipeline
│   ├── x00_reformatData.R        wide raw_data.csv → long longform.csv
│   ├── x01_simpleAnalysis.R      lme4 baseline + simple brms (RUN_BRMS=TRUE)
│   ├── x02_phyloModel.R          phylogenetic partial-pooling brms
│   ├── x03_massData.R            classify taxa, fill mammalian masses from PanTHERIA, write gap report
│   └── x04_summariseFit.R        load a fit_*.rds → effects, Rhat/ESS, phylo-signal λ
│
├── data/                         ← canonical datasets (extant + fossil)
│   │  -- Extant --
│   ├── raw_data.csv              wide predator-records × prey-species matrix (source of truth)
│   ├── extant_new_observations.csv  long-format staging for newly-ingested papers
│   │  -- Fossil --
│   ├── fossil_predprey.csv       curated (pre-existing) fossil cases
│   ├── fossil_diet_extractions.csv  newer extractions (cleaner schema)
│   │  -- Mass references --
│   ├── pantheria                 (malformed; read mass at COL POSITION 5, not header)
│   ├── mass_references/          theropod-Benson and supplementary mass datasets
│   │  -- Pipeline outputs (regenerable) --
│   ├── longform.csv              from x00
│   ├── mass_overrides.csv        from x03 (consumed by x00)
│   ├── mass_gaps.csv             from x03 (review queue)
│   ├── phylo_{tree,A}.rds, phylo_model.stan   from x02 validation
│   ├── fit_simple.rds, fit_phylo.rds  Stan fits (when complete)
│   └── reconciliations.md        living log of data-quality issues + their fixes
│
├── pdfs/                         ← source materials
│   ├── fossil_cases/             19 PDFs of named gut-content / coprolite cases
│   ├── extant_to_ingest/         queued extant diet studies
│   │   ├── ai_found/   (11 reptile-diet PDFs)
│   │   ├── literature/ (3 PDFs — turned out to be extant, not fossil)
│   │   └── toadd/      (148 PDFs awaiting ingestion)
│   └── reference/                methods + monographs (Campione 2014, Ostrom 1978,
│                                  Heckert 2009 Coelophysis monograph, PredatorPreyFossilBook)
│
├── images/fossil/                14 specimen JPGs for the fossil cases
│
├── archive_owl/                  Jebel Owl paper artifacts — DONE work, kept for the record
└── archive_old_pipeline/         pre-2020 analysis (clade/species/complex Stan models, etc.),
                                  superseded by code/x00–x04
```

## Running the pipeline

```bash
# rebuild the long-format dataset
Rscript code/x00_reformatData.R
# (optional) regenerate mass overrides from PanTHERIA
Rscript code/x03_massData.R
# lme4 baseline (always)
Rscript code/x01_simpleAnalysis.R
# Bayesian fits (compile + sample Stan; see relaunch.md)
RUN_BRMS=TRUE Rscript code/x01_simpleAnalysis.R   # data/fit_simple.rds
RUN_BRMS=TRUE Rscript code/x02_phyloModel.R       # data/fit_phylo.rds
# summarize a fit
Rscript code/x04_summariseFit.R data/fit_phylo.rds
```

## Conventions

- **No silent edits to `raw_data.csv`.** New extant observations go to
  `extant_new_observations.csv` as a reviewable side table. Mass fills go to
  `mass_overrides.csv`, which `x00` applies to NAs only (delete the file to
  revert).
- **Data quality issues are logged in `data/reconciliations.md`** with a
  comparison table, candidate explanations, recommended action, and (once
  applied) a resolution line. See the NIGP 127587 femur entry for the template.
- **Archives are write-once.** Files in `archive_owl/` and
  `archive_old_pipeline/` document past work — they aren't expected to be
  edited or re-run. If you find yourself wanting to revive something from
  there, copy it into `code/` and treat it as new.
