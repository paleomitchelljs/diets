# code/db_migrate: building `db/` (Phase 2 extant migration, Phase 3 fossil rows)

Run everything (Phase 2 extant scripts and the Phase 3 fossil script): `Rscript code/db_migrate/run_all.R` (add `--evidence` to first rebuild the evidence tables; that needs `pdftotext` and the PDFs under `archive_old_pipeline/compiled/` and `pdfs/`).

Results, counts and open decisions: `db/MIGRATION_REPORT.md`.

## Ownership rule

Each `0N_*.R` script owns the `sources.csv` rows whose `extracted_from` starts with its input path (`OWNS` at the top of the script). On each run it deletes those sources, every pair and measurement citing them, and appends what it builds. It stops if a source_id or pair_id it is about to write belongs to another script. `taxa.csv` and `methods.csv` are shared and upserted by key. Rows are written sorted by key, so a rerun of any script, alone or through `run_all.R`, leaves the tables byte-identical. Nothing here empties `db/`, so rows from other phases survive.

| Script | Input | Owns `extracted_from` prefix |
|---|---|---|
| `00_merged_sources.R` | `evidence/duplicate_studies.csv` (one source per publication found in several upstream tables; writes no pairs; run it before 01–09) | `merged:` |
| `01_rawdata.R` | `data/raw_data.csv`, `longform_refs.csv`, `mass_overrides.csv` | `data/raw_data.csv` |
| `02_aviandietdb.R` | `data/structured/aviandiet_gut_ppmr.csv` (+ `aviandietdb_dietdb.csv` for the original study) | `data/structured/aviandiet_gut_ppmr.csv` |
| `03_extant_new_observations.R` | `data/extant_new_observations.csv` | `data/extant_new_observations.csv` |
| `04_raptordiets.R` | `data/structured/raptor_diets_long.csv` | same |
| `05_tucker2016.R` | `data/structured/tucker2016_mammal_ppmr.csv` | same |
| `06_jekyll.R` | `data/structured/gator_jekyll_stomach.csv` | same |
| `07_glaudas2019.R` | `data/structured/glaudas2019_snake_rpm.csv` | same |
| `08_tegu.R` | `data/structured/tegu_diet_individuals.csv`, `tegu_diet_dietcategories.csv` | `data/structured/tegu_diet_` |
| `09_varanus.R` | `data/structured/varanus_*_summary.csv` (+ Mazzotti Table 2 in `pdfs/.../nilemonitor/`) | `data/structured/varanus_` |
| `10_fossil_step2.R` (Phase 3) | `data/fossil_rebuild/step1_*.csv` + Benson et al. 2014 Dataset S1; also writes `data/fossil_rebuild/step2_*.csv` and `db/excluded_fossil_records.csv` | `data/fossil_rebuild/` (pairs: step-1 entry ids, `pair_tag` `^[A-Za-z]+[0-9]{4}[a-z]?_[0-9]{2}$`) |

## Evidence tables (`evidence/`)

Every coding decision that is not mechanical lives in a table here, with the source wording it rests on, not in the scripts:

- `methods_registry.csv`: registered equations, with coefficients transcribed from the rendered pages of the PDFs in `pdfs/methodology/`. `methods.csv` rows are copied from here.
- `squamate_genera.csv`: genus → family, `analysis_clade` (Serpentes or Lacertilia) and the equation to use. It replaces x20's genus regex.
- `rawdata_study_decisions.csv`: hand decisions for 27 raw_data studies (`duplicate_of` marks a RefNo whose records repeat another RefNo's) (`remains_dimension` overrides the default "wet mass per item" for remains weights).
- `rawdata_cell_decisions.csv`: decisions for one record or one prey column where a study-level rule does not fit (Maritz 2011, Korschgen 1959, Sovada 1999), and corrected predator masses (`pred_mass_g`, with the reason in `pred_basis`: Da Silveira 1999 line 124). Applied by `01_rawdata.R`; a row that matches no cell stops the script.
- `duplicate_studies.csv` (hand-checked) and `build_duplicate_evidence.R` → `duplicate_records.csv`: publications found in more than one upstream table, and which copy of each record is kept.
- `build_rawdata_evidence.R` writes `rawdata_studies.csv`, `rawdata_prey_columns.csv` and `rawdata_pred_records.csv`.
- `extant_new_obs_decisions.csv`: decisions by ref_id range, applied in order.
- `parse_oliveira2025.R` writes `oliveira2025_tableS2.csv`.

## Checks

- `compare_model_input.R` rebuilds x20's model input from the old files and compares it with the db tier A–D subset.
- `migration_counts.R` writes the report tables.
- `run_all.R` then runs `code/db_validate.R` and saves its output to `db/migration/validator_output.txt`. Run `Rscript db/tests/run_tests.R` separately.
- `run_all.R` runs every `NN_*.R` script in this folder, including the Phase 3 fossil script `10_fossil_step2.R`, so a build that starts from an empty `db/` ends with the fossil rows too. (On 2026-11-12 `run_all.R` ran only `0N_*.R`, and a rebuild from an empty `db/` lost the 74 fossil pairs.) If you run scripts one at a time, rerun `10_fossil_step2.R` after any rebuild of `db/` from empty.
- Do not run two sessions that write `db/` at the same time: each script reads the tables, replaces its own rows and writes the tables back, so concurrent runs can overwrite each other.

The old files in `data/` are only read.
