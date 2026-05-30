# Source-material intake system

A single registry tracks every source file (PDFs + structured data drops) and
whether we've pulled data out of it. The goal: at any moment, know what's been
processed and what's left, and trace every observation back to its source.

## The registry: `data/intake_manifest.csv`

One row per source file, built/refreshed by `code/x08_intake.R` (scans `pdfs/`).
Columns:

| column | meaning |
|---|---|
| `file` | basename (the stable key; survives moves) |
| `path` | current location relative to repo root |
| `category` | `extant_pdf` / `extant_data` / `fossil_pdf` / `reference` |
| `format` | pdf / csv / xlsx / … |
| `obstype_hint` | guessed observation mode (`gut`/`scat`/`pellet`/`observation`/`unknown`) — refine on processing |
| `status` | `queued` → `processing` → `done`; also `irrelevant`, `superseded`, `structured` |
| `target_table` | where extracted rows landed |
| `n_obs` | rows extracted |
| `ref_ids`, `date_added`, `date_processed`, `notes` | bookkeeping |

`x08_intake.R` is **idempotent**: it matches existing rows by `file`, so re-running
never resets a `status` you've set. New files appear as `queued`. Files whose name
contains `_done` seed as `done` (e.g. the already-extracted fossil cases).

## Provenance rule

Every extracted observation row carries a **`source_file`** column = the manifest
`file`. So any data point in a staging table traces back to one source. (The
older `data/extant_new_observations.csv` predates this and should get a
`source_file` column on its next edit.)

## Two intake paths

1. **Papers (PDFs)** — read and extract by hand into the long staging schema
   (`data/extant_new_observations.csv` for extant, `data/fossil_diet_extractions.csv`
   for fossil), setting `source_file`. Then set the manifest row `status=done`,
   fill `n_obs`/`ref_ids`/`date_processed`.
2. **Structured drops (CSV/XLSX)** — each gets a small harmoniser
   `code/x0N_ingest<Name>.R` that tidies it to a long table under
   `data/structured/` **and stamps its own manifest row done** (see
   `x09_ingestGatorJekyll.R` as the template). No hand-editing needed.

## Clearing the queue (folder convention — proposed)

Once a file is `done`, move it to `pdfs/_processed/<category>/` so the
`extant_to_ingest/` and `fossil_cases/` folders show only remaining work. The
manifest keeps the link by `file`, and `x08_intake.R` re-finds it under the new
path on the next scan. (Status-only tracking, without moving, also works — pick one.)

## Typical loop

```bash
Rscript code/x08_intake.R          # refresh: new files -> queued
# ...process a file (paper by hand, or write/run an x0N ingest script)...
Rscript code/x08_intake.R          # status preserved; queue shrinks
```
