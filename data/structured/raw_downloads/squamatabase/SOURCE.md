# SquamataBase (snake diet records)

Downloaded 2026-11-11 by the source-intake session. Files are stored exactly as downloaded (zips not extracted; nothing in them has been run).

| file | what | source | version | md5 |
|---|---|---|---|---|
| `squamatabase-sb_v1.0.0.zip` | archived stable release of the R package (data in `inst/diet.csv`, 11,091 lines; `data/diet.RData`) | Zenodo, https://doi.org/10.5281/zenodo.3667777 (concept DOI 10.5281/zenodo.3667776) | sb_v1.0.0, 2020-02-14 | 62c434ee673bd03908a818100d1579ce (matches Zenodo) |
| `squamatabase-master-97cf7d7.zip` | snapshot of the current GitHub master (same layout; `inst/diet.csv`, 11,209 lines) | https://github.com/blueraleigh/squamatabase, commit 97cf7d756f59e3ee0fb72d4c3fe5e1c1fccb42ec (2026-09-20) | development, post-1.0 | a7be1490045bfda12a5721995deb23ea |

- Licence: CC0 (package DESCRIPTION, both versions). Zenodo labels the record "other-open". The GitHub repo has no LICENSE file.
- Paper: Grundler MC (2020) SquamataBase: a natural history database and R package for comparative biology of snake feeding habits. Biodivers Data J 8:e49943. doi:10.3897/BDJ.8.e49943. Copy at `pdfs/extant_to_ingest/toadd/Grundler_2020_BDJ_SquamataBase.pdf`.
- `diet.csv` columns: predator, predator_verbatim, predator_rank, predator_taxon, predator_age, predator_sex, predator_count, predator_mass, predator_svl, predator_tl, predator_voucher, prey, prey_verbatim, prey_rank, prey_taxon, prey_age, prey_sex, prey_count, prey_mass, prey_tl, prey_ingested, prey_voucher, locality_adm0_name, locality_adm1_name, locality_adm2_name, locality_misc, locality_longitude, locality_latitude, event_basis, event_setting.
- Correction to the plan: `docs/ASSESSMENT_AND_PLAN_2026-11.md` gives "Zenodo record 3739652" for the data. That record is Figure 1 of the paper (a Plazi deposit), not the data. The data DOI is 10.5281/zenodo.3667777.
- Line counts are raw text lines minus the header, not checked as parsed records. The plan's counts (18,304 predators, 58,633 prey items) presumably sum `predator_count` and `prey_count`. Not checked.
