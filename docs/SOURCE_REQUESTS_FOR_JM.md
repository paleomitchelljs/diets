# Source requests for JM

First written 2026-11-11 at the end of the source-intake session (Prompt 0 in `docs/ASSESSMENT_AND_PLAN_2026-11.md`). Updated the same day after JM's pass through the list. The numbers below (A1–C34) are the numbers from the first version, so they match JM's replies.

The full record is `docs/source_intake_log.csv`, one row per item. Each row touched in JM's pass has a note starting `[2026-11-11 JM pass]`.

## Added in Phase 3 (2026-11-12): method papers behind the fossil masses

The fossil mass estimates (`data/fossil_rebuild/step2_mass_estimates.md`) rely on five method papers that are not on disk. Each is now a `NEEDED` row in `docs/methodology_index.csv`, so the statement further down that "nothing is marked `NEEDED`" no longer holds. Most important first:

1. **Christiansen P, Fariña RA (2004)** Mass prediction in theropod dinosaurs. Historical Biology 16:85–92. Save as `pdfs/methodology/Christiansen_Farina_2004_HistBiol_theropod_mass.pdf`. The published masses of *Gorgosaurus* TMP 2009.12.14, both *Citipes* and four *Microraptor* specimens use it. It is now taken second-hand from Therrien et al. 2023 SI. Citation from Hone 2022 and Therrien 2023; DOI not checked.
2. **Benson et al. 2014, Appendix S1** (PLoS Biol 12:e1001853; open access). Save as `pdfs/methodology/Benson_etal_2014_PLoSBiol_dinosaur_body_mass_AppendixS1.pdf`. It holds the regressions that turn shaft diameters into circumferences for the "FC estimated" masses in S11.
3. **Froese R (2006)** Cube law, condition factor and weight–length relationships: history, meta-analysis and recommendations. J Appl Ichthyol 22:241–253. Save as `pdfs/methodology/Froese_2006_JApplIchthyol_fish_length_weight.pdf`. **Citation from memory; verify it before fetching.** It is needed to replace an assumed length–weight coefficient for three fossil fish prey.
4. **Alexander RMcN, Jayes AS, Maloiy GMO, Wathuta EM (1979)** Allometry of the limb bones of mammals from shrews (Sorex) to elephant (Loxodonta). J Zool Lond 189:305–314. Save as `pdfs/methodology/Alexander_etal_1979_JZool_mammal_limb_allometry.pdf`. Behind Hu et al. 2005's *Repenomamus* mass.
5. **Van Valkenburgh B (1990)** Skeletal and dental predictors of body mass in carnivores. In Damuth J, MacFadden BJ (eds) Body Size in Mammalian Paleobiology, pp. 181–205. Cambridge UP. Save as `pdfs/methodology/VanValkenburgh_1990_BodySizeMammPaleobiol_carnivore_mass.pdf`. Same use; book chapter.

Fossil sources that would settle open conflicts (`data/fossil_rebuild/step2_conflicts.md`) belong on the Phase 4 gap list. They are Xu et al. 2003 (*Microraptor gui*; conflict 28), Ji et al. 2007 (A7 below; conflict 22) and Ji & Ji 1997 (conflict 25).

## Still needed

One item, and it blocks nothing.

**A3, Farlow et al. 2005, is now on disk.** The first save was an empty file. JM saved it again as `Farlow_2005_JVP_alligator_femur_mass.pdf`, and I renamed it to the index name `pdfs/methodology/Farlow_etal_2005_JVP_alligator_femur_body_size.pdf`. It has 16 pages and the title matches, but it is image-only. A tesseract OCR text (300 dpi, about 11,700 words) sits beside it as `Farlow_etal_2005_JVP_alligator_femur_body_size.ocr.txt`. OCR can garble numbers and equations, so check every equation against the page image before using it. The paper also points to supplementary data on the SVP website; we don't have those and don't yet know that we need them.

1. **A7. Ji S, Ji Q, Lü J, Yuan C (2007) A new giant compsognathid dinosaur with long filamentous integuments from Lower Cretaceous of northeastern China. Acta Geol Sin 81(1):8–15.** Optional now.
   The two alternatives JM suggested cover part of it:
   - Xing et al. 2012 (PLoS ONE, already in step 1) describes the holotype's gut (JMP-V-05-8-01; dromaeosaurid leg, tibia 15.5 cm). It also gives maxilla, metatarsal III and radius lengths for the predator. It gives no femur.
   - `e989b8f2c12a54fcec914815fef65c9f91f0.pdf` is a second copy of Qiu et al. 2025 (same OUP file, without the download watermark). Qiu 2025 is already in step 1. Neither it nor its SI measures *Sinocalliopteryx*; SI Table S1 is *Sinosauropteryx lingyuanensis*. I moved the copy to `pdfs/fossil_cases/_duplicates/Qiu2025__e989b8f2c12a54fcec914815fef65c9f91f0.pdf`.
   - A third source fills the femur gap. Benson et al. 2014 S11 (`data/mass_references/journal.pbio.1001853.s011.XLS`) lists JMP-V-05-8-01 with femur length 250 mm, femur mediolateral diameter 25.5 mm, tibia 280, humerus 125 and radius 94 mm, "measured from figures in Ji et al. 2007". These are figure measurements, so they are less precise than measurements from a table. (Benson's radius is 94 mm; Xing 2012 measured 100.7 mm.)
   Ji 2007 is now needed only if we want Ji's own table values or his original gut description.
   Save as `pdfs/fossil_cases/Ji_etal_2007_ActaGeolSin_Sinocalliopteryx_gut.pdf`

## Closed without a file

- **A1, Xing 2013 SI.** JM checked: Fig. S1 is a photograph of the *Microraptor* skull. Status `not_needed`. So the QM V1002 radius, tibia and humerus values in the old tables (rebuild conflict 11) still have no traceable source. Benson 2014 S11 doesn't list QM V1002 either; its *M. gui* row is IVPP V13352. Phase 3 should use only the femur and other values printed in Xing 2013's main text.
- **A11, Kear 2006 SI.** Turtles are low priority. Status `deferred`.
- **A15, Yuan 2004 (*Yanornis*).** JM asked what this was for. Xing 2012's Table 1 cites "Yuan; Zhou et al." as the sources for fish in *Yanornis*. Zhou et al. 2002 (A17, now on disk) turns out to be the primary report: macerated fish (teleost vertebra, fin rays, opercular fragments) in IVPP V13259. Zhou et al. 2004 (A13) cites Zhou 2002, not Yuan, for that specimen. So Yuan is probably redundant. Status `not_needed`. Reopen it only if a second fish-bearing *Yanornis* specimen turns up that is credited to Yuan.
- **C24, Galician porpoise series.** Dropped. Marine vertebrates are deprioritised (see decision 6 in the plan).
- **C33, CarniDIET Dryad v2.** Not needed; the GitHub CSV is the working copy. One caution: GitHub's CSV was last changed on 2021-04-06, and Dryad v2 is dated 2022-03-16, so the dates don't show that GitHub is newer. The 248-byte size gap between the two files fits Dryad still holding non-breaking spaces that a 2021-04-06 GitHub commit removed. If that's the cause, the records are identical. Not verified. Details are in `data/structured/raw_downloads/carnidiet/SOURCE.md`.
- **C34, Gelman & Hill 2007.** JM has the printed book. Status `print_only`; cite from print.

## Received and checked (2026-11-11)

Each file was opened and its title and authors checked. Page count matches the citation and every file has a text layer.

| item | file | note |
|---|---|---|
| A2 | `pdfs/fossil_cases/Nesbitt_etal_2006_BiolLett_Coelophysis_crocodylomorph_gut_SI.pdf` | 5 pp |
| A4 | `pdfs/methodology/Meiri_2010_JZool_lizard_length_mass.pdf` | 9 pp |
| A5 | `pdfs/methodology/Feldman_Meiri_2013_BiolJLinnSoc_snake_length_mass.pdf` | 12 pp |
| A6 | `pdfs/fossil_cases/Hone_etal_2022_JVP_Microraptor_mammal_gut.pdf` | Saved to `pdfs/methodology/`; moved to `pdfs/fossil_cases/`. Publisher version, JVP 42(2):e2144337 |
| A8 | `pdfs/fossil_cases/Currie_Chen_2001_CanJEarthSci_Sinosauropteryx_gut.pdf` | 23 pp. JM's note grouped A8 with A7 as not found, but this file is on disk and correct |
| A9 | `pdfs/fossil_cases/Peyer_2006_JVP_Compsognathus_Canjuers_gut.pdf` | 19 pp |
| A10 | `pdfs/fossil_cases/Hwang_etal_2004_JSystPalaeontol_Huaxiagnathus_gut.pdf` | 19 pp |
| A12 | `pdfs/fossil_cases/Wang_etal_2020_JSystPalaeontol_Abitusavis_fish_gut.pdf` | 24 pp |
| A13 | `pdfs/fossil_cases/Zhou_etal_2004_Naturwiss_Yanornis_gastroliths_fish_gut.pdf` | 4 pp. A byte-identical copy named `hou_etal_2004_...` was moved to `_duplicates/` |
| A14 | `pdfs/fossil_cases/Cau_2024_BSPI_GMV2124_tyrannosauroid_mammal_gut.pdf` | 19 pp. JM: treat taxonomic reassignments with great scepticism unless several papers support the new frame (decision 5 in the plan) |
| A16 | `pdfs/fossil_cases/Zhou_Wang_2010_SciChinaEarthSci_Yabeinosaurus_fish_gut.pdf` | 14 pp. A review; may not be the primary report |
| A17 | `pdfs/fossil_cases/Zhou_etal_2002_Nature_Yanornis_Archaeoraptor.pdf` | 1 p. Reports fish in *Yanornis* IVPP V13259 |
| B18 | `pdfs/methodology/Chabreck_Joanen_1979_Herpetologica_alligator_growth.pdf` | 8 pp. Check the second-hand TL→mass equation against it |
| C19 | `pdfs/methodology/Radloff_duToit_2004_JAnimEcol_savanna_predator_prey_size.pdf` | 14 pp |
| C20 | `pdfs/extant_to_ingest/toadd/Andreasen_etal_2017_MarMammSci_porpoise_diet_Baltic.pdf` | 27 pp. Early View version (no volume or pages in the PDF) |
| C21 | `pdfs/extant_to_ingest/toadd/Storer_1966_Auk_Accipiter_dimorphism_food.pdf` | 15 pp incl. repository cover page |
| C22 | `pdfs/extant_to_ingest/toadd/Santos_etal_2004_MarMammSci_porpoise_diet_Scotland.pdf` | 27 pp |
| C23 | `pdfs/extant_to_ingest/toadd/Knopff_etal_2010_JWildlManage_cougar_kill_prey.pdf` | 14 pp |
| C25 | `pdfs/methodology/Gilljam_etal_2011_AdvEcolRes_size_vs_taxonomic_food_webs.pdf` | 67 pp |
| C26 | `pdfs/methodology/Vezina_1985_Oecologia_terrestrial_vertebrate_prey_size.pdf` | 11 pp |
| C27 | `pdfs/methodology/Cohen_etal_1993_JAnimEcol_predator_prey_body_sizes.pdf` | 13 pp |
| C28 | `pdfs/methodology/vandePol_Wright_2009_AnimBehav_within_between_centering.pdf` | 6 pp |
| C29 | `pdfs/methodology/Hadfield_Nakagawa_2010_JEvolBiol_phylogenetic_mixed_models.pdf` | 15 pp |
| C30 | `pdfs/fossil_cases/Zheng_etal_2011_PNAS_Hongshanornis_Sapeornis_seed_crop.pdf` | 4 pp |
| C31 | `pdfs/fossil_cases/Zheng_etal_2018_ProcRSocB_Eogranivora_seed_crop.pdf` | 10 pp |
| C32 | `pdfs/fossil_cases/Zhou_Zhang_2002_Nature_Jeholornis_seed_gut.pdf` | 5 pp. Page 1 opens with the end of the preceding Nature letter; the *Jeholornis* letter follows |

`docs/methodology_index.csv` now has 27 `on_disk`, 1 `on_disk_elsewhere` and 1 `print_only`. Nothing is marked `NEEDED`.

## Status values added in this pass

`not_needed` (checked, holds nothing we need), `deferred` (JM postponed it), `dropped` (out of current scope) and `print_only` (JM has a paper copy only). These join the six values already in use: `obtained`, `already_on_disk`, `open_but_download_failed`, `paywalled`, `not_found`, `citation_unresolved`.
