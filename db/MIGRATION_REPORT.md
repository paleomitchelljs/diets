# Phase 2 migration report: extant predator–prey data into `db/`

Written 2026-11-12 for Prompt 2 of `docs/ASSESSMENT_AND_PLAN_2026-11.md`. Revised the same afternoon to cover JM's decisions of 2026-11-12 (duplicate publications, OS-Prey masses, two vocabulary values, pooled samples, one corrected mass) and a final check of the raw_data stomach studies (section "Final check of raw_data observation types and unchecked masses"). `db/` also holds the Phase 3 fossil rows; they are described in `data/fossil_rebuild/step2_mass_estimates.md`, not here. Every number below comes from `db/` or from `db/migration/*.csv`, which `code/db_migrate/run_all.R` regenerates.

Rerun: `Rscript code/db_migrate/run_all.R`. Add `--evidence` to first rebuild the evidence tables, which needs the PDFs and `pdftotext`. The run migrates, prunes unreferenced taxa, compares against the model input, writes the counts and runs the validator (output saved to `db/migration/validator_output.txt`).

## Result in brief

- The nine extant sources give 65,466 pairs, 1,352 sources and 839 measurements. With the Phase 3 fossil rows (74 pairs), `db/` holds 65,540 pairs, 1,388 sources, 6,590 taxa, 930 measurements and 38 methods. `code/db_validate.R`: PASS, 0 errors, 523 warnings. 495 are `unknown_method` flags on extant rows, which the prompt asks for. 25 `unverified_method` and 3 `index_status` come from the fossil rows. `db/tests/run_tests.R`: all tests pass.
- Each source has its own script under `code/db_migrate/`, plus `00_merged_sources.R` for publications that reached us through more than one table. A script deletes and re-appends only the sources whose `extracted_from` points at its input. It stops if a source_id or pair_id it wants to write belongs to another script. Two consecutive full runs leave the five tables, every file in `db/migration/`, every evidence table and the fossil side tables byte-identical (checked after the last change). `run_all.R` also runs the fossil script, so a build from an empty `db/` keeps the fossil rows.
- 63 publications reached us through two upstream tables. Each is now one source. 3,043 OS-Prey rows and 25 raw_data cells were dropped because another table holds the same records. One raw_data study (Holycross & Mackessy 2002) is entered twice within raw_data. The copy whose predator matches the paper is kept, and the other (21 cells) is dropped.
- No mass was invented. Where a source gave only a length, the mass comes from an equation transcribed and checked against the rendered PDF page. OS-Prey masses are AVONET and PanTHERIA species or genus means, as x15 did for AvianDietDB (JM's decision). 491 pairs have a mass whose method cannot be determined; they carry `unknown` and are listed.
- The rebuild of the model input reproduces the 3 June log exactly (144,152 vertebrate-prey rows, 15,260 after subsampling). Of the 144,152 rows, 135,479 (94.0%) are identical in the migrated tier A–D vertebrate subset. 1,203 (0.8%) are in it with a changed observation level or mass. 7,470 (5.2%) are not in it. Each difference has a named reason. After the same subsample the db gives 12,410 rows, not 15,260.
- The losses fall in the stomach cells. Tier A–D keeps 5 of 1,189 crocodylian stomach rows, 208 of 923 lizard stomach rows and 1,055 of 1,502 snake stomach rows. The crocodylian reference in the current fit rests on prey masses whose origin cannot be determined (Delany 1986, Delany 1990, Rice 2004) or on stomach-remains weights (Da Silveira & Magnusson 1999).
- Two raw_data studies coded "Stomach" are faecal studies (Stanner & Mendelssohn 1986, Goetz et al. 2017). They are now `scat`. Mayes 2006's prey values are converted remains weights, not prey masses.
- 72 individual pairs have tier A or B, vertebrate prey and both masses. 65 of them are Oliveira et al. 2025 lizard records whose observation type is `unknown`. Only 7 have a known observation type.
- The 3 June model input had no rows from `extant_new_observations.csv` ref 206–251. The assessment says the coarse alligator rows ref 207–226 were fitted; the log shows they were not.

## Rows in and out, per source

| Script | Input | Rows in | Dropped | Pairs out | Both masses | Individual pairs | Measurements | Sources written |
|---|---|---|---|---|---|---|---|---|
| `00_merged_sources.R` | `evidence/duplicate_studies.csv` | 63 merged publications in evidence/duplicate_studies.csv | 0 | 0 | 0 | 0 | 0 | 63 |
| `01_rawdata.R` | `data/raw_data.csv` (+ `longform_refs.csv`, `mass_overrides.csv`) | 517 predator records (rows 4+ of raw_data.csv); 7683 record x prey cells with count > 0; longform.csv has 171406 item rows | 315 | 7,369 | 6,000 | 77 | 156 | 143 |
| `02_aviandietdb.R` | `data/structured/aviandiet_gut_ppmr.csv` | 214 rows of aviandiet_gut_ppmr.csv | 0 | 214 | 214 | 3 | 0 | 14 |
| `03_extant_new_observations.R` | `data/extant_new_observations.csv` | 251 rows of extant_new_observations.csv | 7 | 244 | 188 | 92 | 176 | 79 |
| `04_raptordiets.R` | `data/structured/raptor_diets_long.csv` | 58,533 rows of raptor_diets_long.csv | 3,043 | 55,490 | 38,092 | 0 | 0 | 1,048 |
| `05_tucker2016.R` | `data/structured/tucker2016_mammal_ppmr.csv` | 107 rows of tucker2016_mammal_ppmr.csv; x13 already dropped 7 of 114 species that lack a prey mass | 0 | 107 | 107 | 0 | 0 | 1 |
| `06_jekyll.R` | `data/structured/gator_jekyll_stomach.csv` | 69 rows of gator_jekyll_stomach.csv (alligator x prey category); 27 alligators | 4 | 65 | 0 | 65 | 119 | 1 |
| `07_glaudas2019.R` | `data/structured/glaudas2019_snake_rpm.csv` | 115 rows of glaudas2019_snake_rpm.csv (one per snake species) | 0 | 115 | 115 | 0 | 115 | 1 |
| `08_tegu.R` | `tegu_diet_individuals.csv` + `tegu_diet_dietcategories.csv` | 9,625 rows of tegu_diet_dietcategories.csv (275 tegus x 35 categories); 2414 with Found = 1 | 647 | 1,767 | 0 | 1,767 | 273 | 1 |
| `09_varanus.R` | `varanus_niloticus_summary.csv` (prey list in Mazzotti Table 2) + `varanus_varius_summary.csv` | 95 prey taxa in the two Varanus tables (88 Mazzotti Table 2 rows + 7 Weavers top prey) | 0 | 95 | 0 | 0 | 0 | 1 |
| **All** | | | **4,016** | **65,466** | **44,716** | **2,004** | **839** | **1,352** |

Pair granularity: raw_data gives one pair per predator record × prey column, with `n_items` = the count. x00 expands the same cells to one row per item, which is why longform has 171,406 rows. Other sources give one pair per input row. For raw_data the "dropped" column mixes units: 314 cells plus 1 record with no prey.

"Sources written" counts the sources each script owns. Pairs from publications in `evidence/duplicate_studies.csv` cite the merged source written by `00_merged_sources.R` instead: 2,902 pairs from script 01; 137 pairs from script 02; 3 pairs from script 03; 59 pairs from script 04; 7 pairs from script 09.

`tegu_salvator_summary.csv` summarises the tegu tables and adds no records. These are not pair records and were not migrated: the predator-mass ranges in the two *Varanus* tables, the Guarino 2002 masses, and the Weavers individual record (already migrated by script 03 as `Weavers1989_ref90`).

## Publications in more than one upstream table

JM asked for these to be merged on 2026-11-12.

- `evidence/duplicate_studies.csv` (hand-checked) lists 63 publications found in two upstream tables: raw_data and OS-Prey for most of them, plus AvianDietDB, extant_new_observations and the Weavers occurrence list. `00_merged_sources.R` writes one source for each, and the other scripts cite it.
- `evidence/build_duplicate_evidence.R` decides which records are duplicates. A block of records is a duplicate when another copy has the same predator species and the same kind of evidence. Three synonyms are mapped, for example *Circus cyaneus* = *C. hudsonius*. The two kinds are gut (stomach, regurgitate) and other (pellets, scats, remains, observations). Different kinds from one paper are different data and are all kept; Heintzelman 1964, for example, has stomachs in AvianDietDB and pellets in raw_data. The copy kept is AvianDietDB, then raw_data, then OS-Prey.
- Of 183 blocks, 88 are dropped. 86 are OS-Prey blocks (3,043 rows, 61 publications); the kept copy is raw_data for 83 blocks and AvianDietDB for 5. The other 2 are raw_data records (Errington 1938 *Buteo lagopus* and Woodgerd 1952 *Aquila chrysaetos*, 25 cells), kept from AvianDietDB. Auffenberg 1981 and Weavers 1989 are merged at the source level only, because their records differ between tables.
- None of the dropped records was in the 3 June model input. x20 never read OS-Prey and leaves raw_data bird stomach rows out.
- `db/migration/possible_duplicate_sources.csv` now lists 4 AuthorYear keys that occur under two tables. Each pair has different author strings or journals, so they are different publications: Campbell et al. 1987 (*Can. J. Zool.*) and Campbell 1987 (*Am. Midl. Nat.*); Collopy & Koplin 1983 and Collopy 1983; Korpimäki & Sulkava 1987 and Korpimäki 1987; Luiselli 1996 (*J. Herpetol.*) and Luiselli & Capizzi 1996.
- Holycross & Mackessy 2002 is entered twice within raw_data. Both copies have the same 21 prey columns with the same counts, 165 items, and the paper says "From these, 165 prey were identified". They differ in the predator:
  - RefNo 121 (year, journal, PDF on disk): *Sistrurus catenatus*, New Mexico, 117 g;
  - RefNo 128 (no year, no PDF): *S. c. edwardsii*, Arizona, 32.16 g.

  RefNo 128 is kept and RefNo 121's cells are dropped. The paper is about *S. c. edwardsii* ("… with Emphasis on Sistrurus catenatus edwardsii (Desert Massasauga)"). Its predators that ate prey average 329–362 mm SVL ("Snakes that ate mammals were longest (mean SVL = 362 ± 7 mm, N = 51)"). Feldman & Meiri 2013's Viperidae row puts that at 34–43 g, near 32.16 g; 117 g is the mass of a ~530 mm snake. Neither mass is printed in the paper, so 32.16 g stays `species_average_literature` (compiler's value). RefNo 128's source row points at the PDF filed under RefNo 121, and RefNo 121's source row stays with a note. x20 counted both copies, so the 3 June snake Scat cell held this study twice.

The full list of dropped duplicate blocks is in the appendix.

## How each field was assigned

### Observation type

| Source | Rule and evidence |
|---|---|
| raw_data | `ObsType` Stomach → `stomach_content`; mammal and squamate Scat → `scat`; Larder → `prey_remains` (shrike larders). Bird Scat → `pellet`, unless the study PDF mentions prey remains or plucking more often than pellets, and pellets fewer than 3 times; then `prey_remains`. Follow → `direct_observation`, unless kill site, carcass or GPS cluster outnumber "observ"; then `prey_remains`. Keyword counts per study are in `evidence/rawdata_studies.csv`. Overrides from the source text: Fredriksson's python (`direct_observation`); Stanner & Mendelssohn 1986 and Goetz et al. 2017 (`scat`); Maritz 2011, record by record (`regurgitate`, `direct_observation`, pooled). |
| AvianDietDB | Study_Type `emetic` → `regurgitate` (101); `stomach contents` → `stomach_content`; the 8 rows of `stomach contents; pellet contents` → `stomach_content`, noted as pooled. |
| extant_new_observations | Per row from `evidence/extant_new_obs_decisions.csv`, each with the source wording. Oliveira 2025 (67 rows) and Martín & López 1996 (5 rows) → `unknown`, because neither says how each event was observed. |
| OS-Prey | x10's primary method per dataset (priority Stomach > Pellet > PreyRemains > Follow > DNA > Photos). Photos → `direct_observation`. The 65 rows with no method in the metadata → `unknown`. Pooled methods are recorded in notes. |
| Tucker 2016 | `unknown`: a species-level summary with no method per row. |
| Jekyll, tegu, Mazzotti | `stomach_content` (GI tracts). |
| Glaudas 2019 | `stomach_content`. Their regurgitation is palpation-induced, which SCHEMA.md places under `stomach_content`. |
| Weavers 1989 occurrence list | `unknown`: "I collected scats and regurgitated stomach contents", pooled per prey. |

Pooled gut and faecal samples. Three raw_data studies pool stomachs or guts with faeces: Weber 1989 (120 guts, 354 scats), Delibes et al. 1984 (83 gut contents, 105 droppings) and Holycross & Mackessy 2002 (faeces of live snakes, guts of road-kills and museum specimens). raw_data's coding is kept: Stomach, Stomach and Scat. JM decided on 2026-11-12 that this is fine for modern taxa. The pooling is quoted in each pair's notes.

### Masses and methods

**raw_data** (`evidence/build_rawdata_evidence.R`). raw_data stores one prey mass per prey column and one predator mass per record. No field says where either came from. Reading x00, x03 and `mass_overrides.csv` recovers only the 36 x03 fills. The rest was classified from evidence in this order:

1. x03 override (PanTHERIA/AVONET binomial or genus mean) → `species_average_database`.
2. The value matches PanTHERIA (column 5, read by position) or AVONET to within 1% → `species_average_database`.
3. The same label has the same mass in ≥ 2 studies, so it is a taxon value → `species_average_literature`. If the label is coarser than genus ("Indet", "Fish", "Small passerine") → `assigned_approximate`.
4. Otherwise the value is study-specific. Most used prey columns belong to a single study, and several studies have several columns for one taxon with different masses (Rice 2004 *Ameiurus*: 320, 216.1 and 99.1 g). The 2019 study PDFs are in `archive_old_pipeline/compiled/Compiled/`: 162 have a text layer and 40 are image-only or missing. The script searches each PDF for the value, or for value × count, and compares the hit rate with a control set of random masses. 64 studies print their own prey values. In a pellet, scat, remains or observation study, a value found in the paper → `species_average_literature`, because prey in pellets, scats or observed kills are not weighed. In a stomach study, a value found in the paper → `unknown`, unless the study was read by hand.
5. Hand decisions with quotes for 27 studies (`evidence/rawdata_study_decisions.csv`), plus record- and column-level decisions for 4 studies (`evidence/rawdata_cell_decisions.csv`: Maritz 2011, Korschgen 1959, Sovada 1999, and the Da Silveira 1999 mass correction):
   - remains weights: Dalhuijsen 2014, Da Silveira & Magnusson 1999, Mercolli & Yanosky 1994, Jaman 2007 and Mayes 2006, whose values are dry weights × 3;
   - remains reconstituted by the authors: Shine 1986 → `reconstructed_from_remains`;
   - weighed predators: Shine 1986, Rice 2004, Pauwels 2007, Moldowan 2016, Maritz 2011, Mayes 2006, Dryden 1985, Goetz 2017;
   - weighed prey: Capula 2002, Moldowan 2016, Maritz 2011 (two items);
   - literature or reference-collection prey weights: Goetz 2017, Delibes 1984, Maritz 2011 (Table 7.1 means);
   - false text matches corrected: Stanner & Mendelssohn 1986, Korschgen 1959, Sovada 1999.
6. A study-specific value not found in the paper → `species_average_literature` if the label is a genus or species with one value in that study, else `unknown`.

Predator masses follow the same steps: database match, a species value reused across studies, a value found in the paper, or a hand decision. A value that is one of more than two masses for the species within a study, and is not in the paper, → `unknown` (Itamies 1972, Luiselli 1996).

**Remains weights.** Where the evidence says a prey value is a weight of stomach or scat contents, `prey_mass_g` is empty and the value is in `measurements.csv` with element `stomach remains`, `scat remains` or `stomach contents`. This covers 108 raw_data prey columns, the Jekyll categories and the Paulissen & Montgomery prey volume. The dimension says wet mass, or for Mayes 2006 "wet-equivalent mass per item (thesis dry mass × 3)".

**AvianDietDB.** Predator: AVONET species mean (all 214). Prey: AVONET or PanTHERIA, species mean or genus mean, as x15 did. x15's lookup was redone so the basis says which. The original study comes from `aviandietdb_dietdb.csv`, where all 214 rows match one Source. Tier D.

**extant_new_observations** (`evidence/extant_new_obs_decisions.csv`).
- *Staged values citing AVONET:* re-read from AVONET1_BirdLife. *Zonotrichia capensis* goes from 20 to 20.3 g and *Bubo virginianus* from "~1400" to 1354.5 g.
- *Lizards with a recorded SVL* (Oliveira, Passos, the Paulissen predator, Poulin *Anolis*): recomputed with Meiri 2010 Table 3, by family where the 2009 family is unambiguous, otherwise the all-lizard row. This replaces the uncited "1.5e-5·SVL³". For the 67 Oliveira rows the predator mass roughly doubles (median ×1.9, range 0.98–2.35) and prey mass rises less (median ×1.44). ln(prey/predator) changes by a median of −0.02 (range −0.62 to +0.52).
- *Dutra-Araújo prey:* *Atractus* TL 480 mm → Feldman & Meiri 2013 Colubridae TL equation (23.0 g; staged 30 g).
- *Nifong 2016 (ref 230–251):* `size_class_midpoint` with `ChabreckJoanen1979_Fig2_classmid`. Recomputing reproduces the staged 1,025, 9,034 and 46,240 g exactly. The 86 cm midpoint lies below the 1 m the authors give as the reliable range.
- *Delany 1999 and Gabrey 2010 (ref 207–226):* `size_class_midpoint` without an equation (the representative TL was assigned without one) and `assigned_approximate` prey. Tier E, as the prompt says.
- *Approximate values ("~10 g lit", "~65 g", "ca. 1 kg")* → `assigned_approximate`.
- *Mixed weighed and estimated biomass* (Capizzi & Luiselli 1997, Wiseman 2019) → `unknown`. So are Weavers' "500 g rabbit" (weighing not described), Auffenberg (book not on disk), Kasperoviczus (length-based estimates whose lengths are not recorded) and the Poulin frog prey (0.3 g from 14.5 mm SVL by an unrecorded equation; no frog equation is registered).

**OS-Prey** (JM, 2026-11-12). x10 left both masses empty "for the mass layer". They are now filled as x15 filled AvianDietDB. Predator: AVONET species mean for 54,523 rows and genus mean for 813 rows where the name is not an AVONET species; 154 rows have none. Prey: only prey identified to species, subspecies or genus, and only birds (AVONET) or mammals (PanTHERIA). That gives a species mean for 31,811 rows and a genus mean for 6,325. 17,354 rows have no prey mass: insects 5,798, birds and mammals not identified to genus or not found 6,440, reptiles 1,986, amphibians 1,006, fish 769, and other invertebrates. Genus means are coded `species_average_database`, as x15 did; `*_mass_basis` says "genus mean", so they can be filtered. All OS-Prey rows are tier D.

**Da Silveira & Magnusson 1999, raw_data line 124.** The largest *Caiman crocodilus* class is 520,008.5 g in raw_data. The class series runs 20,222.2, 28,627.3, 39,139 g, so this is a misplaced decimal. It is migrated as 52,008.5 g on JM's instruction (2026-11-12), through `evidence/rawdata_cell_decisions.csv`. `pred_mass_basis` records the correction, and `raw_data.csv` is unchanged. The row stays tier E (`size_class_midpoint`).

**Jekyll Island.** Predator: the source weight for the 8 weighed alligators. The 8 that were not weighed but have TL ≥ 100 cm get Chabreck & Joanen 1979 Fig. 2 from their own TL. The 11 below 100 cm and unweighed get no mass. Prey category weights are remains: they go to measurements, `prey_mass_g` stays empty, and the note says so.

**Glaudas 2019.** Predator mass = Feldman & Meiri 2013 Table 1, family row, on the species-mean SVL. Colubrinae, Dipsadinae, Natricinae and Grayiinae use the Colubridae row; Aniliidae uses All. This is the mass of a snake of mean SVL, not a mean mass, and `pred_mass_basis` says so. Prey mass = RPM_mean × predator mass (`ratio_derived`, `Glaudas2019_RPM`). The interval is RPM_min and RPM_max × predator mass for the 96 species with a usable min and max. Tier E. Prey taxon `Vertebrata_pooled` when the diet has no invertebrates, else `Animalia_pooled`.

**Tucker 2016.** Predator and prey are compiled species means → `species_average_literature`. Prey taxon `Animalia_pooled`. Tier E, because the prey mass is an aggregate over a whole diet. One prey mass was 0 in the staged table because x13 rounds to 3 decimals (*Eubalaena glacialis*). It was recovered as pred_mass × exp(ppmr_ln) from the same row.

**Tegu, Mazzotti, Weavers list.** No prey masses. Tegu predators were weighed (`measured`, 274 tegus); the diet is presence-only.

### The prompt's specific rules, checked against `db/`

| Rule | Result |
|---|---|
| Taxon-average masses are `species_average_*` | AVONET, PanTHERIA and x03 fills → `species_average_database`; compiled or reused species values → `species_average_literature`. |
| Alligator rows ref 207–251: tier E, `size_class_midpoint` predators, `assigned_approximate` prey | All 42 alligator rows (Delany 1999 and Gabrey 2010, ref 207–226; Nifong 2016, ref 230–251). Ref 227–229 in that range are Kasperoviczus 2015 tegus, tier E with `unknown` or `assigned_approximate` masses. |
| Jekyll prey masses are remains weights | 65 pairs with `prey_mass_g` empty and a note; 119 measurements (`stomach contents`, wet mass, total of category). |
| Glaudas: tier E, `ratio_derived` prey, documented length–mass equation | 115 pairs, all tier E; predator `regressed_from_length` with one of seven Feldman & Meiri 2013 Table 1 rows (in `methods.csv` with coefficients, input range and locator); prey `ratio_derived` with `Glaudas2019_RPM`. |
| Never invent a mass | Every mass is a source value, a database value, or an equation from `methods.csv` applied to a recorded length. One raw_data value was corrected on JM's instruction (Da Silveira caiman, 520,008.5 → 52,008.5 g), with the correction in `pred_mass_basis`. |
| `unknown` where a method cannot be determined, rows listed | 491 pairs; `db/migration/unknown_methods.csv` (one row per pair) and the table below. |
| Do not change the old files | The input files' modification times predate the migration (the latest is 2026-11-11 10:08, from the Phase 0 intake). The scripts open them read-only. |

### Tier and individual pairs

The tier comes from the two methods, through the rule in SCHEMA.md and `tier_of()` in `_common.R`. Two choices the schema left open:

- **A side with no mass** is treated as if it would be filled from a taxon average, the coarser reading, unless the source records a length or bone of that animal. So tegu and Jekyll rows are tier B, and the Mazzotti rows and OS-Prey rows without a mass are tier D. Any count of "tier A–B records" must also require both masses.
- **A predator sample mean** that is not specific to the animals whose prey were recorded is treated as an average. This is the Poulin case in SCHEMA.md. It applies to Capizzi & Luiselli 1997 (script 03) and to Maritz 2011's pooled record (15.8 g is the study population's mean adult mass). Poulin's *Anolis* SVL is pooled across predators, so those rows are tier B with `individual_pair` FALSE, as SCHEMA.md describes.

`individual_pair` is TRUE only where the record is one animal:
- raw_data records with Nobservations = 1 in Pauwels, Moldowan, Maritz and Itamies, plus Fredriksson's python (77);
- the 3 single-bird AvianDietDB rows;
- 92 extant_new_observations rows;
- all Jekyll and tegu rows.

## Final check of raw_data observation types and unchecked masses

The earlier draft left 41 `unknown` prey masses with the basis "not checked", and it took raw_data's "Stomach" at face value. Both were checked on 2026-11-12 against the papers' text layers:

- **Stanner & Mendelssohn 1986** (*Varanus griseus*; 90 pairs). "Fecal pellets were collected and analyzed microscopically", with "the commonly used 'frequency of occurrences' method". It is a faecal study with no prey masses. The values the script had "found" in the text are percentages, counts and a page range (55.5%, "pp. 37-55", 12.1, 12.4). Now `scat`, and the prey values are the compiler's taxon values (`species_average_literature`). This moves its 495 June items from the lizard stomach cell to lizard scat.
- **Goetz et al. 2017** (*Crotalus horridus*). "We used fecal analyses to study the diets of 37 adult C. horridus". Now `scat` (38 June items move from snake stomach to snake scat).
- **Mayes 2006 thesis** (*Varanus mertensi*). "Prey groups were weighed to record the dry mass of each constituent prey group in each stomach content sample", and the same for scats. The raw_data values are those dry masses × 3, divided by the number of items: *Mus musculus* 5.355 g × 3 = 16.065; *Holthuisana* 19.28 g × 3 / 45 = 1.2853; Agamidae 0.359 × 3 = 1.077. These are remains weights, not prey masses. They are now measurements (`stomach remains` or `scat remains`), and `prey_mass_g` is empty.
- **Maritz 2011 thesis** (*Bitis schneideri*). Two prey items were weighed with their snakes:
  - line 97: "a juvenile snake (… mass = 4.3 g …) that had consumed an adult Meroles knoxii (… mass = 4.3 g)", regurgitated (Fig. 7.2);
  - line 99: "an adult male snake (… mass = 19.1 g) consuming a Trachylepis variegata (… mass = 2.1 g)".

  Both are now tier A individual pairs, the first snakes among the individual pairs. The pooled line 98 reuses those two weights for 6 other items (`assigned_approximate`). It takes *Breviceps* and *Pachydactylus* from the Table 7.1 means of pitfall-trapped animals (`species_average_literature`). Its predator mass, 15.8 g, is the population's "mean ± SD adult mass" (`species_average_literature`).
- **Korschgen 1959 and Sovada 1999.** Neither paper reports prey masses. "391" matched a page range in a reference, and "24.6" is not in the text. The two values are compiler values (`species_average_literature`).
- **Pooled gut and faecal samples** (Weber 1989, Delibes 1984, Holycross 2002): see "Observation type".
- **Holycross 2002 entered twice:** see "Publications in more than one upstream table".

Studies whose PDF has no text layer (Korschgen 1957, Niethammer 1983, Glaudas 2008, Hill 2001) keep raw_data's coding, which was not verified.

## Counts

### By tier (all extant pairs)

| Script | A | B | C | D | E | All |
|---|---|---|---|---|---|---|
| 01_rawdata | 154 | 145 | 5 | 6,345 | 720 | 7,369 |
| 02_aviandietdb | 0 | 0 | 0 | 214 | 0 | 214 |
| 03_extant_new_observations | 69 | 24 | 0 | 53 | 98 | 244 |
| 04_raptordiets | 0 | 0 | 0 | 55,490 | 0 | 55,490 |
| 05_tucker2016 | 0 | 0 | 0 | 0 | 107 | 107 |
| 06_jekyll | 0 | 65 | 0 | 0 | 0 | 65 |
| 07_glaudas2019 | 0 | 0 | 0 | 0 | 115 | 115 |
| 08_tegu | 0 | 1,767 | 0 | 0 | 0 | 1,767 |
| 09_varanus | 0 | 0 | 0 | 95 | 0 | 95 |
| **All** | **223** | **2,001** | **5** | **62,197** | **1,040** | **65,466** |

Pairs with both masses: 44,716. Predator mass only: 20,372. Prey mass only: 68. Neither: 310.

### Vertebrate prey with both masses, by tier and predator clade

| Tier | Aves | Crocodylia | Lacertilia | Mammalia | Serpentes | All |
|---|---|---|---|---|---|---|
| A | 0 | 2 | 119 | 0 | 2 | 123 |
| B | 17 | 3 | 8 | 0 | 13 | 41 |
| C | 0 | 0 | 0 | 0 | 5 | 5 |
| D | 41,579 | 1 | 58 | 1,405 | 151 | 43,194 |
| E | 164 | 138 | 29 | 138 | 141 | 610 |
| **All** | **41,760** | **144** | **214** | **1,543** | **312** | **43,973** |

OS-Prey contributes 38,092 of the 41,579 bird tier D pairs. None of them was in the June input, because x20 never read OS-Prey. How analyses weight them is decided downstream of the database (JM, 2026-11-12).

### Individual pairs: tier A–B, vertebrate prey, both masses

This is what the within-species slope needs (improvement 2).

| Pair | Predator | Prey | Predator g | Prey g | Tier | Observation type |
|---|---|---|---|---|---|---|
| Greer1966_ref83 | *Buteo ventralis* | *Vanellus chilensis* | 950 | 327 | B | stomach_content |
| Greer1966_ref84 | *Elanus leucurus* | *Zonotrichia capensis* | 317 | 20.3 | B | stomach_content |
| DutraAraujo2017_ref206 | *Paleosuchus palpebrosus* | *Atractus torquatus* | 2800 | 23.01996 | A | direct_observation |
| Moldowan2016_DM179_L83C3255 | *Caiman crocodilus* | *Helicops angulatus* | 2700 | 16.6 | A | stomach_content |
| Passos2016_ref97 | *Tropidurus hispidus* | *Tropidurus jaguaribanus* | 41.83888 | 13.90861 | A | direct_observation |
| Maritz2011_DM171_L97C3174 | *Bitis schneideri* | *Meroles knoxii* | 4.3 | 4.3 | A | regurgitate |
| Maritz2011_DM171_L99C3220 | *Bitis schneideri* | *Trachylepis variegata* | 19.1 | 2.1 | A | direct_observation |
| 65 Oliveira et al. 2025 Table S2 records | lizards | lizards | | | A or B | unknown |

No mammals. Only 7 of the 72 have an observation type that can be matched to a fossil filter.

### By mass method (pairs)

Predator:

| Pred mass method | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 |
|---|---|---|---|---|---|---|---|---|---|
| (no mass) | 51 | 0 | 49 | 154 | 0 | 26 | 0 | 3 | 95 |
| measured | 468 | 0 | 59 | 0 | 0 | 20 | 0 | 1,764 | 0 |
| regressed_from_length | 0 | 0 | 68 | 0 | 0 | 19 | 115 | 0 | 0 |
| species_average_database | 1,967 | 214 | 9 | 55,336 | 0 | 0 | 0 | 0 | 0 |
| species_average_literature | 4,775 | 0 | 0 | 0 | 107 | 0 | 0 | 0 | 0 |
| size_class_midpoint | 75 | 0 | 42 | 0 | 0 | 0 | 0 | 0 | 0 |
| assigned_approximate | 0 | 0 | 13 | 0 | 0 | 0 | 0 | 0 | 0 |
| unknown | 33 | 0 | 4 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_bone | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **All** | **7,369** | **214** | **244** | **55,490** | **107** | **65** | **115** | **1,767** | **95** |

Prey:

| Prey mass method | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 |
|---|---|---|---|---|---|---|---|---|---|
| (no mass) | 1,345 | 0 | 56 | 17,354 | 0 | 65 | 0 | 1,767 | 95 |
| measured | 8 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_length | 0 | 0 | 82 | 0 | 0 | 0 | 0 | 0 | 0 |
| reconstructed_from_remains | 151 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| species_average_database | 928 | 214 | 8 | 38,136 | 0 | 0 | 0 | 0 | 0 |
| species_average_literature | 4,325 | 0 | 2 | 0 | 107 | 0 | 0 | 0 | 0 |
| ratio_derived | 0 | 0 | 0 | 0 | 0 | 0 | 115 | 0 | 0 |
| assigned_approximate | 190 | 0 | 60 | 0 | 0 | 0 | 0 | 0 | 0 |
| unknown | 422 | 0 | 36 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_bone | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **All** | **7,369** | **214** | **244** | **55,490** | **107** | **65** | **115** | **1,767** | **95** |

### By observation type (pairs)

| Observation type | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 |
|---|---|---|---|---|---|---|---|---|---|
| stomach_content | 1,523 | 113 | 113 | 1,441 | 0 | 65 | 115 | 1,767 | 88 |
| regurgitate | 1 | 101 | 35 | 0 | 0 | 0 | 0 | 0 | 0 |
| pellet | 3,031 | 0 | 1 | 44,548 | 0 | 0 | 0 | 0 | 0 |
| scat | 1,650 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| prey_remains | 166 | 0 | 2 | 6,774 | 0 | 0 | 0 | 0 | 0 |
| direct_observation | 998 | 0 | 10 | 2,552 | 0 | 0 | 0 | 0 | 0 |
| dna_detection | 0 | 0 | 10 | 110 | 0 | 0 | 0 | 0 | 0 |
| unknown | 0 | 0 | 72 | 65 | 107 | 0 | 0 | 0 | 7 |
| bite_trace | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| combat_association | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **All** | **7,369** | **214** | **244** | **55,490** | **107** | **65** | **115** | **1,767** | **95** |

## Rows dropped

4,016 input rows produced no pair. One row per dropped input row is in `db/migration/dropped_rows.csv`; the grouped list is in the appendix.

| Script | Reason | Rows |
|---|---|---|
| 01_rawdata | non-animal food item (prey class string is plant, fungus, placeholder or unclassified); not a predator-prey link | 268 |
| 01_rawdata | duplicate record: the same publication, predator species and evidence kind is kept from AvianDietDB (evidence/duplicate_records.csv) | 25 |
| 01_rawdata | duplicate entry within raw_data.csv: the same records are entered under another RefNo, which is kept (evidence/rawdata_study_decisions.csv) | 21 |
| 01_rawdata | record has no prey count > 0 in any prey column | 1 |
| 03_extant_new_observations | no prey taxon recorded (row states the predator's diet breadth only; species detail in Data S1 not extracted) | 6 |
| 03_extant_new_observations | duplicate of ref_id 97: the same Tropidurus hispidus x T. jaguaribanus predation (Passos et al. 2016), entered once from the paper (ref 97, kept) and once via Oliveira et al. 2025 Table S2 row 79 | 1 |
| 04_raptordiets | duplicate record: the same publication, predator species and evidence kind is kept from another upstream table (evidence/duplicate_records.csv) | 3,043 |
| 06_jekyll | seeds are not prey (non-animal food item) | 4 |
| 08_tegu | non-animal or man-made item (fruit, vegetation, seeds, man-made material); not a predator-prey link | 647 |
| **All** | | **4,016** |

## Rows with an unknown method

491 pairs have at least one `unknown` mass method: 422 raw_data prey, 33 raw_data predator, 36 extant prey and 4 extant predator. Four pairs have both, hence 523 validator warnings. One row per pair, with both bases, is in `db/migration/unknown_methods.csv`. By source, largest first:

| Source | Side | Pairs | Why (`*_mass_basis`) |
|---|---|---|---|
| Rice2004_DM146 | prey | 91 | mixed: live mass of fresh prey estimated from same-size field data, museum specimens and three reference books; raw_data value is a lake-level mean |
| Radloff_DM93 | prey | 60 | study-specific value for 'Aepyceros_melampus' that differs between records of this study; not found in the source text |
| Dryden1985_DM186 | prey | 48 | prey mass method not stated |
| Vitt1992_DM122 | prey | 37 | paper estimated prey volumes (prolate spheroid), not masses; conversion used in raw_data not recorded |
| Delany1986_DM126 | prey | 30 | paper reports volumetric stomach measurements; origin of the per-taxon masses in raw_data not found |
| Angelici2000_DM136 | prey | 28 | no statement of prey mass method found in the text layer |
| Itamies1972 | predator | 27 | one of 5 different masses for Pernis apivorus within this study; not found in the source text |
| Shine1998_DM120 | prey | 27 | paper reports prey mass summaries; how each value in raw_data was obtained is not stated |
| Delany1990_DM124 | prey | 26 | paper reports stomach contents volumes (ml); conversion to mass not recorded |
| Korpimaki1987 | prey | 24 | study-specific value for 'Indet' that differs between records of this study; source PDF not on disk or image-only |
| Poulin2001 | prey | 16 | staged 0.3 g from mean SVL 14.5 mm by an unrecorded equation; no frog length-mass equation is registered |
| Sutter2001_DM189 | prey | 10 | study-specific value for 'Indet' that differs between records of this study; source PDF not on disk or image-only |
| Olsen2013a | prey | 8 | study-specific value for 'Cyprinus_carpio' that differs between records of this study; source PDF not on disk or image-only |
| CapizziLuiselli1997 | prey | 7 | Table 2 biomass / count; biomass 'calculated on the basis of the fresh weight if the prey was in optimal condition, or' otherwise estimated: weighed and estimated items mixed |
| 22 further source × side rows | | 52 | see `db/migration/unknown_methods_by_source.csv` |
| **All** | | **491** | |

251 pairs have `observation_type` `unknown`: Tucker 2016 (107), extant_new_observations (72: Oliveira 2025 and Martín & López 1996), OS-Prey datasets with no method in the metadata (65) and the Weavers occurrence list (7). They are listed in `db/migration/unknown_observation_type.csv`.

## Reproducing the model input

### Step 1: the rebuild matches the log

`compare_model_input.R` rebuilds x20's assembly from the old files, keeping a row id per item:
- x00's expansion of raw_data with the x03 overrides, matched against `longform.csv`: 171,406 items each, with identical ln masses;
- x20's filters, obstype recoding and clade rules;
- x20's subsample with the same seed and caps.

The 3 June state is `extant_new_observations.csv` ref_id ≤ 205. That cut reproduces the log's "newobs=142" exactly. Ref 206–229 arrived with the 4 June herp intake and ref 230–251 on 11 November, and git holds no June copy. With that cut the rebuild gives 144,152 vertebrate-prey rows and 15,260 after subsampling, with the same cell table as the log. On the current files x20 would see 144,198 and 15,306.

### Step 2: the db subset

Pairs from the same three sources (raw_data, AvianDietDB, extant_new_observations) are kept if they meet all of these:
- tier A–D;
- prey `is_vertebrate` TRUE;
- both masses present;
- predator `analysis_clade` in x20's five clades;
- observation type mapped to x20's levels: `stomach_content` and `regurgitate` → Stomach; `pellet` → Pellet; `scat` → Scat; `direct_observation` and `prey_remains` → Follow.

raw_data pairs are expanded by `n_items`; the others count once, as in x20. raw_data bird stomach rows are left out, as x20 leaves them out. Result: 136,683 rows, 12,410 after the same subsample. The subset contains no row that x20 on the current files did not have. Its one row that is not in the June state is Dutra-Araújo 2017 (ref 206, Crocodylia Follow).

**Tolerance.** A row reproduces when both ln masses agree to 1e-6; the db stores 8 significant digits. All other differences are listed below as changes, not absorbed by a looser tolerance.

### Step 3: item-level reconciliation of the 144,152 June rows

| Outcome | Items | Share |
|---|---|---|
| identical | 135,479 | 93.98% |
| tier E in db: pred species_average_literature, prey unknown | 4,560 | 3.16% |
| tier E in db: pred species_average_literature, prey assigned_approximate | 1,236 | 0.86% |
| observation level Pellet -> Follow (prey_remains) | 651 | 0.45% |
| tier E in db: pred species_average_database, prey assigned_approximate | 610 | 0.42% |
| tier E in db: pred measured, prey unknown | 541 | 0.38% |
| observation level Stomach -> Scat (scat) | 533 | 0.37% |
| prey mass removed (stomach-remains weight moved to measurements.csv) | 154 | 0.11% |
| no db pair (dropped at migration) | 104 | 0.07% |
| tier E in db: pred size_class_midpoint, prey none | 96 | 0.07% |
| tier E in db: pred unknown, prey species_average_literature | 66 | 0.05% |
| observation type unknown is outside x20's four levels | 65 | 0.05% |
| mass changed (re-read or recomputed) | 18 | 0.01% |
| tier E in db: pred species_average_database, prey unknown | 13 | 0.01% |
| tier E in db: pred assigned_approximate, prey assigned_approximate | 12 | 0.01% |
| tier E in db: pred measured, prey assigned_approximate | 7 | 0.00% |
| tier E in db: pred species_average_database, prey species_average_literature | 2 | 0.00% |
| tier E in db: pred unknown, prey unknown | 2 | 0.00% |
| observation type dna_detection is outside x20's four levels | 1 | 0.00% |
| observation level Stomach -> Follow (direct_observation) | 1 | 0.00% |
| tier E in db: pred unknown, prey species_average_database | 1 | 0.00% |
| **Total** | **144,152** | |

How to read the outcomes:
- "tier E in db" = the row is in the db but outside A–D, and the label gives the two mass methods. `unknown` = method cannot be determined; `assigned_approximate` = a coarse label's reused value or an approximate value; `size_class_midpoint` = Da Silveira 1999's caiman SVL classes; "prey none" = the prey value was a remains weight.
- "observation level" = the row is in A–D but at a different x20 level:
  - Pellet → Follow: bird "Scat" coded `prey_remains` from PDF keywords (Hector 1985, Olsen 2013, Sergio 2002, Rangel-Salazar 1993) and shrike larders (Brzezinski 2010);
  - Stomach → Scat: Stanner 1986 and Goetz 2017;
  - Stomach → Follow: Maritz 2011 line 99.
- "mass changed" = AVONET re-read or Meiri / Feldman & Meiri recomputation.
- "no db pair" = dropped at migration: the ref 177 duplicate and the Holycross RefNo 121 copy.

### Step 4: subsampled cell counts

| Clade | Level | x20, 3 June (log) | x20 on current files | db A–D | db − June |
|---|---|---|---|---|---|
| Aves | Follow | 1,500 | 1,500 | 1,500 | 0 |
| Aves | Pellet | 1,500 | 1,500 | 1,500 | 0 |
| Aves | Stomach | 255 | 255 | 232 | −23 |
| Crocodylia | Follow | 0 | 1 | 1 | +1 |
| Crocodylia | Stomach | 1,189 | 1,231 | 5 | −1,184 |
| Lacertilia | Follow | 72 | 75 | 1 | −71 |
| Lacertilia | Scat | 161 | 161 | 300 | +139 |
| Lacertilia | Stomach | 923 | 923 | 208 | −715 |
| Mammalia | Follow | 1,464 | 1,464 | 1,191 | −273 |
| Mammalia | Scat | 1,500 | 1,500 | 1,500 | 0 |
| Mammalia | Stomach | 4,830 | 4,830 | 4,617 | −213 |
| Serpentes | Follow | 1 | 1 | 2 | +1 |
| Serpentes | Scat | 363 | 363 | 298 | −65 |
| Serpentes | Stomach | 1,502 | 1,502 | 1,055 | −447 |
| **All** | | **15,260** | **15,306** | **12,410** | **−2,850** |

Before subsampling:

| Clade | Level | x20, 3 June | db A–D |
|---|---|---|---|
| Aves | Follow | 8,406 | 8,785 |
| Aves | Pellet | 95,296 | 92,487 |
| Aves | Stomach | 255 | 232 |
| Crocodylia | Follow | 0 | 1 |
| Crocodylia | Stomach | 1,189 | 5 |
| Lacertilia | Follow | 72 | 1 |
| Lacertilia | Scat | 3,733 | 4,217 |
| Lacertilia | Stomach | 923 | 208 |
| Mammalia | Follow | 7,362 | 4,862 |
| Mammalia | Scat | 20,220 | 19,913 |
| Mammalia | Stomach | 4,830 | 4,617 |
| Serpentes | Follow | 1 | 2 |
| Serpentes | Scat | 363 | 298 |
| Serpentes | Stomach | 1,502 | 1,055 |

### Explanation of each cell difference

Items are June rows that are not identical in db A–D. Per-source detail: `db/migration/compare_reconciliation_by_source.csv`.

- **Crocodylia Stomach (1,189 → 5).**
  - Delany & Abercrombie 1986 (691 items) and Delany 1990 (119): prey masses of undetermined origin; the papers report volumes.
  - Rice 2004 (277): the alligators were weighed, but prey live mass was estimated from field data, museum specimens and reference books, then averaged per lake.
  - Da Silveira & Magnusson 1999 (96): the prey values are remains weights, and the predator masses per SVL class are not in the paper.
  - Moldowan 2016 (1): one prey with an approximate mass.
  - Left in A–D: 1 Delany 1990 row (tier D), 1 Moldowan 2016 row (tier A) and 3 Rice 2004 rows whose prey values are taxon constants (tier B).
- **Lacertilia Stomach (923 → 208).**
  - Stanner & Mendelssohn 1986 (495): a faecal study, now Scat.
  - Remains weights: Mercolli & Yanosky 1994 (69), Mayes 2006 (41), Dalhuijsen 2014 (30), Paulissen & Montgomery 2024 (1).
  - Unknown prey method: Vitt 1992 (42; the paper gives volumes), Dryden 1985 (12), Weavers 1989 (1).
  - Approximate values for coarse labels: Dalhuijsen 2014 (12), Mercolli & Yanosky 1994 (6), Best & Pfaffenberger 1987 (4), Jessop 2010 (1), Shine 1986 (1).
  - Shine 1986's other items stay, at tier A (`reconstructed_from_remains`).
- **Lacertilia Scat (161 → 300; 3,733 → 4,217 items before subsampling).** Gains Stanner's 495 items and loses Mayes 2006's 11 (remains weights).
- **Lacertilia Follow (72 → 1).**
  - The 65 Oliveira records have observation type `unknown`.
  - Auffenberg 1981 (2) has unknown methods.
  - Two prey values were remains weights (Montechiaro 2009, Oliveira 2020a).
  - One row has approximate masses.
  - The ref 177 duplicate was dropped.
  - Passos 2016 stays, with a recomputed mass.
- **Serpentes Stomach (1,502 → 1,055).**
  - Unknown prey method: Shine et al. 1998 (221), Diller 1988 (106), Capizzi & Luiselli 1997 (7), Wiseman 2019 (7).
  - Unknown predator mass: Luiselli 1996 (41).
  - Approximate prey values: Glaudas 2008 (17), Hill 2001 (3), Maritz 2011 line 98 (6).
  - Recoded: Goetz 2017 (38) → Scat; Maritz 2011 line 99 (1) → Follow.
- **Serpentes Scat (363 → 298).** Loses the duplicate copy of Holycross & Mackessy 2002 (RefNo 121, 103 items) and gains Goetz 2017 (38).
- **Serpentes Follow (1 → 2)** gains Maritz 2011 line 99. **Crocodylia Follow (0 → 1)** gains Dutra-Araújo 2017 (ref 206), which was not in the June state.
- **Mammalia Follow (1,464 → 1,191; 7,362 → 4,862 items before subsampling).** Radloff's record-specific prey masses (2,366) and coarse labels in Spaulding 2000 (134).
- **Mammalia Stomach (4,830 → 4,617).**
  - Angelici 2000 (80): 70 with unknown prey method, 10 coarse labels.
  - Coarse labels with values reused across studies, now `assigned_approximate`: Korschgen 1959 (41), Robinette 1959 (31), Korschgen 1957 (22), Spalding 1971 (13), Sovada 1999 (12), Edwards 2003 (9), Hockman 1983 (4), Cutter 1958 (1).
- **Mammalia Scat** stays at the 1,500 cap. Before subsampling it loses 307 items, mostly coarse labels: Robertshaw 1985 (116), Lofroth 2007 (48), Pechacek 2000 (29), Cutter 1958 (24).
- **Aves Stomach (255 → 232).**
  - Out of A–D: Poulin 2001 frog rows (16; prey method unknown), Greer & Bullock 1966 (5; approximate prey masses), Boyle 1917 (1) and Elmore 2024 ref 75 (1; DNA detection).
  - Another 17 stay with changed masses: the 15 Poulin *Anolis* rows recomputed with Meiri 2010, and two AVONET re-reads.
- **Aves Pellet and Follow** stay at the 1,500 cap. Before the cap, Pellet loses 2,809 items:
  - 651 recoded to Follow: Hector 1985 (225), Brzezinski 2010 (167), Olsen 2013b (150), Sergio 2002 (97), Rangel-Salazar 1993 (12);
  - Korpimäki & Sulkava 1987 (714): image-only PDF, record-specific values for coarse labels;
  - coarse labels in Kloubec 2001, Bosakowski 1992, Randall 1940, Artuso 2010 and others.

  Follow loses 272 items to tier E and gains the 651.

### What this means

The tier A–D subset does not reproduce the model input. It reproduces 94.0% of the rows identically, and every other row has a reason. The rows that leave A–D, or change level, are concentrated in the stomach cells. x20 keeps those cells in full, and they anchor the fossil placement. The crocodylian line in the current fit (the M2 misfit, slope 0.17) rests on prey masses that the evidence classes as undetermined or as remains weights. This is a stronger version of problem M3 in the assessment. The lizard stomach cell also held a faecal study (Stanner 1986, 495 items), so part of the June "stomach" signal for lizards was scat.

## Decisions

### Made by JM on 2026-11-12

1. `observation_type` gained `unknown` (`db/vocab.yml`; SCHEMA.md decision 9). Rows: listed above.
2. `mass_method` gained `reconstructed_from_remains` (SCHEMA.md decision 10). Used for Shine 1986 (151 pairs, tier A).
3. Publications found in two upstream tables are merged into one source, and duplicate records are dropped. The same rule covers the Holycross entry duplicated within raw_data.
4. OS-Prey masses are filled from AVONET and PanTHERIA as x15 did. The database is assembled first; how analyses use or weight OS-Prey is decided downstream.
5. Pooled gut and faecal samples keep raw_data's coding for modern taxa.
6. The Da Silveira caiman mass is corrected from 520,008.5 to 52,008.5 g.

### Open, for JM

1. **Holycross predator mass.** 32.16 g has no source. If a sourced value is wanted, the paper's mean SVLs per prey class (329–362 mm) with Feldman & Meiri 2013's Viperidae row would give `regressed_from_length` masses. That is new estimation, so it was not done.
2. **Test 1 of `db/tests/run_tests.R`** required zero issues on the empty tables. It now requires exit 0 and no errors, with warnings allowed. Confirm.
3. **x20 weights sources unequally.** raw_data contributes one row per item (expanded by count); AvianDietDB and extant_new_observations contribute one row per link.
4. **The methodology index notes are stale** for Chabreck & Joanen 1979 ("cited second-hand"), Feldman & Meiri 2013 and Meiri 2010 ("paywalled"). All three PDFs are on disk, and the coefficients were read from them. `docs/methodology_index.csv` was not edited.
5. **The assessment overstates what was in the fit.** The 3 June input had no ref 206–251 rows. M2's 1,189 crocodylian stomach rows all come from raw_data, through longform.

### Limits

- The 27 study decisions and the cell decisions for 4 studies rest on reading the methods text. The other studies were classified mechanically: value search, keyword counts, reuse across studies, and the inference that pellet, scat, remains and kill studies use reference weights. That inference is strong, but it was not checked paper by paper. The value search can match by coincidence (Stanner, Korschgen, Sovada). Coincidental matches matter only for stomach studies, and every stomach study with a "found" value has now been read.
- The observation-type check compared stomach and faeces wording in every raw_data study coded "Stomach" and every non-bird study coded "Scat" that has a text layer. Studies without a text layer were not checked.
- Taxon names are the source labels, with case normalised and stage words moved to `prey_stage`. Synonyms and misspellings (e.g. *Vpiera*, *Rattus novegicus*) are not resolved.
- Same publication under two ids: Bhaisare et al. 2010 is entered under `Dolia2018`, the paper that reports it.
- A Phase 2 rebuild at 08:29 on 2026-11-12 started from an empty `db/` and removed Phase 3's 74 fossil pairs, because `run_all.R` then ran only `0N_*.R`. The Phase 2 change to `commit()` (required `pair_tag`) also broke the fossil script. Both are fixed: the fossil script passes its `pair_tag`, and `run_all.R` runs it after 00–09. The 74 pairs are back, and the script's side tables are byte-identical to before. Hu2005_01 and Xing2012_01 were re-checked on the real `db/` against the step-2 write-up and the step-1 quotes.

## Files

| Path | What |
|---|---|
| `code/db_migrate/_common.R` | ownership rule, table I/O, taxon rows, `tier_of()`, equation evaluation |
| `code/db_migrate/00_merged_sources.R` | one source per publication found in two upstream tables |
| `code/db_migrate/01_rawdata.R` … `09_varanus.R` | one script per source |
| `code/db_migrate/10_fossil_step2.R` | Phase 3 fossil rows (run by `run_all.R`; documented in `data/fossil_rebuild/`) |
| `code/db_migrate/run_all.R` | runs everything, prunes unreferenced taxa, compares, counts, validates |
| `code/db_migrate/compare_model_input.R` | the reproduction test above |
| `code/db_migrate/migration_counts.R` | the tables in this report |
| `code/db_migrate/evidence/methods_registry.csv` | registered equations with coefficients, transcribed from the PDFs in `pdfs/methodology/` |
| `code/db_migrate/evidence/squamate_genera.csv` | genus → family, analysis_clade, equation; replaces x20's genus regex |
| `code/db_migrate/evidence/build_rawdata_evidence.R` → `rawdata_studies.csv`, `rawdata_prey_columns.csv`, `rawdata_pred_records.csv` | raw_data mass provenance |
| `code/db_migrate/evidence/rawdata_study_decisions.csv`, `rawdata_cell_decisions.csv` | hand decisions with quotes |
| `code/db_migrate/evidence/extant_new_obs_decisions.csv` | per-row decisions for extant_new_observations, with quotes |
| `code/db_migrate/evidence/duplicate_studies.csv`; `build_duplicate_evidence.R` → `duplicate_records.csv` | merged publications and dropped duplicate blocks |
| `code/db_migrate/evidence/parse_oliveira2025.R` → `oliveira2025_tableS2.csv` | Oliveira et al. 2025 Table S2 (127 records) |
| `db/migration/` | dropped rows, unknown-method rows, unknown observation types, comparison tables, counts, validator output |

## Validator output

`Rscript code/db_validate.R` (default `--max-print=50`), run by `run_all.R` after all scripts. It covers the fossil rows too:

```
=== db_validate ===
  tables : /Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject/db 
  vocab  : /Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject/db/vocab.yml 
  index  : /Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject/docs/methodology_index.csv 

  sources.csv         1388 rows
  taxa.csv            6590 rows
  methods.csv           38 rows
  pairs.csv          65540 rows
  measurements.csv     930 rows

WARN  methods.csv [index_status]  3 rows
      line 8     ChristiansenFarina2004_FL  method_file='Christiansen_Farina_2004_HistBiol_theropod_mass.pdf'  -- methodology index status is 'NEEDED' (paper not on disk)
      line 27    Froese2006_cube_generic  method_file='Froese_2006_JApplIchthyol_fish_length_weight.pdf'  -- methodology index status is 'NEEDED' (paper not on disk)
      line 32    Hu2005_SI_Alexander1979_VanValkenburgh1990  method_file='Alexander_etal_1979_JZool_mammal_limb_allometry.pdf'  -- methodology index status is 'NEEDED' (paper not on disk)
WARN  pairs.csv [unknown_method]  495 rows
      line 1539  Angelici2000_DM136_L208C2128  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1540  Angelici2000_DM136_L208C2413  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1541  Angelici2000_DM136_L208C244  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1542  Angelici2000_DM136_L208C2475  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1546  Angelici2000_DM136_L208C2692  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1547  Angelici2000_DM136_L208C3200  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1548  Angelici2000_DM136_L208C3243  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1560  Angelici2000_DM136_L209C2413  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1562  Angelici2000_DM136_L209C260  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1563  Angelici2000_DM136_L209C2692  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1564  Angelici2000_DM136_L209C289  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1565  Angelici2000_DM136_L209C3243  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1574  Angelici2000_DM136_L210C2128  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1575  Angelici2000_DM136_L210C2413  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1576  Angelici2000_DM136_L210C244  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1579  Angelici2000_DM136_L210C2692  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1580  Angelici2000_DM136_L210C2799  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1581  Angelici2000_DM136_L210C3052  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1582  Angelici2000_DM136_L210C3199  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1583  Angelici2000_DM136_L210C3200  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1584  Angelici2000_DM136_L210C3243  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1585  Angelici2000_DM136_L210C3279  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1594  Angelici2000_DM136_L211C2128  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1595  Angelici2000_DM136_L211C2413  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1596  Angelici2000_DM136_L211C2475  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1600  Angelici2000_DM136_L211C2692  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1601  Angelici2000_DM136_L211C3052  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1602  Angelici2000_DM136_L211C3200  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1905  Auffenberg1981_ref91  pred_mass_method='unknown'  -- pred_mass_method is 'unknown'
      line 1905  Auffenberg1981_ref91  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 1906  Auffenberg1981_ref92  pred_mass_method='unknown'  -- pred_mass_method is 'unknown'
      line 1906  Auffenberg1981_ref92  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 4117  Begg2006_DM199_L17C1614  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 4119  Begg2006_DM199_L17C1656  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 4121  Begg2006_DM199_L17C1731  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 6667  Bowland1993_DM169_L100C346  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 6815  Brown1965_DM87_L336C2781  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7046  Brzezinski2010_DM162_L128C2556  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7113  Brzezinski2010_DM162_L132C2556  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7675  CapizziLuiselli1997_ref16  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7676  CapizziLuiselli1997_ref17  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7677  CapizziLuiselli1997_ref18  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7678  CapizziLuiselli1997_ref19  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7679  CapizziLuiselli1997_ref20  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7680  CapizziLuiselli1997_ref21  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 7681  CapizziLuiselli1997_ref22  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 9642  Cui2008_L90C792  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 9649  Cui2008_L91C792  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 10395 Delany1986_DM126_L236C103  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      line 10396 Delany1986_DM126_L236C112  prey_mass_method='unknown'  -- prey_mass_method is 'unknown'
      ... and 445 more (use --max-print=Inf or --report=FILE)
WARN  pairs.csv [unverified_method]  25 rows
      line 8586  Chen1998_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9277  Conrad2017_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9915  DalSasso2011_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9916  DalSasso2011_02  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9917  DalSasso2011_03  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9918  DalSasso2011_04  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9919  DalSasso2011_05  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9919  DalSasso2011_05  prey_mass_method_id='Froese2006_cube_generic'  -- method 'Froese2006_cube_generic' is marked verified = FALSE
      line 9920  DalSasso2011_06  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9921  DalSasso2011_07  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 9921  DalSasso2011_07  prey_mass_method_id='Froese2006_cube_generic'  -- method 'Froese2006_cube_generic' is marked verified = FALSE
      line 21976 Hone2022_01  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 22132 Hu2005_01  pred_mass_method_id='Hu2005_SI_Alexander1979_VanValkenburgh1990'  -- method 'Hu2005_SI_Alexander1979_VanValkenburgh1990' is marked verified = FALSE
      line 22493 Hwang2004_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 45108 OConnor2011_01  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 45109 OConnor2019_01  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 45109 OConnor2019_01  prey_mass_method_id='Hone2022_Meiri2010_unstated'  -- method 'Hone2022_Meiri2010_unstated' is marked verified = FALSE
      line 47592 Peyer2006_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 58531 Therrien2023_01  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 58531 Therrien2023_01  prey_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 58532 Therrien2023_02  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 58532 Therrien2023_02  prey_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 64023 Xing2012_01  pred_mass_method_id='Benson2014_S11_FCestimated'  -- method 'Benson2014_S11_FCestimated' is marked verified = FALSE
      line 64027 Xing2013_01  pred_mass_method_id='ChristiansenFarina2004_FL'  -- method 'ChristiansenFarina2004_FL' is marked verified = FALSE
      line 65186 Zheng2014_01  prey_mass_method_id='Froese2006_cube_generic'  -- method 'Froese2006_cube_generic' is marked verified = FALSE

RESULT: PASS  (0 errors in 0 checks; 523 warnings)
```

## Appendix: every dropped row

Line and column refer to the input file. Each tegu row is one tegu × one diet category recorded present; the tegu IDs are in `db/migration/dropped_rows.csv`.

**01_rawdata** (data/raw_data.csv): duplicate entry within raw_data.csv: the same records are entered under another RefNo, which is kept (evidence/rawdata_study_decisions.csv). 21 rows.

- RefNo 121 duplicates RefNo 128: L243C315 Spea_bombifrons, L243C1409 Scolopendra, L243C1788 Indet, L243C2142 Notiosorex_crawfordi, L243C2393 Baiomys_taylori, L243C2641 Onychomys_leucogaster, L243C2661 Perognathus, L243C2663 Perognathus_flavescens, L243C2672 Peromyscus, L243C2732 Reithrodontomys_megalotis, L243C3036 Indet, L243C3099 Cnemidophorus, L243C3100 Cnemidophorus_sexlineatus, L243C3101 Cnemidophorus_uniparens, L243C3123 Holbrookia_maculata, L243C3141 Sceloporus, L243C3151 Sceloporus_undulatus, L243C3153 Urosaurus_ornatus, L243C3156 Uta_stansburiana, L243C3195 Eumeces_obsoletus, L243C3306 Tantilla_nigriceps

**01_rawdata** (data/raw_data.csv): duplicate record: the same publication, predator species and evidence kind is kept from AvianDietDB (evidence/duplicate_records.csv). 25 rows.

- RefNo 23 (merged source Woodgerd1952): L439C613 Phasianus_colchicus, L439C617 Tympanuchus_phasianellus, L439C866 Passerine, L439C1801 Antilocapra_americana, L439C1864 Ovis_aries, L439C2211 Lepus_townsendii, L439C2225 Sylvilagus, L439C2401 Chaetodipus_hispidus, L439C2422 Cynomys_ludovicianus, L439C2549 Microtus, L439C2581 Muridae, L439C2674 Peromyscus, L439C2771 Spermophilus_franklinii
- RefNo 79 (merged source Errington1938): L361C365 Indet, L361C595 Gallus_gallus, L361C613 Phasianus_colchicus, L361C1384 Orthoptera, L361C2189 Lepus, L361C2233 Sylvilagus_floridanus, L361C2549 Microtus, L361C2606 Mus_musculus, L361C2673 Peromyscus, L361C2674 Peromyscus, L361C2771 Spermophilus_franklinii, L361C2777 Spermophilus_tridecemlineatus

**01_rawdata** (data/raw_data.csv): non-animal food item (prey class string is plant, fungus, placeholder or unclassified); not a predator-prey link. 268 rows.

- RefNo 105: L273C2979 Plant
- RefNo 106: L271C2979 Plant, L272C2979 Plant
- RefNo 107: L270C2979 Plant
- RefNo 108: L269C2979 Plant
- RefNo 125: L237C2861 Anacardium spp, L237C2865 Annona monticula, L237C2866 Annona spp, L237C2879 Coconut Indet(Drawf), L237C2885 Diospyros hispida, L237C2934 Grass indet, L237C2960 Mycrtaceae indet, L237C2971 Parinari Ontusifolia, L237C2981 Poulteria spp, L237C3006 Solanum lycopersicum, L238C2861 Anacardium spp, L238C2865 Annona monticula, L238C2866 Annona spp, L238C2870 Bromelia balansae, L238C2879 Coconut Indet(Drawf), L238C2885 Diospyros hispida, L238C2898 Fruit indet, L238C2934 Grass indet, L238C2960 Mycrtaceae indet, L238C2971 Parinari Ontusifolia, L238C2981 Poulteria spp, L238C2993 Rubiaceae indet, L238C3006 Solanum lycopersicum, L239C2861 Anacardium spp, L239C2865 Annona monticula, L239C2866 Annona spp, L239C2870 Bromelia balansae, L239C2871 Byrsonima spp, L239C2879 Coconut Indet(Drawf), L239C2885 Diospyros hispida, L239C2898 Fruit indet, L239C2934 Grass indet, L239C2939 Hortia brasiliensis, L239C2954 Melancium campestris, L239C2960 Mycrtaceae indet, L239C2969 Palicourea sp, L239C2971 Parinari Ontusifolia, L239C2981 Poulteria spp, L239C2993 Rubiaceae indet, L239C3006 Solanum lycopersicum, L239C3009 Talisea angustifolia
- RefNo 129: L232C2867 Arbutus unedo, L232C2884 Cultivated fruits, L232C2897 Fruit indet, L232C2946 Juniperus oxycedrus, L232C2961 Myrtus communis
- RefNo 131: L225C2945 Indet, L225C2967 Own Hair, L226C2945 Indet, L227C2945 Indet, L228C2945 Indet, L229C2945 Indet, L230C2945 Indet
- RefNo 135: L212C2864 Annona, L212C2872 Campomanesia, L212C2875 Cayaponia espelina, L212C2887 Duguetia furfuracea, L212C2888 Emmotum nitens, L212C2890 Erythroxyllum suberosum, L212C2951 Mangifera indica, L212C2957 Miconia, L212C2966 Ouratea hexasperma, L212C2982 Pouteria ramiflora, L212C2984 Psidium, L212C2997 Salacia crassiflora, L212C2999 Schefflera macrocarpa, L212C3004 Solanaceae, L212C3005 Solanum lycocarpum, L212C3007 Syagrus flexuosa, L212C3016 Zea mays
- RefNo 136: L208C2897 Fruit indet, L208C2927 Garbages, L209C2897 Fruit indet, L210C2897 Fruit indet, L210C2927 Garbages, L211C2897 Fruit indet
- RefNo 139: L201C2952 Manguifera_indica, L201C2979 Plant, L202C2874 Carica_papaya, L202C2952 Manguifera_indica, L202C2979 Plant, L203C2874 Carica_papaya, L203C2891 Ficus_sp, L203C2979 Plant
- RefNo 145: L189C1198 Lycoperdon_pyriforme/perlatum, L189C2926 Fruits berries, L189C2935 Grasses, L189C2949 Leymus_arenarius, L189C2953 MarineAlgae, L189C2959 Moss
- RefNo 149: L164C2895 Fruit, L167C2944 Indet, L168C2944 Indet, L169C2914 Fruit_Sacoglottis_gabonensis, L169C2944 Indet, L169C2985 Pteridophyta, L180C2944 Indet, L181C2944 Indet
- RefNo 150: L163C2886 Diospyros_virginiana, L163C2905 Fruit_Fragaria_Strawberry, L163C2908 Fruit_Morus_alba, L163C2913 Fruit_Pyrus_communis_pear, L163C2916 Fruit_Vittis_Grapes, L163C2927 Garbages, L163C2934 Grass indet, L163C2963 Nut_Carya, L163C3016 Zea mays
- RefNo 151: L162C2945 Indet
- RefNo 16: L504C2979 Plant, L505C2979 Plant
- RefNo 161: L133C2858 Acorn, L133C2900 FRUIT_Cherokee rose, L133C2947 Leaves, L133C2968 Paddy rice, L134C2900 FRUIT_Cherokee rose, L134C2902 FRUIT_Chinarot greenbrier, L134C2903 Fruit_Chinese berry, L134C2917 FRUIT_Watermelonm, L134C2933 Grass, L135C2900 FRUIT_Cherokee rose, L135C2902 FRUIT_Chinarot greenbrier, L135C2903 Fruit_Chinese berry, L135C2906 Fruit_Hawthorn, L135C2907 Fruit_Kiwi fruit, L135C2909 Fruit_Peach, L135C2911 Fruit_Persimmon, L135C2917 FRUIT_Watermelonm, L135C2919 Fruit1, L135C2920 Fruit2, L135C2921 Fruit3, L135C2922 Fruit4, L135C2923 Fruit5, L135C2933 Grass, L136C2883 Cotton ball
- RefNo 167: L102C2860 Aguay, L102C2880 Cocu, L102C2881 Congorosa, L102C2882 Coquito, L102C2929 Granadilla, L102C2936 Guabiyu, L102C2938 Higueron, L102C2943 Indet, L102C2958 Mora, L102C2962 Nangapiri, L102C2970 Palma, L102C2974 Pindo, L102C3000 Seeds, L102C3008 Tala, L102C3011 Tomatillo, L102C3014 Vegetal
- RefNo 17: L460C2979 Plant
- RefNo 172: L94C2932 Grass, L95C2932 Grass
- RefNo 177: L87C2896 Fruit, L87C2899 Fruit_Apple, L87C2901 Fruit_Cherry, L87C2904 Fruit_Fig, L87C2910 Fruit_Peanut, L87C2912 Fruit_Plum, L87C2913 Fruit_Pyrus_communis_pear, L87C2915 Fruit_Tomato, L87C2948 Leaves
- RefNo 179: L83C2940 Indet
- RefNo 180: L79C2942 Indet
- RefNo 181: L70C2941 Indet, L71C2941 Indet, L73C2941 Indet, L74C2941 Indet, L77C2941 Indet
- RefNo 182: L68C2927 Garbages
- RefNo 183: L67C3017 Indet
- RefNo 188: L42C2918 Fruit_Zizyphus, L42C2925 Fruits, L42C2931 Grass, L43C2925 Fruits, L43C2931 Grass
- RefNo 191: L34C2965 Other, L34C3002 Seeds_Pinus_albicaulis, L35C2965 Other, L35C3002 Seeds_Pinus_albicaulis, L36C2965 Other, L36C3002 Seeds_Pinus_albicaulis, L37C2965 Other, L37C3002 Seeds_Pinus_albicaulis
- RefNo 196: L24C2868 Arctostaphylus, L24C2975 Pinus, L24C2983 Prunus_emarginata, L24C2986 Quercus, L24C2988 Rhamnus, L24C2989 Ribes, L24C2994 Rubus_parviflorus, L24C2998 Sambucus
- RefNo 198: L19C2868 Arctostaphylus, L19C2991 Ribes_nevadense, L19C2992 Ribes_roezlii, L20C2868 Arctostaphylus, L20C2991 Ribes_nevadense, L21C2868 Arctostaphylus
- RefNo 2: L512C2979 Plant
- RefNo 200: L16C1199 Indet
- RefNo 202: L13C1165 19.2, L13C2930 Grass, L14C1164 13.6, L14C2930 Grass
- RefNo 204: L6C2924 Fruits
- RefNo 21: L515C2979 Plant, L520C2979 Plant
- RefNo 24: L503C2979 Plant
- RefNo 28: L496C2979 Plant, L497C2979 Plant
- RefNo 35: L456C2979 Plant, L457C2979 Plant, L458C2979 Plant, L459C2979 Plant
- RefNo 36: L518C2979 Plant, L519C2979 Plant
- RefNo 37: L516C2979 Plant, L517C2979 Plant
- RefNo 4: L461C2979 Plant, L462C2979 Plant
- RefNo 41: L501C2979 Plant
- RefNo 45: L433C2892 Forbs, L433C2935 Grasses, L433C2964 Nuts
- RefNo 46: L432C2859 Agoseris, L432C2862 Anacharis, L432C2863 Angelica, L432C2869 Aster, L432C2873 Carex, L432C2876 Chlorophycaea, L432C2877 Cirsium, L432C2878 Claystonia, L432C2889 Equisetum, L432C2892 Forbs, L432C2893 Formicidae, L432C2894 Fragaria, L432C2928 Graminae, L432C2937 Heracleum, L432C2950 Lomatium, L432C2955 Melica, L432C2956 Mertensia, L432C2972 Pastinaca, L432C2973 Perideridia, L432C2976 Pinus, L432C2980 Polygonium, L432C2987 Ranunculus, L432C2990 Ribes, L432C2995 Ruppia, L432C2996 Russula, L432C3003 Smilacina, L432C3010 Taraxacum, L432C3012 Trifolium, L432C3013 Vaccinium, L432C3015 Vespidae
- RefNo 55: L413C2979 Plant, L413C3001 Seeds
- RefNo 71: L376C2893 Formicidae
- RefNo 9: L498C2979 Plant, L499C2979 Plant
- RefNo 94: L315C2979 Plant, L316C2979 Plant
- RefNo 95: L313C2979 Plant, L314C2979 Plant

**01_rawdata** (data/raw_data.csv): record has no prey count > 0 in any prey column. 1 row.

- line 231: RefNo 130; Caiman latirostris; ObsType Stomach

**03_extant_new_observations** (data/extant_new_observations.csv): duplicate of ref_id 97: the same Tropidurus hispidus x T. jaguaribanus predation (Passos et al. 2016), entered once from the paper (ref 97, kept) and once via Oliveira et al. 2025 Table S2 row 79. 1 row.

- ref_id 177: Oliveira_etal_2025_SciRep_s41598-025-89810-7_TableS1; Tropidurus hispidus; obs Observation

**03_extant_new_observations** (data/extant_new_observations.csv): no prey taxon recorded (row states the predator's diet breadth only; species detail in Data S1 not extracted). 6 rows.

- ref_id 10: Lu_et_al_2023_CurrBiol_33_647; Vulpes vulpes; obs Fecal_DNA_metabarcoding
- ref_id 11: Lu_et_al_2023_CurrBiol_33_647; Canis lupus; obs Fecal_DNA_metabarcoding
- ref_id 12: Lu_et_al_2023_CurrBiol_33_647; Panthera uncia; obs Fecal_DNA_metabarcoding
- ref_id 13: Lu_et_al_2023_CurrBiol_33_647; Panthera pardus; obs Fecal_DNA_metabarcoding
- ref_id 14: Lu_et_al_2023_CurrBiol_33_647; Ursus arctos; obs Fecal_DNA_metabarcoding
- ref_id 15: Lu_et_al_2023_CurrBiol_33_647; Canis lupus_familiaris; obs Fecal_DNA_metabarcoding

**04_raptordiets** (data/structured/raptor_diets_long.csv): duplicate record: the same publication, predator species and evidence kind is kept from another upstream table (evidence/duplicate_records.csv). 3043 rows. Listed by publication and predator in the table at the end of this appendix; by OS-Prey study, dataset and file line in `db/migration/dropped_appendix.md`; one row per dropped line in `db/migration/dropped_rows.csv`.

**06_jekyll** (data/structured/gator_jekyll_stomach.csv): seeds are not prey (non-animal food item). 4 rows.

- line 5: alligator CDF; count 2; mass 0.04 g
- line 8: alligator CR; count 1; mass 0.5 g
- line 17: alligator BFN; count 1; mass 0.5 g
- line 33: alligator CIM; count 1; mass 0.04 g

**08_tegu** (data/structured/tegu_diet_dietcategories.csv): non-animal or man-made item (fruit, vegetation, seeds, man-made material); not a predator-prey link. 647 rows.

- Fruit: 179 tegus
- Man.made: 9 tegus
- Misc.Veg: 265 tegus
- Non.Fruit.Seeds: 194 tegus

**Duplicate blocks dropped** (`code/db_migrate/evidence/duplicate_records.csv`, action `drop`). One row per publication × upstream table × predator × evidence kind.

| Merged source | Dropped from | Study string | Predator | Evidence | Rows | Kept copy |
|---|---|---|---|---|---|---|
| Artuso2010 | OS-Prey | Artuso 2010 | *Megascops asio* | pellet, scat, remains or observation | 56 | raw_data |
| Becker1985 | OS-Prey | Becker 1985 | *Falco columbarius* | pellet, scat, remains or observation | 34 | raw_data |
| Blair1982 | OS-Prey | Blair and Schitoskey 1982 | *Buteo regalis* | pellet, scat, remains or observation | 38 | raw_data |
| Cahn1930 | OS-Prey | Cahn and Kemp 1930 | *Asio flammeus* | pellet, scat, remains or observation | 7 | raw_data |
| Cahn1930 | OS-Prey | Cahn and Kemp 1930 | *Asio otus* | pellet, scat, remains or observation | 20 | raw_data |
| Cahn1930 | OS-Prey | Cahn and Kemp 1930 | *Megascops asio* | pellet, scat, remains or observation | 9 | raw_data |
| Cahn1930 | OS-Prey | Cahn and Kemp 1930 | *Strix varia* | pellet, scat, remains or observation | 11 | raw_data |
| Cahn1930 | OS-Prey | Cahn and Kemp 1930 | *Tyto alba* | pellet, scat, remains or observation | 6 | raw_data |
| Campbell1987 | OS-Prey | Campbell et al 1987 | *Tyto alba* | pellet, scat, remains or observation | 91 | raw_data |
| Carnie1954 | OS-Prey | Carnie 1954 | *Aquila chrysaetos* | pellet, scat, remains or observation | 46 | raw_data |
| Collopy1983 | OS-Prey | Collopy and Koplin 1983 | *Falco sparverius* | pellet, scat, remains or observation | 23 | raw_data |
| Cruz1976 | OS-Prey | Cruz 1976 | *Falco sparverius* | pellet, scat, remains or observation | 13 | raw_data |
| Cui2008 | OS-Prey | Cui et al 2008 | *Bubo bubo* | pellet, scat, remains or observation | 7 | raw_data |
| Cui2008 | OS-Prey | Cui et al 2008 | *Buteo hemilasius* | pellet, scat, remains or observation | 6 | raw_data |
| Detienne2008 | OS-Prey | Detienne et al 2008 | *Bubo scandiacus* | pellet, scat, remains or observation | 22 | raw_data |
| Dunstan1975 | OS-Prey | Dunstan and Harper 1975 | *Haliaeetus leucocephalus* | pellet, scat, remains or observation | 12 | raw_data |
| Ellis2004 | OS-Prey | Ellis et al 2004 | *Falco peregrinus* | pellet, scat, remains or observation | 123 | raw_data |
| Errington1938 | raw_data | RefNo 79 | *Buteo lagopus* | stomach or regurgitate | 1 record | AvianDietDB |
| Estes2003 | OS-Prey | Estes and Mannan 2003 | *Accipiter cooperii* | pellet, scat, remains or observation | 21 | raw_data |
| Fielder1982 | OS-Prey | Fielder 1982 | *Haliaeetus leucocephalus* | pellet, scat, remains or observation | 54 | raw_data |
| Franklin1988 | OS-Prey | Franklin 1988 | *Strix nebulosa* | pellet, scat, remains or observation | 11 | raw_data |
| Goodman1993 | OS-Prey | Goodman et al 1993 | *Asio madagascariensis* | pellet, scat, remains or observation | 24 | raw_data |
| Goodman1998 | OS-Prey | Goodman and Thorstrom 1998 | *Tyto soumagnei* | pellet, scat, remains or observation | 19 | raw_data |
| Graham2012 | OS-Prey | Graham 2012 | *Strix varia* | pellet, scat, remains or observation | 142 | raw_data |
| Hamerstrom1951 | OS-Prey | Hamerstrom and Hamerstrom 1951 | *Accipiter cooperii* | stomach or regurgitate | 92 | AvianDietDB |
| Hawbecker1945 | OS-Prey | Hawbecker 1945 | *Tyto alba* | pellet, scat, remains or observation | 90 | raw_data |
| Hector1985 | OS-Prey | Hector 1985 | *Falco femoralis* | pellet, scat, remains or observation | 81 | raw_data |
| Heintzelman1964 | OS-Prey | Heintzelman 1964 | *Falco sparverius* | pellet, scat, remains or observation | 18 | raw_data |
| Hiraldo1991 | OS-Prey | Hiraldo et al 1991 | *Accipiter cooperii* | pellet, scat, remains or observation | 22 | raw_data |
| Hiraldo1991 | OS-Prey | Hiraldo et al 1991 | *Buteo albonotatus* | pellet, scat, remains or observation | 21 | raw_data |
| Hiraldo1991 | OS-Prey | Hiraldo et al 1991 | *Buteo jamaicensis* | pellet, scat, remains or observation | 11 | raw_data |
| Hiraldo1991 | OS-Prey | Hiraldo et al 1991 | *Buteogallus anthracinus* | pellet, scat, remains or observation | 5 | raw_data |
| Hiraldo1991 | OS-Prey | Hiraldo et al 1991 | *Falco sparverius* | pellet, scat, remains or observation | 13 | raw_data |
| Hussain2016 | OS-Prey | Hussain et al 2016 | *Accipiter nisus* | pellet, scat, remains or observation | 34 | raw_data |
| Hussain2016 | OS-Prey | Hussain et al 2016 | *Milvus migrans* | pellet, scat, remains or observation | 25 | raw_data |
| Itamies1972 | OS-Prey | Itamies and Mikkola 1972 | *Pernis apivorus* | stomach or regurgitate | 4 | raw_data |
| Korpimaki1985 | OS-Prey | Korpimaki 1985 | *Falco tinnunculus* | pellet, scat, remains or observation | 18 | raw_data |
| Korpimaki1987 | OS-Prey | Korpimaki and Sulkava 1987 | *Strix uralensis* | pellet, scat, remains or observation | 68 | raw_data |
| MacLaren1988 | OS-Prey | MacLaren et al 1988 | *Aquila chrysaetos* | pellet, scat, remains or observation | 7 | raw_data |
| MacLaren1988 | OS-Prey | MacLaren et al 1988 | *Buteo jamaicensis* | pellet, scat, remains or observation | 5 | raw_data |
| MacLaren1988 | OS-Prey | MacLaren et al 1988 | *Buteo regalis* | pellet, scat, remains or observation | 6 | raw_data |
| MacLaren1988 | OS-Prey | MacLaren et al 1988 | *Falco mexicanus* | pellet, scat, remains or observation | 6 | raw_data |
| Marr1983 | OS-Prey | Marr and Knight 1983 | *Aquila chrysaetos* | pellet, scat, remains or observation | 35 | raw_data |
| Marti1974 | OS-Prey | Marti 1974 | *Asio otus* | pellet, scat, remains or observation | 35 | raw_data |
| Marti1974 | OS-Prey | Marti 1974 | *Athene cunicularia* | pellet, scat, remains or observation | 83 | raw_data |
| Marti1974 | OS-Prey | Marti 1974 | *Bubo virginianus* | pellet, scat, remains or observation | 106 | raw_data |
| Marti1974 | OS-Prey | Marti 1974 | *Tyto alba* | pellet, scat, remains or observation | 70 | raw_data |
| McEwan1980 | OS-Prey | McEwan and Hirth 1980 | *Haliaeetus leucocephalus* | pellet, scat, remains or observation | 35 | raw_data |
| Meng1959 | OS-Prey | Meng Heinz 1959 | *Accipiter cooperii* | pellet, scat, remains or observation | 28 | raw_data |
| Meng1959 | OS-Prey | Meng Heinz 1959 | *Accipiter gentilis* | pellet, scat, remains or observation | 9 | raw_data |
| Meserve1977 | OS-Prey | Meserve 1977 | *Elanus leucurus* | pellet, scat, remains or observation | 5 | raw_data |
| Mikkola1970 | OS-Prey | Mikkola and Sulkava 1970 | *Strix nebulosa* | stomach or regurgitate | 9 | AvianDietDB |
| Miranda2018 | OS-Prey | Miranda 2018 | *Harpia harpyja* | pellet, scat, remains or observation | 34 | raw_data |
| Mohedano2014 | OS-Prey | Mohedano et al 2014 | *Bubo ascalaphus* | pellet, scat, remains or observation | 8 | raw_data |
| Olsen2013a | OS-Prey | Olsen et al 2013 | *Haliaeetus leucogaster* | pellet, scat, remains or observation | 27 | raw_data |
| Olsen2013a | OS-Prey | Olsen et al 2013 | *Haliastur sphenurus* | pellet, scat, remains or observation | 36 | raw_data |
| Olsen2013b | OS-Prey | Olsen et al 2013 | *Aquila audax* | pellet, scat, remains or observation | 24 | raw_data |
| Olsen2013b | OS-Prey | Olsen et al 2013 | *Hieraaetus morphnoides* | pellet, scat, remains or observation | 15 | raw_data |
| Pande2011 | OS-Prey | Pande 2011 | *Bubo bengalensis* | pellet, scat, remains or observation | 47 | raw_data |
| Pande2011 | OS-Prey | Pande et al 2011 | *Bubo bengalensis* | pellet, scat, remains or observation | 47 | raw_data |
| Patterson2007 | OS-Prey | Patterson 2007 | *Bubo scandiacus* | pellet, scat, remains or observation | 6 | raw_data |
| Petty1999 | OS-Prey | Petty 1999 | *Strix aluco* | pellet, scat, remains or observation | 60 | raw_data |
| Randall1940 | OS-Prey | Randall 1940 | *Circus hudsonius* | pellet, scat, remains or observation | 42 | raw_data |
| RangelSalazar1993 | OS-Prey | Rangel-Salazar and Enriquez-Rocha 1993 | *Spizaetus tyrannus* | pellet, scat, remains or observation | 6 | raw_data |
| Restani1991 | OS-Prey | Restani 1991 | *Beteo swainsoni* | pellet, scat, remains or observation | 5 | raw_data |
| Restani1991 | OS-Prey | Restani 1991 | *Buteo jamaicensis* | pellet, scat, remains or observation | 10 | raw_data |
| Restani1991 | OS-Prey | Restani 1991 | *Buteo regalis* | pellet, scat, remains or observation | 7 | raw_data |
| Restani1991 | OS-Prey | Restani 1991 | *Buteo swainsoni* | pellet, scat, remains or observation | 5 | raw_data |
| Reynolds1984 | OS-Prey | Reynolds and Meslow 1984 | *Accipiter cooperii* | pellet, scat, remains or observation | 95 | raw_data |
| Reynolds1984 | OS-Prey | Reynolds and Meslow 1984 | *Accipiter gentilis* | pellet, scat, remains or observation | 45 | raw_data |
| Reynolds1984 | OS-Prey | Reynolds and Meslow 1984 | *Accipiter striatus* | pellet, scat, remains or observation | 67 | raw_data |
| Rodriguez2010 | OS-Prey | Rodriguez et al 2010 | *Falco naumanni* | pellet, scat, remains or observation | 23 | raw_data |
| Rohner1995 | OS-Prey | Rohner et al 1995 | *Surnia ulula* | pellet, scat, remains or observation | 38 | raw_data |
| Rusch1972 | OS-Prey | Rusch and Doerr 1972 | *Buteo platypterus* | pellet, scat, remains or observation | 28 | raw_data |
| Sergio2002 | OS-Prey | Sergio et al 2002 | *Buteo buteo* | pellet, scat, remains or observation | 28 | raw_data |
| Shao2006 | OS-Prey | Shao and Liu 2006 | *Asio otus* | pellet, scat, remains or observation | 6 | raw_data |
| Shao2007 | OS-Prey | Shao et al 2007 | *Athene noctua* | pellet, scat, remains or observation | 14 | raw_data |
| Shehab2008 | OS-Prey | Shehab and Ciach 2008 | *Bubo ascalaphus* | pellet, scat, remains or observation | 14 | raw_data |
| Swenson1986 | OS-Prey | Swenson et al 1986 | *Haliaeetus leucocephalus* | pellet, scat, remains or observation | 79 | raw_data |
| Tilley1982 | OS-Prey | Tilley 1982 | *Ninox strenua* | pellet, scat, remains or observation | 26 | raw_data |
| Todd1982 | OS-Prey | Todd et al 1982 | *Haliaeetus leucocephalus* | pellet, scat, remains or observation | 97 | raw_data |
| Virani2008 | OS-Prey | Virani 2008 | *Otus ireneae* | pellet, scat, remains or observation | 5 | raw_data |
| Whitman2009 | OS-Prey | Whitman 2009 | *Aegolius funereus* | pellet, scat, remains or observation | 46 | raw_data |
| Wiley2010 | OS-Prey | Wiley 2010 | *Tyto alba* | pellet, scat, remains or observation | 114 | raw_data |
| Wiley2010 | OS-Prey | Wiley 2010 | *Tyto glaucops* | pellet, scat, remains or observation | 125 | raw_data |
| Woodgerd1952 | OS-Prey | Woodgerd 1952 | *Aquila chrysaetos* | stomach or regurgitate | 14 | AvianDietDB |
| Woodgerd1952 | raw_data | RefNo 23 | *Aquila chrysaetos* | stomach or regurgitate | 1 record | AvianDietDB |
| Young1997 | OS-Prey | Young et al 1997 | *Strix occidentalis* | pellet, scat, remains or observation | 64 | raw_data |
