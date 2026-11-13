# Step 3b: mass top-up for fossil pairs

Written 2026-11-12 for Phase 3b (Prompt 3b in `docs/ASSESSMENT_AND_PLAN_2026-11.md`). Everything here is produced by `code/db_migrate/10_fossil_step2.R`, which now sources `code/db_migrate/10b_fossil_topup.R`. The per-pair data are in `data/fossil_rebuild/step3b_topup.csv`. Re-running the script replaces only its own rows.

## Summary

- Before: 14 of 74 pairs had both masses. After: **53 have both a predator and a prey mass or bracket**. 21 pairs still lack one side.
- Prey side: 13 pairs keep their Phase 3 mass (rung 0); **6 rung 1, 1 rung 2, 6 rung 3, 25 rung 4, 4 rung 5**; 19 stay empty. Predator side: 63 unchanged, 3 rung 5 (taxon range), 8 empty.
- **Rung 1 is six pairs.** Three fish with an author-estimated length (DalSasso2011_05, _07, Zheng2014_01) move from an assumed cube-law coefficient to Froese 2006 Table 1. *Protoceratops* (Carpenter1998_01) and the two juvenile hadrosaurs (Varricchio2001_01, _02) use the Seebacher 2001 length–mass equations, which arrived after the first pass (Table 3 verified on the rendered page). All six are `verified = TRUE`.
- 29 prey masses (rungs 4 and 5) are now **brackets with no point value** (`prey_mass_lo_g` and `_hi_g` only), and 2 predator masses (Varricchio2001_01, _02) and 1 (Dalsatt2006_01) likewise. Treat them as interval-censored data, not estimates.
- **Most of the new values are weak.** Rungs 4 and 5 rest on a comparison or an assumption, not on a measurement of the animal plus a published equation, so they use `assigned_approximate` and fall in tier E. Twenty-nine pairs are now tier E (none were before this phase). The earlier count of 67 tier A is now 30. Do not read the loss of tier A as lost data: those prey had no mass.
- `Rscript code/db_validate.R`: PASS, 0 errors. New warning `bracket_only` lists the bracket rows. `Rscript db/tests/run_tests.R`: all tests pass.

## How I read the ladder

- **Stop at the first rung that applies.** Where I stopped below a rung that might have applied, I say why in the pair's basis. Rung 2 needs an equation after the measurement; where none is on disk (Mesozoic mammal jaws, lizard femur to SVL, ornithischian scapula) I could not complete rung 2.
- **Brackets built from a comparison or limit are `assigned_approximate` (tier E).** A bracket built from a measurement of the item plus a published equation (none of the new ones) would be `regressed_*` and could be tier A. This keeps the weakest rows out of the headline comparison.
- **Bounds must be positive.** The validator and the log-scale model need lo > 0, and the schema wants both bounds. An upper-bound-only record is therefore written with a weak physical floor as its lower bound (the length of the bone cluster, or the fish length that fits a measured fin ray). Those rows are rung 5 and say so.
- **Intervals are not confidence intervals.** For rung 3 they are x/÷ 3 (the rule), or wider where noted. For rung 4 and 5 they are the bracket itself.
- **Not estimated, by rule:** bite traces (Carpenter1998_02, DePalma2013_01: predator left empty; their Phase 3 prey values stand), the isolated pellets Zheng2018_07 and _08, invertebrate prey (Kear2006_01–03), and records marked WEAK in step 1 (Zhou2009_01 and Zhou2010_01).

## What I tried and what failed

- **Rung 2 for *Confuciusornis* (Xing2012_04, _05).** I rendered Xing et al. 2012 Fig. 3E, where the humerus is drawn against a 10 mm bar (38 px at 130 dpi). My reading, about 26 mm, would give 53 g by Field et al. 2013, a tenth of any published *Confuciusornis* mass. The drawing is too small to trust, or the element is incomplete. I dropped to rung 3.
- **Rung 2 for the *Sinosauropteryx* lizard (Chen1998_01).** This one worked: Smithwick et al. 2017 SI Fig. S2C has a 20 mm bar (89 px at 150 dpi, measured on the render). Snout to pelvis is about 515 px, 116 mm. I measured by eye, so ±20 %, and the skull may run past the image edge. Meiri 2010 gives 34 g (19–60 g for SVL 95–140 mm). Chen et al.'s own femur and SVL of 106 mm give about 26 g under the same equation, inside the interval.
- **Rung 2 for the *Anchiornis* pellet lizards (Zheng2018_01–03).** I measured femora from Zheng et al. 2018 Fig. 1c (10 mm bar = 77 px at 200 dpi): about 10, 9 and 4 mm. No femur-to-SVL equation is on disk, so these are rung 4 brackets using the one comparison I have, *Bavarisaurus* (femur 21.9 mm, precaudal length 91 mm; Conrad 2017), with a ratio range of 0.18–0.30 that is my choice.
- **Dryad (Huang et al. 2024 `allmass.csv`) refused an automated download; JM then supplied it.** It covers Eutriconodonta only (units not stated; read as g), so it helps the gobiconodontid (Qiu2025_01) and nothing else. No *Zhangheotherium*, *Sinobaatar*, eutherian or *Meemannodon*.
- **Seebacher 2001** (ceratopsian and hadrosaur length–mass). My download returned an HTML page; JM supplied the PDF, now in `pdfs/methodology/` and `on_disk` in the index.
- **Not searched:** a sauropod-hatchling length–mass equation and a lizard femur-to-SVL equation. The prompt asked me to obtain them; I did not find or look for a source beyond the checks above.
- **Obtained:** Froese 2006 (author-hosted copy, 10 pages, Table 1 transcribed from p. 249) and Chiappe et al. 2023 (open access, for the *Confuciusornis* mass). Both are in `pdfs/methodology/` and in `docs/methodology_index.csv`.

## Methods registered or changed

| method_id | Change |
|---|---|
| `Froese2006_T1_fusiform` | new, `verified = TRUE`: W = 0.0137 L³ (g, cm); 5th–95th percentile of a3.0 0.0061–0.0240 |
| `Froese2006_T1_elongated` | new, `verified = TRUE`: W = 0.00838 L³; used for fish bracket lower bounds |
| `Froese2006_T1_shortdeep` | new, `verified = TRUE`: W = 0.0187 L³; registered, not used yet |
| `Froese2006_cube_generic` | removed. It was unverified and nothing uses it now |
| `Seebacher2001_T3_Ceratopsia` | new, `verified = TRUE`: M = 12.58 TL^2.90 kg (TL in m; exponent ±0.94; calibration 1.4–8.0 m) |
| `Seebacher2001_T3_Ornithopoda` | new, `verified = TRUE`: M = 11.81 TL^2.66 kg (exponent ±0.14; calibration 0.65–17 m) |
| `Meiri2010_T3_Sauria` | now used by Chen1998_01 (rung 2) |

Froese's L is total length. The fossil authors say "body length", so for the three rung 1 fish the length may be standard length, which would push the mass slightly high. The Table 1 intervals do not cover that.

## Judgement calls that move numbers

- **Fish ceiling of 160 mm.** The only fish a yanornithid is known to have swallowed whole and measured are the two in STM9-15, 70–80 mm (Zheng et al. 2014). I used twice that as the upper length for every unmeasured fish bracket, with the 95th-percentile fusiform form factor, giving 98 g. For *Microraptor* (1.9 kg) that ceiling may be low. The lower bound is the measured fin ray, bone cluster, or half the STM9-15 fish.
- ***Jinanichthys*-identified crop fish** (Zheng2014_07, _08, _13): rung 3, 5.8 g (1.9–17 g) from the STM9-15 length, which is itself a tentative identification. Dalsatt2006_01 gets the same point at x/÷ 10 because 7–9 vertebrae in 6 mm imply a much smaller fish.
- ***Protoceratops* (Carpenter1998_01).** Rung 1 now: 24 kg (18–34 kg) from the Seebacher ceratopsian equation at 1.2–1.3 m. That is below the 1.4 m calibration minimum, and the paper does not say the length is total length. It replaces my first-pass rung 3 value of 82.7 kg (an adult). Seebacher's own 1.4 m *Protoceratops* is 23.7 kg.
- ***Confuciusornis* (Xing2012_04, _05; Dalsatt2006_01).** 540 g is the geometric mean of 615 g (Chiappe et al. 2023, from Serrano et al. 2015, which I have not read) and 475 g (S11, MOR 1063, labelled near-maximal). Interval x/÷ 3. For the predator, IVPP V13313 has no published measurement, so it is a bracket (180–1,620 g).
- **Jehol mammals** (Hurum2006_01, _02; Qiu2025_02): one shared bracket of 4.3–130 g, from the 13–43 g Hone et al. 2022 quote for *Eomaia* and *Yanoconodon*, widened x/÷ 3. These are other taxa. It carries no information about *Zhangheotherium*, *Sinobaatar* or the eutherian beyond "small".
- **Gobiconodontid** (Qiu2025_01): 4.7–281 g, from the Huang et al. 2024 species medians for *Gobiconodon bathoniensis* (14.0 g) to *Meiconodon lii* (93.6 g), widened x/÷ 3. *Meemannodon*, the closest named taxon, is not in that file.
- **Lepidosaurs in *Scipionyx*** (DalSasso2011_02, _03): total length times 0.3–0.6 gives SVL (my range), then Meiri. The lower length of _03 (50 mm) is my floor, not from the source.
- ***Sanajeh* prey** (Wilson2010_01): 217 g to 2,316 g. Floor: the Farlow 2005 alligator equation at 0.5 m (a crocodylian proxy). Ceiling: the associated egg. The pair is a combat association, not ingestion.
- ***Daspletosaurus* OTM 200** (Varricchio2001_01, _02): 330 kg to 5,890 kg from the smallest and largest limb-based S11 *D. torosus* masses, each widened by x/÷ 1.83. The paper gives no stage, so the ratio is wide.
- **Juvenile hadrosaurs** (Varricchio2001_01, _02): replaced the Phase 3 isometric scaling from *Edmontosaurus* (352 kg and 17.3 kg) with the Seebacher ornithopod equation: 220 kg (144–334) for 'about 3 m', and 15.2 kg (11.8–19.7) for 1.0–1.2 m. The first fell by about a third, as Phase 3 expected. Both come from reconstructed adults and subadults, so a juvenile's thinner body may sit lower still. The Phase 3 values are in the notes as cross-checks.
- **Identifications** follow decision 5. Where fish or mammal identity is tentative the bracket does not depend on it.

## Every pair

Rung 0 means the mass is unchanged from Phase 3. Values are point (lower–upper); "no point" is a bracket. Tier is the letter of `pair_resolution`. The method is the `method_id`, or the `mass_method` where there is none.

| Pair | Predator | Prey | Tier | Pred rung | Predator mass | Pred method | Prey rung | Prey mass | Prey method |
|---|---|---|---|---|---|---|---|---|---|
| Hu2005_01 | Repenomamus robustus | Psittacosaurus | A | 0 (Phase 3, unchanged) | 4.9 kg (4 kg–6 kg) | `Hu2005_SI_Alexander1979_VanValkenburgh1990` | 0 (Phase 3, unchanged) | 82.6 g (37.3 g–107 g) | `ErickTum2000_DME_bone` |
| Han2023_01 | Repenomamus robustus | Psittacosaurus lujiatunensis | A | 0 (Phase 3, unchanged) | 3.43 kg (2.01 kg–4.85 kg) | `Han2023_DME_CampioneEvans2012` | 0 (Phase 3, unchanged) | 10.6 kg (4.6 kg–16.6 kg) | `Han2023_DME_Campione2014` |
| Chen1998_01 | Sinosauropteryx prima | Lacertilia | A | 0 (Phase 3, unchanged) | 992 g (542 g–1.82 kg) | `Benson2014_S11_FCestimated` | 2 | 33.8 g (18.6 g–60.2 g) | `Meiri2010_T3_Sauria` |
| Hurum2006_01 | Sinosauropteryx prima | Zhangheotherium | E | 0 (Phase 3, unchanged) | 1.94 kg (1.06 kg–4.06 kg) | `ErickTum2000_DME_bone` | 4 | no point (4.33 g–129 g) | `assigned_approximate` |
| Hurum2006_02 | Sinosauropteryx prima | Sinobaatar | E | 0 (Phase 3, unchanged) | 1.94 kg (1.06 kg–4.06 kg) | `ErickTum2000_DME_bone` | 4 | no point (4.33 g–129 g) | `assigned_approximate` |
| Dalsatt2006_01 | Confuciusornis sanctus | Jinanichthys | D | 5 | no point (180 g–1.62 kg) | `species_average_literature` | 3 | 5.78 g (0.578 g–57.8 g) | `species_average_literature` |
| Kear2006_01 | Notochelone | Inoceramidae | A | none | — | `` | none | — | `` |
| Kear2006_02 | Notochelone | Inoceramidae | A | none | — | `` | none | — | `` |
| Kear2006_03 | Notochelone | Inoceramidae | A | none | — | `` | none | — | `` |
| Zhou2014_01 | Piscivoravis lii | Teleostei | E | 0 (Phase 3, unchanged) | 464 g (190 g–1.13 kg) | `Field2013_T1_HL` | 4 | no point (0.0184 g–98.3 g) | `assigned_approximate` |
| Zhou2014_02 | Piscivoravis lii | Teleostei | E | 0 (Phase 3, unchanged) | 464 g (190 g–1.13 kg) | `Field2013_T1_HL` | 5 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Huang2026_01 | Kunpengornis anhuimusei | Teleostei | E | 0 (Phase 3, unchanged) | 587 g (241 g–1.43 kg) | `Field2013_T1_HL` | 5 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Xing2013_01 | Microraptor gui | Teleostei | E | 0 (Phase 3, unchanged) | 1.89 kg (891 g–2.98 kg) | `ChristiansenFarina2004_FL` | 4 | no point (0.00252 g–98.3 g) | `assigned_approximate` |
| OConnor2011_01 | Microraptor gui | Enantiornithes | B | 0 (Phase 3, unchanged) | 760 g (760 g–3.57 kg) | `ChristiansenFarina2004_FL` | 0 (Phase 3, unchanged) | 65 g (60 g–70 g) | `species_average_literature` |
| OConnor2019_01 | Microraptor zhaoianus | Indrasaurus wangi | A | 0 (Phase 3, unchanged) | 570 g (216 g–725 g) | `ChristiansenFarina2004_FL` | 0 (Phase 3, unchanged) | 8.49 g (6 g–12 g) | `Hone2022_Meiri2010_unstated` |
| Zheng2018_01 | Anchiornis | Lacertilia | E | 0 (Phase 3, unchanged) | 863 g (362 g–1.58 kg) | `ErickTum2000_DME_bone` | 4 | no point (0.398 g–6.37 g) | `assigned_approximate` |
| Zheng2018_02 | Anchiornis | Lacertilia | E | 0 (Phase 3, unchanged) | 863 g (362 g–1.58 kg) | `ErickTum2000_DME_bone` | 4 | no point (0.265 g–4.89 g) | `assigned_approximate` |
| Zheng2018_03 | Anchiornis | Lacertilia | E | 0 (Phase 3, unchanged) | 863 g (362 g–1.58 kg) | `ErickTum2000_DME_bone` | 4 | no point (0.0204 g–0.45 g) | `assigned_approximate` |
| Zheng2018_04 | Anchiornis | Ptycholepidae | A | 0 (Phase 3, unchanged) | 827 g (347 g–1.51 kg) | `ErickTum2000_DME_bone` | none | — | `` |
| Zheng2018_05 | Anchiornis | Ptycholepidae | A | 0 (Phase 3, unchanged) | 103 g (43.4 g–189 g) | `ErickTum2000_DME_bone` | none | — | `` |
| Zheng2018_06 | Anchiornis | Ptycholepidae | A | 0 (Phase 3, unchanged) | 160 g (67.2 g–293 g) | `ErickTum2000_DME_bone` | none | — | `` |
| Zheng2018_07 | Anchiornis | Ptycholepidae | C | none | — | `` | none | — | `` |
| Zheng2018_08 | Anchiornis | Ptycholepidae | C | none | — | `` | none | — | `` |
| Xing2012_01 | Sinocalliopteryx gigas | Sinornithosaurus | A | 0 (Phase 3, unchanged) | 32.3 kg (17.7 kg–59.2 kg) | `Benson2014_S11_FCestimated` | 0 (Phase 3, unchanged) | 2.62 kg (1.43 kg–9.9 kg) | `ErickTum2000_DME_bone` |
| Xing2012_04 | Sinocalliopteryx gigas | Confuciusornis sanctus | B | 0 (Phase 3, unchanged) | 58.2 kg (22.9 kg–163 kg) | `ErickTum2000_DME_bone` | 3 | 540 g (180 g–1.62 kg) | `species_average_literature` |
| Xing2012_05 | Sinocalliopteryx gigas | Confuciusornis sanctus | B | 0 (Phase 3, unchanged) | 58.2 kg (22.9 kg–163 kg) | `ErickTum2000_DME_bone` | 3 | 540 g (180 g–1.62 kg) | `species_average_literature` |
| Xing2012_06 | Sinocalliopteryx gigas | Ornithischia | A | 0 (Phase 3, unchanged) | 58.2 kg (22.9 kg–163 kg) | `ErickTum2000_DME_bone` | none | — | `` |
| Nesbitt2006_01 | Coelophysis bauri | Crocodylomorpha | A | 0 (Phase 3, unchanged) | 8.29 kg (4.39 kg–15.2 kg) | `ErickTum2000_DME_bone` | none | — | `` |
| Varricchio2001_01 | Daspletosaurus | Hadrosauridae | C | 5 | no point (330 kg–5,890 kg) | `species_average_literature` | 1 | 219 kg (144 kg–334 kg) | `Seebacher2001_T3_Ornithopoda` |
| Varricchio2001_02 | Daspletosaurus | Hadrosauridae | C | 5 | no point (330 kg–5,890 kg) | `species_average_literature` | 1 | 15.2 kg (11.8 kg–19.7 kg) | `Seebacher2001_T3_Ornithopoda` |
| Therrien2023_01 | Gorgosaurus libratus | Citipes elegans | A | 0 (Phase 3, unchanged) | 334 kg (194 kg–649 kg) | `ChristiansenFarina2004_FL` | 0 (Phase 3, unchanged) | 9.2 kg (5.28 kg–17.7 kg) | `ChristiansenFarina2004_FL` |
| Therrien2023_02 | Gorgosaurus libratus | Citipes elegans | A | 0 (Phase 3, unchanged) | 334 kg (194 kg–649 kg) | `ChristiansenFarina2004_FL` | 0 (Phase 3, unchanged) | 11.5 kg (6.53 kg–21.9 kg) | `ChristiansenFarina2004_FL` |
| Godoy2014_01 | Aplestosuchus sordidus | Sphagesauridae | A | 0 (Phase 3, unchanged) | 43.2 kg (33.6 kg–55.4 kg) | `Farlow2005_T3_logM_logTL_wild` | 0 (Phase 3, unchanged) | 817 g (301 g–1.06 kg) | `Farlow2005_T3_logM_logSKULL_all` |
| Wilson2010_01 | Sanajeh indicus | Titanosauria | E | 0 (Phase 3, unchanged) | 5.32 kg (5.32 kg–16.7 kg) | `FeldmanMeiri2013_T1_All_TL` | 4 | no point (216 g–2.32 kg) | `assigned_approximate` |
| DePalma2013_01 | Tyrannosaurus rex | Edmontosaurus | B | none | — | `` | 0 (Phase 3, unchanged) | 5,430 kg (4,460 kg–6,610 kg) | `species_average_literature` |
| Carpenter1998_01 | Velociraptor mongoliensis | Protoceratops andrewsi | A | 0 (Phase 3, unchanged) | 18.8 kg (10.3 kg–34.5 kg) | `Benson2014_S11_FCmeasured` | 1 | 24 kg (18 kg–34.5 kg) | `Seebacher2001_T3_Ceratopsia` |
| Carpenter1998_02 | Tyrannosaurus rex | Edmontosaurus annectens | C | none | — | `` | 0 (Phase 3, unchanged) | 4,460 kg (1,950 kg–10,000 kg) | `Campione2014_eqn7` |
| Qiu2025_01 | Huadanosaurus sinensis | Gobiconodontidae | E | 0 (Phase 3, unchanged) | 1.82 kg (991 g–3.77 kg) | `ErickTum2000_DME_bone` | 4 | no point (4.67 g–281 g) | `assigned_approximate` |
| Qiu2025_02 | Huadanosaurus sinensis | Eutheria | E | 0 (Phase 3, unchanged) | 1.82 kg (991 g–3.77 kg) | `ErickTum2000_DME_bone` | 5 | no point (4.33 g–129 g) | `assigned_approximate` |
| White2022_01 | Confractosuchus sauroktonos | Ornithopoda | A | 0 (Phase 3, unchanged) | 37.4 kg (26.6 kg–57.7 kg) | `Farlow2005_T3_logM_logSKULL_all` | 0 (Phase 3, unchanged) | 1.3 kg (1 kg–1.7 kg) | `Campione2014_eqn7` |
| Conrad2017_01 | Compsognathus longipes | Bavarisaurus | A | 0 (Phase 3, unchanged) | 1.57 kg (856 g–2.87 kg) | `Benson2014_S11_FCestimated` | 0 (Phase 3, unchanged) | 9.1 g (7.45 g–9.2 g) | `Meiri2010_T3_Sauria` |
| DalSasso2011_01 | Scipionyx samniticus | Vertebrata indet | A | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | none | — | `` |
| DalSasso2011_02 | Scipionyx samniticus | Lepidosauria | E | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | 4 | no point (1.94 g–308 g) | `assigned_approximate` |
| DalSasso2011_03 | Scipionyx samniticus | Lepidosauria | E | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | 5 | no point (0.0697 g–8.04 g) | `assigned_approximate` |
| DalSasso2011_04 | Scipionyx samniticus | Osteichthyes | A | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | none | — | `` |
| DalSasso2011_05 | Scipionyx samniticus | Clupeomorpha | A | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | 1 | 0.214 g (0.0488 g–0.648 g) | `Froese2006_T1_fusiform` |
| DalSasso2011_06 | Scipionyx samniticus | Vertebrata indet | A | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | none | — | `` |
| DalSasso2011_07 | Scipionyx samniticus | Teleostei | A | 0 (Phase 3, unchanged) | 220 g (120 g–403 g) | `Benson2014_S11_FCestimated` | 1 | 1.25 g (0.39 g–3 g) | `Froese2006_T1_fusiform` |
| Zhou2002_01 | Yanornis martini | Teleostei | E | 0 (Phase 3, unchanged) | 482 g (198 g–1.17 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_01 | Yanornis | Jinanichthys | A | 0 (Phase 3, unchanged) | 545 g (224 g–1.33 kg) | `Field2013_T1_HL` | 1 | 5.78 g (2.09 g–12.3 g) | `Froese2006_T1_fusiform` |
| Zheng2014_02 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 545 g (224 g–1.33 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_03 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 451 g (185 g–1.1 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_04 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 451 g (185 g–1.1 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_05 | Yanornis | Protopsephurus | E | 0 (Phase 3, unchanged) | 490 g (201 g–1.19 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_06 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 490 g (201 g–1.19 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_07 | Yanornis | Jinanichthys | B | 0 (Phase 3, unchanged) | 517 g (212 g–1.26 kg) | `Field2013_T1_HL` | 3 | 5.78 g (1.93 g–17.3 g) | `species_average_literature` |
| Zheng2014_08 | Yanornis | Jinanichthys | B | 0 (Phase 3, unchanged) | 380 g (46.1 g–3.14 kg) | `Field2013_T1_TaL` | 3 | 5.78 g (1.93 g–17.3 g) | `species_average_literature` |
| Zheng2014_09 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 380 g (46.1 g–3.14 kg) | `Field2013_T1_TaL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_10 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 531 g (218 g–1.29 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_11 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 545 g (224 g–1.33 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_12 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 545 g (224 g–1.33 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_13 | Yanornis | Jinanichthys | B | 0 (Phase 3, unchanged) | 438 g (180 g–1.07 kg) | `Field2013_T1_HL` | 3 | 5.78 g (1.93 g–17.3 g) | `species_average_literature` |
| Zheng2014_14 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 438 g (180 g–1.07 kg) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_15 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 378 g (155 g–919 g) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_16 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 378 g (155 g–919 g) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_17 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 389 g (160 g–948 g) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zheng2014_18 | Yanornis | Teleostei | E | 0 (Phase 3, unchanged) | 389 g (160 g–948 g) | `Field2013_T1_HL` | 4 | no point (0.359 g–98.3 g) | `assigned_approximate` |
| Zhou2009_01 | Jianchangornis microdonta | Teleostei | A | 0 (Phase 3, unchanged) | 490 g (201 g–1.19 kg) | `Field2013_T1_HL` | none | — | `` |
| Zhou2010_01 | Yabeinosaurus | Teleostei | D | none | — | `` | none | — | `` |
| Hone2022_01 | Microraptor zhaoianus | Mammalia | B | 0 (Phase 3, unchanged) | 150 g (63.3 g–256 g) | `ChristiansenFarina2004_FL` | 0 (Phase 3, unchanged) | 25.1 g (21 g–30 g) | `species_average_literature` |
| Peyer2006_01 | Compsognathus longipes | Lepidosauria | A | 0 (Phase 3, unchanged) | 2.32 kg (1.27 kg–4.26 kg) | `Benson2014_S11_FCestimated` | none | — | `` |
| Hwang2004_01 | Huaxiagnathus orientalis | Vertebrata indet | A | 0 (Phase 3, unchanged) | 20.4 kg (11.1 kg–37.3 kg) | `Benson2014_S11_FCestimated` | none | — | `` |
| Charig1997_01 | Baryonyx walkeri | Lepidotes | A | 0 (Phase 3, unchanged) | 2,580 kg (897 kg–7,400 kg) | `ErickTum2000_DME_bone` | none | — | `` |
| Charig1997_02 | Baryonyx walkeri | Iguanodon | A | 0 (Phase 3, unchanged) | 2,580 kg (897 kg–7,400 kg) | `ErickTum2000_DME_bone` | none | — | `` |

Source and basis for every value are in `db/pairs.csv` (`*_mass_basis`, `notes`) and `step3b_topup.csv`. Sources by rung: rung 1 Froese 2006 and the authors' fish lengths (DalSasso & Maganuco 2011, Zheng et al. 2014), and Seebacher 2001 with lengths from Carpenter 1998 and Varricchio et al. 2001; rung 2 Smithwick et al. 2017 SI Fig. S2C with Meiri 2010; rung 3 Chiappe et al. 2023 (*Confuciusornis*), Zheng et al. 2014 with Froese 2006 (*Jinanichthys*); rungs 4–5 as stated in each basis.

## Pairs still empty

| Pair | Side | Reason |
|---|---|---|
| Kear2006_01 | prey (Inoceramidae) | invertebrate prey (inoceramid bivalve); excluded by rule |
| Kear2006_01 | predator (Notochelone) | turtle with no equation on disk; turtles are low priority (decision 6) |
| Kear2006_02 | prey (Inoceramidae) | invertebrate prey (inoceramid bivalve); excluded by rule |
| Kear2006_02 | predator (Notochelone) | turtle with no equation on disk; turtles are low priority (decision 6) |
| Kear2006_03 | prey (Inoceramidae) | invertebrate prey (inoceramid bivalve); excluded by rule |
| Kear2006_03 | predator (Notochelone) | turtle with no equation on disk; turtles are low priority (decision 6) |
| Zheng2018_04 | prey (Ptycholepidae) | fish scales and fragments only (Ptycholepidae); no size, and no fish-scale size-to-length relationship on disk |
| Zheng2018_05 | prey (Ptycholepidae) | fish scales and fragments only (Ptycholepidae); no size, and no fish-scale size-to-length relationship on disk |
| Zheng2018_06 | prey (Ptycholepidae) | fish scales and fragments only (Ptycholepidae); no size, and no fish-scale size-to-length relationship on disk |
| Zheng2018_07 | prey (Ptycholepidae) | isolated pellet with no producer specimen; excluded by rule |
| Zheng2018_07 | predator (Anchiornis) | isolated pellet; producer unknown |
| Zheng2018_08 | prey (Ptycholepidae) | isolated pellet with no producer specimen; excluded by rule |
| Zheng2018_08 | predator (Anchiornis) | isolated pellet; producer unknown |
| Xing2012_06 | prey (Ornithischia) | one incomplete scapula (135 x 65 mm); identity 'contentious' (Psittacosaurus or Yueosaurus-like); no ornithischian scapula-to-mass equation or comparison scapula on disk |
| Nesbitt2006_01 | prey (Crocodylomorpha) | proximal femora, ilium and a sacral vertebra only; no length published; the femur is drawn at a 5 mm scale (Fig. 2) but no crocodylomorph (cf. Hesperosuchus) femur-to-mass equation or comparison mass is on disk |
| DePalma2013_01 | predator (Tyrannosaurus rex) | bite trace; excluded by rule (the prey bracket from Phase 3 stands) |
| Carpenter1998_02 | predator (Tyrannosaurus rex) | bite trace; excluded by rule (the prey estimate from Phase 3 stands) |
| DalSasso2011_01 | prey (Vertebrata indet) | indeterminate elements, probably part of another prey item; no size information |
| DalSasso2011_04 | prey (Osteichthyes) | isolated vertebra, probably part of prey 2 or 5; no size information |
| DalSasso2011_06 | prey (Vertebrata indet) | origin indeterminate |
| Zhou2009_01 | prey (Teleostei) | marked WEAK in step 1 (association only; 'no compelling evidence' of ingestion); excluded by rule |
| Zhou2010_01 | prey (Teleostei) | marked WEAK in step 1 (one-line personal observation, no specimen); excluded by rule |
| Zhou2010_01 | predator (Yabeinosaurus) | no specimen |
| Peyer2006_01 | prey (Lepidosauria) | imprints; description 'beyond the scope of this paper'; no measurement |
| Hwang2004_01 | prey (Vertebrata indet) | unidentified rounded objects averaging 7 mm; not identified as vertebrate prey |
| Charig1997_01 | prey (Lepidotes) | Lepidotes scales and teeth only; no size in the paper; no Lepidotes mass or length on disk (a genus-level figure of about 30 cm exists only on a Wikipedia mirror, not a published source) |
| Charig1997_02 | prey (Iguanodon) | 'a young Iguanodon', elements listed by block (Appendix B) and not measured; no stage-specific reference on disk; the only bound is the adult (I. bernissartensis IRSNB R51, 8.27 t), which would span four orders of magnitude |

## For JM: what to obtain

1. **Confirm the Huang et al. 2024 DOI and authors** (I inferred 10.1111/pala.12692 from a bibcode; JM supplied the Dryad zip). For *Zhangheotherium*, *Sinobaatar* and eutherians the file has nothing, so a source for symmetrodont and multituberculate masses is still needed.
2. **Seebacher 2001** is on disk. DOI 10.1671/0272-4634(2001)021[0051:ANMTCA]2.0.CO;2 comes from the BioOne URL, not checked against the paper.
3. **Meng et al. (2017)** the source of the *Eomaia* and *Yanoconodon* masses Hone et al. 2022 use. Citation not checked; Hone 2022's reference list gives it.
4. **A *Lepidotes mantelli* size** (species description or revision of Wealden semionotiform fishes; leads only: Maisey, Kriwet) and the scale-size-to-length relationship, for Charig1997_01.
5. **Serrano et al. (2015)** the confuciusornithid mass regression behind Chiappe et al.'s 0.615 kg. Citation from Chiappe 2023's text; not checked.
6. **A lizard femur-to-SVL equation** (or published SVL and femur lengths for *Dalinghosaurus* or *Liushusaurus*) and **a sauropod-hatchling length–mass equation**. I did not find sources; anything that gives femur length against SVL for Jehol lizards would turn the *Anchiornis* pellet brackets into rung 1.
7. Already on the Phase 3 list and still open: Christiansen & Fariña 2004, Benson 2014 Appendix S1, Alexander et al. 1979, Van Valkenburgh 1990.

## Files

- `code/db_migrate/10b_fossil_topup.R` (all top-up estimates, one `tp()` call per pair), sourced by `10_fossil_step2.R`.
- `data/fossil_rebuild/step3b_topup.csv`: one row per pair with rung, value, interval, method, basis and, where empty, the reason.
- `db/pairs.csv`, `db/measurements.csv` (96 fossil rows; adds the figure measurements, each marked `estimated` with "measured from the figure" in its locator), `db/methods.csv`.
- `code/db_validate.R`, `db/tests/make_fixtures.R`, `db/SCHEMA.md`: new check `bracket_basis` (a bracket with no point must say what sets its bounds) and warning `bracket_only`.
- `docs/methodology_index.csv`: Froese 2006, Chiappe 2023 and Seebacher 2001 all `on_disk`.
