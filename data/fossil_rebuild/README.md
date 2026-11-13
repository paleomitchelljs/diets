# Fossil-case rebuild

A from-scratch re-extraction of the fossil predator–prey papers in `pdfs/fossil_cases/`, done one step at a time. This folder deliberately does not reuse the older curated tables (`data/fossil_predprey.csv`, `data/fossil_diet_extractions.csv`, `data/fossil_ppmr_points.csv`). Values here come straight from the PDFs.

## Step 1: specimen and gut-content compilation (step 1 and step 1b, both 2026-11-11)

| file | one row per | rows (step 1 + step 1b) | key |
|---|---|---|---|
| `step1_papers.csv` | source | 47 (29 + 18) | `paper_id`; specimen count, original→renamed file, duplicates, supplements |
| `step1_gut_entries.csv` | predator specimen × gut item | 106 (59 + 47) | `entry_id` = `<paper_id>_<nn>`; `_00` = described specimen with no gut contents |
| `step1_measurements.csv` | single measurement (long format) | 996 (486 + 418 in step 1b + 92 from the two supplements JM added afterwards) | `paper_id`, `entry_id`, `specimen`, `role`; `value_type` = measured / estimated / preserved / derived (plus approximate and derived-from-external-source labels) |
| `step1_notes.md` | source | 46 paper sections + one supplements section (Benson 2014 is under supplements) | verbatim quotes supporting every row, plus flagged conflicts |
| `step1_rename_log.csv` | file move | 41 | original → new path (reverse with this if needed) |

`evidence_type` in the entries table separates true ingesta (`gut_content*`) from pellets, predation associations, healed bites, gastroliths, rejected claims and specimens with no gut contents (`no_gut_contents`; Daurlong has a preserved intestine but nothing in it). Filter on it before any prey-size analysis.

Three more filters are needed now.

1. Repeat observations. 13 entries carry "(same item as ...)", "(superseded by ...)" or "(identity statement only)" in `evidence_type`, and "do NOT count twice" in `notes`. They record a later paper describing a specimen and gut item that is already counted (for example Currie2001_01 = Chen1998_01; Hone2022_01 supersedes Larsson2010_01). Drop them before counting.
2. Plant diets. `gut_content_plant` covers *Minmi* (step 1) and the three seed papers added in step 1b (`Zheng2011`, `Zheng2018b`, `ZhouZhang2002`).
3. Weak records. Zhou2009_01 (*Jianchangornis*) is `gut_content (associated; uncertain)`, because the authors say there is "no compelling evidence" the fish were eaten. Zhou2010_01 (*Yabeinosaurus*) is a one-line personal observation in a review, with no specimen number.

After filters 1 and 2 there are 62 entries of animal gut contents.

Line endings differ by file: `step1_papers.csv`, `step1_gut_entries.csv` and `step1_rename_log.csv` use CRLF; `step1_measurements.csv` uses LF. Keep each file's convention when appending.

`gut_measurements_reported` records what the paper gives. `gut_measurements_possible` records what could still be measured from figures or SI.

Some measurement rows are secondary. A row whose `paper_id` is `Hone2022` or `Benson2014` but whose `entry_id` belongs to another paper is a value copied or derived by that compilation, not a new observation. `Benson2014` is the body-size dataset S11 (`data/mass_references/journal.pbio.1001853.s011.XLS`), entered only for gut-bearing specimens it names.

## Step 1b: what was added (2026-11-11)

Prompt 1b in `docs/ASSESSMENT_AND_PLAN_2026-11.md` extended step 1 to the sources that arrived during the phase 0 intake.

New papers (17): Cau 2024 (GMV 2124 identity only), Charig & Milner 1997, Currie & Chen 2001, Hone et al. 2022, Larsson et al. 2010 (abstract; kept as its own row, superseded by Hone 2022), Hwang et al. 2004, Peyer 2006, Ostrom 1978, Wang et al. 2020, Zheng et al. 2014, Zhou et al. 2002, Zhou et al. 2004, Zhou et al. 2009, Zhou & Wang 2010, Zheng et al. 2011, Zheng et al. 2018 (`Zheng2018b`, *Eogranivora*) and Zhou & Zhang 2002 (`ZhouZhang2002`). Plus `Benson2014` (S11), checked as instructed.

Supplements read (9): Nesbitt 2006 ESM, Qiu 2025 SI, O'Connor 2019 SI, Therrien 2023 SI and Data S1–S3, Han 2023 SI 1 and 2, White 2022 SI Figs S31–S34, Smithwick 2017 SI, Wang 2022 SI, Wilson 2010 Texts S1–S6. Godoy 2014 Text S1 and Hu 2005 SI were skipped because step 1 already quotes them. Kear 2006 SI stays deferred. Nothing inside a zip was run; the two zips were listed, and Data S1 was read in memory.

One file was renamed: `pdfs/reference/Ostrom_1978.pdf` → `pdfs/fossil_cases/Ostrom_1978_Zitteliana_Compsognathus_Bavarisaurus_gut.pdf` (logged). `data/intake_manifest.csv` and `docs/source_intake_log.csv` (item FP12) still point to the old path. Every other new PDF already followed the naming pattern.

Step-1 rows were not edited. That includes the `supplement_files` column of `step1_papers.csv`, which still says "not on disk" for nine papers whose supplements are now on disk and read. The list below is the current status. (Step-1b rows for Zheng2014, Wang2020 and Zhou2002 were updated when JM added the two supplements below.)

After step 1b JM added two more supplements, both read and renamed: Zheng 2014 File S1 (`Zheng_2014.pdf` → `Zheng_etal_2014_PLoSONE_Yanornis_fish_gut_SI.pdf`), whose Table S1 gives limb lengths for all ten STM9 *Yanornis* (68 rows), and the Wang 2020 Supplementary Materials (`Wang_2020_SI.doc` → `Wang_etal_2020_JSystPalaeontol_Abitusavis_fish_gut_SI.doc`), whose Table S1 gives IVPP V13259's limb lengths (humerus 75.4, femur 56.10, tibiotarsus 71.70 mm) plus *Piscivoravis* V17078, *Jianchangornis* V16708 and STM9-19 (24 rows).

What changed, in brief.

- Therrien 2023 SI supplies the missing raw values: *Gorgosaurus* TMP 2009.12.14 femur 543.3 mm (334.4 kg), posterior *Citipes* femur 191 mm (11.5 kg), anterior *Citipes* femur 178 mm estimated from a metatarsal IV/femur ratio (9.2 kg).
- O'Connor 2019 SI Table S1 gives *Indrasaurus* femur 13.4 mm and nine other elements.
- Benson 2014 S11 confirms *Sinocalliopteryx* JMP-V-05-8-01 femur 250 mm, mediolateral shaft diameter 25.5, tibia 280, humerus 125, radius 94, "measured from figures in Ji et al. 2007". S11 has no row for QM V1002, IVPP V12330, OTM 200, TMP 2009.12.14 or AMNH FR 7224.
- The "*Microraptor* mammal" is a foot in the thoracic cavity of the *M. zhaoianus* holotype IVPP V12330 (Larsson 2010; Hone 2022), not an *M. gui* bone.
- The "*Abitusavis* fish" cited by Huang 2026 is the Zhou 2002 *Yanornis* fish in IVPP V13259, which Wang 2020 refers to *Abitusavis*. Zheng 2014 adds ten new fish-bearing yanornithids (STM9 series), with crop and ventriculus contents recorded as separate entries.
- *Baryonyx* (BMNH R9951): acid-etched *Lepidotes* scales and teeth, and bones of a young *Iguanodon*, assigned to the rib-cage region by excavation block; femur only estimated (~1,200 mm).
- Decision 5 was applied throughout. Original identifications are kept in `pred_taxon`/`gut_taxon` and any reassignment is written beside them with its source (V13259, V13278, the STM9 yanornithids, STM35-3, GMV 2124, the *Compsognathus* gut lizard).

Not a fossil record, but worth having: Therrien 2023 Data S1 is a species-level table of maximum and minimum prey mass for extant crocodylians, varanids and other lizards (n = 179 reptilian predators) and about 51 mammalian predators. Crocodylia is the thinnest extant cell, so this belongs on the phase 4 extant list.

### Conflicts to resolve before step 2

Items 1–11 are from step 1 and are not resolved here. "Step 1b:" lines add what the new sources say.

**Step 2 (2026-11-12): all of these are decided in `step2_conflicts.md`.** Five remain unresolved because a file is missing: 4 (White 2022 femoral circumference), 11 (untraced old-table values), 22 (Ji et al. 2007), 25 (Ji & Ji 1997), and 28 below (Xu et al. 2003). Conflict 9 is unresolved as to meaning, but nothing depends on it. Step 2 added two conflicts:

28. ***Microraptor gui* holotype IVPP V13352 femur.** 106 mm (Benson 2014 S11, "JNC") vs 78.7* mm, estimated (O'Connor 2011 Table S1). The tibiae agree (128.64 vs 126.7). It matters for scaling QM V1002 and IVPP V17972 from the holotype.
29. ***Sinocalliopteryx* JMP-V-05-8-01 radius.** 100.7 mm (Xing 2012 Table 2) vs 94 mm (S11, from Ji 2007's figures).

1. **Conrad 2017**: *Schoenesmahl* femur/tibia. The text gives 16.9/17.3 mm and says "tibia longer than femur". Table 1 gives 18.2/16.9. The old project table used the Table 1 values. Step 1b: Ostrom 1978 Table 3 gives femur 16.8–18.0, tibia 16.9 for the same lizard (see 20).
2. **Zheng 2018**: STM0-179 pellet is 70 × 27 mm in the text but 53 × 20 mm in Table 1.
3. **Han 2023**: *Psittacosaurus* femur "minimum shaft circumference" is 17.7 mm for a 137.7 mm femur. This is implausible as a circumference. Step 1b: SI 1 does not give it; still open.
4. **White 2022**: prey mass 1.0–1.7 kg, but the tibia is 43 mm long with 10 mm circumference. Step 1b: SI Figs S31–S34 are 3D models with captions only ("Left femur" etc.), no measurements. The input would have to be measured from the model. Still open.
5. **GMV 2124**: Hurum 2006 calls it *Sinosauropteryx* (3 mammal jaws). Qiu 2025, citing Cau 2024, calls it a small tyrannosauroid ("NGMC 2124"). Step 1b: Currie & Chen 2001 also refer NGMC 2124 to *S. prima* ("symmetrodont mammal jaws and skeletal elements in the stomach region", after Ji & Ji 1997b). Cau 2024's basis is Longrich 2002 (an abstract, "represents a distinct taxon") plus Cau's own analysis placing it "among the earliest branches of Tyrannosauroidea". So the original ID has three papers and the reassignment one (Qiu follows Cau by citation). Kept as *S. prima* under decision 5.
6. **Chen 1998 "eggs"**: the authors read them as the predator's own unlaid eggs. Wang 2022 reads them as intestinal remnants. They are not prey under either reading. Step 1b: Currie & Chen 2001 keep them as eggs ("no doubt that they were within the body cavity"); Cau 2024 (p.11) sides with Wang 2022.
7. **Hu 2005 prey age**: Hu says "not from an embryo" (worn teeth). Han 2023 calls it "perinatal".
8. **Godoy 2014**: prey body length is ~55 cm in Text S1 but ~60 cm in the main text.
9. **Kear 2006**: "maximum carapace lengths ~50–60 mm" is ambiguous. It is too small for a <1 m turtle.
10. **Specimen-number typos**: O'Connor 2011 Table S1 prints "IVPP V13972" for V17972. O'Connor 2019 cites "V17992". Huang 2026 Table 2 prints Piscivoravis as "IVPP V17.78". Step 1b: Hone 2022 Table 2 repeats "IVPP V13972A". More in 13.
11. **Values in the old project tables that are not in these papers' main text**: QM V1002 radius/tibia/humerus (Xing 2013) and the *Hesperosuchus* prey femur 71.2 mm (Nesbitt 2006). Step 1b: the Nesbitt ESM has no prey femur length; Xing 2013 Fig. S1 is a skull photograph (JM's check; the file is not on disk); S11 has no QM V1002 row. Both values remain untraced.
12. **Huang 2026 Table 2 prints IVPP V12558 for both *Yixianornis* and *Yanornis*.** Zhou 2002, Zhou 2004 and Wang 2020 all give V12558 as the *Yanornis martini* holotype, and Wang 2020 Table 1's V12558 values equal Huang's *Yanornis* column. So the *Yixianornis* column is the likely typo. Wang 2020 SI Table S1 lists *Yixianornis* as IVPP V12631, which supports that. The step-1 note on Huang2026 points at the wrong column; the step-1 *Yanornis* rows ("IVPP 'V12558' as printed") are correctly numbered.
13. **Typos and printing errors found in step 1b.** Zheng 2014 text prints the gastrolith *Yanornis* as "IVPP V13558" three times (its Table 1, Zhou 2004 and Wang 2020: V13358), and Fig. 3D prints "SMT9-18". Therrien 2023 SI Table S2 prints "TMP 2009.14.12" for TMP 2009.12.14, and gives the Christiansen & Fariña equation as "body mass (in kg) = -6.288 + 3.22*LOG(femur length)" (the tabulated masses need log10 mass on the left). Hone 2022 calls a pedal element "metacarpal". Currie & Chen 2001 cite "Ji and Ji 1997a" for the third *Sinosauropteryx* specimen but "Ji and Ji (1997b)" for NGMC 2124 (see 25). Wang 2020's main text calls its limb-length table "Supplemental Table S2" (21 specimens); in the SI it is Table S1 (20 rows).
14. **Yanornithid and *Hongshanornis* identities (decision 5).** IVPP V13259: *Yanornis martini* (Zhou 2002, 2004; Zheng 2014) vs *Abitusavis lii* (Wang 2020; Huang 2026 by citation). IVPP V13278: *Y. martini* (Zhou 2004) vs *Similiyanornis brevipectus* (Wang 2020; Huang 2026 Table 2). STM9-15 to STM9-52: *Yanornis* sp. (Zheng 2014) vs Yanornithidae indet. (Wang 2020). STM35-3: *Hongshanornis longicresta* (Zheng 2011) vs *Eogranivora edentulata* (Zheng 2018b). Each reassignment rests on one paper; original IDs kept.
15. **IVPP V13358 grit.** Gizzard gastroliths, a sign of diet switching (Zhou 2004), vs sand impacted in the intestine (Zheng 2014). Not prey either way.
16. ***Microraptor* mammal host.** O'Connor 2011 (ref 26): "a small mammal bone associated with the holotype of M. gui". Larsson 2010 and Hone 2022: the holotype of *M. zhaoianus*, IVPP V12330. The step-1 README repeated O'Connor's version. O'Connor cites the abstract as 30:114A; the abstract book prints 120A.
17. **IVPP V12330 size.** Femur 49.8 mm (Hone 2022 Table 2) vs 53 mm (Zhou & Zhang 2002 Table 1; O'Connor 2011 Table S1). Predator mass 150 g (Hone, from femur) vs 100–150 g (Larsson). Mammal 21–30 g (Hone) vs 20–25 g (Larsson).
18. ***Compsognathus* holotype femur, four values.** 67 mm estimated (Ostrom 1978 Tables 1–2; Peyer 2006 cites 67), 71 (Chen 1998 Table 1, attributed to Ostrom), 71.5 (Benson 2014 S11, "BSP ASI 563 (MTC); Ostrom 1978"), 65.8 (Hone 2022, from Dececchi et al. 2020b).
19. ***Compsognathus* holotype total length.** Ostrom 1978 (p.74): "no more than 70 to 75 cm from snout tip to tail tip". Conrad 2017 (p.46): "approximately 890 mm (Ostrom, 1978)". The step-1 row (Conrad2017, 890 mm) is labelled measured.
20. ***Compsognathus* gut lizard.** Identity: *Bavarisaurus* cf. *macrodactylus* (Ostrom 1978, the first taxonomic ID) vs *Schoenesmahl dyspepsia* (Conrad 2017, new taxon). Sizes: Ostrom Table 3 humerus 14.0, femur 16.8–18.0, tibia 16.9 vs Conrad text 13.5/16.9/17.3 and Table 1 13.5/18.2/16.9. Body length 46 mm (Ostrom text) vs 47 est. (Ostrom Table 3). The *Bavarisaurus* type B.S.P. 1873 III 501 also differs: femur 21.15, tibia 19.2 (Ostrom) vs 21.9 or 22, 18.7 or 18.1 (Conrad).
21. ***Compsognathus* MNHN CNJ 79.** Humerus 51.9 R / 56.3 L (Peyer 2006 Table 1) vs 67 est. (Ostrom 1978 Table 2; also Chen 1998 Table 1). Femur 108.8 (Peyer) vs 110 (Ostrom; Chen; S11).
22. ***Sinocalliopteryx* JMP-V-05-8-01 femur.** 250 mm (S11, measured from Ji 2007's figures) vs 210 mm (Hone 2022 Table 1, from Dececchi 2020b). Both are secondary; Ji 2007 is not on disk.
23. **Hone 2022 Table 1 femora are taxon-level, not specimen values.** Matched against S11: *Coelophysis* 209 = AMNH 7223 (the gut-bearing AMNH FR 7224 is 203 per Nesbitt 2006); *Daspletosaurus* 1030 = AMNH 5438 (OTM 200 has no limb bones); *Tyrannosaurus* 1321 = FMNH PR 2081 ("Sue"). *Huaxiagnathus* 163 (Hone) vs 164 (S11, "Measured from Hwang et al. (2004)"); Hwang 2004 itself gives only ratios. Do not use Hone Table 1 as specimen measurements.
24. **Qiu 2025 SI, internal.** *Huadanosaurus* IVPP V 14202 humerus 94.91 (l) / 93.75 (r) in Table S2, but the SI text puts the radius at "about 64.2%" and Mc II at "approximately 37.7%" of the humerus, which with radius 33.94 and Mc II 20.77 implies about 53–55 mm. *S. lingyuanensis* IVPP V 12415 tibia 140.94 (l) vs 112.15 (r); the text ratio ("femur is approximately 90% of the length of the tibia") fits 112. Its metatarsus III is 70.10/71.63 in Table S1 and 65.38 in Table S3.
25. **GMV 2124 femur 108 mm, attribution by elimination.** Currie & Chen 2001 give femur 108 mm for "the third specimen of Sinosauropteryx (Ji and Ji 1997a)". The only other referred specimen they list is NGMC 2124, so the value is entered against it with that inference stated. Hone 2022 ("108#") and Peyer 2006 (largest *Sinosauropteryx*, 108 mm) agree on the number but not the source. Check against Ji & Ji 1997 if it is ever obtained.
26. **STM35-3 seeds and gastroliths (plant diet; low priority).** Zheng 2011: >20 seeds, ~4 × 2 mm; >50 gastroliths, ~1 mm. Zheng 2018b: ≥15 seeds in the counterslab, 4.3 × 2.6 mm; 12 + ~100 gastroliths, 1–2 mm. Probably different counting (by slab), not a real disagreement.
27. ***Jianchangornis* IVPP V16708 limb lengths.** Zhou 2009 Table 1: humerus 76 (l) / 75 (r), ulna 83 / 82, femur 60 / 59, tibiotarsus 76 / 75, tarsometatarsus 37 (l). Wang 2020 SI Table S1: 75.4, 83.1, 58.4, 75.2, 37.6. The femur differs by 0.6–1.6 mm. Huang 2026 Table 2 follows Zhou (60).

### Supplementary files that hold needed measurements

From the step-1 list.

| supplement | status | what it gave |
|---|---|---|
| Therrien 2023 SI (+ Data S1–S3) | on disk, read | femur lengths, *Citipes* bones, mass method (Table S2, S4) |
| O'Connor 2019 Table S1 | on disk, read | *Indrasaurus* element lengths |
| Qiu 2025 Tables S1–S3 | on disk, read | full limb lengths for V 14202 and V 12415 (see conflict 24) |
| White 2022 SI Figs S31–S34 | on disk, read (text only) | captions only; no measurements |
| Nesbitt 2006 ESM | on disk, read | AMNH FR 7223 hindlimb "62%"; no prey femur |
| Han 2023 SI 1 and 2 | on disk, read | method-by-method mammal masses; no *Psittacosaurus* femur circumference |
| Xing 2013 Fig. S1 | still missing; not needed | skull photograph per JM |
| Smithwick 2017 SI | on disk, read | third specimen = IVPP V12415; Fig. S2 shows the stomach lizard (20 mm scale), no values |
| Wang 2022 SI | on disk, read | *Daurlong* limb lengths |
| Wilson 2010 Texts S1–S6 | on disk, read | block list; total length 3.46 m; egg mass 2,316 g. Figs S1–S12 not on disk, not needed |
| Kear 2006 SI | still missing; deferred (turtles low priority) | |

New in step 1b.

| supplement | status | why it matters |
|---|---|---|
| Zheng 2014 File S1 | on disk (JM, after step 1b), read | Table S1 limb lengths for all ten STM9 *Yanornis* (asterisked values estimated; STM9-31 has only skull 60 and tarsometatarsus 52*, STM9-26 only skull, humerus, ulna) |
| Wang 2020 Supplementary Materials | on disk (JM, after step 1b), read | Table S1 (the main text's "Table S2") has IVPP V13259 limb lengths; Fig. S7 is an unlabelled photograph of V13259, fish not marked |
| Zhou 2002 Supplementary information | missing; low priority | measurements of the 'Archaeoraptor' avialan part and the *Yanornis* holotype, probably not of V13259 |
| Zheng 2018b ESM table S1 | missing; low priority | *Eogranivora* measurements (plant diet) |

### Gut records cited in these papers but not in the folder

From the step-1 list.

- *Yanornis* fish (Zhou et al. 2002/2004): on disk (`Zhou2002`, `Zhou2004`).
- *Abitusavis* fish: on disk (`Wang2020`); it is the same fish in the same specimen as `Zhou2002`.
- *Jianchangornis* fish (Zhou et al. 2009; possibly chimaeric): on disk (`Zhou2009`). The chimaera suggestion (O'Connor 2019, cited by Huang 2026; not the *Microraptor* paper) is still missing.
- *Hongshanornis*/*Sapeornis* seeds (Zheng et al. 2011): on disk.
- *Jeholornis* seeds (Zhou & Zhang 2002): on disk.
- *Microraptor* mammal (Larsson et al. 2010): on disk, with Hone et al. 2022. Host is the *M. zhaoianus* holotype (conflict 16).
- *Sinosauropteryx* (Currie & Chen 2001): on disk.
- *Compsognathus* (Ostrom 1978; French specimen, Peyer 2006): on disk.
- *Sinocalliopteryx* original (Ji et al. 2007): still missing (not found; optional now that S11 gives the femur).
- *Huaxiagnathus* (Hwang et al. 2004): on disk.
- *Baryonyx* (Charig & Milner 1997): on disk.
- *Yabeinosaurus* fish: `Zhou2010` is on disk but is only "(Y. Wang, pers. observ.)" in a review. The primary description, if one exists, is still missing and was not identified.
- Cau 2024 (GMV 2124): on disk.

Found in step 1b, all still missing.

- Ji & Ji 1997b: primary report of the GMV 2124 mammal jaws (and source of the 108 mm femur, conflict 25).
- Ackerman 1998: the earlier "two jaws" report for GMV 2124, cited by Hurum 2006.
- Bidar et al. 1972a, b; Michard 1991: primary reports of the gut contents of *Compsognathus* MNHN CNJ 79, which Peyer 2006 mentions but does not describe ("sphenodontids or lizards", several individuals).
- Longrich 2002: the abstract behind "distinct taxon" for GMV 2124 (identity only).
- Wang et al. 2019: *Ambopteryx*, "Indeterminate bone" (Hone 2022 Table 1).
- Hone et al. 2012: *Velociraptor* with an azhdarchid pterosaur bone (Hone 2022 Table 1).
- Chin et al. 1998: *Tyrannosaurus* coprolite with ornithischian bone (coprolite; producer attribution needed).
- Freimuth et al. 2021: mammal-bearing pellets "tentatively referred ... to Troodon" (Hone 2022), and the source of the 93 g estimate for the *Anchiornis* STM0-224 pellet.
- Zhou & Zhang 2003b: gizzard stones in *Sapeornis* IVPP V13275 (gastroliths only; low priority).

## Step 2: masses, conflicts and db rows (2026-11-12)

Phase 3 of `docs/ASSESSMENT_AND_PLAN_2026-11.md`. Run `Rscript code/db_migrate/10_fossil_step2.R`. It is idempotent and replaces only the `db/` rows whose sources it owns (`extracted_from` starting `data/fossil_rebuild/`).

| file | content |
|---|---|
| `step2_conflicts.md` | decision and evidence for conflicts 1–29 |
| `step2_mass_estimates.md` | every estimate with its equation, input and interval; entries without masses and why; the table of differences of more than 25% from the old fossil tables; method papers JM needs to supply |
| `step2_estimates.csv` | one row per counted entry: point, interval, method, basis, cross-check |
| `step2_crosschecks.csv` | independent estimates that are not in `db/` |
| `step2_old_table_comparison.csv` | every comparison with `fossil_ppmr_points.csv`, `fossil_predprey.csv`, `fossil_diet_extractions.csv` |
| `../../db/excluded_fossil_records.csv` | the 32 entries not in `db/pairs.csv`, with reasons |

Counts: 74 pairs (61 stomach content, 8 pellet, 3 combat association, 2 bite trace); 14 with both masses. Original identifications are kept throughout (decision 5).
