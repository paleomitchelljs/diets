# Step 2: body-mass estimates for the fossil gut entries

Written 2026-11-12 for Phase 3 (Prompt 3 in `docs/ASSESSMENT_AND_PLAN_2026-11.md`). Conflict decisions are in `step2_conflicts.md`. Everything here is produced by `code/db_migrate/10_fossil_step2.R`, which also writes the fossil rows of `db/`. Re-running it replaces only its own rows.

## Summary

- 106 step-1 entries: **74 are in `db/pairs.csv`**; 32 are in `db/excluded_fossil_records.csv`: 9 repeat observations, 8 gastrolith or sediment records, 6 plant diets, 4 specimens without contents, 2 claims the source rejects, 2 rows for the *Sinosauropteryx* "eggs", and the *Sinocalliopteryx* feather mass, which probably belongs to the counted *Sinornithosaurus* prey. Four of the gastrolith, plant and egg rows are also repeats; the `counted_as` column says which entry each repeat duplicates.
- Observation types: 61 `stomach_content`, 8 `pellet`, 3 `combat_association`, 2 `bite_trace`.
- Masses: **14 pairs have both masses**, 49 have a predator mass only, 4 a prey mass only, and 7 neither. Most gaps are small prey with no published measurement (fish in birds, lizards in pellets, mammal jaws).
- Tiers: 67 A, 3 B, 3 C, 1 D. Tier A here means both masses would come from the specimens. Many tier A rows have no mass yet, because the schema lets a missing mass leave the tier unchanged. **Filter on both masses being present before any ratio analysis.**
- Five method rows are marked `verified = FALSE`, and 25 pair-sides rely on them (validator warnings). The reasons: Christiansen & Fariña 2004, Alexander et al. 1979, Van Valkenburgh 1990 and Froese 2006 are not on disk; Benson et al. 2014's diameter-to-circumference step is in Appendix S1, which is not on disk; and Hone et al. do not say which Meiri equation they used. These are listed for JM at the end.
- `Rscript code/db_validate.R`: **PASS** (0 errors). Warnings: 25 `unverified_method` on fossil rows, 3 `index_status` (NEEDED method papers), and Phase 2's existing `unknown`-method rows.

The 14 pairs with both masses, as log10(predator/prey):

| Pair | Predator | Prey | Predator mass | Prey mass | log10 PPMR |
|---|---|---|---|---|---|
| Han2023_01 (combat) | *Repenomamus* | *Psittacosaurus lujiatunensis* | 3.43 kg | 10.6 kg | -0.49 |
| Hone2022_01 | *Microraptor zhaoianus* | mammal | 150 g | 25 g | 0.78 |
| OConnor2011_01 | *Microraptor gui* | enantiornithine | 760 g | 65 g | 1.07 |
| Xing2012_01 | *Sinocalliopteryx* | *Sinornithosaurus* | 32.3 kg | 2.62 kg | 1.09 |
| White2022_01 | *Confractosuchus* | ornithopod | 37.4 kg | 1.30 kg | 1.46 (conflict 4) |
| Therrien2023_02 | *Gorgosaurus* | *Citipes* | 334 kg | 11.5 kg | 1.46 |
| Therrien2023_01 | *Gorgosaurus* | *Citipes* | 334 kg | 9.2 kg | 1.56 |
| Godoy2014_01 | *Aplestosuchus* | sphagesaurid | 43.2 kg | 817 g | 1.72 |
| Hu2005_01 | *Repenomamus* | *Psittacosaurus* | 4.90 kg | 83 g | 1.77 |
| OConnor2019_01 | *Microraptor zhaoianus* | *Indrasaurus* | 570 g | 8.5 g | 1.83 |
| Zheng2014_01 | *Yanornis* STM9-15 | *Jinanichthys* | 545 g | 4.2 g | 2.11 (prey unverified) |
| Conrad2017_01 | *Compsognathus* | *Bavarisaurus* | 1.57 kg | 9.1 g | 2.24 |
| DalSasso2011_07 | *Scipionyx* | teleost | 220 g | 0.91 g | 2.38 (prey unverified) |
| DalSasso2011_05 | *Scipionyx* | clupeomorph | 220 g | 0.16 g | 3.15 (prey unverified) |

## Rules applied

**Order of preference** (from the prompt):

1. A mass the authors report for this specimen, with their method.
2. A limb-bone circumference or length equation for the clade.
3. A length-based equation.
4. Scaling from a named comparison specimen.

I read "the authors" as any published estimate made for this specimen with a stated method. That covers Hone et al. 2022 for the *Microraptor* specimens, which they did not describe. A generic literature value for the taxon (O'Connor et al. 2011's "1500 g" for *Microraptor*) is not specimen-specific and is used only as a cross-check.

**Benson et al. 2014 Dataset S1 (S11)** is the main on-disk source of specimen masses for theropods. Its values are Campione & Evans 2012 eqn 1 (OLS) with the Campione et al. 2014 biped correction α = √2. I checked this by reproducing S11 from measured circumferences: AMNH 7223, FC 49 mm, gives 9.04 kg; MPD-C 100/25, FC 64 mm, gives 18.84 kg. Where S11 says "FC estimated", the circumference came from a shaft diameter through regressions in Appendix S1, which is not on disk. Those rows use `Benson2014_S11_FCestimated` (`verified = FALSE`).

**Scaling (DME).** Erickson & Tumanova 2000 developmental mass extrapolation: mass = reference mass × (L / L_ref)³. The authors scaled femur length within one species. Using another element, or a reference of a related species, is an extension of the method. Every such use names its element and its reference in `*_mass_basis`. The reference masses come from S11. Where S11 has a specimen with a *measured* circumference, I preferred it.

**Intervals.** What an interval means depends on the method, and each row's basis says which applies:

- Campione-family masses (S11, Campione 2014 eqn 7): point ×/÷ 10^(1.96 × 0.134) = ×/÷ 1.83, the 95% range from the SEE of Campione & Evans 2012 eqn 1 (Fig. 4A, SEE = 0.134). Benson et al. 2014 Table 1 states 0.135.
- Field et al. 2013: their published 95% prediction interval (Table 1).
- Farlow et al. 2005: ±1.96 SEE from Table 3.
- Authors' values: the authors' range or ±. Where the authors give none (Hone 2022, Therrien 2023), the interval is the envelope of the authors' value and the 95% range of a verified cross-check.
- Scaling from several references or elements: from the lowest estimate's lower bound to the highest estimate's upper bound.
- Meiri 2010, Feldman & Meiri 2013, and the fish cube law publish no prediction error usable here, so those brackets cover the input or the choice of equation only. Say so if they are used as error bars.

**Ages.** The paper's own number where it gives one (for example "ca 93 Ma", "~75.3 Ma", "210 Myr"). For the Yixian and Jiufotang formations, which most Jehol papers name without a number: 125 and 120 Ma, as Hwang 2004, Qiu 2025, Zhou 2009 and 2014, and O'Connor 2019 state. For stage names only (Albian, Campanian, Oxfordian, Barremian, Tithonian, Maastrichtian): the midpoint of the ICS stage, with the stage bounds as `age_ma_lo`/`age_ma_hi`. **The ICS chart is not on disk; I used the 2023 chart values from memory, and each row's notes say so.** This affects ten sources.

**Decision 5.** Identifications stay as the original describers gave them. Where identity changes the reference specimen (GMV 2124), the mass is given under both.

## Methods registered in `db/methods.csv`

| method_id | Kind | Equation | File | Verified |
|---|---|---|---|---|
| `Benson2014_S11_FCmeasured` | bone | S11 mass; log10 BM = 2.749 log10(√2·C_femur) − 1.104 | Benson 2014 (on disk) | TRUE |
| `Benson2014_S11_FCestimated` | bone | as above, circumference from a diameter (Appendix S1) | Benson 2014 (Appendix S1 NEEDED) | FALSE |
| `Campione2014_eqn7` | bone | log10 BM = 2.754 log10(C_femur) − 0.683 (g, mm) | Campione 2014 p. 915, rendered | TRUE |
| `CampioneEvans2012_eqn1` | bone | log10 BM = 2.749 log10(CH + CF) − 1.104 (cross-check only) | Campione & Evans 2012 | TRUE |
| `ErickTum2000_DME_bone` | bone | BM = BM_ref × (L/L_ref)³ | Erickson & Tumanova 2000 (in `pdfs/fossil_cases/`) | TRUE |
| `ErickTum2000_DME_bodylength` | length | same, with body length | same | TRUE |
| `Field2013_T1_HL` | bone | ln BM = 2.07 ln HL − 2.77; 95% PI ±0.89 | Field 2013 Table 1, rendered | TRUE |
| `Field2013_T1_FL` | bone | ln BM = 2.82 ln FL − 4.74 (cross-check) | same | TRUE |
| `Field2013_T1_TaL` | bone | ln BM = 2.23 ln TaL − 2.87; PI ±2.11 | same | TRUE |
| `Farlow2005_T3_logM_logTL_wild` | length | log10 M(kg) = 3.47 log10 TL(mm) − 10.03; SEE 0.0553 | Farlow 2005 p. 359, page image | TRUE |
| `Farlow2005_T3_logM_logSKULL_all` | bone | log10 M(kg) = 3.48 log10 SKULL(mm) − 6.97; SEE 0.0581 | same (OCR misreads the slope as 3.4) | TRUE |
| `FeldmanMeiri2013_T1_All_TL` | length | log10 M = −5.465 + 2.597 log10 TL | Feldman & Meiri 2013 Table 1, rendered | TRUE |
| `FeldmanMeiri2013_T1_Boidae_TL` | length | log10 M = −5.886 + 2.856 log10 TL (bracket) | same | TRUE |
| `FeldmanMeiri2013_T1_Pythonidae_TL` | length | log10 M = −5.131 + 2.611 log10 TL (bracket) | same | TRUE |
| `Meiri2010_T3_Sauria` | length | log10 M = −4.717 + 3.027 log10 SVL (copied from the Phase 2 registry) | Meiri 2010 | TRUE |
| `ChristiansenFarina2004_FL` | bone | log10 BM(kg) = 3.22 log10 FL − 6.288 (as printed by Therrien 2023 SI) | NEEDED | FALSE |
| `Hu2005_SI_Alexander1979_VanValkenburgh1990` | bone | Hu 2005 SI equations (limb diameters; skull and head-body length) | NEEDED (two papers) | FALSE |
| `Han2023_DME_CampioneEvans2012` | bone | DME from IVPP V12728 at 5.54 kg | Campione & Evans 2012 | TRUE (value read, not recomputable) |
| `Han2023_DME_Campione2014` | bone | DME from D2591 at 23.5 kg (reference reproduced exactly) | Campione 2014 | TRUE (DME step not recomputable) |
| `Hone2022_Meiri2010_unstated` | length | a Meiri 2010 equation; which one, and the SVL, not stated | Meiri 2010 | FALSE |
| `Froese2006_cube_generic` | length | W = a L³, a = 0.01 (0.005–0.02) g cm⁻³, ASSUMED | NEEDED; citation unverified | FALSE |

Five rows were added to `docs/methodology_index.csv` with status `NEEDED`: Christiansen & Fariña 2004, Alexander et al. 1979, Van Valkenburgh 1990, Froese 2006, and Benson et al. 2014 Appendix S1.

## Predator estimates

One row per predator specimen. Mass in g unless marked kg. "Cross-checks" are independent estimates that are not in `db/` but bear on the value; `step2_crosschecks.csv` lists them as numbers.


| Predator specimen | Entries | Point | Interval | Method | Input and basis | Cross-checks |
|---|---|---|---|---|---|---|
| IVPP V13605 (*Repenomamus robustus*) | Hu2005_01 | 4.9 kg | 4 kg-6 kg | `Hu2005_SI_Alexander1979_VanValkenburgh1990` | authors' range: 'we consider the body mass to be ... 4-6 kg for R. robustus' (Hu et al. 2005 SI I); point = geometric midpoint; interval = authors' range | C&E 2012 eqn 1 on V13605 midshaft circumferences (humerus 28 + femur 26 mm) = 4.55 kg (95% 2.49 kg-8.34 kg); Hu SI: limb-diameter mean 4.13 kg, skull/HBL mean 3.85 kg; Han 2023 SI: HBL equation 2.08 kg |
| WZSSM VF000011 (mammal) (*Repenomamus robustus*) | Han2023_01 | 3.43 kg | 2.01 kg-4.85 kg | `Han2023_DME_CampioneEvans2012` | authors: 'a mass of 3.43 +/- 1.42 kg for the mammal' (Han et al. 2023 p.3; SI Table S1, stylopodial-circumference DME); interval = authors' +/- (undefined in the paper) | Han Table 1 'minimum shaft circumference' values (humerus 11.4, femur 7.3 mm) are not usable as circumferences (conflict 3), so no independent check |
| NIGP 127587 (*Sinosauropteryx prima*) | Chen1998_01 | 992 g | 542 g-1.82 kg | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for NIGP 127587 (femur 86.4 mm, FAP 8.2 mm; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) |  |
| CAGS GMV 2124 (*Sinosauropteryx prima*) | Hurum2006_01, Hurum2006_02 | 1.94 kg | 1.06 kg-4.06 kg | `ErickTum2000_DME_bone` | DME by femur length 108 mm (Currie & Chen 2001; attribution to NGMC 2124 by elimination, conflict 25) from Sinosauropteryx NIGP 127587 (S11 0.992 kg, femur 86.4 mm; FC estimated); interval = lower reference bound to upper bound of the alternative identification | decision 5 alternative (Cau 2024: early tyrannosauroid): DME from Dilong IVPP V14243 (S11 10.1 kg, femur 179 mm, juvenile) = 2.22 kg |
| IVPP V17078 (*Piscivoravis lii*) | Zhou2014_01, Zhou2014_02 | 464 g | 190 g-1.13 kg | `Field2013_T1_HL` | humerus length 74 mm (Zhou2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 56 mm) = 744 g |
| AGB 15483 (*Kunpengornis anhuimusei*) | Huang2026_01 | 587 g | 241 g-1.43 kg | `Field2013_T1_HL` | humerus length 82.93 mm (Huang2026); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 58.25 mm) = 831 g |
| QM V1002 (*Microraptor gui*) | Xing2013_01 | 1.89 kg | 891 g-2.98 kg | `ChristiansenFarina2004_FL` | Hone et al. 2022 Table 2: femur 109 mm -> 1890 g (Christiansen & Farina 2004); interval = envelope of this value and the 95% range of the DME cross-check (1,630 g) | DME by femur from M. gui IVPP V13352 (S11 1,500 g, femur 106 mm) = 1,630 g; O'Connor 2011 Table S1 gives V13352 femur 78.7* mm, which would give 3,980 g (conflict 28) |
| IVPP V17972 (*Microraptor gui*) | OConnor2011_01 | 760 g | 760 g-3.57 kg | `ChristiansenFarina2004_FL` | Hone et al. 2022 Table 2: femur 82.3 mm (estimated, O'Connor 2011 Table S1 slab B) -> 760 g (Christiansen & Farina 2004); interval = envelope of this value and the 95% range of the tibia DME cross-check (1,950 g) | DME by tibia 140.4 mm (measured) from IVPP V13352 (S11 1,500 g, tibia 128.6 mm; O'Connor gives 126.7) = 1,950 g; by femur 82.3* = 702 g; O'Connor 2011 cites a generic 1500 g for Microraptor (Turner et al. 2007), not this specimen |
| STM5-32 (*Microraptor zhaoianus*) | OConnor2019_01 | 570 g | 216 g-725 g | `ChristiansenFarina2004_FL` | Hone et al. 2022 Table 2: femur 'est. 75' mm -> 570 g (Christiansen & Farina 2004); interval = envelope with the 95% range of the DME cross-check (396 g) | DME from M. zhaoianus CAGS 20-7-004 (S11 396 g, femur 75 mm, FC estimated) = 396 g |
| STM0-179 (*Anchiornis*) | Zheng2018_01, Zheng2018_02, Zheng2018_03 | 863 g | 362 g-1.58 kg | `ErickTum2000_DME_bone` | DME by femur 71 mm (Zheng 2018 Table 1) from Anchiornis LPM-B00169 (S11 700 g, femur 66.2 mm; FAP 'measured from figure', FC estimated); interval = 95% range across this and the Mei long reference | DME from troodontid Mei long IVPP V12733 (S11 846 g, femur 77 mm, measured FC) = 663 g |
| STM0-224 (*Anchiornis*) | Zheng2018_04 | 827 g | 347 g-1.51 kg | `ErickTum2000_DME_bone` | DME by femur 70 mm (Zheng 2018 Table 1) from Anchiornis LPM-B00169 (S11 700 g, femur 66.2 mm; FAP 'measured from figure', FC estimated); interval = 95% range across this and the Mei long reference | DME from troodontid Mei long IVPP V12733 (S11 846 g, femur 77 mm, measured FC) = 636 g |
| STMA0-4 (*Anchiornis*) | Zheng2018_05 | 103 g | 43.4 g-189 g | `ErickTum2000_DME_bone` | DME by femur 35 mm (Zheng 2018 Table 1) from Anchiornis LPM-B00169 (S11 700 g, femur 66.2 mm; FAP 'measured from figure', FC estimated); interval = 95% range across this and the Mei long reference | DME from troodontid Mei long IVPP V12733 (S11 846 g, femur 77 mm, measured FC) = 79.5 g |
| STM0-38 (*Anchiornis*) | Zheng2018_06 | 160 g | 67.2 g-293 g | `ErickTum2000_DME_bone` | DME by femur 40.5 mm (Zheng 2018 Table 1) from Anchiornis LPM-B00169 (S11 700 g, femur 66.2 mm; FAP 'measured from figure', FC estimated); interval = 95% range across this and the Mei long reference | DME from troodontid Mei long IVPP V12733 (S11 846 g, femur 77 mm, measured FC) = 123 g |
| JMP-V-05-8-01 (*Sinocalliopteryx gigas*) | Xing2012_01 | 32.3 kg | 17.7 kg-59.2 kg | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for JMP-V-05-8-01 (femur 250 mm and ML shaft diameter 25.5 mm 'measured from figures in Ji et al. 2007'; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) | femur length is disputed (S11 250 mm vs Hone 2022 210 mm, conflict 22) but the S11 mass rests on the shaft diameter, not on the length |
| CAGS-IG-T1 (*Sinocalliopteryx gigas*) | Xing2012_04, Xing2012_05, Xing2012_06 | 58.2 kg | 22.9 kg-163 kg | `ErickTum2000_DME_bone` | no femur; DME from the holotype JMP-V-05-8-01 (S11 32.3 kg) by three Xing 2012 Table 2 ratios: metatarsal III 206.3/147.3 -> 88.8 kg, radius 118.64/100.7 -> 52.8 kg, maxillary height 44.5/40.8 -> 41.9 kg; point = geometric mean; interval = 95% range across the three | Xing et al. note that metatarsals grow faster than the radius or maxilla, so the element ratios disagree; Hone 2022 'est. 290' mm femur |
| AMNH FR 7224 (*Coelophysis bauri*) | Nesbitt2006_01 | 8.29 kg | 4.39 kg-15.2 kg | `ErickTum2000_DME_bone` | DME by femur 203 mm (Nesbitt 2006) from AMNH 7223 (S11 9.04 kg, femur 209 mm, measured FC; same quarry); interval = 95% range across AMNH 7223 and AMNH 2704 | DME from AMNH 2704 (S11 9.82 kg, femur 217 mm) = 8.04 kg; Hone 2022 Table 1 femur 209 belongs to AMNH 7223 (conflict 23) |
| TMP 2009.12.14 (*Gorgosaurus libratus*) | Therrien2023_01, Therrien2023_02 | 334 kg | 194 kg-649 kg | `ChristiansenFarina2004_FL` | authors: femur 543.3 mm -> 334.4 kg (Therrien et al. 2023 SI Table S2, Christiansen & Farina 2004); interval = envelope with the 95% range of the DME cross-check (355 kg) | DME from Gorgosaurus NMC 2120 (S11 2,490 kg, femur 1040 mm, measured FC) = 355 kg; printed C&F equation gives 330 kg |
| LPRP/USP 0229a (*Aplestosuchus sordidus*) | Godoy2014_01 | 43.2 kg | 33.6 kg-55.4 kg | `Farlow2005_T3_logM_logTL_wild` | total length 2300 mm estimated by the authors 'based on Baurusuchus salgadoensis' (Text S1; preserved 1100 mm); Farlow 2005 alligator TL equation; interval = 95% (SEE 0.0553); input is itself an estimate and a baurusuchid is not an alligator |  |
| GSI/GC/2901-2906 (*Sanajeh indicus*) | Wilson2010_01 | 5.32 kg | 5.32 kg-16.7 kg | `FeldmanMeiri2013_T1_All_TL` | total length 3460 mm (Wilson 2010 Text S3, from skull length 95 mm by the authors' regression y = 34.2x + 215.4); Feldman & Meiri 2013 all-snake TL equation (madtsoiids belong to no extant family); interval = range across the All, Boidae and Pythonidae TL equations (no prediction error is published) | Boidae TL = 16.7 kg; Pythonidae TL = 12.9 kg |
| GI SPS 100/25 (*Velociraptor mongoliensis*) | Carpenter1998_01 | 18.8 kg | 10.3 kg-34.5 kg | `Benson2014_S11_FCmeasured` | Benson et al. 2014 S11 mass for MPD-C 100/25 (femur 190 mm, femoral circumference 64 mm measured; S11 note 'measured circumferences'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) |  |
| IVPP V 14202 (*Huadanosaurus sinensis*) | Qiu2025_01, Qiu2025_02 | 1.82 kg | 991 g-3.77 kg | `ErickTum2000_DME_bone` | DME by femur 105.69 mm (Qiu 2025 SI Table S2) from compsognathid Sinosauropteryx NIGP 127587 (S11 992 g, femur 86.4 mm; same formation); interval = 95% range across this and Compsognathus MNHN CNJ 79 | DME from Compsognathus MNHN CNJ 79 (S11 2,320 g, femur 110 mm) = 2,060 g |
| AODF0890 (*Confractosuchus sauroktonos*) | White2022_01 | 37.4 kg | 26.6 kg-57.7 kg | `Farlow2005_T3_logM_logSKULL_all` | skull length 285 mm (White et al. 2022 'The skull (28.5 cm long, 19 cm wide)'; their landmark may differ from Farlow's occipital-table definition); Farlow 2005 skull equation; interval = envelope of its 95% range and the TL estimates | Farlow TL equation with the authors' 2.0-2.5 m: 26.6 kg-57.7 kg |
| SNSB-BSPG AS I 563 (*Compsognathus longipes*) | Conrad2017_01 | 1.57 kg | 856 g-2.87 kg | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for BSP ASI 563 (femur 71.5 mm (MTC), FAP 9.65 mm; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) | Ostrom 1978 p.109 'estimated live weight, 3-3.5 kg' (no method given) |
| SBA-SA 163760 (*Scipionyx samniticus*) | DalSasso2011_01, DalSasso2011_02, DalSasso2011_03, DalSasso2011_04, DalSasso2011_05, DalSasso2011_06, DalSasso2011_07 | 220 g | 120 g-403 g | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for (no number; Dal Sasso & Signore 1998) (femur 37.3 mm, FAP 4.8 mm; juvenile; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) | Dal Sasso & Maganuco 2011 (PDF p.177, 'Body length and body mass'): 'tentatively estimate a weight in life of no more than 0.2 kg' from body proportions of small coelurosaurs (no equation); total length 'did not exceed 50 cm' |
| IVPP V13259 (*Yanornis martini*) | Zhou2002_01 | 482 g | 198 g-1.17 kg | `Field2013_T1_HL` | humerus length 75.4 mm (Wang2020); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 56.1 mm) = 747 g |
| STM9-15 (*Yanornis*) | Zheng2014_01, Zheng2014_02 | 545 g | 224 g-1.33 kg | `Field2013_T1_HL` | humerus length 80 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 51 mm) = 571 g |
| STM9-18 (*Yanornis*) | Zheng2014_03, Zheng2014_04 | 451 g | 185 g-1.1 kg | `Field2013_T1_HL` | humerus length 73 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 49 mm) = 510 g |
| STM9-19 (*Yanornis*) | Zheng2014_05, Zheng2014_06 | 490 g | 201 g-1.19 kg | `Field2013_T1_HL` | humerus length 76 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 58 mm) = 821 g |
| STM9-26 (*Yanornis*) | Zheng2014_07 | 517 g | 212 g-1.26 kg | `Field2013_T1_HL` | humerus length 78 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) |  |
| STM9-31 (*Yanornis*) | Zheng2014_08, Zheng2014_09 | 380 g | 46.1 g-3.14 kg | `Field2013_T1_TaL` | tarsometatarsus 52 mm (estimated; Zheng 2014 File S1 Table S1; outside the 31-40 mm of the other nine birds); the only limb length for this bird; interval = Field's 95% PI (+/- 2.11 ln) | skull 60 mm, the same as STM9-18 (451 g by humerus length) |
| STM9-37 (*Yanornis*) | Zheng2014_10 | 531 g | 218 g-1.29 kg | `Field2013_T1_HL` | humerus length 79 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 55 mm) = 707 g |
| STM9-46 (*Yanornis*) | Zheng2014_11, Zheng2014_12 | 545 g | 224 g-1.33 kg | `Field2013_T1_HL` | humerus length 80 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 57 mm) = 782 g |
| STM9-49 (*Yanornis*) | Zheng2014_13, Zheng2014_14 | 438 g | 180 g-1.07 kg | `Field2013_T1_HL` | humerus length 72 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 50 mm) = 540 g |
| STM9-51 (*Yanornis*) | Zheng2014_15, Zheng2014_16 | 378 g | 155 g-919 g | `Field2013_T1_HL` | humerus length 67 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 57 mm) = 782 g |
| STM9-52 (*Yanornis*) | Zheng2014_17, Zheng2014_18 | 389 g | 160 g-948 g | `Field2013_T1_HL` | humerus length 68 mm (Zheng2014); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 50 mm) = 540 g |
| IVPP V16708 (*Jianchangornis microdonta*) | Zhou2009_01 | 490 g | 201 g-1.19 kg | `Field2013_T1_HL` | humerus length 76 mm (Zhou2009); Field et al. 2013 humerus-length equation; interval = Field's 95% prediction interval (+/- 0.89 ln) | Field 2013 femur-length equation (FL 60 mm) = 903 g; Wang 2020 SI humerus 75.4 mm = 482 g; Benson 2014 S11 = 746 g (cQE, FC estimated) |
| IVPP V12330 (*Microraptor zhaoianus*) | Hone2022_01 | 150 g | 63.3 g-256 g | `ChristiansenFarina2004_FL` | Hone et al. 2022 Table 2: femur 49.8 mm -> 150 g (Christiansen & Farina 2004); interval = envelope with the 95% ranges of the DME cross-checks at femur 49.8 and 53 mm (conflict 17) | DME from CAGS 20-7-004 (S11 396 g, femur 75 mm): femur 49.8 mm (Hone) = 116 g, femur 53 mm (Zhou & Zhang 2002; O'Connor 2011) = 140 g; Larsson et al. 2010 '100 and 150 g' ('metric comparisons to closely related taxa') |
| MNHN CNJ 79 (*Compsognathus longipes*) | Peyer2006_01 | 2.32 kg | 1.27 kg-4.26 kg | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for MNHN CNJ 79 (femur 110 mm, FAP 11.1 mm; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) |  |
| CAGS-IG02-301 (*Huaxiagnathus orientalis*) | Hwang2004_01 | 20.4 kg | 11.1 kg-37.3 kg | `Benson2014_S11_FCestimated` | Benson et al. 2014 S11 mass for CAGS-IG02-301 (femur 164 mm, FAP 24 mm 'Measured from Hwang et al. (2004)'; S11 note 'FC estimated'); interval = point x/÷ 1.83 (95%, C&E 2012 eqn 1 SEE 0.134) |  |
| BMNH R9951 (*Baryonyx walkeri*) | Charig1997_01, Charig1997_02 | 2,580 kg | 897 kg-7,400 kg | `ErickTum2000_DME_bone` | DME from Suchomimus MNN GDF 500 (S11 2,910 kg, femur 1075, humerus 560 mm, measured FC): femur 1200 mm ('probably a reasonable approximation', Charig & Milner p.51) -> 4,040 kg; humerus 463 mm -> 1,640 kg; point = geometric mean; interval = 95% range across the two | R9951 is a juvenile (Charig & Milner); the humerus and femur ratios disagree by 2.5 times |


Notes on particular predators:

- ***Repenomamus* IVPP V13605.** Hu et al.'s "4–6 kg" is a judgement across equations not on disk. The verified Campione & Evans 2012 equation applied to the specimen's own circumferences gives 4.55 kg, inside that range.
- ***Microraptor*.** All four masses are Hone et al. 2022's, from femur length by Christiansen & Fariña 2004 (`verified = FALSE`). For IVPP V17972 the femur is only estimated, and scaling by the measured tibia gives 1.95 kg, against Hone's 760 g. This agrees with O'Connor et al.'s statement that the specimen is "10% larger than the holotype of M. gui" by tibia. The interval (760 g–3.57 kg) carries the disagreement. See conflict 28.
- ***Anchiornis*.** The S11 reference (LPM-B00169, 0.70 kg at femur 66.2 mm) rests on a shaft diameter "measured from figure". The troodontid *Mei long*, which has a measured circumference, gives values about 25% lower. Both are in the interval.
- ***Sinocalliopteryx* CAGS-IG-T1** has no femur. The three element ratios to the holotype disagree (41.9–88.8 kg) because, as Xing et al. note, the metatarsals grew faster than the radius and maxilla.
- ***Baryonyx* BMNH R9951.** The femur length is the authors' approximation from non-overlapping ends ("1,200 mm is probably a reasonable approximation"). Humerus and femur give estimates 2.5 times apart; the point is their geometric mean.
- **Crocodyliforms.** Farlow et al.'s equations are for *Alligator*. A baurusuchid (*Aplestosuchus*) and an eusuchian-grade crocodyliform (*Confractosuchus*) have different proportions, and the *Aplestosuchus* length is itself scaled from another baurusuchid. For *Confractosuchus* the skull equation is preferred because the skull is measured. The skull is broad (19 cm wide), and White et al.'s length landmark may differ from Farlow's occipital-table definition.
- ***Sanajeh*.** No snake family equation fits a madtsoiid. The all-snake equation gives 5.3 kg and the boid equation 16.7 kg. The point is the taxon-neutral value; the interval spans both.

## Prey estimates

Only the 18 entries with a prey mass are listed. log10 PPMR is empty where the predator mass is missing.


| Entry | Prey | Point | Interval | Method | Input and basis | Notes | log10 PPMR |
|---|---|---|---|---|---|---|---|
| Hu2005_01 | *Psittacosaurus* | 82.6 g | 37.3 g-107 g | `ErickTum2000_DME_bone` | femur not reported; estimated 33.8 mm from tibia 36 mm / mean tibia:femur 1.066 of four Psittacosaurus in Benson 2014 S11 (bracket 25.9-36.8 mm from the tibia and from humerus 21 mm with humerus:femur 0.57-0.81 in Han 2023, Hu 2005 SI juvenile and S11); DME from Erickson & Tumanova 2000 Table 2 (femur 21.0 cm, 19.87 kg; same result from any row); interval = DME at the femur bracket ends (no error is stated for DME) | DME from humerus via E&T Table 1 element lengths is not usable: E&T's humeri and femora come from different individuals | 1.77 |
| Han2023_01 | *Psittacosaurus lujiatunensis* | 10.6 kg | 4.6 kg-16.6 kg | `Han2023_DME_Campione2014` | authors: 'we estimate its body mass to have been 10.6 +/- 6.0 kg at time of death' (Han et al. 2023 p.3; DME from D2591, femoral circumference 69.362 mm -> 23.5 kg); interval = authors' +/- (undefined) | DME from Erickson & Tumanova 2000 (P. mongoliensis, femur 21.0 cm, 19.87 kg) with femur 137.7 mm = 5.6 kg | -0.49 |
| OConnor2011_01 | *Enantiornithes* | 65 g | 60 g-70 g | species_average_literature | O'Connor et al. 2011: 'the enantiornithine, roughly estimated to be 60-70 g (30)', ref 30 = Sanz et al. 2002 (wing loading); a literature value for enantiornithines, not this individual; point = midpoint | Hone 2022 gives the prey ulna as 10.5 mm (not checked against the figure) | 1.07 |
| OConnor2019_01 | *Indrasaurus wangi* | 8.49 g | 6 g-12 g | `Hone2022_Meiri2010_unstated` | Hone et al. 2022 Table 2: Indrasaurus '6-12 g', 'estimated from SVL using Meri (2010); SVL estimated based on closely related and similar sized (femur length 83% of that of Indrasaurus) specimen of Liushusaurus'; SVL and equation not stated; point = geometric midpoint |  | 1.83 |
| Xing2012_01 | *Sinornithosaurus* | 2.62 kg | 1.43 kg-9.9 kg | `ErickTum2000_DME_bone` | prey tibia 155 mm (Xing 2012 'tibial length 15.5 cm'); DME by tibia from microraptorine Microraptor gui IVPP V13352 (S11 1,500 g, tibia 128.6 mm); interval = 95% range across this, Mei long and Velociraptor references | DME by tibia from Velociraptor MPD-C 100/25 (tibia 235 mm, measured FC) = 5,410 g; from Mei long IVPP V12733 (tibia 100.5 mm) = 3,100 g; S11 has no tibia for Sinornithosaurus IVPP V12811 | 1.09 |
| Varricchio2001_01 | *Hadrosauridae* | 352 kg | 154 kg-790 kg | `ErickTum2000_DME_bodylength` | authors: caudals 'represent individuals about 3 m in total body length'; DME by body length from Edmontosaurus DMNH 1493 (6995 mm mounted length, Carpenter 1998 Table I; 4,460 kg by cQE, this study); interval carries the reference's interval; isometry is a poor assumption for juvenile hadrosaurs | no hadrosaur length-mass equation is on disk |  |
| Varricchio2001_02 | *Hadrosauridae* | 17.3 kg | 5.7 kg-50.6 kg | `ErickTum2000_DME_bodylength` | authors: dentary 'indicates a very young animal roughly 1 to 1.2 m in length'; DME by body length (point at 1.1 m) from Edmontosaurus DMNH 1493 as for Varricchio2001_01; interval = 1.0 m with the reference lower bound to 1.2 m with the upper bound |  |  |
| Therrien2023_01 | *Citipes elegans* | 9.2 kg | 5.28 kg-17.7 kg | `ChristiansenFarina2004_FL` | authors: femur 178 mm -> 9.2 kg (Therrien et al. 2023 SI Table S2, Christiansen & Farina 2004) (femur estimated from the metatarsal IV/femur ratio of the posterior individual); interval = envelope with the 95% range of the DME cross-check | DME from Chirostenotes pergracilis TMP 79.20.1 (S11 51.1 kg, femur 310 mm, measured FC) = 9.67 kg | 1.56 |
| Therrien2023_02 | *Citipes elegans* | 11.5 kg | 6.53 kg-21.9 kg | `ChristiansenFarina2004_FL` | authors: femur 191 mm -> 11.5 kg (Therrien et al. 2023 SI Table S2, Christiansen & Farina 2004); interval = envelope with the 95% range of the DME cross-check | DME from Chirostenotes pergracilis TMP 79.20.1 (S11 51.1 kg, femur 310 mm, measured FC) = 12 kg | 1.46 |
| Godoy2014_01 | *Sphagesauridae* | 817 g | 301 g-1.06 kg | `Farlow2005_T3_logM_logSKULL_all` | skull length 95 mm, estimated from parietal length 25 mm 'based on Cairpirasuchus paulistanus' (Text S1); Farlow 2005 skull equation; interval = envelope of its 95% range and the TL-based estimates | Farlow TL equation: 550 mm (Text S1) = 301 g; 600 mm (main text) = 408 g | 1.72 |
| DePalma2013_01 | *Edmontosaurus* | 5,430 kg | 4,460 kg-6,610 kg | species_average_literature | no element of this individual allows an estimate (two coalesced caudals); DePalma et al. call it 'an adult hadrosaur'; bracket = two adult E. annectens with limb-based masses: DMNH 1493 (4,460 kg, this study) and AMNH 5730 (Benson 2014 S11, 6,610 kg); point = geometric mean |  |  |
| Carpenter1998_02 | *Edmontosaurus annectens* | 4,460 kg | 1,950 kg-10,000 kg | `Campione2014_eqn7` | femur circumference 'below 4th trochanter' 49.5 cm (left) and 42.4 cm (right) (Carpenter 1998 Table I); cQE on the mean 459.5 mm; left alone 5,470 kg, right alone 3,570 kg; interval = right-side value / 1.83 to left-side value x 1.83 | DME by mean femur length 1034.5 mm from E. annectens AMNH 5730 (S11 6,610 kg, femur 1147.7 mm) = 4,840 kg; no humeral circumference, so the quadrupedal equation cannot be used |  |
| White2022_01 | *Ornithopoda* | 1.3 kg | 1 kg-1.7 kg | `Campione2014_eqn7` | authors: 'The ornithopod's estimated body mass (1.0-1.7 kg; calculated using the bipedal formula of Campione et al. (2014))'; input femoral circumference not reported (only proximal femora preserved; conflict 4 open); point = geometric midpoint; interval = authors' range | DME by the preserved tibia (43 mm long, 10 mm circumference): from Orodromeus MOR 473 = 187 g, Jeholosaurus = 188 g, Haya IGM 100/2015 = 243 g; about 1/5 to 1/9 of the authors' range | 1.46 |
| Conrad2017_01 | *Bavarisaurus* | 9.1 g | 7.45 g-9.2 g | `Meiri2010_T3_Sauria` | precaudal length (= SVL) 'approximately 75' mm reconstructed by Conrad 2017; Meiri 2010 Sauria; bracket: PCL 70.2-75.3 mm from the Bavarisaurus type (PCL 91 mm) scaled by femur ratios 16.9/21.9 (text) and 18.2/22 (Table 1); Meiri gives no prediction interval, so the bracket covers the input only | Ostrom's 'body length 46-47 mm' is not an SVL: his value for the Bavarisaurus type is 52 mm against Conrad's PCL 91 mm (conflict 20) | 2.24 |
| DalSasso2011_05 | *Clupeomorpha* | 0.156 g | 0.04 g-0.54 g | `Froese2006_cube_generic` | fish body length 20-30 mm estimated by the authors; W = a L^3 with a = 0.01 g/cm3 at the midpoint, bracket a = 0.005 at the lower length to 0.02 at the upper (UNVERIFIED: coefficient assumed, Froese 2006 not on disk) |  | 3.15 |
| DalSasso2011_07 | *Teleostei* | 0.911 g | 0.32 g-2.5 g | `Froese2006_cube_generic` | fish body length 40-50 mm estimated by the authors; W = a L^3 with a = 0.01 g/cm3 at the midpoint, bracket a = 0.005 at the lower length to 0.02 at the upper (UNVERIFIED: coefficient assumed, Froese 2006 not on disk) |  | 2.38 |
| Zheng2014_01 | *Jinanichthys* | 4.22 g | 1.72 g-10.2 g | `Froese2006_cube_generic` | fish body length 70-80 mm estimated by the authors; W = a L^3 with a = 0.01 g/cm3 at the midpoint, bracket a = 0.005 at the lower length to 0.02 at the upper (UNVERIFIED: coefficient assumed, Froese 2006 not on disk) |  | 2.11 |
| Hone2022_01 | *Mammalia* | 25.1 g | 21 g-30 g | species_average_literature | Hone et al. 2022: 'The mid range estimates for Eomaia (digit length 9.1 mm) at 30 g and that of Yanoconodon (digit length 7.5mm) at 21 g define a reasonable size bracket' (masses from Meng et al. 2017, not on disk); full range 13-43 g; point = geometric midpoint | DME by pedal digit I (about 9 mm) from Eomaia = 29 g and Yanoconodon = 36.3 g; Larsson et al. 2010: '20 - 25 g' | 0.78 |


Notes on particular prey:

- **Brackets for fragmentary prey.** The juvenile *Psittacosaurus* in *Repenomamus* (Hu2005_01) has no femur. Its bracket comes from the range of tibia:femur and humerus:femur ratios in other *Psittacosaurus* specimens (Han 2023 Table 1, Hu 2005 SI, four specimens in S11). The *Sinornithosaurus* leg (Xing2012_01) is bounded by which dromaeosaurid or troodontid reference is used. The juvenile hadrosaurs (Varricchio2001_01, _02) are bounded by the reference interval and by the authors' length range. Isometric scaling from an adult *Edmontosaurus* probably overestimates the mass of slender juveniles.
- **White2022_01.** The authors' range is kept, but the preserved tibia gives 5–9 times less (conflict 4, unresolved). Treat this pair's ratio as uncertain by about an order of magnitude.
- **DePalma2013_01** has no element that allows an estimate. Its prey mass is a bracket between two adult *E. annectens* (one from S11, one from Carpenter's DMNH 1493). It is `species_average_literature`, so the pair is tier B, not A.
- **Fish prey** (DalSasso2011_05 and _07, Zheng2014_01) use an assumed cube-law coefficient, not one transcribed from a paper (`verified = FALSE`). Use them only until a length–weight source is on disk.

## Entries with no prey mass, and why


| Entry | Prey | Why no prey mass |
|---|---|---|
| Chen1998_01 | *Lacertilia* | no prey measurement is published; Smithwick 2017 Fig. S2 (20 mm scale bar) could be measured, but no lizard femur-to-mass or skull-to-mass equation is on disk |
| Hurum2006_01 | *Zhangheotherium* | no measurement of the jaws is published; no literature mass for Zhangheotherium is on disk |
| Hurum2006_02 | *Sinobaatar* | no measurement of the jaw is published; no literature mass for Sinobaatar is on disk |
| Dalsatt2006_01 | *Jinanichthys* | seven to nine vertebrae in a 6 mm cluster; no fish length |
| Kear2006_01 | *Inoceramidae* | bivalve shell fragments 5-20 mm; no estimate of individual bivalve size or number |
| Kear2006_02 | *Inoceramidae* | bivalve shell fragments 5-20 mm; no estimate of individual bivalve size or number |
| Kear2006_03 | *Inoceramidae* | bivalve shell fragments 5-20 mm; no estimate of individual bivalve size or number |
| Zhou2014_01 | *Teleostei* | 13 x 8 mm cluster of disarticulated bones; no fish length |
| Zhou2014_02 | *Teleostei* | no fish length |
| Huang2026_01 | *Teleostei* | no fish length |
| Xing2013_01 | *Teleostei* | disarticulated fin rays (up to 6.7 mm), vertebrae and cranial fragments; Hone 2022's 90 g rests on comparing the 3.5 x 1.6 cm bone mass with an Anchiornis pellet (Freimuth et al. 2021, not on disk), which is not a body-size estimate |
| Zheng2018_01 | *Lacertilia* | femora and humeri are drawn in Fig. 1c (10 mm scale) but not measured, and no lizard limb-to-mass equation is on disk |
| Zheng2018_02 | *Lacertilia* | femora and humeri are drawn in Fig. 1c (10 mm scale) but not measured, and no lizard limb-to-mass equation is on disk |
| Zheng2018_03 | *Lacertilia* | femora and humeri are drawn in Fig. 1c (10 mm scale) but not measured, and no lizard limb-to-mass equation is on disk |
| Zheng2018_04 | *Ptycholepidae* | fish scales and fragments only |
| Zheng2018_05 | *Ptycholepidae* | fish scales and fragments only |
| Zheng2018_06 | *Ptycholepidae* | fish scales and fragments only |
| Zheng2018_07 | *Ptycholepidae* | fish scales and fragments only |
| Zheng2018_08 | *Ptycholepidae* | fish scales and fragments only |
| Xing2012_04 | *Confuciusornis sanctus* | no measurement of the confuciusornithid bones is published; no species mean for C. sanctus is on disk (Benson 2014 S11 lists only MOR 1063, 'near-maximal-sized', 0.475 kg) |
| Xing2012_05 | *Confuciusornis sanctus* | no measurement of the confuciusornithid bones is published; no species mean for C. sanctus is on disk (Benson 2014 S11 lists only MOR 1063, 'near-maximal-sized', 0.475 kg) |
| Xing2012_06 | *Ornithischia* | one incomplete scapula (135 x 65 mm, preserved); identity 'contentious' and no scapula-based equation is on disk |
| Nesbitt2006_01 | *Crocodylomorpha* | proximal femora, ilium and a sacral vertebra only; no length is published (the old table's 71.2 mm femur is untraced, conflict 11) |
| Wilson2010_01 | *Titanosauria* | the hatchling is 'approximately 0.5 m long' with no measured element and no sauropod length-mass equation is on disk; the associated egg GSI/GC/2905 is 2,316 g (Text S5), which bounds a fresh hatchling only if it had not yet grown |
| Carpenter1998_01 | *Protoceratops andrewsi* | only a body length ('120-130 cm long', from Barsbold 1974) is given and no Protoceratops length-mass equation is on disk |
| Qiu2025_01 | *Gobiconodontidae* | two mandibles in SI Fig. S7 (5 mm scale bar) but no measurement; no literature mass for Meemannodon is on disk |
| Qiu2025_02 | *Eutheria* | 'the eutherian content is badly preserved'; no measurement |
| DalSasso2011_01 | *Vertebrata indet* | indeterminate elements; probably the authors' prey 5 ('smaller vertebrates of uncertain affinities') |
| DalSasso2011_02 | *Lepidosauria* | authors give total length 15-40 cm ('half that of Scipionyx'); no SVL and no length-mass equation for total length is on disk |
| DalSasso2011_03 | *Lepidosauria* | authors give 'no more than 10-12 cm long' (total length) from scale size; no SVL |
| DalSasso2011_04 | *Osteichthyes* | isolated vertebra; possibly part of prey 2 or 5 |
| DalSasso2011_06 | *Vertebrata indet* | origin indeterminate |
| Zhou2002_01 | *Teleostei* | macerated fish remains; no fish length |
| Zheng2014_02 | *Teleostei* | no fish length is published |
| Zheng2014_03 | *Teleostei* | no fish length is published |
| Zheng2014_04 | *Teleostei* | no fish length is published |
| Zheng2014_05 | *Protopsephurus* | no fish length is published |
| Zheng2014_06 | *Teleostei* | no fish length is published |
| Zheng2014_07 | *Jinanichthys* | no fish length is published |
| Zheng2014_08 | *Jinanichthys* | no fish length is published |
| Zheng2014_09 | *Teleostei* | no fish length is published |
| Zheng2014_10 | *Teleostei* | no fish length is published |
| Zheng2014_11 | *Teleostei* | no fish length is published |
| Zheng2014_12 | *Teleostei* | no fish length is published |
| Zheng2014_13 | *Jinanichthys* | no fish length is published |
| Zheng2014_14 | *Teleostei* | no fish length is published |
| Zheng2014_15 | *Teleostei* | no fish length is published |
| Zheng2014_16 | *Teleostei* | no fish length is published |
| Zheng2014_17 | *Teleostei* | no fish length is published |
| Zheng2014_18 | *Teleostei* | no fish length is published |
| Zhou2009_01 | *Teleostei* | fragmentary fish remains; no length |
| Zhou2010_01 | *Teleostei* | no description; no specimen |
| Peyer2006_01 | *Lepidosauria* | imprints; 'Their description and interpretation is, however, beyond the scope of this paper' |
| Hwang2004_01 | *Vertebrata indet* | unidentified rounded objects averaging 7 mm |
| Charig1997_01 | *Lepidotes* | scales and teeth only; no size |
| Charig1997_02 | *Iguanodon* | 'a young Iguanodon'; elements listed by block (Appendix B) but none measured |


## Entries with no predator mass, and why

| Entry | Predator | Why |
|---|---|---|
| Dalsatt2006_01 | *Confuciusornis* IVPP V13313 | Dalsätt et al. give no measurements of the specimen. A species mean for *C. sanctus* is not on disk; S11 lists only MOR 1063 ("near-maximal-sized", 0.475 kg). |
| Kear2006_01–03 | cf. *Notochelone* | Only "small (<1 m total length)". No turtle equation is on disk, and the "carapace lengths" figure is ambiguous (conflict 9). Turtles are low priority (decision 6). |
| Varricchio2001_01, _02 | *Daspletosaurus* OTM 200 | "Notably absent are the cranium, shoulder girdle and all limb elements." No equation on disk uses a dentary or vertebrae. |
| DePalma2013_01 | *T. rex* tooth PBMNH.P.09.039 | Only a tooth crown; no tooth-to-mass equation on disk. The authors call it a subadult. |
| Carpenter1998_02 | *T. rex* (inferred) | No specimen; the attacker is inferred from the bite. |
| Zheng2018_07, _08 | *Anchiornis* (by context) | Isolated pellets; producer unknown. |
| Zhou2010_01 | *Yabeinosaurus* | Personal observation in a review; no specimen. |

## Excluded records

`db/excluded_fossil_records.csv` lists the 32 step-1 entries not in `db/pairs.csv`, each with `reason` and, for repeats, the entry it duplicates (`counted_as`). No masses were estimated for them: none has an animal prey item, so none can give a ratio. That is a deliberate departure from "every entry" in the prompt. Where a predator in that list matters for another purpose (for example *Daurlong*, femur 180 mm), its measurements are in `step1_measurements.csv`.

## Differences from the old fossil tables

Built by the script from `data/fossil_ppmr_points.csv`, `data/fossil_predprey.csv` and `data/fossil_diet_extractions.csv`, which were read only for this comparison. The full table, including agreements, is `step2_old_table_comparison.csv`. Below are all differences of more than 25%, grouped by entry. Each was checked against the source paper before writing the "why" column. Where the old value has no traceable source, that is stated; it is not called wrong for that reason alone.

| Entry | Side | Old | New | Why |
|---|---|---|---|---|
| White2022_01 | predator | 75 kg (ppmr_points; diet_extractions "expert_estimate_JM_2026-06 (~50-100 kg)") | 37.4 kg | White et al. give no mass, only "around 2–2.5 m in length". The new value is Farlow's skull equation on the measured 285 mm skull. Farlow's length equation on the authors' 2–2.5 m gives 27–58 kg, which does not reach 75 kg. The old value has no equation behind it. |
| Hu2005_01 | predator | 3.85 kg (both tables) | 4.90 kg | Both are Hu et al.'s numbers. 3.85 kg is their skull and head-body-length mean (SI Table 4), one input to their conclusion. The new point is the midpoint of the conclusion itself ("4–6 kg"). |
| Hu2005_01 | prey | 40 g (15–150; "cube laws 11g(TL)-240g(skull) under juvenile allometry") | 83 g (37–107) | The old note cites no published equation. The new value is Erickson & Tumanova's DME on a femur reconstructed from the measured tibia (36 mm) and humerus (21 mm). The intervals overlap. |
| Wilson2010_01 | predator | 18 kg ("expert_estimate_JM_2026-06 (~18 kg +/-7)") | 5.32 kg | Wilson et al. give total length (3.46 m), not mass. The new point is Feldman & Meiri's all-snake TL equation; their boid equation gives 16.7 kg, close to the old value. The choice of equation decides it, and the db interval (5.3–16.7 kg) contains both. |
| Xing2013_01 | predator | 4.07 kg (ppmr_points, "x19_estimate; pred mass=lm-Benson(femur)") | 1.89 kg | `fossil_predprey` had 1.87 kg, which agrees with the new value (Hone et al. 2022, 1.89 kg). The 4.07 kg came from a femur-length regression the project fitted to Benson's data (x19), not a published equation. Scaling from the *M. gui* holotype gives 1.63 or 3.98 kg depending on which published femur length of the holotype is right (conflict 28). |
| Varricchio2001_01 | prey | 108 kg ("x19_estimate") | 352 kg (154–790) | Varricchio gives "about 3 m" only. The old value records no method. The new value scales *Edmontosaurus* DMNH 1493 isometrically by body length; no hadrosaur length–mass equation is on disk. Isometry probably overestimates a slender juvenile, so the true value may lie nearer the lower bound. |
| Varricchio2001_02 | prey | 5.32 kg ("x19_estimate") | 17.3 kg (5.7–51) | As above, for "roughly 1 to 1.2 m in length". |
| DalSasso2011_02, _03, _05, _07 | predator | 30 g (both tables, "hatchling_coarse_TL220mm_~15-60g") | 220 g | The old value assumed a total length of 220 mm. The specimen's preserved length is 237 mm, its estimated total length 461 mm (Table 1), and the authors say it "did not exceed 50 cm" and estimate "no more than 0.2 kg" (PDF p. 177). The new value (S11, from the femur) agrees with the authors; the old one does not. |
| Xing2012_04 (and _05, _06) | predator | 30.5 kg (both tables, "benson_lm_FL210mm_was55kg_no_femur") | 58.2 kg (22.9–163) | CAGS-IG-T1 has no femur; 210 mm is Hone's femur for the holotype. Xing et al. say CAGS-IG-T1 "is larger based on the postcranial measurements" than the holotype (S11 32.3 kg), so a mass below the holotype's contradicts the source. The new value scales the holotype by three Xing 2012 Table 2 ratios. |
| Qiu2025_01, _02 | predator | 3.70 kg ("x19_estimate") | 1.82 kg | The old value came from the project's own femur-length regression. The new value scales two compsognathid references with S11 masses (1.82 and 2.06 kg). |
| Conrad2017_01 | predator | 1.12 kg (ppmr_points, x19) | 1.57 kg | `fossil_predprey` had 1.567 kg ("benson"), the same S11 value as the new one. |
| Chen1998_01 | predator | 2.0 kg (ppmr_points, "Chen_et_al_1998_Nature_femur+...") | 0.99 kg | Chen et al. give no mass, and the old note names no equation. The new value is S11's for this specimen. |
| OConnor2011_01 | predator | 1.72 kg (ppmr_points, "Oconnoretal_2011") | 0.76 kg | O'Connor et al. print 1500 g as a generic *Microraptor* mass (their ref. 29), not 1.72 kg. `fossil_predprey` had 764 g, which matches Hone et al.'s 760 g (the new value). Tibia scaling gives 1.95 kg, which the interval (to 3.57 kg) covers. |
| Nesbitt2006_01 | predator | 27.5 kg (ppmr_points, x19); 11.7 kg (predprey, "brms_benson") | 8.29 kg | The new value scales AMNH 7223 (same species and quarry; measured femoral circumference; 9.04 kg at femur 209 mm) to AMNH FR 7224's 203 mm femur. A smaller femur cannot give 11.7 or 27.5 kg under the same equation family. |
| Xing2012_01 | predator | 11.4 kg (ppmr_points); 8.32 kg (predprey, with femur entered as 152.3 mm) | 32.3 kg | No source on disk gives the holotype a 152.3 mm femur (S11: 250; Hone: 210). The new value is S11's, which rests on the shaft diameter. |
| Xing2012_01 | prey | 942 g (both tables, "brms_benson tibiotarsus_length") | 2.62 kg (1.43–9.9) | The old value came from a project regression. The new value is DME by the 155 mm tibia from the microraptorine *M. gui*. |
| Carpenter1998_01 | predator | 15 kg (ppmr_points; diet_extractions "JM_estimate_10-20kg") | 18.8 kg | `fossil_predprey` had 18.84 kg, the same S11 value (measured circumference) as the new one. |

Old prey or predator masses with **no new value**. In each case nothing published on disk supports an estimate (reasons in the tables above):

| Entry | Side | Old | Old basis as recorded |
|---|---|---|---|
| Wilson2010_01 | prey | 3 kg (0.5–6) | "expert_estimate_JM_2026-06" |
| Carpenter1998_01 | prey | 63 kg (44–82) | "JM_estimate_44-82kg" |
| Varricchio2001_01, _02 | predator | 1,376 kg | "scaled_benson_dentary_from_Currie_2003_APP_for_NMC8506" (Currie 2003 not on disk; the old row also gives OTM 200 a 609 mm femur, but the specimen has no limb bones) |
| Nesbitt2006_01 | prey | 1,810 g | x19; the 71.2 mm femur is untraced (conflict 11) |
| Chen1998_01 | prey | 18 g | "squamate_femur_FL22.31_SVL106"; no lizard femur length is published |
| Hurum2006_01, _02 | prey | 30 g, 12 g | x19 from "dentary_length" / "dentary_height"; no jaw measurements are published |
| Xing2012_04, _05 | prey | 230 g | "brms_benson humerus_length"; no *Confuciusornis* bone is measured |
| Qiu2025_01, _02 | prey | 32 g each | x19; no measurement |
| Xing2013_01 | prey | 2.16 g | x19 from "fin_ray_length"; no published fish-size equation |
| Zheng2018_01–03 | prey | 0.842 g | x19 from limb lengths; no measurements published |
| DalSasso2011_02, _03 | prey | 28.4 g, 1.82 g | x19; the authors give total lengths only, and no total-length equation is on disk |
| Dalsatt2006_01 | both | 300 g; 2 g | "literature ... ~0.18-0.5kg"; prey "ROUGH" |
| Zhou2014_01 | prey | 15 g (4–40) | "ROUGH ... NO_fish_length" |

Not carried over: the old *Velociraptor* + azhdarchid row (Hone et al. 2012). That paper is not on disk and is not in step 1; it is on the gap list in the fossil-rebuild README.

## For JM: what to obtain

Method papers. Save each in `pdfs/methodology/` under exactly this name; each is a `NEEDED` row in `docs/methodology_index.csv`.

1. **Christiansen P, Fariña RA (2004)** Mass prediction in theropod dinosaurs. *Historical Biology* 16:85–92. → `Christiansen_Farina_2004_HistBiol_theropod_mass.pdf`. The masses of *Gorgosaurus* and both *Citipes*, and of four *Microraptor* specimens, rest on it (8 pair-sides). Citation from the reference lists of Hone 2022 and Therrien 2023; DOI not checked.
2. **Benson et al. (2014) Appendix S1** (PLoS Biol 12:e1001853, open access). → `Benson_etal_2014_PLoSBiol_dinosaur_body_mass_AppendixS1.pdf`. Holds the diameter-to-circumference regressions behind the S11 masses of 6 predator specimens used directly, and of several scaling references.
3. **Froese R (2006)** Cube law, condition factor and weight–length relationships. *J Appl Ichthyol* 22:241–253. → `Froese_2006_JApplIchthyol_fish_length_weight.pdf`. **Citation from memory; verify it first.** A FishBase length–weight relationship for a related living fish would also do. Needed for the three fish prey.
4. **Alexander RMcN, Jayes AS, Maloiy GMO, Wathuta EM (1979)** Allometry of the limb bones of mammals from shrews (Sorex) to elephant (Loxodonta). *J Zool Lond* 189:305–314. → `Alexander_etal_1979_JZool_mammal_limb_allometry.pdf`. Behind Hu et al.'s *Repenomamus* mass.
5. **Van Valkenburgh B (1990)** Skeletal and dental predictors of body mass in carnivores. In Damuth & MacFadden (eds), *Body Size in Mammalian Paleobiology*, pp. 181–205. → `VanValkenburgh_1990_BodySizeMammPaleobiol_carnivore_mass.pdf`. Same use; book chapter.

Fossil sources that would settle open conflicts. These belong on the Phase 4 gap list, not the method index:

- Xu et al. 2003 (*Microraptor gui* description): conflict 28.
- Ji et al. 2007 (*Sinocalliopteryx*): conflict 22.
- Ji & Ji 1997: conflict 25.
- The femoral circumference behind White et al.'s 1.0–1.7 kg (measure SI Fig. S34, or ask the authors): conflict 4.

Not requested but worth noting:

- The ICS chronostratigraphic chart is not on disk; ten sources' ages use stage bounds from memory.
- A species mean mass for *Confuciusornis sanctus*, *Zhangheotherium*, *Sinobaatar* and *Meemannodon* would give five more prey a tier-B mass.

## Files

- `code/db_migrate/10_fossil_step2.R`: all computations and the `db/` write. It owns sources whose `extracted_from` starts with `data/fossil_rebuild/`. `run_all.R` does not run it (it runs `0N_*.R` only), and running `run_all.R` leaves these rows alone.
- `db/pairs.csv` (74 fossil rows), `db/measurements.csv` (91), `db/sources.csv` (36), `db/taxa.csv` (fossil taxa upserted), `db/methods.csv` (21 rows registered or updated), `db/excluded_fossil_records.csv` (32).
- `data/fossil_rebuild/step2_estimates.csv`: one row per counted entry with point, interval, method, basis and cross-check text.
- `data/fossil_rebuild/step2_crosschecks.csv`: the cross-check estimates as numbers.
- `data/fossil_rebuild/step2_old_table_comparison.csv`: every comparison with the old tables.
- `data/fossil_rebuild/step2_conflicts.md`: conflict decisions.
