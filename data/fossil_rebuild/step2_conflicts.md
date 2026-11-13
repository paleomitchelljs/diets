# Step 2: conflict decisions

Written 2026-11-12 for Phase 3 (Prompt 3 in `docs/ASSESSMENT_AND_PLAN_2026-11.md`). The prompt names 11 conflicts, but Prompt 1b added 16 more to `README.md` (12–27), and several of those decide which input goes into a mass equation. All 27 are decided here, plus two found in this step (28–29).

Status words:

- **resolved**: the PDFs on disk settle it.
- **resolved for use**: the disagreement stays, but the PDFs show which value to use, or show that no estimate depends on it.
- **unresolved**: settling it needs a file that is not on disk. The missing file is named.

"Rendered" means I read the value from a page image, because the text layer was ambiguous or wrong.

## Summary

| # | Topic | Status | Affects a mass in `db/`? |
|---|---|---|---|
| 1 | Conrad 2017 *Schoenesmahl* femur/tibia | resolved for use | bracket only (prey SVL) |
| 2 | Zheng 2018 STM0-179 pellet size | resolved for use (short axis); long axis unresolved | no |
| 3 | Han 2023 "minimum shaft circumference" | resolved for use | no |
| 4 | White 2022 ornithopod mass input | **unresolved** (needs the femoral circumference) | yes: White2022_01 prey |
| 5 | GMV 2124 identity | resolved (decision 5) | yes: reported under both |
| 6 | Chen 1998 "eggs" | resolved (excluded either way) | no |
| 7 | Hu 2005 prey age | resolved | prey stage only |
| 8 | Godoy 2014 prey length 55 vs 60 cm | resolved | yes: bracket |
| 9 | Kear 2006 "maximum carapace lengths" | **unresolved** (meaning) | no |
| 10 | Specimen-number typos | resolved | no |
| 11 | Untraced values in the old tables | **unresolved** (Ji 2007 or other unknown source) | no: values not used |
| 12 | Huang 2026 V12558 printed twice | resolved | no |
| 13 | Other typos, incl. the C&F equation | resolved | yes: C&F equation form |
| 14 | Yanornithid and *Hongshanornis* identities | resolved (decision 5) | no |
| 15 | IVPP V13358 grit | resolved (excluded either way) | no |
| 16 | *Microraptor* mammal host | resolved | yes: predator specimen |
| 17 | IVPP V12330 femur 49.8 vs 53 mm | resolved for use | yes: interval |
| 18 | *Compsognathus* holotype femur | resolved for use | no |
| 19 | *Compsognathus* holotype total length | resolved | no |
| 20 | *Compsognathus* gut lizard identity and size | resolved | yes: prey SVL |
| 21 | *Compsognathus* MNHN CNJ 79 humerus and femur | resolved for use | no |
| 22 | *Sinocalliopteryx* holotype femur 250 vs 210 mm | **unresolved** (needs Ji et al. 2007) | no: mass rests on shaft diameter |
| 23 | Hone 2022 Table 1 femora are taxon-level | resolved | no |
| 24 | Qiu 2025 SI internal inconsistencies | resolved for use | no: femur only used |
| 25 | GMV 2124 femur 108 mm attributed by elimination | **unresolved** (needs Ji & Ji 1997) | yes: flagged |
| 26 | STM35-3 seed counts | resolved (plant, excluded) | no |
| 27 | *Jianchangornis* limb lengths | resolved for use | yes: 1.6% |
| 28 | NEW: *M. gui* holotype IVPP V13352 femur 106 vs 78.7* mm | **unresolved** (needs Xu et al. 2003) | yes: cross-checks |
| 29 | NEW: *Sinocalliopteryx* holotype radius 100.7 vs 94 mm | resolved for use | no |

Also found: step 1 missed the authors' own mass statement for *Scipionyx* (see the end of this file). It is not a conflict between sources.

## 1. Conrad 2017: *Schoenesmahl* femur and tibia

- Evidence. Table 1 (PDF p. 27, rendered): row *Schoenesmahl dyspepsia* has PCl blank, then SRl 15.1, Ml 18.3, Hl 13.5, dwH 4.1, mwH 0.9, **Fl 18.2**, mwF 2.1, **Tl 16.9**, Pl 30.6. The column alignment is clear on the image, so this is not a text-layer shift. Text (PDF p. 30): "This femur is approximately 16.9 mm long"; "The tibia is 17.3 mm long"; "the tibia is longer than the femur in S. dyspepsia, but the femur is longer than the tibia in Ba. macrodactylus." Ostrom 1978 Table 3 (independent measurement of the same lizard): femur 16.8–18.0, tibia 16.9.
- Decision. Use Table 1 (femur 18.2, tibia 16.9). Two of the three sets put the femur at or above the tibia. The text's femur value (16.9) equals the table's and Ostrom's tibia value, which looks like a transposition in the text, but I cannot confirm that without the specimen, so I am not calling the text wrong.
- Effect. The prey mass rests on precaudal length, not on limb lengths. Both femur values enter the SVL bracket (16.9/21.9 and 18.2/22 against the *Bavarisaurus* type), so the choice moves the bracket, not the point.

## 2. Zheng 2018: STM0-179 pellet size

- Evidence. Text (p. 1): "The long axis of the pellet measures 70 mm, whereas the short axis measures 2.7 cm." Table 1 (p. 4): 53 × 20 mm. I measured the pellet in Fig. 1c, the line drawing where the pellet is shaded blue. I rendered panel c at 600 dpi and found the blue region's principal axes in pixels. The 10 mm scale bar is 234 px, so the blue region is **62 × 27 mm**, with the long axis about 83° from horizontal. The red frame in Fig. 1a (43 × 29 mm by that panel's own scale bar) is the close-up window for panel d, not the pellet outline, so I did not use it.
- Decision. Short axis 27 mm: the text and the figure agree, and the table's 20 mm does not fit. Long axis unresolved. The drawing gives 62 mm, between the table (53) and the text (70). The blue shading may include matrix around the bones, so 62 is an upper bound for the drawn outline, not a check on either printed value.
- Effect. None. No mass depends on the pellet size.

## 3. Han 2023: *Psittacosaurus* femur "minimum shaft circumference" 17.7 mm

- Evidence. Table 1 (PDF p. 3, rendered) prints 17.7 mm against femur length 137.7 mm. The same row gives the mammal 7.3 mm against a 58.8 mm femur, and the humerus row gives 13.6 and 11.4 mm. Hu 2005 SI Table 1 gives the *Repenomamus* IVPP V13605 femur (67 mm long) a midshaft circumference of 26 mm and a midshaft diameter of 8 mm, so a 58.8 mm *Repenomamus* femur cannot have a 7.3 mm circumference. All four values are near the diameter, not the circumference, of such bones. The mammal's humerus value (11.4) is also larger than its femur value (7.3), which is unusual for a circumference. Han's masses do not use these numbers. Methods (p. 7): the dinosaur by "developmental mass extrapolation (DME)" from D2591's "femur circumference (reported as 69.362 mm)"; the mammal by DME "scaled against femur length" (SI Table S1).
- Decision. The four "minimum shaft circumference" values in Table 1 are not used as circumferences. They may be diameters, but the paper does not say so, and I have not relabelled them. The authors' masses are used as published.
- Effect. None on `db/`. It removes the only independent check on Han's mammal mass.

## 4. White 2022: ornithopod mass 1.0–1.7 kg

- Evidence. Discussion (PDF p. 19): "The ornithopod's estimated body mass (1.0–1.7 kg; calculated using the bipedal formula of Campione et al. (2014))". The input is not given in the main text. SI Figs S31–S34 are 3D models with captions only. Only proximal femora are preserved: "Both femora are incomplete, sheared obliquely below the level of the fourth trochanter." Campione et al. 2014 eqn 7 (verified on p. 915) needs a femoral circumference of about 22–26 mm to return 1.0–1.7 kg. The range is also what ±25% around about 1.36 kg would give, the usual way Campione reports error (Campione & Evans 2012 Table 6 caption). The preserved tibia ("43 mm long, 10 mm in mid-length circumference") gives about 0.19–0.24 kg by isometric scaling from three small ornithopods in Benson et al. 2014 S11.
- Decision. **Unresolved.** It needs the femoral circumference the authors used, which could be measured on the 3D model (Fig. S34, "Left femur") or requested from the authors (N. E. Campione is a co-author). The authors' range stays in `db/pairs.csv` as their published estimate. The tibia check is in the row's notes and in `step2_mass_estimates.md`. I have not called the authors' value wrong. The disagreement is large (5–9 times), and an input I cannot see could explain it, for example a circumference taken near the fourth trochanter rather than at the narrowest point.

## 5. GMV 2124: *Sinosauropteryx prima* or an early tyrannosauroid

- Evidence. As in the README: the original identification has three papers (Ji & Ji 1997b, per Currie & Chen 2001; Currie & Chen 2001; Hurum et al. 2006). The reassignment has one (Cau 2024, resting on Longrich 2002, an abstract). Qiu et al. 2025 follow Cau by citation.
- Decision. *Sinosauropteryx prima* (decision 5). The identity changes the reference specimen for scaling, so the mass is reported under both: 1.94 kg scaled from *Sinosauropteryx* NIGP 127587, and 2.22 kg scaled from the early tyrannosauroid *Dilong* IVPP V14243. The interval in `db/` spans both.

## 6. Chen 1998: the "eggs" in NIGP 127587

- Decision. Not prey under either reading, whether unlaid eggs (Chen et al. 1998; Currie & Chen 2001) or intestinal remnants (Wang et al. 2022; Cau 2024). Chen1998_02 and Currie2001_02 are in `db/excluded_fossil_records.csv`. The interpretation itself is left open.

## 7. Hu 2005 prey age: juvenile or "perinatal"

- Evidence. Hu et al. 2005, p. 151: "There are at least seven teeth on each jaw quadrant ... most are worn. This demonstrates that the Psittacosaurus skeleton is not from an embryo." Han et al. 2023 (p. 1) call it "a much smaller, perinatal Psittacosaurus sp." with a citation to Hu 2005 and no new observation.
- Decision. `prey_stage` = juvenile, following the describers' direct observation. "Perinatal" is a later description of the same specimen without new evidence.

## 8. Godoy 2014: prey length 55 or 60 cm

- Evidence. Text S1: "Body length estimated based on Araripesuchus gomesi ≈ 55 cm." Main text: "the remains correspond to an animal of about 60 cm in length (see Text S1)".
- Decision. The main text points to Text S1 for the calculation, so 55 cm is the computed value and "about 60" is rounded. Both enter the prey bracket. The point estimate uses the estimated skull length (95 mm), not the body length (see `step2_mass_estimates.md`).

## 9. Kear 2006: "maximum carapace lengths ~50–60 mm"

- Evidence. Rendered p. 2: "(maximum carapace lengths ~50–60 mm)". The unit is mm on the page, so this is not a text-layer error. The sentence sits inside a description of the gastric masses, so "carapace lengths" may mean the masses' extent along the carapace, not the turtles' size, but the paper does not say.
- Decision. **Unresolved as to meaning.** Nothing depends on it: no turtle length-to-mass equation is on disk, and turtles are low priority (decision 6). The predator masses for Kear2006_01–03 are empty.

## 10. Specimen-number typos

- Decision. Use IVPP V17972 (O'Connor 2011 main text and Fig. 1; "V13972" in SI Table S1 and in Hone 2022 Table 2 is a typo), IVPP V12330, and IVPP V17078 ("V17.78" in Huang 2026). These are the values in `db/pairs.csv`. Each typo is noted on its pair row.

## 11. Values in the old project tables that are in no source on disk

- Evidence. The QM V1002 radius, tibia and humerus in `data/fossil_predprey.csv` (93.46, 164.91, 106.28 mm) are not in Xing et al. 2013 (main text gives femur 109 mm and axial length 800 mm only), not in Benson 2014 S11 (no QM V1002 row), and Xing 2013 Fig. S1 is a skull photograph (JM's check; file not on disk). The *Hesperosuchus* prey femur 71.2 mm is not in Nesbitt et al. 2006 or its ESM; the ESM gives only "This proximal portion of a crocodylomorph femur".
- Decision. **Unresolved**, with the source unknown. The values are not used. No estimate in `db/` needs them.

## 12. Huang 2026 prints IVPP V12558 for both *Yixianornis* and *Yanornis*

- Decision. Resolved as in the README: the *Yixianornis* column is the typo (Wang 2020 SI Table S1: *Yixianornis* IVPP V12631). It touches no gut specimen.

## 13. Other typos

- Accepted as typos: Zheng 2014 "IVPP V13558" (= V13358) and "SMT9-18" (= STM9-18); Therrien SI "TMP 2009.14.12" (= TMP 2009.12.14); Hone 2022 "metacarpal" for a pedal element; Currie & Chen 2001 "Ji and Ji 1997a/b"; Wang 2020 "Table S2" (= SI Table S1).
- The Christiansen & Fariña equation as printed by Therrien et al. ("body mass (in kg) = -6.288 + 3.22*LOG(femur length)") needs log10 on the left. With that correction it reproduces Therrien's Table S2 within 1.3% (543.3 mm gives 330 kg against their 334.4) and Hone's Table 2 within 1% (109 mm gives 1.87 kg against their 1.89). It is registered that way, as `ChristiansenFarina2004_FL` with `verified = FALSE`, because the paper is not on disk.

## 14. Yanornithid and *Hongshanornis* identities

- Decision. Original identifications kept (decision 5): IVPP V13259 *Yanornis martini*; STM9 birds *Yanornis* sp.; STM35-3 *Hongshanornis*. Each reassignment rests on one paper. Identity does not change the equation (Field et al. 2013 humerus length for all yanornithids).

## 15. IVPP V13358 grit

- Decision. Not prey either way (gizzard stones or intestinal sand). Zhou2004_01 and Zheng2014_19 are excluded.

## 16. *Microraptor* mammal host

- Evidence. Larsson et al. 2010 (abstract, the primary report): "The holotype of Microraptor zhaoianus includes ... an articulated foot of a mammal." Hone et al. 2022 (the full description): "the holotype specimen of ... Microraptor zhaoianus", IVPP V 12330. O'Connor et al. 2011 say "the holotype of M. gui", citing the abstract.
- Decision. Host = *M. zhaoianus* IVPP V12330. Two primary sources agree. O'Connor et al. cite one of them and misstate it.

## 17. IVPP V12330 femur 49.8 or 53 mm

- Evidence. Hone et al. 2022 Table 2: 49.8 mm. Zhou & Zhang 2002 Table 1: 53 (left). O'Connor et al. 2011 Table S1: 53. The paper gives no reason for the difference.
- Decision. Resolved for use. The point is Hone et al.'s published mass (150 g, from 49.8 mm). The interval covers scaling at both 49.8 and 53 mm (63–256 g).

## 18. *Compsognathus* holotype femur: 67, 71, 71.5 or 65.8 mm

- Evidence. Ostrom 1978 Table 1: right femur "67 est". Chen et al. 1998 Table 1: 71, attributed to Ostrom, but Ostrom prints 67 est., so Chen's 71 is not Ostrom's value. Benson 2014 S11: 71.5, "BSP ASI 563 (MTC); Ostrom 1978". "(MTC)" marks M. T. Carrano's own measurement. Hone et al. 2022: 65.8, from Dececchi et al. 2020b (not on disk).
- Decision. Resolved for use. The two first-hand values are 67 (estimated) and 71.5 (measured). The predator mass (S11, 1.57 kg) rests on the femoral shaft diameter (9.65 mm), not the length, so no estimate depends on this choice.

## 19. *Compsognathus* holotype total length

- Evidence. Ostrom 1978 p. 74 (PDF p. 2): "In life, it measured no more than 70 to 75 cm from snout tip to tail tip." Conrad 2017 (PDF p. 46): "approximately 890 mm (Ostrom, 1978)".
- Decision. Ostrom's own statement is 70–75 cm. Conrad's figure is a misquotation of it. The step-1 measurement row labels Conrad's 890 mm "measured"; it should read "cited". Step-1 rows were not edited. No estimate uses total length.

## 20. *Compsognathus* gut lizard: identity and size

- Identity. *Bavarisaurus* cf. *macrodactylus* (Ostrom 1978, the first identification) is kept (decision 5). *Schoenesmahl dyspepsia* (Conrad 2017) is recorded in `db/taxa.csv` notes.
- Size. Ostrom's "body length" (46 mm in the text, 47 est. in Table 3) and Conrad's precaudal length (about 75 mm) are different measurements, not a disagreement. Ostrom gives 52 mm for the *Bavarisaurus* type, and Conrad gives the same specimen a precaudal length of about 91 mm, so Ostrom's "body length" is not snout–vent length. Ostrom does not define it, but it is probably the trunk. Conrad's precaudal length is used as SVL for the Meiri 2010 equation.
- Limb lengths: as conflict 1.

## 21. *Compsognathus* MNHN CNJ 79: humerus and femur

- Evidence. Peyer 2006 Table 1 (measured on the specimen): humerus 51.9 (right) / 56.3 (left), femur 108.8 (left). Ostrom 1978 Table 2: humerus 67 est., femur 110. Chen 1998 copies Ostrom.
- Decision. Use Peyer's measurements. Ostrom's humerus was an estimate. The predator mass (S11, 2.32 kg) rests on the femoral shaft diameter (11.1 mm), so no estimate changes.

## 22. *Sinocalliopteryx* holotype femur: 250 or 210 mm

- Evidence. Benson 2014 S11: 250 mm, "measured from figures in Ji et al. 2007". Hone 2022 Table 1: 210, from Dececchi et al. 2020b. Xing et al. 2012 Table 2 gives only maxillary height (40.8), metatarsal III (147.3) and radius (100.7) for this specimen. Both femur values are second-hand.
- Decision. **Unresolved**; needs Ji et al. 2007 (not on disk; optional per the intake log). The predator mass in `db/` (S11, 32.3 kg) rests on the femoral mediolateral shaft diameter (25.5 mm, also from Ji's figures), not on the length.

## 23. Hone 2022 Table 1 femora are taxon-level

- Decision. Confirmed against S11. Hone's Table 1 femora are not used as specimen measurements. Specifically: Coelophysis 209 is AMNH 7223, not AMNH FR 7224; Daspletosaurus 1030 is AMNH 5438 (OTM 200 has no limb bones); Tyrannosaurus 1321 is FMNH PR 2081.

## 24. Qiu 2025 SI, internal inconsistencies

- Decision. Resolved for use. Only the *Huadanosaurus* femur (105.69 mm) enters an estimate. It agrees with the main text ("The skull length (100.59 mm) is subequal to that of the femur (105.69 mm)"). The humerus and the *S. lingyuanensis* tibia and metatarsus discrepancies are not resolved and not used.

## 25. GMV 2124 femur 108 mm

- Decision. **Unresolved**; needs Ji & Ji 1997 (not on disk). The attribution is an inference by elimination. The femur is used, with the inference stated in the row's basis.

## 26. STM35-3 seeds and gastroliths

- Decision. Plant diet; excluded. No action.

## 27. *Jianchangornis* IVPP V16708 limb lengths

- Decision. Use the describers' values (Zhou et al. 2009 Table 1: humerus 76 left). Wang 2020's 75.4 changes the mass by 1.6%; it is noted in the cross-check.

## 28. NEW: *Microraptor gui* holotype IVPP V13352 femur, 106 or 78.7 mm

- Evidence. Benson 2014 S11: femur 106 mm, "IVPP V13352 (JNC); Xu et al. 2003; femoral anteroposterior shaft diameter measured from figure"; tibia (or tibiotarsus-based estimate) 128.64 mm; mass 1.50 kg. O'Connor et al. 2011 SI Table S1: femur 78.7* ("*Estimated length"), tibia 126.7. The tibiae agree within 2%. The femora differ by 35%.
- Why it matters. V13352 is the reference for scaling QM V1002 and IVPP V17972. Scaling QM V1002 (femur 109 mm) gives 1.63 kg against S11's femur and 3.98 kg against O'Connor's. The db values for both specimens are Hone et al.'s published masses, so the conflict affects only the cross-checks and the interval of V17972, which uses the tibia (the two sources agree on it).
- Decision. **Unresolved**; needs Xu et al. 2003 (the *M. gui* description, not on disk) or first-hand measurement. S11's value is the more recent direct measurement (JNC), and O'Connor's is flagged as an estimate, but I have not chosen between them.

## 29. NEW: *Sinocalliopteryx* holotype radius, 100.7 or 94 mm

- Evidence. Xing et al. 2012 Table 2: 100.7 mm. Benson 2014 S11: 94, "measured from figures in Ji et al. 2007".
- Decision. Resolved for use. CAGS-IG-T1 is scaled from the holotype using Xing's own Table 2 values for both specimens, so the comparison uses one source's measurements throughout. S11's 94 mm is not used.

## Addition: *Scipionyx* mass statement missed in step 1

Dal Sasso & Maganuco 2011, "Body length and body mass" (PDF p. 177): "the approximate total length of this specimen of Scipionyx did not exceed 50 cm" and "comparison with usual body proportions of small extinct and extant coelurosaurs, including birds, permitted us to tentatively estimate a weight in life of no more than 0.2 kg." This is the authors' estimate, but they give no equation, so the S11 value (220 g, from the specimen's femur) is kept as the point. The authors' bound is in the pair notes. The old tables' 30 g is incompatible with the authors' own estimate (see `step2_mass_estimates.md`).
