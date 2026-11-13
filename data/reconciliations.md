# Data reconciliations

Issues found while extracting fossil predator-prey data that warrant verification before downstream analysis. Each entry has the measurement comparison, the most likely cause, and the action to take.

---

## NIGP 127587 — *Sinosauropteryx prima* skeletal measurements [RESOLVED 2026-05-28]

**Resolution**: column-swap hypothesis confirmed by the user. Edits applied to `Diets/Fossil Diets/fossil_predprey.csv` row 11:
- `femur_l`: 97.15 → **86.4** (Chen et al. 1998 *Nature* Table 1)
- `mass_pred`: 992 → **blank** (was derived from the wrong femur length; needs Benson recompute from FL=86.4)
- `mass_source`: `benson` → `benson_RECOMPUTE_from_FL_86.4` (loud flag so analysis re-runs catch it)
- `source`: `Smithwicketal_2017` → `Chen_et_al_1998_Nature_femur+Smithwicketal_2017_other`

**Schema note for future redesign**: Chen 1998 also reports SKULL length 97.2 mm and TIBIA 97 mm for this specimen. The current schema has no `skull_l` column and the `dentary` column is for the lower jaw specifically (e.g., row 19 Daspletosaurus dentary = 609 mm), so the skull value was not added. When the fossil schema is unified (see project reorganization), add `pred_skull_l_mm` and backfill 97.2 here.

**Rough mass estimate pending proper Benson recompute**: under Benson allometry (~FL^2.74), the original 992 g at FL=97.15 would scale to roughly **~715 g** at FL=86.4. Use the actual Benson 2014 / 2018 femur-length equation for the production value.

---

## ORIGINAL DIAGNOSTIC (kept for the record)

### NIGP 127587 — *Sinosauropteryx prima* skeletal measurements

**Discrepancy first noticed:** during extraction of `005_sinosauropteryxgut.pdf` (Chen, Dong & Zhen 1998 *Nature*, the original description) we compared its Table 1 against the row already in `Diets/Fossil Diets/fossil_predprey.csv` (line 11, source `Smithwicketal_2017`) for the same specimen.

| Element | Chen et al. 1998 *Nature* (Table 1) | CSV row 11 (Smithwick et al. 2017) | Δ (mm) | Δ (%) |
|---|---|---|---|---|
| Skull length | **97.2** | (not in CSV; `dentary` blank) | — | — |
| Humerus length | 35.5 | 36.06 | +0.56 | +1.6% |
| Radius length | **21** | **30.52** | +9.52 | **+45%** |
| Femur length | **86.4** | **97.15** | +10.75 | **+12%** |
| Tibia length | 97 | 105.67 (`tibio_l`) | +8.67 | +8.9% |

**The pattern:** humerus matches to within rounding, but radius, femur, and tibia in the CSV are systematically larger than Chen 1998 — and the femur value `97.15` is **suspiciously close to Chen's skull length of 97.2 mm** (Δ = 0.05 mm).

**Two candidate explanations:**

1. **Column-swap during extraction**: when row 11 was compiled, the *skull* length from Smithwick 2017 (which presumably matches Chen's 97.2 mm) was entered into the `femur_l` cell. This is consistent with the femur value being a near-perfect match to the skull length, and with the `dentary` column being blank for this row. The other slightly elevated values (radius, tibia) would then be genuinely re-measured by Smithwick from photos and only the femur is the mistake.
2. **Different measurement convention**: Smithwick et al. 2017 (*Current Biology* 27:3337-3343, "Countershading and stripes in the theropod dinosaur Sinosauropteryx") may have measured long bones including epiphyseal cartilage outlines visible in the slab, producing systematically larger values. This would explain the uniform +9–11 mm offsets for femur and tibia. But it does not explain a +45% jump in radius alone, nor the near-identity of the new femur value with Chen's skull.

**Recommendation (in order of confidence):**

- **Most likely**: row 11 has a column-swap; `femur_l = 97.15` should be `skull_l ≈ 97.2`. The true femur length is **86.4 mm** (Chen 1998). To fix:
  ```
  fossil_predprey.csv row 11:
    femur_l    : 97.15  ->  86.4
    (skull or dentary): ""  ->  97.2
  ```
- **Verify before editing**: open `Smithwick et al. 2017, Curr Biol 27:3337-3343`, supplementary tables, and read the actual NIGP 127587 femur length. If Smithwick reports 86.4 mm, the column-swap hypothesis is confirmed. If Smithwick reports ~97 mm, the measurement-convention hypothesis stands and the original Chen value may need annotation.
- **Downstream impact**: NIGP 127587 was used with a Benson-style femur-length → mass regression (`mass_source = brms_benson`). A femur of 86.4 vs 97.15 mm changes the estimated mass by roughly a factor of (97.15/86.4)^2.74 ≈ 1.39 — so a current entry of 992 g would drop to ~715 g if the lower value is correct. Worth knowing before fits depend on it.

---

## Confractosuchus sauroktonos (AODF0890) — predator mass not estimable from paper [OPEN 2026-05-29]

White et al. 2022 (Gondwana Research 106:281-302) describe a Cenomanian crocodyliform with a partially-digested juvenile ornithopod in its abdomen (fossil_diet_extractions.csv ref_id 14, `White_etal_2022_Confractosuchus_gut.pdf`).

- **Prey mass is well-constrained**: ornithopod **1.0–1.7 kg** via the Campione et al. 2014 *bipedal* formula (stated in the paper, p.299). Recorded as prey_mass_g=1350 (low 1000, high 1700).
- **Predator mass is NOT given.** The paper reports skull length 285 mm, skull width 190 mm, and an estimated total length **~2.5 m** (sub-adult), but no body mass. The femur is broken/incomplete, so the Campione & Evans 2012 quadruped limb-circumference method cannot be applied directly.
- **Action**: estimate Confractosuchus mass from total length (~2.5 m) and/or skull length (285 mm) using a defensible crocodyliform regression, with explicit uncertainty bounds, before this case can yield a PPMR point. Flagged in `pred_mass_method = "PENDING_croc_regression..."`. Until then it sits in the mass-pending queue alongside the 10 length-only theropod-prey cases (Compsognathus, Coelophysis, Sinosauropteryx, Daspletosaurus, Velociraptor/Protoceratops, Scipionyx).
- Note: this is a high-value croc-line case (crocs are a key archosaur bracket and gut contents are rare — only the 2nd crocodyliform gut content known, cf. Godoy et al. 2014 baurusuchid = ref_id 10).

---

## Sanajeh indicus (ref_id 15) — added from literature, PDF NOT in repo [OPEN 2026-05-29]

Wilson et al. 2010 (PLoS Biol 8(3):e1000322) — basal snake preserved coiled around a titanosaur egg clutch + a ~0.5 m hatchling (Lameta Fm, Maastrichtian, Gujarat). Added to `fossil_diet_extractions.csv` as ref_id 15 **but the PDF is not in the repo**, so only well-established facts were recorded; masses and the snake's total length are **PENDING** (not transcribed from memory — cf. the NIGP 127587 lesson).

- **Evidence class**: `association_predation`, **NOT a gut content** — the hatchling was adjacent/associated, not ingested/digested. So it should NOT enter the gut-to-gut comparison as a gut sample; it is a predator–prey *size* record (and a high-PPMR snake case).
- **Action**: drop `Wilson et al. 2010 PLoS Biol` PDF into `pdfs/fossil_cases/`, then extract: snake length/mass, hatchling femur→mass (Campione), egg dimensions. Verify the ~0.5 m hatchling figure and the snake size estimate.
- Pairs with the new modern colubrid gut data (Serpentes are the highest-PPMR extant clade) as the fossil-snake vs modern-snake comparison JM requested.

---

## theropoda_benson.csv — masses are in KILOGRAMS, not grams [NOTE 2026-05-31]

`data/mass_references/theropoda_benson.csv` `mass` column is in **kg** (e.g. Brachiosaurus_altithorax = 56000 = 56 t; Gyposaurus 38 kg). The fossil tables (`fossil_predprey.csv`, `fossil_diet_extractions.csv`) and `longform.csv` use **grams**. When converting femur → mass for fossil predators/prey, **multiply the Benson estimate by 1000**.

Fitted femur→mass (for filling PENDING / length-only cases): `ln(mass_kg) = -13.0 + 3.07 * ln(femur_mm)` (lm on n=381 Benson rows, R²=0.97). E.g. Huadanosaurus femur 105.69 mm → ~3.7 kg (3700 g) [95% PI ~1–13 kg]. Carry the prediction interval to propagate mass uncertainty.

---

## Scipionyx samniticus (SALERNO) — total length was in the mass column [RESOLVED 2026-06-01]

`fossil_predprey.csv` rows 20–23 (4 gut items: 2 fish TL 45/25 mm, 2 lizards TL 110/275 mm; Dal Sasso & Maganuco 2011) had the predator's **220 mm total length entered into `mass_pred`**, mislabeled `mass_source = benson` — but this perinate has no femur, so Benson was never applicable. The 220 looked like a plausible small-theropod mass (g), which is exactly why it hid.

- **Fix applied** (all 4 rows): `total_length` blank → **220**; `mass_pred` 220 → **30** (x19 `hatchling_coarse`, ~15–60 g from TL 220 mm); `mass_source` `benson` → `hatchling_coarse_TL220mm_~15-60g_see_x19`.
- No downstream effect on PPMR (these rows have `mass_prey` blank → already dropped by x07; x19 is the source of truth for Scipionyx's 4 estimated points). This was a record-cleanliness fix.
- Lesson for the audit: a length-in-mass error is NOT caught by "mass==total_length" when the length sits *only* in the mass cell. Better detector: `mass_source` claims a femur method (benson/campione) while `femur_l` is blank.

---

## Sinosauropteryx — two spellings of one species [RESOLVED 2026-06-01]

`fossil_predprey.csv` carried both **`Sinosauropteryx_primus`** (row 11, NIGP-127587, femur 86.4, eating *Dalinghosaurus*) and **`Sinosauropteryx_prima`** (femur 93.07 rows). The valid epithet is *prima*; *primus* is a typo (wrong gender ending). The x12 `age()` lookup had been carrying BOTH spellings as a workaround.

- **Fix applied**: row 11 `Sinosauropteryx_primus` → `Sinosauropteryx_prima`. (The duplicate `primus` key in x12 is now dead/harmless; can be removed on next x12 edit.)
- Its `mass_pred` remains blank — that is the SEPARATE, correctly-flagged `benson_RECOMPUTE_from_FL_86.4` pending item (see NIGP 127587 above), not a new error.

---

## Sinocalliopteryx gigas — `brms_benson` mass with no femur [RESOLVED 2026-06-01]

**Resolution**: JM supplied the missing femur — **CAGS-IG-T1 femur = 210 mm**. Recomputed mass via the project lm-Benson (x19 fitB): **30,471 g (~30.5 kg) [95% PI 8.6–108 kg]**, replacing the unsupported ~55–56 kg. Applied to both CAGS-IG-T1 rows (13, 14; same specimen, two prey — also fixes that they previously carried two different masses, 55,570 vs 56,408). `femur_l` blank → 210; `mass_pred` → 30471; `mass_sd` cleared; `mass_source` → `benson_lm_FL210mm_was55/56kg_no_femur`. Note: the holotype row (JMP-V-05-8-01, femur 152.3) still carries the legacy `brms_benson` 8,317 g (lm-Benson would give 11,367 g for that femur — a ~0.73× brms-vs-lm gap); harmonize the genus to one Benson variant in a future pass.

---

### (original flag, kept for the record)

`fossil_predprey.csv` has two `Sinocalliopteryx_gigas` rows with **`mass_pred` ≈ 55,570 / 56,408 g (~55–56 kg)** and `mass_source = brms_benson`, but **`femur_l` is blank** — Benson cannot have been run without a femur, so the source label and the value are internally inconsistent. (Two other Sinocalliopteryx rows DO have femur 152.3 mm → 8,317 g, which is consistent.) Likely either (a) the holotype's femur length is missing from the record and ~55 kg is a real estimate from it, or (b) the mass came from a different method/dimension and the source is mislabeled, or (c) a transcription error.

- **Action (needs JM)**: confirm the holotype *Sinocalliopteryx* femur length (Ji et al. 2007 / Xing et al. 2012) and the basis for the ~55 kg figure; then either fill `femur_l` + keep the mass, or correct `mass_source` to the true method. Until resolved, treat these two rows' predator mass as UNVERIFIED.

---

## Sinocalliopteryx gigas × cf Psittacosaurus — record removed [RESOLVED 2026-06-01]

`fossil_predprey.csv` had a CAGS-IG-T1 prey "cf Psittacosaurus" (Ornithischia, ~10.6 kg from a 135 mm scapula, `scaled_from_IVPP_12_0888_2`) attributed to Xing et al. 2012 — but that paper documents only **confuciusornithids + a dromaeosaurid** for Sinocalliopteryx; no ornithischian. JM confirmed no source. **Row removed.** Sinocalliopteryx retains its real prey: Sinornithosaurus (holotype) + Confuciusornis (CAGS-IG-T1).

---

## fossil_predprey.csv `mass_sd` column is CORRUPT [NOTE 2026-06-01]

The `mass_sd` values cluster at ~1565–1655 g for EVERY row regardless of `mass_pred` (764 → "sd 1606", 11680 → "sd 1565", 8317 → "sd 1647", 924 → "sd 1648"). A ~constant ~1600 SD across a 15× mass range is not a real SD — it's a misaligned/pasted artifact. **Consequence**: x19 read Anchiornis's bogus `mass_sd`=1648 as the predator MASS (true `mass_pred`=924, itself too high — see below). **Action**: discard this column in consolidation; recompute masses + proper prediction intervals from femur via lm-Benson.

---

## Anchiornis (STM0-179, Zheng et al. 2018) — reclassify gut→PELLET [RESOLVED 2026-06-01, source PDF read]

Source `pdfs/fossil_cases/x_004_anchiornis_diets.pdf` READ. **STM0-179 is a regurgitated GASTRIC PELLET, not gut contents** — reclassify `evidence_type` gut_content → **pellet** (distinct fossil obstype; see `key_cites.md`). JM: keep it as a PPMR point (a novel fossil obstype is an asset).

Two of my earlier alarms were WRONG and are retracted (the source fixed them):
- **Femur 71 mm is CORRECT** — Zheng Table 1 lists STM0-179 femur = 71 mm (STM0-224 = 70 mm); these are genuinely larger *Anchiornis* individuals. So Benson's ~0.9–1.1 kg from a 71 mm femur is SOUND, not "4–9× too large." My ~0.11 kg figure was a smaller/different specimen, mis-applied from memory.
- Therefore **no mass-magnitude error**. The real issues were only: (a) obstype gut→pellet; (b) the corrupt `mass_sd`=1648 that x19 misread as the predator MASS (use lm-Benson(71 mm) instead).

Prey = Squamata indet, ≥3 lizards ("at least two relatively large lizards and one small lizard"). **Action in consolidation**: obstype=pellet, predator mass = lm-Benson(71 mm), distinct symbol, kept as a PPMR point.

---

## Velociraptor × Protoceratops (Fighting Dinosaurs) — double-entered across files [OPEN 2026-06-01]

Present in BOTH `fossil_predprey.csv` row 18 (Carpenter 2000; mass 18840 g, prey Protoceratops 1250 mm TL, no prey mass) AND `fossil_diet_extractions.csv` ref 18 (the attack case; Velociraptor 10–20 kg, Protoceratops 44–82 kg, PPMR +1.44). Same event, two files, different mass conventions. **Action**: keep ONE (the diet_extractions version is the curated attack record with masses) during consolidation; drop/archive the predprey duplicate.

---

## How to use this file

When you (or future-you) verify an item here:

- If you confirm a correction, edit the source CSV and append a line under that item like `Resolved YYYY-MM-DD: source X reports value Y; row N updated.`
- If a question turns out to be a non-issue, append `No discrepancy: explanation.` and strike (`~~`) the item.
- New issues should follow the same template: comparison table, candidate explanations, recommended action.
