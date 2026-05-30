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

## How to use this file

When you (or future-you) verify an item here:

- If you confirm a correction, edit the source CSV and append a line under that item like `Resolved YYYY-MM-DD: source X reports value Y; row N updated.`
- If a question turns out to be a non-issue, append `No discrepancy: explanation.` and strike (`~~`) the item.
- New issues should follow the same template: comparison table, candidate explanations, recommended action.
