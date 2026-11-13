# Theropod gut-content mass estimates — review me

_Generated overnight 2026-05-30/31 by `code/x19_fossilMassEstimates.R`. **Every mass below is a COARSE ESTIMATE with an explicit method + range — none are measured.** Goal: expand the theropod gut sample from the 6 already-measurable points so PPMR comparisons have more to work with for the abstract. Review/override anything that looks wrong; values flow into the analyses via `data/fossil_mass_estimates.csv` + `data/theropod_gut_ppmr_estimated.csv`._

## Bottom line
- **Theropod(-ish) gut sample: 6 measured → ~21 points.** With these estimates the gut PPMR spans **ln ≈ −7.6 to +1.4** — from tiny prey (Anchiornis ← ~1 g lizard) through prey at/above the predator's own mass (Coelophysis ← a crocodylomorph its own size; the Fighting Dinosaurs, below).
- For the abstract: theropod gut contents now cover essentially the whole extant macropredator-to-small-prey range, so they can be placed against the per-clade lines (x07/x17) rather than resting on 6 points.

## Estimated cases (predator mass × prey-mass estimate → PPMR)

| case (predator) | prey | predator mass (g) | prey mass est (g) [low–high] | ln PPMR [low–high] | method | confidence |
|---|---|--:|--|--:|---|---|
| **Coelophysis bauri** | Hesperosuchus (crocodylomorph) | 1,565 (Benson) | 1,810 [≈400–8,200] | **+0.15** [−1.3, +1.6] | Campione femur (71.2 mm) | **good** (calibrated) |
| **Scipionyx samniticus** | large lizard (TL 275 mm) | 30 (est.) | 28 [11–71] | −0.05 [−1.0, +0.9] | squamate TL→SVL→mass | low (both ends estimated) |
| **Daspletosaurus** | hadrosaur juv (TL 3.0 m) | 1,375,938 | 108,000 [36k–324k] | −2.54 [−3.6, −1.5] | ornithopod TL cube-law | low–med |
| **Scipionyx** | small lizard (TL 110 mm) | 30 (est.) | 1.8 [0.7–4.6] | −2.80 [−3.7, −1.9] | squamate TL | low |
| **Scipionyx** | fish (TL 45 mm) | 30 (est.) | 0.9 [0.5–1.8] | −3.49 [−4.2, −2.8] | fish W=0.01·L³ | low |
| **Repenomamus** (mammal, incl. for ref) | Psittacosaurus juv (HB 140 mm) | 5,000 | 120 [50–250] | −3.73 [−4.6, −3.0] | coarse | med |
| **Sinosauropteryx** | Zhangheotherium (symmetrodont) | 1,655 | 30 [15–70] | −4.01 [−4.7, −3.2] | Mesozoic-mammal coarse | med |
| **Huadanosaurus** | eutriconodont (gobiconodontid-grade) | 3,700 (Benson) | 60 [25–180] | −4.12 [−5.0, −3.0] | Mesozoic-mammal coarse | low–med |
| **Sinosauropteryx** | Sinobaatar (multituberculate) | 1,652 | 12 [5–30] | −4.93 [−5.8, −4.0] | Mesozoic-mammal coarse | med |
| **Huadanosaurus** | eutherian (Eomaia-grade) | 3,700 (Benson) | 25 [10–60] | −5.00 [−5.9, −4.1] | Mesozoic-mammal coarse | low–med |
| **Compsognathus longipes** | Schoenesmahl (lizard) | 1,602 (Benson) | 9.8 [4–24] | −5.10 [−6.0, −4.2] | squamate femur (18.2 mm) | med |
| **Scipionyx** | fish (TL 25 mm) | 30 (est.) | 0.16 [0.08–0.31] | −5.26 [−5.9, −4.6] | fish W=0.01·L³ | low |
| **Daspletosaurus** | hadrosaur juv (TL 1.1 m) | 1,375,938 | 5,320 [1.8k–16k] | −5.56 [−6.7, −4.5] | ornithopod TL cube-law | low–med |
| **Microraptor gui** | fish (TL ~6 cm) | 764 | 2.2 [1.1–4.3] | −5.87 [−6.6, −5.2] | fish W=0.01·L³ | low |
| **Anchiornis** | lizard (femur 8.0 mm) | 1,648 (Benson) | 0.8 [0.3–2.1] | −7.58 [−8.5, −6.7] | squamate femur | med |

_(low/high in PPMR propagate the **prey-mass** uncertainty; for Scipionyx the predator mass is also estimated, so true uncertainty is wider.)_

## Predator masses estimated (no prior value)
| predator | mass (g) [low–high] | method |
|---|--|---|
| Huadanosaurus sinensis | 3,700 [1,050–13,100] | Benson femur 105.69 mm, R²=0.97 (`theropoda_benson` is in **kg** — ×1000) |
| Confractosuchus (croc) | 40,000 [25,000–80,000] | coarse, 2.5 m gracile crocodyliform (no femur for Campione) |
| Scipionyx (perinate) | 30 [15–60] | coarse hatchling, TL ~220 mm |

## Methods & reliability
- **Calibrated (most trustworthy):** *Benson* theropod femur→mass (n=381, R²=0.97) and *Campione & Evans 2012* quadruped femur→mass (n=242, R²=0.93), both fit from local data. Used where a femur length exists (Huadanosaurus predator; Compsognathus & Anchiornis lizard prey; Coelophysis's Hesperosuchus).
- **Coarse scalings (treat as order-of-magnitude):**
  - fish: W(g)=0.01·TL_cm³ (Fulton K≈1), range K 0.005–0.02.
  - squamate: femur→SVL (SVL≈femur/0.21) then mass≈1.5e−5·SVL³; or TL→SVL≈0.45·TL.
  - Mesozoic mammal: reasoned from jaw size + the small body sizes typical of Jehol symmetrodonts/multituberculates/eutherians.
  - ornithopod: mass(kg)≈4·(TL_m)³, anchored to juvenile-ornithopod cube scaling.
- **⚠ Caveats:** squamate/Mesozoic-mammal/ornithopod scalings are NOT from the source taxa and carry large error — flagged "low/med" above. The two **Scipionyx** size pairs (fish vs lizard) and the **Daspletosaurus** hadrosaurs hinge on a single linear dimension and a generic scaling; widen the ranges in your head. The **best** estimates are the femur-based ones (Benson/Campione).

## The Fighting Dinosaurs (added separately, distinct evidence type)
Velociraptor (10–20 kg) vs Protoceratops (44–82 kg), Djadokhta Fm, ~75 Ma. **PPMR ≈ +1.4 [+0.8, +2.1] — prey MUCH larger than predator.** Recorded as an **attack/predation event (Follow-type, NOT gut)** in `fossil_diet_extractions.csv` and plotted with a distinct symbol. Note its extreme taphonomic bias: a *fossilized rare attack-type* (predator vs much-larger prey) is exceptionally over-represented relative to its true frequency — a striking comparison point, not a typical one.

## Most worth verifying (highest impact on the abstract)
1. **The two femur-based dino/croc points** — Coelophysis←Hesperosuchus (+0.15) and Huadanosaurus's predator mass — are the most defensible; sanity-check against published estimates.
2. **Daspletosaurus hadrosaurs** — these set the large-predator end; a real ornithopod femur-circumference (Campione) would tighten them a lot.
3. **Sinosauropteryx mammals & Compsognathus/Anchiornis lizards** — the classic "lizard/mammal in a belly" cases; published mass estimates likely exist (e.g. for Zhangheotherium) and would beat my coarse scalings.

## What would make these real (pending refs, see `external_data_wanted.md`)
- Squamate SVL/femur→mass (Meiri 2010 / Feldman et al. 2016) and a Mesozoic-mammal jaw→mass relationship — to replace the coarse scalings.
- Fish prey identified to family → FishBase a,b (`x16`) instead of the generic cube law.
