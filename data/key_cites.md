# Key citations — KEEP THIS VERY SHORT

A deliberately tiny, curated list: only the citations that carry real argumentative
weight for the paper. This is NOT a bibliography — the full record lives in
`intake_manifest.csv` / `INTEGRATED_SOURCES.md`. Add an entry only if it changes how
we argue or model. Each entry: why it's load-bearing + the quote/fact we'd actually cite.

---

## Zheng et al. 2018 — *Anchiornis* gastric pellets · Sci Rep 8:14217
`pdfs/fossil_cases/x_004_anchiornis_diets.pdf` · doi:10.1038/s41598-018-32202-x

- **Why key:** the only definitively-known gastric **pellets** from a non-avialan theropod. Establishes a *fossil pellet observation mode* — directly relevant to our obstype/preservation-bias framing. Reclassifies our STM0-179 record from `gut_content` → **pellet**.
- **Load-bearing quote (the preservation bias, stated by the authors themselves):**
  > "some kind of bias may have favoured the preservation of fish-bearing pellets over those containing the remains of terrestrial vertebrates, so that the pellet evidence is not really representative of the normal diet of *Anchiornis*."
  (5 of 6 pellets are fish scales; STM0-179 is the lizard-bearing one.)
- **Vocabulary anchor:** the Methods define **gastric pellet** (regurgitated, ~all indigestible) vs **coprolite** (cloacal/anal) vs **gut contents** (still inside stomach/intestine). Our `evidence_type` field should follow these definitions exactly.
- **Facts we use:** STM0-179 pellet = ≥3 lizards (≥2 large + 1 small); *Anchiornis* femur = 71 mm (Table 1) — so its ~1 kg Benson mass is sound, not an error.

---

## Elmore et al. 2024 — barn-owl seabird diet via DNA metabarcoding · J. Heredity 115:432
`pdfs/extant_to_ingest/ai_found/esad068.pdf` · doi:10.1093/jhered/esad068

- **Why key:** introduces a **genetic-marker observation mode** — 12S DNA metabarcoding of gut + cloacal/esophageal swabs. Molecular detection found seabird prey in **20%** of owls vs **2%** by morphology (10× more) — a quantified obstype/detection-bias datum for the observation-process axis.
- **Records:** invasive *Tyto alba* (403 g) eats native Hawaiian seabirds **at/above its own mass** — *Pterodroma sandwichensis* (ESA-endangered Hawaiian Petrel, ~401 g, **PPMR ≈ 0.00**), *Ardenna pacifica*, *Puffinus newelli*, *Sula sula* (1031 g, +0.94 — but taken as chicks), *Anous minutus*, *Bulweria bulwerii* (the one morphological skull). Barn owl = an extreme high-PPMR Aves predator.
- **Caveat:** molecular detection ≠ whole-prey consumption (owls eat muscle of large adult seabirds); adult masses per Raine 2019 except *S. sula* (chicks).

## Engler et al. 2022 — goshawk load-carrying capacity vs prey size · Bird Study
`pdfs/extant_to_ingest/ai_found/Engleretal.2022.pdf` · doi:10.1080/00063657.2022.2103097

- **Why key:** quantifies the **physical ceiling on relative prey size** — flight-muscle maximum load-carrying capacity (MLCC: male 684 g / female 971 g, ~84–96% of body mass) caps transportable prey; *"prey transportation [is] one of the major bottlenecks."* This is the mechanistic grounding for the predator-mass↔max-prey-size boundary that the PPMR-vs-predmass slope reflects (the boundary effect JM flagged).
- **Data:** *Accipiter gentilis* (M 715 g / F 1143 g), 898 breeding-season prey items, urban vs rural Germany; mean prey **299 g urban / 255 g rural**; PPMR ≈ −1.0 to −1.2 (macropredator band). Bird-dominated (pigeons/corvids/starlings).
- **Obstype note:** this is **nest prey-REMAINS** (Merling de Chapa 2020 / Rutz method), NOT gut contents — a remains-mode that over-represents large prey (cf. Collopy 1983). Carry it as `prey_remains`, not gut.
