# Data organization, cleaning & flow audit — 2026-06-01

Triggered by accumulating conflicts during fossil/extant data entry. Goal: a single,
clear, restart-safe data flow before the next model run. Pairs with `reconciliations.md`
(per-record fixes) — this file is the STRUCTURAL view.

---

## 1. Flow map (what feeds what)

```
raw_data.csv ──x00──> longform.csv (171,315 extant prey items)         ┐
                          └─ x20 uses obstype=="Stomach" subset          │
AVONET + AvianDietDB ──x14/x15──> aviandiet_gut_ppmr.csv (214 bird gut) ─┤──> x20 (extant)
                                                                          │
extant_new_observations.csv (74 manual obs) ──x11 ONLY (sources doc)     ┘   ⚠ ORPHAN: not in any model

FOSSILS — three overlapping sources, no single source of truth:
  fossil_predprey.csv         (23, ALL Theropoda, raw meas + masses; legacy brms_benson)  ┐
  fossil_diet_extractions.csv (19, multi-clade, newer; mixed PENDING/filled)              ├─ x07 ─> fossil_gut_points.csv (12) ─┐
  x19 ─> fossil_mass_estimates.csv (18) + theropod_gut_ppmr_estimated.csv (15, coarse) ───┘                                     ├─> x20 (fossil overlay)
                                                              x19 output ─────────────────────────────────────────────────────┘
predator_traits.csv (168, foraging/sociality/habitat) ── not yet joined to any fit
```

**The crux:** x20 ingests fossils from TWO paths — `fossil_gut_points.csv` (via x07, built from predprey + diet_extractions) AND `theropod_gut_ppmr_estimated.csv` (x19). Dedup is *implicit* (x20's Theropoda-only filter on path 1 + a hard-coded `Repenomamus_gut` exclusion on path 2). It works today by luck, not design.

---

## 2. Conflict / issue register (concrete, 2026-06-01)

| # | Issue | Detail | Status |
|---|---|---|---|
| 1 | **Anchiornis predator mass conflict** | `fossil_predprey`=924 g vs `x19`=1648 g (~1.8×). x20 uses x19 (predprey row lacks prey mass → dropped), so 924 is unused — but the files disagree. | needs reconcile |
| 2 | **Repenomamus×Psittacosaurus (Hu 2005) prey mass conflict** | `fossil_diet_extractions` ref 1 = 40 g (new, TL140) vs `x19` `Repenomamus_gut` = 120 g (old coarse). Same real event. x20 uses the extraction (x19 Repenomamus excluded), but x19's CSV still carries stale 120 g. | needs reconcile |
| 3 | **Mass-METHOD inconsistency** | `fossil_predprey` legacy `brms_benson` vs `x19` lm-Benson (`fitB`). Systematic ~0.73× gap (Sinocalliopteryx 152.3 mm: brms 8317 g vs lm 11367 g; Compsognathus 1567 vs 1602). | standardize on one |
| 4 | **Unit trap** | `mass_references/theropoda_benson.csv` is in **kg**; every other table is **grams** (×1000). | document + guard |
| 5 | **Suspect record: Sinocalliopteryx × cf Psittacosaurus** | `fossil_predprey` row 14: ornithischian prey (~10.6 kg, scapula 135 mm) attributed to Xing et al. 2012 — but that paper documents only confuciusornithids + a dromaeosaurid. Likely an extraction error. | AWAIT JM confirm |
| 6 | **Orphaned curated data** | `extant_new_observations.csv` (74 obs; 15 gut-mode with both masses: Elaphe, Lampropeltis→Serpentes; Chunga→Aves) read only by x11, **not the model**. | wire into x20 |
| 7 | **No automated validation** | The Scipionyx length-in-mass and Sinosauropteryx spelling errors hid until found by hand. | build x_validate.R |
| 8 | **Fragile dual-path fossil ingestion** | x20 reads fossils from two files with implicit dedup; a new taxon entered in two places will double-count or pull a wrong mass. | single source of truth |

(Resolved this session, logged in `reconciliations.md`: Scipionyx length-in-mass; Sinosauropteryx primus→prima; Sinocalliopteryx no-femur→FL 210.)

---

## 3. Proposed consolidation (NEEDS JM SIGN-OFF before executing)

1. **One curated fossil record + one analysis-input file.**
   - `fossil_diet_extractions.csv` = the MASTER hand-curated record (multi-clade, all evidence types, provenance). Migrate the genuinely-unique `fossil_predprey.csv` rows into it; retire `fossil_predprey.csv` (or keep strictly as raw-measurement archive, NOT read by analysis).
   - `x19` stays the mass-ESTIMATION layer for length-only cases, but writes estimates that are RECONCILED against the curated masses (no taxon with two different masses).
   - One builder produces a single **`fossil_ppmr_points.csv`** (one row per predator–prey gut/diet item; columns: predator, prey, clade, masses+CrI, evidence_type, obs_type, provenance). **Both x07 and x20 read only this.** Kills the dual path.

2. **Resolve the live conflicts** (#1–#5): pick femur-based masses, standardize on lm-Benson (recompute legacy brms rows once), grams everywhere, remove/justify cf Psittacosaurus.

3. **Wire `extant_new_observations.csv` gut subset into the extant data** (#6): filter obs_type∈{Stomach,Gizzard,Gut} with both masses; map pred_class/order→clade (Aves; Mammalia; Crocodylia; Squamata→Serpentes/Lacertilia via genus); union into x20's extant data.

4. **Build `x_validate.R`** (#7): run before any fit; checks = femur-method-without-femur, length-in-mass heuristics, kg-vs-g sanity, cross-file duplicate-taxon mass agreement, clade-vocabulary consistency, orphaned curated rows, PPMR range sanity. Prints a pass/fail report.

5. **Document** the finalized flow here + in the pipeline memory.

---

## 4. Open decisions for JM
- **(a)** Sinocalliopteryx × cf Psittacosaurus — remove the prey record? (strong suspicion it's an extraction error)
- **(b)** Retire `fossil_predprey.csv` into the extractions master, or keep it as a raw archive?
- **(c)** Standardize all fossil masses on lm-Benson (recompute the ~6 legacy brms_benson rows)? — yes recommended for consistency.
- **(d)** Anchiornis: keep x19's 1648 g (femur-based) and drop predprey's 924 g? (need to know predprey's 924 basis)
