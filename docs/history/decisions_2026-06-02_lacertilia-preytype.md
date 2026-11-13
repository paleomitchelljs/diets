# The Lacertilia problem — options, appraisal, synthesis, and what ran overnight

*Written 2026-06-02 (evening) for morning review. Autonomous work per your request: "draft options for
Lacertilia, critically appraise then steelman then synthesize against your approach, then implement it and run
overnight."*

---

## 1. The problem, stated precisely

Across every refit, Lacertilia (lizards) was the persistent misfit. With the clade-fixed model on the
re-curated data, the obstype-adjusted clade gap sat at roughly **+1.3 to +1.5** — the Lacertilia line predicted
*smaller* prey than the data carried — while every other clade sat near zero. Re-curating the certainty weights
barely moved it (−1.32 → −1.34 across that change), which ruled out weighting as the cause. The subsampler
rebuild ruled out single-study domination. So the gap is **structural**, not a data-hygiene artifact.

Two things were tangled together inside that one number:

1. **Lacertilia diet is genuinely bimodal.** Most lizards are insectivores (tiny invertebrate prey); a minority
   are saurophagous/vertebrate-eating (prey near or above their own mass). A single Gaussian line through both
   modes fits neither and lands in the empty middle.

2. **The fossils are an apples-to-oranges comparison.** Every fossil we place ate *vertebrates* (theropod gut =
   mammal/lizard/bird; *Repenomamus* = baby dinosaur; the snakes/crocs = vertebrate prey). But the extant
   reference — Lacertilia especially — is dominated by *invertebrate* prey. Placing a vertebrate-eating fossil
   against an invertebrate-dominated extant line is not a like-for-like test.

These are the same defect viewed twice: **prey type (invertebrate vs vertebrate) is an omitted variable.**

### A discovery while implementing this

Deriving prey type from `raw_data` row 2 (the prey *class* string, e.g. *Actinopterygii*) surfaced something I
had not expected: a large block of "Lacertilia prey" is **not predation at all**. ~3,500 Squamata prey rows —
dominated by *Salvator merianae* (Argentine tegu, 2,872 rows) and *Varanus* spp. — are **PLANT / fruit** items
(plus a few eggs and a handful of vertebrate classes my first regex missed). These are **omnivores eating
fruit**, and that frugivory was being scored as "prey" with a plant mass, inflating the Lacertilia prey
distribution. Frugivory is not predation and has no business in a predator–prey mass model. Catching this is a
data-quality win independent of the modelling choice, and it is now stripped from *every* variant (see §5).

After stripping plant items, the real Lacertilia split is **2,949 invertebrate-prey rows vs 4,728
vertebrate-prey rows** — unambiguously bimodal. Serpentes (snakes), as expected, is essentially all vertebrate.

---

## 2. The options

### Option A — `obstype : clade` interaction (pure model fix)
Let each clade carry its own observation-process bias (a Lacertilia-specific Scat/Follow shift), rather than one
shared obstype effect.

- **Appraisal.** This is the minimal, mechanism-agnostic move. But it targets the *wrong* axis. The Lacertilia
  gap is not primarily an observation-mode artifact — it is diet composition. I tested this overnight (variant
  `_voc`): with `obstype:clade` and **no** prey_type, the Lacertilia vert-prey gap was still **+1.63**. Letting
  observation bias flex per clade does not fix a bimodal diet. It also spends parameters on near-empty cells
  (Crocodylia has only Stomach; Serpentes Follow = 1 row).
- **Steelman.** Observation bias *is* plausibly clade-specific (lizard scat over-represents chitinous
  invertebrates differently than mammal scat), and this is the move that requires no new data column — it is
  defensible as a conservative, model-only response. If prey-type had turned out to be unavailable or unreliable,
  this would be the fallback.

### Option B — `prey_type` (invertebrate / vertebrate) covariate  ← my synthesis
Add prey type as a covariate in mean and sigma; place fossils at the vertebrate-prey level.

- **Appraisal.** This is the only option that names the actual mechanism. It (i) separates the two diet modes so
  each is fit on its own data, (ii) makes fossil placement apples-to-apples (vertebrate-eating fossils vs
  vertebrate-eating extant predators of the same size), and (iii) — as a bonus — gave the hook that exposed the
  frugivory contamination. Cost: it needs a prey-type label on every row. That turned out to be cheanly derivable
  (`raw_data` row 2 for longform; `prey_class` for ADB and the new observations).
- **Steelman of the objection.** "Invertebrate vs vertebrate" is a coarse binary; a beetle and a crayfish differ,
  as do a frog and a deer. And for *display* PPMR you arguably want the whole realized diet, invertebrates
  included. Both true — but the fossil question is specifically about vertebrate predation, so conditioning on
  vertebrate prey is the honest comparison, and the binary captures the dominant bimodality that the finer
  splits would only refine.

### Option C — split Lacertilia into insectivore / carnivore guilds (taxonomic)
Hand-assign each lizard species to a guild and treat them as separate clades.

- **Appraisal.** This *encodes the answer as an assumption.* It requires a subjective per-species guild call,
  fails for the many species that are facultatively both, and bakes in exactly the structure we should be
  estimating. Option B recovers the same separation **from the data** (a species' prey-type mix) without the
  hand-labelling.
- **Steelman.** For a small, well-known fauna with clean guild membership it would be transparent and easy to
  explain, and it sidesteps any prey-mass measurement error by working at the species level. But our taxon set is
  neither small nor clean.

### Option D — accept the gap / drop Lacertilia
Report Lacertilia as poorly-fit, or exclude it.

- **Appraisal.** Lacertilia is the most fossil-relevant extant analog for small theropods (small, vertebrate-
  capable, sprawling-to-erect range). Dropping it discards the comparison we most want. "Accept the gap" is only
  honest if the gap is irreducible — and §1 shows it is not; it is an omitted variable.
- **Steelman.** If every modelled fix had left the gap intact, the honest scientific move would be to *report*
  it as a real biological signal (lizards' bimodal diet defeats a single scaling line) rather than paper over it.
  That remains the fallback if the overnight results disappoint.

---

## 3. Synthesis (what I chose and why)

**Adopt Option B (`prey_type`), prefer the `prey_type * clade` form, strip non-animal "prey" from all
variants, and place fossils at the vertebrate-prey level.** Reasoning:

- It is the only option that addresses the *cause* (diet composition) rather than a symptom or a proxy axis
  (Option A) — confirmed by `_voc` still showing +1.63.
- `prey_type * clade` rather than a main effect, because the invert/vert gap is **not** uniform across clades:
  Lacertilia's invertebrate prey is far smaller relative to its vertebrate prey than, say, a bird's. A single
  global shift (`_vpt`, main effect) left the Lacertilia vert gap at ~+1.1 in smoke testing; only the
  clade-specific interaction lets Lacertilia have its own large gap while Serpentes (all-vert) has none.
- It fixes the fossil comparison directly: with `prey_type` referenced to **vert**, the per-clade lines and the
  predictive bands x07 places fossils against are the *vertebrate-prey* lines.

**A correction to how the "gap" should be read.** The population-line gap (line with random effects stripped vs
the data mean) is a *misleading* diagnostic once `(1|species)` is in the model: vertebrate-prey Lacertilia rows
come from the high-random-effect saurophagous species, so the population *average* line sits below them by
construction — even when the model is fine. That is not how x07 places fossils. x07 predicts a **new** species,
integrating the species random-effect spread. So I added the fossil-relevant diagnostic: the percentile of the
observed vertebrate-prey median inside each clade's posterior-*predictive* (new species, vert, Stomach). In smoke
testing that diagnostic read **Lacertilia 0.34** — i.e. the vertebrate-prey predictive band comfortably contains
the saurophagy data, slightly above its centre. The "+1.08 gap" was an artifact of the wrong yardstick. **This is
the single most important interpretive point for the morning:** judge the fit by the fossil-relevant predictive
percentile (target ~0.5), not by the population-line gap.

---

## 4. What is running overnight

`code/run_lacertilia_overnight.sh` (launched detached under `caffeinate`) fits **four variants** at full
sampling (4 chains × 2,500 iter, cap 500/cell), all on the plant-stripped data:

| suffix  | `GUT_PREYTYPE` | `GUT_OBSCLADE` | what it isolates |
|---------|----------------|----------------|------------------|
| `_v0`   | 0              | 0              | baseline (plant-stripped only) — the reference |
| `_vpt`  | main           | 0              | global vert/invert shift |
| `_vptx` | clade          | 0              | **synthesis** — clade-specific vert/invert gap |
| `_voc`  | 0              | 1              | Option A — clade-specific observation bias, no prey_type |

Then `code/x22_compareVariants.R` LOO-compares the four and tabulates per-clade slopes/breadth; the script
**promotes `_vptx` to the default** (`fit_gutslopes_cfix.rds` + meta) and runs `code/x07_fossilPlacement.R` so the
morning figures reflect the synthesis. If you disagree with the LOO verdict, switching the promoted variant is a
one-line `cp` + re-run of x07.

### How to read the results in the morning
1. **`logs/lacertilia_overnight_*.log`** — per-variant: per-clade slopes, the population-line gap (now labelled
   as the *non*-fossil-relevant diagnostic), and the **fossil-relevant predictive percentile** (the one to trust,
   target ~0.5). Confirm `_vptx` Lacertilia percentile is near 0.5 and the slopes are sane (~0.8–1.0).
2. **`data/lacertilia_loo_compare.csv`** — does `_vptx` win, and by more than ~2× its se? Expect `_vptx` ≳ `_vpt`
   ≳ `_v0`, and `_voc` to *not* fix Lacertilia.
3. **`data/lacertilia_slope_compare.csv`** — do the clade slopes stay stable across variants (robustness) while
   Lacertilia's intercept/breadth shift as prey_type is added?
4. **`figures/` (default suffix)** — the x07 figures on the synthesis fit: `fig_fossil_placement`,
   `fig_fossil_percentile`, `fig_clade_panels`, `fig_ppmr_vs_time`, `fig_theropod_similarity`, plus
   `data/theropod_best_match.csv`.

### Caveats I want flagged, not buried
- The invert/vert binary is coarse; it captures the dominant bimodality but not within-type structure.
- Squamate prey masses derive from the SVL³ relation; absolute masses for robust-bodied teiids may be
  underestimated, though PPMR = 3·ln(SVL ratio) is robust to that (your earlier point).
- `_voc` may still post a respectable LOO (clade-specific observation bias is real) without fixing Lacertilia —
  that is the point of running it: it separates "improves predictive fit somewhere" from "fixes the lizard gap."
- LOO with the `weights(certainty)` likelihood can throw high Pareto-k; if so, treat the LOO ranking as
  directional and lean on the fossil-relevant percentile + slope stability instead.

---

## 5. RESULTS (overnight run completed 2026-06-02 22:03; clean — no divergences, no R-hat warnings)

### LOO — decisive for the synthesis
| variant | elpd_diff | se_diff |
|---|---:|---:|
| **prey_type × clade** | **0.0** | 0.0 |
| prey_type (main) | −29.0 | 6.2 |
| V0 baseline | −280.6 | 25.9 |
| obstype : clade | −281.1 | 25.9 |

- `prey_type*clade` beats baseline by **280.6 (se 25.9) ≈ 11 se** — overwhelming.
- It beats the global main effect by **29.0 (se 6.2) ≈ 4.7 se** — the *clade-specific* invert/vert gap is real and
  the synthesis choice (×clade over main) is vindicated, not just my prior.
- **`obstype:clade` (Option A) is statistically tied with baseline** (−281.1 vs −280.6, within 1 se). Letting
  observation bias flex per clade buys **nothing** in predictive fit. The prey-type axis explains ~280 elpd; the
  observation-bias axis explains ~0. The diagnosis — diet bimodality, not method — is confirmed out-of-sample.

### The slope correction (the biological payoff)
| clade | V0 | prey_type(main) | **prey_type×clade** | obstype:clade |
|---|---:|---:|---:|---:|
| Aves | 1.077 | 1.045 | 1.055 | 1.089 |
| Crocodylia | 0.215 | 0.072 | **0.071** | 0.219 |
| **Lacertilia** | **0.595** | 0.918 | **0.886** | 0.656 |
| Mammalia | 0.824 | 0.899 | 0.914 | 0.786 |
| Serpentes | 0.712 | 0.707 | 0.707 | 0.704 |

- **Lacertilia slope 0.60 → 0.89.** Without prey_type the lizard slope was dragged shallow by the invertebrate
  mode; conditioning on vertebrate prey it rises to ~0.89 — in line with the other vertebrate-eating clades (Aves
  1.06, Mammalia 0.91). The "lizards barely scale prey with size" result was a **mixing artifact**, now resolved.
- Aves / Mammalia / Serpentes slopes are stable across all four variants (robustness — those clades are far less
  bimodal). Serpentes is rock-stable at 0.71 (all-vertebrate, nothing to condition on).
- The fossil-relevant Lacertilia percentile is well-centred in every variant (V0 0.43, main 0.35, ×clade 0.41,
  oc 0.48) — i.e. **the plant-frugivory strip alone already de-pathologised the percentile diagnostic**; prey_type
  then delivered the predictive-fit (LOO) win and the slope correction. Honest read: the percentile is necessary
  but not very discriminating here; the LOO and the slope are where prey_type earns its place.

### Fossil placement on the promoted synthesis fit (x07, default outputs)
- `data/theropod_best_match.csv` populated (regression fixed; `extrapolated=TRUE` only for *Daspletosaurus* at
  1.4 t, beyond all extant ranges). Small feathered theropods (*Scipionyx*, *Anchiornis*, *Microraptor*,
  *Sinosauropteryx*) best-match size-matched **Lacertilia / Crocodylia / Aves** at sensible percentiles; the big
  tyrannosaurs sit at 0.95–0.99 vs Crocodylia (they ate large prey for their size, even relative to crocs).
- `data/theropod_clade_similarity.csv` preserves the **body-size confound flip**, now apples-to-apples (vert prey
  vs vert prey): size-IGNORING overlap → **Mammalia** (0.63); size-MATCHED (lowest KS D 0.16) → **Lacertilia**.
  Ignore size and theropods look mammal-like; control for size and they look like saurophagous lizards.

### Flag for your eye (uncertainty, not buried)
- **Crocodylia vertebrate-prey slope collapses to ~0.07** under prey_type, on only **n=26** vertebrate-prey rows.
  That is thin; the flat slope may be sparse-data, not biology. Worth checking whether more croc vertebrate-prey
  records exist before leaning on it.
- **Lacertilia vertebrate-prey breadth rises** (sigma 1.63→1.93): saurophagy is genuinely broad (near-own-size
  cannibalism down to small juveniles), so the wider band is expected, not a defect.
- LOO `p_loo` ≈ 220 (many effective params from the species/specimen REs) but **no Pareto-k warnings surfaced** —
  the weighted-LOO caveat did not bite this time.

**Bottom line:** the synthesis (`prey_type*clade` + frugivory strip + vert-level fossil placement) is adopted and
promoted. LOO is decisive, the Lacertilia slope is corrected to a biologically coherent value, Option A is ruled
out as predicted, and the fossil comparison is now like-for-like.
