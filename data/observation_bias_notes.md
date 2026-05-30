# Observation-mode & preservation-bias notes (methodological)

Conceptual notes that shape the modelling, distinct from data-quality reconciliations.
Source papers are tracked in `intake_manifest.csv` as `reference/methods` (no PPMR rows).

---

## Oehm et al. 2017, Ecol. Evol. 7 — "Diet analysis in piscivorous birds: what can molecular tools offer?"
(Great Cormorant *Phalacrocorax carbo sinensis*, Chiemsee; pellets vs feces vs regurgitated fish; morphological hard-part vs DNA ID.)

Why it matters here (flagged by JM as having expansive implications):

1. **Observation mode materially changes the inferred diet.** Empirically, the three sample types differ significantly in detected prey spectrum AND in between-sample variability (PERMANOVA F=25.8, p=.001). Pellets = most complete + LEAST variable; feces = least informative + MOST variable; regurgitates intermediate. => obstype must be modelled as an observation PROCESS (size/identifiability-dependent), not a swap-able nuisance intercept (supports x06 reframe / P1).

2. **Differential digestion of hard parts biases prey-SIZE estimation — the living analog of fossil preservation bias.** "Morphological prey mass estimations via regression formulae based on prey hard parts are commonly used... the erosion of hard parts during digestion can bias the size estimation and therefore this method is criticized." The size-dependent preservation filter g(size) proposed for fossils has an EXTANT mechanistic counterpart (digestion erosion of bones/otoliths). IMPLICATION: method-comparison + feeding-trial + actualistic-digestion studies can EMPIRICALLY CALIBRATE g(size), instead of leaving it assumed — a route through the identifiability problem raised in the single-item/preservation discussion.

3. **Secondary predation confound.** A prey item in a gut may be the prey's prey (prey ingested its own prey shortly before capture), not a direct diet choice. Relevant in ~20-40% of predatory-fish stomachs with predatory-prey remains. Applies to FOSSIL gut contents too — a fish in a fish's gut, etc. Worth a `secondary_predation_possible` flag when extracting multi-trophic gut assemblages.

4. **"Bird diet" data largely arrives via pellets/regurgitates**, each with its own size bias, NOT true stomach contents. Reinforces the x06/x07 finding that the extant BIRD GUT reference is thin and not directly fossil-comparable. (Pellets = a distinct 4th observation mode, as in the RaptorDiets DB.)

5. **Molecular detects more taxa but cannot quantify mass/number/size** ("molecular tools are not ready for accurate mass quantification... number of consumed prey individuals as well as their sex and developmental stages remain furtively"). For SIZE/MASS we still depend on morphological remains (biased) — so prey-mass uncertainty must be propagated, not treated as exact.

**Action items this seeds:**
- When building the observation-process model, look for actualistic digestion / feeding-trial / method-comparison datasets to calibrate the size-dependent detection/preservation function g_mode(size) per observation mode (pellet, gut, scat, regurgitate).
- Add a `secondary_predation_possible` flag to multi-item gut extractions.
- Keep treating pellets as their own obstype (distinct from gut and scat).
