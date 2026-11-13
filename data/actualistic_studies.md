# Actualistic experiments — the "back pocket" for the observation-process model

Companion to `observation_bias_notes.md` (which holds the conceptual framing) and
`actualistic_studies.csv` (the machine-readable registry). This file is the
quantified target list: experiments that relate **what was preserved/observed** to
**what was actually eaten**, so the size-dependent observation/preservation filter
`g_mode(size)` can be an *informed prior* (at least) or a *mechanical likelihood
layer* (at best) instead of an assumption.

> **Honesty flags.** The three SEED papers are user-supplied and authoritative as
> references, but I have **not** extracted their numbers from the PDFs — every
> "quantity" below is a description of *what the paper yields*, not a transcribed
> value. The LINEAGE papers are **candidates recalled from memory**; treat citations
> (year/journal/volume) as `verify` until checked. Nothing here should be cited
> without opening the source.

---

## The model frame these feed

Two pieces (see `observation_bias_notes.md`):

- **f(prey size | predator size, clade, traits)** — the *latent* diet. The PPMR
  scaling we actually want.
- **g_mode(observed | true, prey size, time-since-meal)** — the *observation filter*.
  Fossils are 100% gut; extant data is mostly scat/pellet/remains. The filter is what
  makes them non-comparable, and it is **size-dependent** (small/soft prey vanish;
  large/robust prey persist) — the same asymmetry in fossil gut over-preservation.

The experiments below calibrate g (and the meal-size part of f).

---

## SEED 1 — Collopy 1983 (golden eagle): method-comparison → g for remains/pellets

**Design:** golden eagle nestling diet measured two ways — (a) direct observation of
prey deliveries, (b) collection of prey remains + pellets at the nest. **Concept:**
the two methods disagree; **prey remains over-represent large, identifiable prey and
under-represent small prey and prey consumed entirely or away from the nest.**

**Why it's the keystone for us:** direct observation is the closest available
*ground truth*, so the observation-vs-remains discrepancy is a **direct empirical
measurement of the remains-mode bias** — and that bias (toward large, robust prey) is
the **living analog of fossil-gut over-preservation**. If we can get the size-resolved
over-representation from the paper's tables, it becomes a literal prior on the slope/shape
of `g_remains(size)`.

**Pull from PDF:** per-prey-class detection ratio (remains ÷ observed); the prey-size
threshold below which remains miss items.

---

## SEED 2 — Secor & Diamond 1995 (Burmese python): SDA → meal-size tail + ID window

**Design:** "pay before pumping" — pythons fed large meals show a massive postprandial
up-regulation of gut and organ mass and metabolic rate (specific dynamic action, SDA).
**Concept:** sit-and-wait snakes ingest **very large meals, infrequently**, and process
them over a long, energetically expensive window.

**Why it matters here — two distinct levers:**
1. **Meal-size distribution (part of f).** Quantifies the large-prey tail for ambush
   snakes — mechanistic support for the **macropredator band** (Serpentes at ln-PPMR
   ≈ −1.5; snakes routinely take prey near/above their own mass).
2. **Detection/ID window (part of g).** Digestion time-course sets **how long a meal
   stays morphologically identifiable in the gut** → the temporal decay of g. A snake
   sampled days post-meal yields a more digested (size-biased) gut than one sampled hours in.

Caveat: this is physiology, not a ready-made size-bias function. Use it to *shape* the
meal-size prior and the ID-window decay, not as a correction coefficient.

---

## SEED 3 — Wachter et al. 2012 (carnivore scat): correction factors → g for scat

**Design:** an advanced scat-survey method that extracts more diet information than
naive frequency-of-occurrence, with **correction factors** tying prey occurrence in
scat to the biomass/number actually consumed. **Concept:** raw scat frequencies are
prey-size-biased (a large prey item yields disproportionately little scat per kg
consumed; small prey are over-counted).

**Why it matters here:** of the three seeds this is the most directly **`g_scat(size)`-
shaped** — a prey-size-dependent correction for the single most common extant mode
(scat dominates the non-bird data; scat + follow ≈ 88.5% of items). If we adopt those
correction factors, we can convert scat-based PPMR onto a gut-comparable footing rather
than discarding it (cf. the gut-only conservatism in x07).

**Pull from PDF:** the correction-factor function vs prey body mass.

---

## Lineages (candidates — verify)

**Method-comparison (Collopy → camera-as-truth):**
Marti 1987 (raptor food-habits methods); Redpath et al. 2001 (hen harrier, pellets vs
remains vs obs); Lewis, Fuller & Titus 2004 (3-method, nest video); García-Salgado et al.
2015 + Tornberg & Reif 2007 (goshawk, trail-camera vs remains). Trajectory: ground truth
moved from human observation → nest cameras, repeatedly confirming remains/pellets miss
small + soft prey and bias the mass spectrum upward.

**Digestion / actualistic taphonomy (Secor → bone survival):**
Secor 2008 (cross-taxon SDA review); McCue 2006 (SDA review). Bone-survival side, the
direct bridge to fossils: **Andrews 1990 (*Owls, Caves and Fossils*)** and
**Fernández-Jalvo & Andrews (1992; 2016 *Atlas of Taphonomic Identifications*)** —
digestion-corrosion **damage stages (0–4)** on prey bone/teeth keyed to **predator
category** (raptor = light etching; mammalian carnivore / crocodilian = heavy
destruction). Fisher 1981 (alligator) for crocodilian bone destruction. This literature
turns "preservation" into an **ordinal, predator- and size-dependent loss process** —
exactly the shape g needs, and already cast in fossil-applicable terms.

**Scat correction (Wachter → biomass models):**
Floyd, Mech & Jordan 1978 (the foundational wolf **biomass-per-scat** regression vs prey
mass) → Weaver 1993 (refinement for large prey) → Klare, Kamler & Macdonald 2011
(comparison/critique). Trites & Joy 2005 for pinnipeds (otolith erosion = the aquatic
analog of bone corrosion). The Floyd/Weaver biomass-per-scat curve is the most
turn-key prey-mass-dependent g_scat(size) prior available.

---

## How this wires in (staged, no commitment yet)

1. **Back-pocket now (done):** registry exists; cite as `reference/methods` in the
   intake manifest (no PPMR rows). Keeps them quantified and findable.
2. **Prior elicitation (next):** from Floyd/Weaver (scat) + Fernández-Jalvo/Andrews
   (gut/pellet damage) + Collopy (remains), specify `g_mode(size)` as a monotone
   size-dependent detection/retention curve per mode, with a prior on its midpoint/slope.
3. **Mechanical layer (aspirational):** make the observed prey mass a *censored/biased*
   draw from the latent meal-size distribution, with g as the bias kernel; the fossil
   gut then becomes one mode of the same likelihood, not a separate dataset.
4. **Secondary-predation + ID-window flags** (from Oehm 2017 + Secor): already action
   items in `observation_bias_notes.md`.

See `external_data_wanted.md` → "Digestion / preservation calibration" for fetch targets.
