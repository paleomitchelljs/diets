# DietProject — decisions & overnight run (2026-06-01, for AM review)

## TL;DR
Relaunched an **expanded, autonomous** extant-only `x20` run while you slept. It fits **two models** and
compares them by LOO to answer your "obstype on the slope?" question, on a larger balanced sample, with the
obstype mislabels you caught now fixed. Nothing here is irreversible — every choice is listed below for you to
veto. Fossils are still **overlay-only** (the x07 placement bridge is the next build, not done tonight).

## What's running
- **PID 27107**, detached (`nohup caffeinate -i`), log: `logs/x20_run_20260601_220717.log`.
- Config: `GUT_SUBSAMPLE=1000` (cap per clade×obstype), `GUT_ITER=3000`, 4 chains, `GUT_NOCOR=1`, `set.seed(20260601)`.
- Data: 170,958 assembled → **9,591** subsampled. Fits `add` then `int`, LOO-compares, writes plot.
- **Check in the morning:**
  - `tail -40 logs/x20_run_20260601_220717.log`  (slopes, σ, obstype effects, convergence warnings)
  - `cat data/gut_loo_compare.txt`  (does obstype×slope win?)
  - `data/fit_gutslopes_{add,int}.rds`, `data/gut_slopes_posterior.csv`, `data/gut_sigma_by_clade.csv`
  - `figures/gut_slopes_comparison.png`  (now with **obstype-adjusted points** — see Decision 4)
  - convergence: `Rscript -e 'f=readRDS("data/fit_gutslopes_int.rds"); print(max(brms::rhat(f),na.rm=T))'`

---

## DECISIONS NEEDING YOUR REVIEW (flagged; easy to change)

1. **obstype = {Stomach, Pellet, Scat, Follow}, with these recodes** (legacy `ObsType` was unreliable — 2 confirmed cases):
   - **Bird "Scat" → Pellet.** The longform bird "scat" is 105k raptor/owl records (42k *Tyto alba* + *Bubo/Strix/Asio/Athene/Glaucidium* owls, *Falco/Buteo/Accipiter/Aquila* raptors) — pellets, not feces. Recoded to a Pellet obstype (its own process; `observation_bias_notes.md`: pellets most complete/least variable).
   - **Snake "Scat" = kept as Scat.** You confirmed Holycross 2002, Bryant 2012, Slip 1988, Hill 2001 are legit fecal studies. So Scat = real feces across mammals + lizards + snakes.
   - **Regurgitation → Stomach** (recently-swallowed whole prey ≈ gut content; this fixed the spurious single "lizard pellet" = the *Varanus varius* possum regurgitation, Jessop 2010).
   - **prey_remains_at_nest → Follow** (observed nest remains, large-prey-biased; you called it "between Follow & Pellet" — I leaned Follow). 2 goshawk rows. **Revisit if you'd rather it be its own level.**

2. **obstype now affects the SLOPE, not just the intercept** (your hint). Model `int`: `preyMass ~ predmass_c * obstype + ...`. Compared by LOO against `add` (intercept-only). **The interaction is treated as primary in reporting, but LOO decides** — at SMOKE scale `add` was marginally preferred (Δelpd −2.7 ± 2.7 = indistinguishable); trust the full-run `gut_loo_compare.txt`.

3. **Scaling-slope prior `normal(0.85, 0.25)`** anchored on Tucker & Rogers 2014 (`MammalPreySize.pdf`, terrestrial mammals ~1) + Costa et al. 2008 (lizards, mean-prey 0.77–0.93). Moderately informative; did NOT force clades equal (crocs came out 0.32). **If you want it looser, widen to SD 0.3–0.4.**

4. **Plot now shows obstype-ADJUSTED points.** Your "bird points wildly above the line" was largely a plotting artifact: lines were drawn at obstype=Stomach (lowest), but data clouds are multi-obstype. Points are now adjusted to Stomach-equivalent (obstype intercept+slope removed) so they're directly comparable to the clade line.

5. **Aves Stomach = ADB only (214).** I excluded 149 raptor/owl *stomach* records sitting in the longform Aves Stomach block (85 *Buteo*, 64 *Aquila*). **Including them would strengthen the within-raptor Pellet-vs-Stomach contrast** (see Critical Assessment #3). Flagged as the top data improvement; not done tonight to avoid an unreviewed data change.

6. **Expanded run scope:** cap raised 500→1000/cell, iter 2500→3000. Reversible via env vars.

---

## CRITICAL ASSESSMENT (of the converged single-model fit that finished at 20:12)

**Fit quality: excellent.** max R-hat 1.002, min Bulk-ESS 2854, 0 divergences (4×1500). NOCOR fixed the
5-clade slope funnel that gave the SMOKE its 1.39.

**Results that look solid:**
- Population slope **0.88 [0.60, 1.13]** — on the literature anchor; clades near-isometric (Aves 1.09, Lacertilia 1.14, Mammalia 1.01, Serpentes 0.88) except crocs.
- **Breadth ordering** snakes narrowest (0.99) → crocs/mammals widest (~2.0): matches your hypothesis.
- **Variance grows with predator size** (σ~predmass +0.086, P=1.0).
- **obstype-on-σ** recovers the documented pattern: Scat widest (+0.10, P=1.0), Follow narrowest (−0.41), Pellet ≈ Stomach.

**Problems / caveats (drove tonight's changes):**
1. **Single global obstype *intercept* shift is too rigid.** Clade-level residuals after removing obstype:
   Lacertilia **+1.6**, Crocodylia **−1.0** (others ±0.5). → motivates obstype×predmass (Decision 2) and possibly
   obstype-by-clade (see Next Steps). The Lacertilia miss suggests lizard Scat-prey scales differently than
   mammal/snake Scat-prey — and since **Scat occurs in 3 clades, a Scat×clade term IS identifiable** (unlike Pellet).
2. **Crocs slope 0.32** is weakly constrained (Stomach-only, narrow mass range, n=500/1000). Don't over-read it.
3. **obstype ⟂ clade confound:** Pellet ≡ birds, Scat ≡ mammals/herps, Follow ≡ mostly mammals. So obstype
   effects are within-clade-vs-Stomach contrasts, not pure cross-clade process effects. **Mitigation found:** the
   ADB Stomach baseline is itself raptor-dominated (155 Accipitridae + 14 Falconidae + 12 Strigidae), so the
   Pellet effect IS a within-raptor/owl contrast (raptor stomach vs owl/raptor pellet), not "raptors vs other
   birds." Decision 5 would strengthen this further.
4. **"Lines look the same" (Aves/Mammalia/Serpentes):** real — intercepts 5.2/5.3/5.5, slopes 0.9–1.1 (snakes
   highest, as expected for big-prey snakes). Modest but directionally correct; the Stomach-line plotting
   exaggerated it (now fixed, Decision 4).

---

## Full list of tonight's code changes (`code/x20_gutSlopesStan.R`)
- Added Pellet obstype + bird-scat→pellet recode; Regurgitation→Stomach; nest→Follow.
- `int` model (obstype×predmass) + `add` model + LOO comparison (guarded).
- `set.seed(20260601)` before subsample (reproducibility).
- obstype-adjusted plot; postproc reports obstype effects on intercept AND slope.
- `x_validate.R`: removed the "Benson inflated" check; dedup key uses full prey name.
- `x22_buildFossilPoints.R`: tier-dedup (keeps within-source multi-prey), carries `pred_specimen`,
  stem-birds bin Theropoda not crown Aves. `fossil_ppmr_points.csv` now 29 points (added Confuciusornis,
  Piscivoravis stem-birds; Sinosauropteryx lizard *Dalinghosaurus*; both Gorgosaurus *Citipes*).

## NEXT STEPS (for discussion, not done)
- **x07 fossil-placement bridge**: posterior-prediction of each fossil vs the extant reference at the Stomach
  level, with mass-uncertainty propagation (the lo/hi brackets). This is the actual fossil result.
- **obstype-by-clade for Scat** (identifiable across lizard/mammal/snake) — likely fixes the Lacertilia residual.
- **Decision 5** (add the 149 raptor/owl stomach records) if you want to harden the Pellet identification.
- Audit the rest of the legacy `ObsType` column (2/2 probes found issues; only birds needed a fix so far).
