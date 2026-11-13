# Talk analyses, steps 1–4 (2026-11-12)

Plan: JM's talk outline (three ideas, theropod payoff, skeptic slides). Steps 1–3 decide what the talk can claim. All fits are the skeleton-tree model (`code/db_analysis/b06_rules.stan`, same likelihood and priors as b04) on the terrestrial table.

## Data

`b05_terrestrial_cells.R` drops predators whose pre-merge minor clade is Procellariiformes, Suliformes, Sphenisciformes, Cetacea or Pinnipedia: 579 rows, 37 species. Kept: Charadriiformes, Pelecaniformes. Result: 75,583 rows, 1,257 species, 3,193 sources, 17 minor clades. Skeleton tree rebuilt from the same table (`results/6_work/terr/skeleton_*`; root 318.8 Ma).

## Fits (`results/6_work/terr/fit_<rule>/`)

| rule | meaning | max R-hat | min bulk ESS | divergences | time |
|---|---|---|---|---|---|
| free | λ estimated per effect | 1.030 | 148 | 0 | 52 min |
| lambda0 | clades exchangeable (white noise around a shared mean) | 1.008 | 607 | 0 | 31 min |
| lambda1 | Brownian motion on the skeleton | 1.009 | 545 | 0 | 69 min |
| unpooled | each clade alone, global prior per clade | 1.017 | 490 | **77** (0, 9, 2, 66) | 43 min |
| pooled | one line for all tetrapods | 1.009 | 446 | 0 | 8 min |

The free fit's worst R-hat and ESS are single species effects (`z_u[78]` and others); clade and global parameters mix well. The unpooled divergences sit mostly in one chain; the likely cause is the thin clades with fixed wide τ (other Squamata, 7 rows; other Mammalia, 11). Refit with `adapt_delta = 0.99` before quoting unpooled numbers beyond the talk animation.

λ in the free fit (posterior median): intercept 0.53, slope 0.41, spread 0.30. τ_a 1.03, τ_b 0.32.

## Step 1. Predictive check (free fit; `fit_free/ppc_*.csv`)

PIT of each observed row under the posterior predictive, for a new species from a new source (the form the fossil test uses). Targets: median 0.5, central 80% coverage 0.8.

- Overall: median 0.43, coverage 0.84; species-weighted 0.44, 0.82.
- Crocodylia: median 0.50, coverage 0.78; 17% of rows below the 10th percentile (lower tail heavy). The old M2 failure (89th percentile) is gone.
- Failing or weak: Teiioidea row-weighted median 0.85 (one species dominates; species-weighted 0.49); Passeriformes 0.72 row-weighted, 0.17 species-weighted (no stomach data; regurgitate and unknown only); mammal prey_remains 0.74 and direct_observation 0.68 (a shared observation offset cannot fit them).
- Stomach contents: Aves 0.54 / 0.83, Mammalia 0.54 / 0.75 (predictive slightly too narrow; 17% above the 90th), Squamata 0.43 / 0.81, Crocodylia 0.50 / 0.78.

Verdict: usable for the talk. Do not lean on Passeriformes as a candidate (see step 2).

## Step 3. Tree prediction for theropods (`fit_free/tree_prediction/`)

A theropod tip on the bird stem at 165 Ma (sensitivity 150, 230), conditional on the fitted clade effects. At stomach contents:

| candidate | ln prey at 1 kg | slope | spread at 1 kg |
|---|---|---|---|
| tree: Theropoda @165 | 3.82 [2.54, 5.09] | 0.77 [0.36, 1.18] | 1.88 [1.08, 3.26] |
| none of the above | 3.83 [2.36, 5.31] | 0.81 [0.37, 1.27] | 1.77 [0.95, 3.23] |
| major: Aves (new order) | 3.94 [2.82, 5.03] | 0.75 [0.37, 1.12] | 1.97 [1.19, 3.26] |
| Crocodylia | 2.60 [1.90, 3.18] | 0.79 [0.62, 0.94] | 1.67 [1.57, 1.84] |
| Accipitriformes | 4.93 [4.79, 5.04] | 1.00 [0.91, 1.10] | 1.72 [1.69, 1.75] |

Intervals are 10–90%. The tip puts about 0.07 weight on each bird order, 0.05 on Crocodylia, 0.01 on each squamate clade, 0 on mammals. Attachment age barely matters (intercept 3.74–3.84). The tree narrows the prediction only slightly relative to "none" (80% intercept width 2.55 vs 2.95 ln units). Reason: relatives disagree (λ_slope 0.41; Feliformia 1.34 vs Caniformia 0.91; Accipitriformes 1.00 vs Strigiformes 0.47), so shared history carries little information. Slide 4 can show the band; the claim is "the tree says: wide", not "the tree predicts where theropods sit".

## Step 2. Fossil similarity test (`fit_free/fossil*/`, `fit_free/calibration/`)

Records: 53 usable (25 theropod records from 11 taxa, 23 stem-bird, 2 crocodyliform, 2 *Repenomamus*, 1 *Sanajeh*). Mass rule: an interval is ±2 sd on the log scale around the point; with no point, uniform on the log scale between bounds. Combat → direct_observation; pellet → pellet_scat.

### Theropods (relative support, equal prior)

| run | n | Aves | Crocodylia | Mammalia | Squamata | none | top minor clades |
|---|---|---|---|---|---|---|---|
| all records | 25 | 0.09 | 0.33 | 0.12 | 0.26 | 0.21 | Crocodylia 0.34, Passeriformes 0.23, Feliformia 0.23 |
| no combat (drop *Velociraptor*) | 24 | 0.12 | 0.22 | 0.12 | 0.16 | 0.38 | Crocodylia 0.42, Passeriformes 0.26 |
| no juvenile predators | 16 | 0.18 | 0.25 | 0.16 | 0.24 | 0.18 | Passeriformes 0.22, Feliformia 0.22, Crocodylia 0.19 |
| no bracket-only prey | 15 | 0.19 | 0.18 | 0.22 | 0.21 | 0.20 | Passeriformes 0.26, Feliformia 0.23, Iguania 0.18 |
| stomach only | 21 | 0.11 | 0.30 | 0.13 | 0.26 | 0.20 | other Squamata 0.34, Passeriformes 0.27, Iguania 0.23 |

Tree tip (Theropoda @165) in the tree-vs-major comparison: 0.13, below Crocodylia, Squamata and none.

Passeriformes has no stomach-content data, so 22 of 25 theropod records are extrapolated under it; its support should not be shown without that flag. "other Squamata" (4 species, 7 rows) has the same problem.

### Calibration at the theropod design (1,150 simulations; 50 per truth)

- When the truth is a major clade (data simulated from a new order of that clade), that clade ranks first 34–44% of the time for Aves, Crocodylia and Mammalia, and 4% for Squamata (chance is 20%). "None" ranks first 6% of the time when it is true.
- Mean support for the true major clade: 0.21–0.36 across truths (chance 0.20).
- When the truth is a specific minor clade, it ranks first among 17 between 12% (other Mammalia) and 82% (Feliformia); median about 42%.
- Thinned extant samples (11 species, 1–4 records; optimistic): true minor clade first 8–82%, true major clade first 2–86%.
- Avialae design: true major clade first 2% (Varanidae truth) to 78% (other Aves truth); mean support for the true major 0.18–0.30; "none" never ranks first when true. True minor clade first 0–54%.

### Positive controls (major vs none; chance 0.20)

- *Repenomamus* (2 records): Mammalia 0.26, best of five but barely; flat (0.18) once the combat record is dropped.
- Crocodyliforms (2): Crocodylia 0.20, flat.
- *Sanajeh* (1, combat bracket): Squamata 0.20, flat; Mammalia 0.22.

### Conclusion for the talk

At this design the test cannot tell which living group non-avian theropods eat like. The leading answer at the major level (Crocodylia 0.33) is a Bayes factor of about 1.6 over chance, the calibration says a true answer would usually not rank first, and the leader changes under every narrowing. The positive controls are flat rather than wrong. That supports the "the model reports its own ignorance" slide; it does not support a bar chart that implies Crocodylia or Passeriformes wins.

Stem birds (all fish prey) fit worst under Aves (0.09): the extant bird reference is raptors and owls eating terrestrial vertebrates. This is a reference-class problem, not evidence about stem birds.

## Step 4. Rules and shrinkage (for slides 3–4)

| clade | unpooled | λ = 0 | free λ | λ = 1 | one line |
|---|---|---|---|---|---|
| Iguania intercept | 4.81 [3.51, 6.16] | 4.32 | 4.07 | 3.97 | 3.98 |
| Iguania slope | 1.07 [0.77, 1.38] | 0.95 | 0.90 | 0.87 | 0.47 |
| Crocodylia intercept | 2.23 [1.29, 3.10] | 2.62 | 2.54 | 2.43 | 3.98 |
| Crocodylia slope | 0.83 | 0.78 | 0.78 | 0.79 | 0.47 |
| Strigiformes slope | 0.45 | 0.46 | 0.47 | 0.48 | 0.47 |

Crocodylia barely moves between rules (166 records pin it); Iguania moves visibly, so the rules slide uses Iguania. The one-line fit has slope 0.47 because owl pellets dominate the rows; it is not a useful "everybody" comparison.

## Scripts

`b05_terrestrial_cells.R`, `b06_rules.stan`, `b06_fit_rules.R`, `run_b06_all.sh`, `b07_ppc.R`, `b08_fossil_lib.R`, `b08_fossil_test.R`, `b09_calibration.R`, `b10_tree_prediction.R`, `run_steps123.sh`, `fossil_tree_tips.csv` (attachment ages are general-knowledge approximations; JM to confirm). `b03_skeleton_tree.R` gained a `WORK` variable. Site data: `code/site_export.R`.

## Dated composite tree for the fossils (added 2026-11-12, `code/db_analysis/a02_theropod_tree.R`)

Topology: Lloyd et al. 2016 dinosaur metatrees (100 trees; `db/tree/raw/lloyd2016/`). The cal3-rescaled versions in JM's BAMM project are not usable for absolute ages (paravian split 224 Ma, root 252–447 Ma). Dated here by minimum branch length (tip = range midpoint, 0.1 Myr per node). *Eshanosaurus* dropped in the primary (alone moves every coelurosaur split from the crown-bird line from 162–168 to 201 Ma); kept as a sensitivity. The 100 metatrees agree on every node used.

Split from the crown-bird line: tyrannosaurs 167.6, compsognathids 162.5 (*Huadanosaurus* attached by rule), paravians 161.8, *Confuciusornis* 155.5, ornithuromorphs 130.5 (*Piscivoravis*, *Kunpengornis* by rule), *Yanornis* 119.9 Ma.

Two uses:
- Single tree tip for the band (slide 5) now at the theropod median, 162.5 Ma (was 165 by rule). Prediction unchanged in practice: ln prey at 1 kg 3.83 [2.54, 5.09].
- "tree:dated": each fossil taxon its own tip at its dated split, fossil pairs sharing history to their dated MRCA, drawn jointly given the extant clades.

Support for tree:dated among {tree:dated, 4 major clades, none}, theropods: 0.08 (all records), 0.07 (no combat), 0.24 (no juvenile predators; highest of the six), 0.08 (no brackets), 0.07 (stomach only). Stem birds: 0.03–0.09.

Calibration at the theropod design: when data are simulated from tree:dated, it ranks first 74% of the time (stem-bird design: 36%). So the test can recognise the dated-tree pattern when it is true, and the real theropod records mostly do not show it. The single exception (no juvenile predators) means this is not a stable conclusion either. Single-tip candidates (one shared effect for all theropods) are never recognised (0–2%), because they are indistinguishable from the major-clade candidates.

### Decisions and placements (2026-11-12, later)

- *Eshanosaurus* dropped from the dating (JM). Kept only as the `sens_withEshano` sensitivity.
- Rule placements now follow the original descriptions (checked from abstracts and secondary summaries, not full text):
  - *Huadanosaurus*: Qiu et al. 2025 Natl Sci Rev, OSP matrix, sister to *Mirischia* + *Sinocalliopteryx*; attached at MRCA(*Sinocalliopteryx*, *Mirischia*). Their TWiG analysis places it differently (Hendrickx 2025 comment).
  - *Piscivoravis*: Zhou, Zhou & O'Connor 2014 Hist Biol (online 2013), polytomy with *Jianchangornis*, *Patagopteryx* and more derived ornithuromorphs; attached at MRCA(*Jianchangornis*, *Patagopteryx*, *Ichthyornis*).
  - *Kunpengornis*: Huang et al. 2025 Cretaceous Research, sister to *Piscivoravis*; same attachment.
- Effect: split ages unchanged (*Huadanosaurus* 162.5, ornithuromorphs 130.5 Ma); *Huadanosaurus* now shares history with *Sinocalliopteryx* and *Compsognathus* to 155 Ma. Similarity-test support unchanged to two decimals. Calibration not rerun for this change.
- Positive-control slide dropped from the talk (JM). Ziphodont marking dropped.
