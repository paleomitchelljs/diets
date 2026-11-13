# Evolution of predator–prey mass ratio: methods (Phase 5)

Written 2026-11-12, for Prompt 5 of `docs/ASSESSMENT_AND_PLAN_2026-11.md`. Sections 1–3 (the trait and the model that estimates it) were written before any model was fitted. Sections 4–6 give the tree and the comparative analysis as planned. Anything changed after fitting is listed in section 7 with the reason. Results are in `results/evolution/EVOLUTION_RESULTS.md`.

Scripts, run in order from the repository root:

| Script | Does |
|---|---|
| `code/evolution/e01_species_trait.R` | Step A: builds the analysis table from `db/`, fits the trait model, writes `results/evolution/species_trait.csv` |
| `code/evolution/e02_build_tree.R` | Step B: matches names, assembles 100 dated trees, fills `tree_label` in `db/taxa.csv`, writes `db/tree/` |
| `code/evolution/e03_comparative.R` | Step C: signal, BM/OU/EB fits, reconstruction, fossil test, sensitivity runs |
| `code/evolution/e04_figures.R` | Figures in `figures/evolution/` |

All logarithms are natural logarithms. Masses are in grams.

## 1. Scope and data

- **Predators:** tetrapods only (decision 2). Extant species with `rank` = species and `analysis_clade` in Aves, Mammalia, Lacertilia, Serpentes, Crocodylia. Fossils are not used to estimate the trait model. They enter only the fossil test (section 6.4).
- **Prey:** `taxa.is_vertebrate` = TRUE (any vertebrate, fish included). Rows whose prey taxon has `is_vertebrate` empty are excluded.
- **Rows:** `db/pairs.csv` with `pair_resolution` in A–D, `is_fossil` FALSE, and both `pred_mass_g` and `prey_mass_g` present. A row whose mass is given only as a bracket (lo and hi, no point) uses the geometric midpoint of the bracket.
- **Unit:** one row is one record (a prey taxon in a source, or one prey item in an individual pair), as stored in `pairs.csv`. Rows are not expanded by `n_items`. The trait is therefore a record-weighted quantity, not a number-weighted or biomass-weighted one (problem M8). This is stated in every result.
- **Study:** `source_id`. For records that came through a compilation, `source_id` is already the original study (SCHEMA, `compiled_in`), so study effects group by original study.

## 2. The species-level trait

**Definition.** For extant predator species *s*, the trait θ_s is the expected log predator–prey mass ratio

  θ_s = E[ ln(prey mass) − ln(predator mass) ]

for one vertebrate-prey record of species *s*, evaluated

1. at the species' reference predator mass M_s (below);
2. for observation type `stomach_content`;
3. for a typical study (study effect set to zero).

θ_s is negative when prey are smaller than the predator. θ_s = −3 means the typical recorded prey is e^−3 ≈ 5% of the predator's mass.

**Reference predator mass M_s.** M_s is the species' own typical adult mass, not one mass shared by all species:

- If species *s* has any record whose predator mass is a taxon average (tiers C and D), M_s is the median of those averages. These are the AVONET, PanTHERIA or literature species means already in the database.
- Otherwise (every record is from a measured predator, tiers A and B), M_s is the geometric mean of the measured predator masses. This applies to a few lizards and snakes. For them M_s is the mean of the sampled animals, which can include juveniles. The species table flags these cases (`ref_mass_basis` = `measured_sample_mean`).

**Why not one shared reference mass.** The predators span about seven orders of magnitude, from shrews and small lizards to the American alligator. A shared reference of, for example, 1 kg would require extrapolating each species' within-species slope across up to four orders of magnitude. That slope is estimated from very few species (section 3). The trait at a shared mass would be determined mostly by the slope prior, not by the data. Body size is handled instead in the comparative models, by fitting them with and without ln M_s as a covariate (section 6). The covariate model asks how the ratio evolves once its dependence on predator size is removed.

**Why stomach contents.** Fossil gut contents are the comparison the project is built around, and `stomach_content` is the reference level in every other analysis in the repository. The trait for a species with no stomach records (most birds, most mammals) is shifted by the estimated pellet, scat or other offset. Section 3 explains why that shift is a strong assumption.

## 3. The trait model (Step A)

For record *i* of species *s*, from study *k*, with observation type *o*:

  ln(prey_i) = α + β_b · (ln M_s − c) + b_s + β_w · d_i + γ_o + u_k + ε_i

- c is the mean of ln M_s over species. It centres the model and does not affect θ_s.
- **β_b** is the among-species slope of ln prey on ln predator mass. It is fitted globally, not per clade, so species shrink toward one line that carries no clade information. Shrinking toward clade means would manufacture phylogenetic signal in the trait (a species with few records would be pulled toward its clade's mean, and the comparative step would read that as inheritance).
- **b_s ~ Normal(0, τ_sp)** is the species-level intercept: the quantity of interest.
- **d_i = ln(pred_i) − ln M_s** for rows whose predator mass comes from the animal (`pred_mass_method` measured or regressed, tiers A and B). It is 0 for rows whose predator mass is a taxon average (tiers C and D). A taxon average carries no information about how big this particular predator was, so a difference between two databases' species means is not treated as within-species size variation. **β_w** is the within-species slope.
- **γ_o** is a fixed offset for observation type, with `stomach_content` = 0. The levels are the ones present in the data: pellet, scat, prey_remains, direct_observation, regurgitate, dna_detection, unknown. The offset is shared across clades. In these data observation type is close to collinear with clade (pellets are birds; scat is mostly mammals). The pellet offset is therefore identified only by bird species with both pellet and stomach records, and the scat offset only by mammals and squamates with both. Each offset is in effect a within-clade estimate applied to that clade.
- **u_k ~ Normal(0, τ_study)**: study effect.
- **ε_i ~ Normal(0, σ_clade)**: residual SD differing by analysis clade.

Then

  θ_s = α + β_b · (ln M_s − c) + b_s − ln M_s.

The model is fitted with brms, with four chains of 2,000 iterations (1,000 warm-up). Priors: α ~ Normal(mean of ln prey, 5); β_b, β_w ~ Normal(0.8, 0.5) (positive scaling is expected; the prior SD allows anything from about −0.2 to 1.8); γ_o ~ Normal(0, 1); τ_sp, τ_study, σ ~ half-Student-t(3, 0, 2.5). θ_s is computed per posterior draw. The reported value is its posterior mean, SD and 95% interval. Diagnostics reported: R-hat, bulk and tail ESS, divergences.

**What is reported per species.** In `results/evolution/species_trait.csv`: θ_s mean, SD, 2.5% and 97.5% quantiles; M_s and its basis; number of records, number of studies, number of records with a measured predator mass; observation types present; and flags `single_record` and `no_stomach_records`.

**Species with a single record.** These are kept in the model. Their b_s is estimated mostly by partial pooling, so θ_s lies close to the global allometric line and its posterior SD is close to √(τ_sp² + σ²) rather than small. The pooled residual SD is what supplies that uncertainty, since one record cannot. In the comparative step the posterior SD enters as measurement error, so these species carry little weight. Shrinkage toward a global, not clade, mean biases phylogenetic signal toward zero, not upward. As a sensitivity check, every comparative fit is repeated on species with at least 3 records from at least 2 studies.

**Tier E species-mean data.** Tier E rows are excluded from the trait model (decision 3). The usable tier E data are the species-mean relative prey masses of Glaudas et al. (2019), stored as `ratio_derived` prey masses. For a species with no A–D trait estimate, a species-level value is formed directly:

  θ_s(E) = ln(mean RPM) − σ_Serpentes² / 2,

with SE = σ_Serpentes / √n_prey. The −σ²/2 term corrects, under a lognormal assumption, for the fact that the logarithm of a mean ratio exceeds the mean of the logarithms. σ_Serpentes is the snake residual SD from the trait model. The observation method for these records is museum dissection, so the value is a stomach-content value. These species enter only the "A–D + E" sensitivity run, never the primary analysis. Other tier E rows (size-class midpoints, assigned or `unknown` prey masses) are not used anywhere in Phase 5.

## 4. The tree (Step B)

**Sources.** One published time tree per group, sampled from each posterior so that phylogenetic uncertainty can be propagated. A published backbone joins the groups. Every file, URL, version and date is recorded in `db/tree/README.md`.

| Group | Source | Trees used |
|---|---|---|
| Aves | Jetz et al. 2012, BirdTree, Hackett backbone, Stage 2 (all species, with imputed placements) | trees 1–100 of `HackettStage2_0001_1000.zip` |
| Mammalia | Upham et al. 2019, MamPhy v1, node-dated, completed 5,911-species, topology-constrained | the 100-tree posterior sample |
| Squamata (with *Sphenodon*) | Tonini et al. 2016, 9,755 species | trees 1–100 of the posterior file |
| Crocodylia | TimeTree 5 (Kumar et al. 2022) pairwise median times | fixed; three species only |
| Backbone | TimeTree 5 pairwise median times for Amniota, Sauria and Archosauria | fixed |

Tree *j* of the combined set joins bird tree *j*, mammal tree *j* and squamate tree *j*. Each group tree is pruned to the matched species and grafted onto the backbone node. The stem branch runs from the backbone node age down to the age of the pruned group's root.

**Name matching.** In order: (1) exact match of the `accepted_name` (spaces → underscores) to a tip label; (2) for birds, the AVONET BirdLife→BirdTree crosswalk (`data/AVONET/PhylogeneticData/BirdLife-BirdTree crosswalk.csv`); (3) a hand-checked synonym table, `db/tree/name_matches_manual.csv`, each row giving its reason. Labels are never matched to a different species by genus alone. A species that fails all three is left unmatched and listed. `tree_label` in `db/taxa.csv` is filled for every taxon (predator or prey) that matches a tip in any of the group trees. The match rate is reported separately for predator species in the trait table and for all vertebrate species.

**Fossil tips.** Fossils are not inserted into the trees used to fit the models. For the fossil test (section 6.4) each fossil is attached separately, by this rule:

1. **Position.** The fossil is placed on the stem lineage of the smallest extant group in the tree of which the publication that describes the specimen places it as a stem member. The published positions used are: tyrannosaurid, compsognathid and dromaeosaurid theropods, and the ornithuromorph *Yanornis*, on the stem of crown Aves. The baurusuchid *Aplestosuchus* (Godoy et al. 2014) and *Confractosuchus*, recovered as sister to susisuchids + hylaeochampsids (White et al. 2022), on the stem of crown Crocodylia. The triconodont *Repenomamus* (Hu et al. 2005; "gobiconodontid" in Han et al. 2023) on the stem of crown Theria, inside crown Mammalia. The original describers' identification is kept (decision 5).
2. **Age.** The fossil tip sits at `age_ma` from `db/pairs.csv`.
3. **Attachment time.** None of the trees we hold dates the branching of these fossil lineages from the stem, and inventing a date is ruled out. So each fossil is attached twice, giving two bounds:
   - *Young bound:* attached at the later of its own age and the crown age of the extant group. If its age is older than the crown, it sits on the stem branch with a zero-length terminal branch, as if it lay on the lineage leading to the crown. Otherwise it is attached at the crown node, with terminal length = crown age − fossil age.
   - *Old bound:* attached at the base of that stem lineage (the backbone node below the group: Archosauria, 243.3 Ma, for birds and crocodylians; crown Mammalia, 181.2 Ma, for *Repenomamus*), with terminal length = node age − fossil age.

   Any dated position the true tree could give lies between the two bounds. Both are reported. When a dated tree with these fossils is supplied (Benson et al. 2014 for dinosaurs; Groh et al. 2022 for crocodyliforms; both on Dryad, see `db/tree/README.md`), a third, published attachment will replace the bracket.

## 5. Inputs to the comparative step

For each species: θ_s (posterior mean), its posterior SD (the measurement error, entered as a known standard error), and ln M_s. Posterior correlations among species' θ values, induced by the shared fixed effects, are ignored. This understates uncertainty in the overall mean. It does not affect differences among species, which are what the evolutionary models describe.

## 6. Comparative analysis (Step C)

All fits use the extant species in each of the 100 trees. Every model is fitted on every tree. Results are reported as the median and the 2.5–97.5% range over trees.

### 6.1 Likelihood

Generalised least squares, written in `e03_comparative.R`. For tips *i*, *j* with root-to-tip distances t_i, t_j and shared path length t_ij:

- BM: V_ij = σ² t_ij
- OU, root fixed at the optimum θ₀: V_ij = σ²/(2a) · exp(−a(t_i + t_j − 2t_ij)) · (1 − exp(−2a t_ij))
- EB: V_ij = σ² (exp(r t_ij) − 1)/r, with r < 0 (rate declining through time)
- Pagel's λ (for signal only): BM with off-diagonal elements multiplied by λ

The total covariance is V + diag(SE_s²). The mean is β₀ (intercept-only) or β₀ + β₁ ln M_s (size-adjusted). β is profiled by GLS, and σ², a, r and λ are found by maximum likelihood. Bounds: a ≤ 10 × ln 2 / tree height (half-life ≥ 1/10 of the tree), r ∈ [−0.2, 0) per Myr, λ ∈ [0, 1]. Models are compared by AICc. The BM and OU intercept-only fits are checked against `geiger::fitContinuous` with the same SE on tree 1.

### 6.2 Phylogenetic signal

Pagel's λ with measurement error (own likelihood, plus `phytools::phylosig(method = "lambda", se = SE)` as a check) and Blomberg's K with measurement error (`phytools::phylosig(method = "K", se = SE)`), with likelihood-ratio and randomisation tests. Both are given for the raw trait and for the size-adjusted model (λ only).

### 6.3 Reconstruction

Ancestral states are computed on tree 1, under BM and under the AICc-best model, as the conditional mean and variance of internal nodes given the tips with measurement error, from the joint multivariate normal (`phytools::vcvPhylo` with `anc.nodes = TRUE` for BM; the same conditioning with the OU or EB covariance otherwise). They are shown as a coloured tree with node intervals for the clade crowns and the backbone nodes.

### 6.4 Fossil test

For each fossil record (one prey item in one specimen), the observed value is ln(prey mass) − ln(predator mass), with an SE from its mass intervals: SD = (ln hi − ln lo)/3.92 for each side, treating each interval as about 95%. The predicted distribution at the fossil's attachment comes from the fitted model (BM, OU, EB; intercept-only and size-adjusted with the fossil's own ln predator mass), conditional on all extant tips. Its variance adds three terms: the conditional phylogenetic variance; the record-level residual variance, since a fossil is one record and not a species mean (σ² averaged over clades in the trait model, plus τ_study²); and the fossil's own measurement variance. The record-level part is needed because a single fossil prey item scatters about its species' θ the way one extant record does. The report gives the fossil's predictive percentile and whether it falls inside the central 95% interval, under each model, for both attachment bounds, as the median over trees. Fossils with an `observation_type` other than `stomach_content` (the *Repenomamus*–*Psittacosaurus* combat association) are reported but marked as not matched to the reference observation type. Fossil predators larger than the largest extant species in the same group are marked as outside the supported range of ln M.

### 6.5 Sensitivity

Each is rerun through Steps A and C:

1. **Tier:** (a) A–C only (drop tier D: records with both masses from databases); (b) A–D plus tier E species-means for species not otherwise present.
2. **Observation type:** (a) stomach-content records only (no offsets); (b) drop `unknown` and `dna_detection`; (c) observation offsets that vary by clade (γ_o,clade) instead of shared.
3. **Best-sampled clades:** (a) drop Aves; (b) drop Aves and Mammalia. Aves has the most species and records; Mammalia has the most stomach and scat records after birds.
4. **Sampling depth:** species with ≥ 3 records from ≥ 2 studies only (Step C only; Step A is unchanged).

For each run the report gives λ, the AICc-best model and its weight, the OU half-life or EB rate, β₁ (size slope), and the fossil percentiles.

## 7. Changes after fitting

None yet. Any change made after seeing a result is listed here with its reason.
