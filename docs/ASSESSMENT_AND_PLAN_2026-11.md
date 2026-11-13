# Assessment of the current approach, and a plan to reach the eight goals

Written 2026-11-11. Scope: the whole repository as it stands today, including the new `data/fossil_rebuild/` step 1 and the 2026-11-11 extant batch.

## Summary

The project has a working model and a large amount of data, but it does not yet have a database. Data live in five stores with different schemas, the model script decides what counts, and the provenance of each mass is recorded in free text or not at all. Goals 1–3 are therefore the weakest, and they block goals 6 and 8. Goal 7 exists in a first version. Goal 5 is half done (specimens compiled, no masses). Goal 6 has not been started in a form that could be published.

The fix is one structural change: build a single long-format pairs database with controlled vocabularies, and make every analysis and the website read from it. Most of the plan below is that change plus the work it unlocks.

## Assessment by goal

Ratings: **met**, **partly met**, **not met**.

### 1. Transparent database of predator–prey sizes for confirmed pairs: not met

- There is no single table. The fit (`code/x20_gutSlopesStan.R`) assembles three sources at run time: `data/longform.csv`, `data/structured/aviandiet_gut_ppmr.csv` and `data/extant_new_observations.csv`. Other ingested tables (raptor diets, Tucker mammals, Jekyll alligators, tegu, Glaudas snakes) sit in `data/structured/` and are not read by the fit.
- "Confirmed pair" is not a property the data can express. Most extant rows are a predator species paired with a prey taxon from a diet list, both given species-average masses. That is a confirmed *trophic link* with *assumed* sizes. It is not a confirmed pair of individuals.
- Some rows are weaker than that. The alligator rows (ref 207–226, and the 22 added today, ref 230–251) pair a size-class midpoint predator with an approximate species-typical prey mass. No individual alligator ate an individual of that size. They carry `certainty` 0.4, but nothing else distinguishes them from a measured pair.
- The subsampling and filtering that decide which rows the model sees live in the model script, so the "database" a reader could inspect is not the data the result rests on.

### 2. How each pairing was found is parseable: partly met

- Extant rows do carry an observation type. But there are three vocabularies: `ObsType` in `raw_data.csv`, eleven `obs_type` values in `extant_new_observations.csv`, and the collapsed levels the model uses. The mapping between them (`OBMAP`) is defined inside `x20`, including judgment calls such as Regurgitation → Stomach and Aves Scat → Pellet.
- Fossil rows use a fourth vocabulary (`evidence_type` in `step1_gut_entries.csv`).
- Nothing separates *how the link was observed* (stomach, pellet, scat, direct observation, combat, bite trace) from *how well the pairing is resolved* (individual, population, occurrence list).

### 3. How each mass was determined is clear: not met for extant, mostly met for fossil step 1

- Extant tables have no mass-method field. Method is in `pred_meas_note` and `notes` as prose, or implied by which script filled it (`x03` for PanTHERIA/AVONET fills, `mass_overrides.csv`).
- `certainty` is a single number that mixes mass quality, pairing quality and source quality. Its values were assigned by hand in different sessions and are also overridden in `x20` (0.75 for all longform, 0.85 for four studies).
- Prey masses in the coarse croc rows were supplied at ingest from general knowledge. They are flagged in notes, but a query cannot find them.
- The fossil rebuild does this properly: `step1_measurements.csv` has `value_type` (measured, estimated, preserved, derived) and a source reference per value. No fossil masses exist yet in the rebuild.

### 4. As complete as feasible: partly met, and uneven

- Birds and mammals are saturated (README: 104k bird and 32k mammal vertebrate-prey records before subsampling).
- Crocodylia is thin and mostly coarse. Snakes are moderate; the Glaudas dataset (115 species) is staged but unused. Lizards are moderate.
- Fish, sharks, amphibians and turtles as predators are excluded by the model's clade filter. If "vertebrates generally" is the claim, this is the largest gap.
- Fossils: 29 papers, 23 predator specimens with ingested prey. Known gaps are listed in `data/fossil_rebuild/README.md` (for example Baryonyx, Yanornis, Compsognathus per Ostrom, Sinocalliopteryx original description). Non-dinosaur fossil records (marine reptiles, fish-in-fish, Messel and Green River vertebrates, pterosaurs, Cenozoic mammals) have not been searched for at all.
- Ten supplementary files holding needed measurements are missing from disk.
- There is no written search protocol, so "as complete as feasible" cannot be demonstrated.

### 5. Fossil taxa included wherever direct relationships are known: partly met

- Step 1 is sound: every specimen and gut item from the 29 papers, with quotes, and with pellets, combat, bites and rejected claims separated from ingested prey.
- Missing: mass estimates for predators and prey with stated methods and intervals; resolution of the 11 logged conflicts; a certainty rubric; the gap papers above.
- The older fossil tables (`fossil_predprey.csv`, `fossil_ppmr_points.csv`) still feed the current fossil placement and disagree with the rebuild in places (Conrad 2017 femur and tibia; values with no traceable source).

### 6. Evolutionary analysis of predator–prey mass ratio across vertebrates: not met

- The current model is a regression with five fixed clades. That is a comparison among groups, not an analysis of how the ratio evolves.
- The only phylogenetic script (`x02`) uses a taxonomy-based placeholder tree, per the pipeline notes. No dated tree is in the repository.
- A species-level trait is not defined. The ratio varies within species with predator size, observation type and prey type, so "the PPMR of species X" needs a rule (for example the intercept at a reference size from a species-level model, with its uncertainty).
- Fossil tips need dates and placement. Five clades do not span vertebrates.

### 7. Fossils compared to extant regressions, controlled by observation type: partly met

- `x07_fossilPlacement.R` does this by predicting at obstype Stomach, vertebrate prey only. This is the right idea.
- Weaknesses: the observation-type effect is shared across clades (`predmass*obstype` with no clade interaction), and the project's own notes call the obstype/preservation effect non-identifiable. The Crocodylia slope is 0.17 with a wide interval. Fossil evidence other than stomach contents (pellets, combat, bite traces) has thin or no extant analogue in the fit. Fossil prey masses are whole-animal estimates from bones, while some extant "stomach" masses are taxon averages and others (Jekyll) would be remains weights. Fossil inputs are the old tables.

### 8. Deployable presentation site: not met, and blocked by 1–3

- Nothing exists yet. Figures are static PDF/PNG.
- An interactive plot needs one tidy export with stable identifiers, a silhouette per taxon, and method fields to drive filters. Those are exactly what goals 1–3 lack.

## Review of the statistical model

Basis: a full read of `code/x20_gutSlopesStan.R` and `code/x07_fossilPlacement.R`, the saved posterior summaries, and the run log `logs/vertonly_20260603_113941.log`. I did not refit or open the fit object, so every number below is from that log.

The model is

```
ln(prey) | weights(certainty) ~ predmass_c*clade + predmass_c*obstype + (1|species) + (1|pred_specimen)
sigma ~ 0 + clade + predmass_c + obstype        (gaussian)
```

fitted to 15,260 subsampled vertebrate-prey rows, then used to place fossils by posterior-predictive percentile at obstype Stomach.

### Problems, most serious first

**M1. The observation-type effect is not identified where it is used.** The fitted design is nearly empty off the diagonal:

| | Stomach | Follow | Pellet | Scat |
|---|---|---|---|---|
| Aves | 255 | 1500 | 1500 | 0 |
| Crocodylia | 1189 | 0 | 0 | 0 |
| Lacertilia | 923 | 72 | 0 | 161 |
| Mammalia | 4830 | 1464 | 0 | 1500 |
| Serpentes | 1502 | 1 | 0 | 363 |

The model assumes one observation-type offset shared by all clades. Follow and Scat are estimated almost entirely from mammals; Pellet exists only in birds. The headline result (theropods sit at the 24th percentile of same-size birds once "controlled" to Stomach) depends on moving the bird line from Follow/Pellet to Stomach. That move rests on 255 bird stomach rows from one database plus a Follow offset (+1.07) learned mostly from mammals. The claim "controlling for the gut filter pulls theropods down" is therefore largely an assumption that mammal observation bias transfers to birds.

**M2. The model fails its own fit check for crocodylians.** The log reports a population-line gap of +1.66 ln units for Crocodylia and an observed median at the 89th percentile of the model's predictive distribution (target 0.5). Lacertilia and Serpentes sit at 0.36 and 0.33. A reference that mis-centres the extant data by this much cannot calibrate a fossil.

**M3. Row counts overstate the information about slopes.** Predator mass is one value per species for most species, and one value per study for crocodylians (925 g for 430 Delany rows; 37–49 kg for Rice). The Crocodylia slope of 0.17 is a line through a handful of distinct predator masses, not 1,189 observations. With predator mass constant within species, `(1|species)` competes with the slope, and what is estimated is an among-species relationship. Fossil specimens, several of them juveniles, are then placed on it as individuals.

**M4. The vertebrate-only filter truncates the response in a size-dependent way.** Small predators eat mostly invertebrates; their vertebrate prey are the upper tail of their diet. Dropping invertebrates keeps that tail for small predators and nearly the whole diet for large ones. This raises the low end of every line and flattens slopes. It also means the "matched filter" is matched on prey type but not on the probability of eating a vertebrate at all.

**M5. The residual distribution was forced.** Project notes record that a Student-t fit drove the degrees of freedom to 1 (Cauchy), and the response was changed to Gaussian with sigma near 1.1–2.0 ln units. That is evidence of heavy tails or mixtures (for example bimodal lizard diets), not of normality. Fossil placement is a tail percentile, so it is exactly the quantity most sensitive to this choice.

**M6. `weights(certainty)` is not an uncertainty model.** It raises each row's likelihood to a power. Hand-assigned values (0.4 to 1.0, with blanket overrides in the script) then change effective sample size and interval widths in ways that have no measurement interpretation. Mass error in predator mass also attenuates slopes, and nothing corrects for it.

**M7. Subsampling shapes the result and is not tested.** Stomach rows are never capped, so 4,830 mammal stomach rows dominate shared parameters. Other cells are capped by selecting evenly spaced predator masses within study-by-species, which flattens the predator-size distribution by design, then by a random draw with one fixed seed. No seed or cap sensitivity is reported.

**M8. Rows are prey items, so the fit is number-weighted.** Abundant small prey dominate each predator's distribution. A fossil gut is one or a few items filtered toward large, bony prey at a single moment. Whether the right extant comparison is per-item, per-stomach maximum, or biomass-weighted is not addressed, and it changes the answer.

**M9. Fossil placement is thinner than it looks.**
- Only prey mass uncertainty is propagated, and only by evaluating the two bracket ends. Predator mass is treated as known.
- Multiple prey in one predator are placed as independent.
- Theropods are compared with birds by decision; the "most resembles lizards" statement comes from comparing percentiles, not from a model comparison.
- Fossils outside a clade's predator-mass range get a flag, but the extrapolated band is still drawn and used.
- All fossils are placed at Stomach, including pellets and combat cases.
- Inputs are the old fossil tables.

**M10. No phylogeny, and clades are assigned by string matching.** Species are exchangeable within five fixed groups. "Lacertilia" is everything squamate that is not on a hard-coded list of 20 snake genera, so a snake genus missing from the list is silently fitted as a lizard.

**M11. Priors pull on the effects of interest.** Observation-type and slope-deviation terms share a normal(0, 0.5) prior; the Follow offset is estimated at +1.07, more than two prior standard deviations out, so the prior is shrinking it. The 0.85 slope anchor applies to the reference clade (birds) only.

**M12. Validation is close to absent.** The only model comparison on file is an additive-versus-interaction LOO difference of 1.0 with standard error 2.3, which shows nothing. There is no leave-one-study-out or leave-one-species-out check, no simulation showing the observation effect can be recovered from this design, and no calibration test of the placement procedure.

### What can and cannot be claimed today

- Supported: among extant tetrapods in these data, prey mass increases with predator mass, spread increases with predator size, and direct-observation studies report larger prey than stomach contents in mammals.
- Not supported yet: any statement about where fossils sit relative to extant clades after controlling for observation type; the 0.17 crocodylian slope as a biological result; the lizard resemblance.

### Improvements

1. **Describe before modelling.** For each clade and observation type, plot raw ln(prey) against ln(predator) by tier, with conditional 10/50/90 percent quantiles and the number of species and studies. Publish the cell-count table alongside every result.
2. **Separate within-species from among-species scaling.** Split predator mass into the species mean and the individual deviation from it. Estimate the within-species slope from individual pairs with a measured predator mass only (`individual_pair` TRUE, tiers A–B; revised 2026-11-11). Place fossils using the slope that matches what a specimen is.
3. **Make the observation effect honest.** Let observation-type offsets vary by clade with partial pooling, and report for each clade whether its offset is informed by data or by the pooled prior. Compare fossils only within an observation type the reference clade actually has; label everything else as extrapolated.
4. **Model the vertebrate filter.** Fit a two-part model: the probability that a prey item is a vertebrate as a function of predator mass and clade, and prey size given vertebrate. Or keep invertebrates and include prey type. Then show how the vertebrate-only lines change.
5. **Fit the tails.** Compare Gaussian, skew-normal, Student-t with a bounded prior on degrees of freedom, and a two-component mixture. Add direct quantile regressions (10th, 50th, 90th) as a distribution-free check on placement.
6. **Replace certainty weights with measurement error.** Give each `mass_method` a standard deviation on the log scale, sourced or stated, and treat masses as latent. Drop likelihood weights.
7. **Stop subsampling, or prove it harmless.** Prefer all rows with study and species effects, or aggregate to study-by-species-by-prey-taxon with counts. If caps stay, report results over at least 20 seeds and three cap sizes.
8. **Decide the weighting explicitly.** Fit per-item, per-predator-largest-item and biomass-weighted versions and report all three for the fossil comparison.
9. **Add phylogeny.** Species effect with phylogenetic covariance from the phase 5 tree; clade membership from `db/taxa.csv`.
10. **Rebuild placement.** Sample predator and prey mass from their intervals; treat prey within one predator jointly; score each fossil against each clade with log predictive density and report relative support; refuse to place outside the supported predator-mass range.
11. **Validate the procedure.** Hold out extant stomach records by study, place them as if they were fossils, and check that their percentiles are uniform. Do the same with held-out species. Run a simulation with a known observation effect on the real design to see whether it is recoverable. Report prior sensitivity.

## Extant data leads (searched 2026-11-11)

Bibliographic details below were checked by web search unless marked. I did not open any dataset, so "what it holds" is from abstracts and documentation. Method papers are indexed separately in `docs/methodology_index.csv`.

| Source | Clade | What it holds | Predator size | Access |
|---|---|---|---|---|
| SquamataBase (Grundler 2020, Biodivers Data J) | Snakes | 18,304+ predator individuals, 58,633+ prey items; prey count and mass fields, predator snout-vent length for single-predator records | Individual (where recorded) | Open; R package on GitHub, Zenodo record 3739652 |
| GATEWAy 1.0 (Brose et al. 2019, Nat Ecol Evol 3:919) | All | 222,151 links in 290 food webs with body masses | Mostly species-level for tetrapods | Open; doi 10.25829/iDiv.283-3-756 |
| CarniDIET 1.0 (Middleton et al. 2021, Glob Ecol Biogeogr 30:1175) | Mammalian carnivores (103 spp) | Diet composition records | Species-level; links only | Open; Dryad doi 10.5061/dryad.2v6wwpzmr |
| Radloff & du Toit 2004 (J Anim Ecol 73:410) | Large African carnivores | 13 years, >4,000 kills | By predator species and sex | Paper needed |
| Owen-Smith & Mills 2008 (J Anim Ecol 77:173) | Kruger carnivores | 46 years of carcass records, 5 predators, 22 prey | Species-level | Paper needed; a repository copy exists at Univ. Pretoria |
| Knopff et al. 2010 (J Wildl Manage 74:1435) | Cougar | 1,509 kills; 42 GPS-collared individuals; prey by species and age class | Individual predators (masses need the full text) | Paper needed |
| Storer 1966 (Auk 83:423) | Accipiter hawks | Prey by predator species and sex | By sex | Open PDF on SORA |
| Harbour porpoise stomach studies (Dutch, Scottish, Icelandic, Galician, Baltic series) | Marine mammals | Per-animal stomachs; prey length and mass from otoliths | Individual length | Papers needed; per-stomach data availability not confirmed |
| Pianka desert lizard data | Lizards | About 28,000 individual lizards with stomach contents | Individual | NOT confirmed public; a release was proposed. Mostly invertebrate prey |
| Costa et al. 2008 (Glob Ecol Biogeogr 17:670) | Lizards (159 spp) | Species-level prey-size summaries | Species-level | On disk in `pdfs/methodology/` |

Priorities: SquamataBase is the largest open source of individual-level tetrapod records and should be ingested first. Bird stomach data remain the weakest cell; no open individual-level bird dataset was found in this search, so that needs a dedicated search of seabird and wading-bird literature.

## Cross-cutting problems

1. **Three kinds of evidence are treated as one.** Individual pairs, population-level links with average sizes, and coarse occurrence pairings answer different questions. Mixed in one regression, the abundant weak kind dominates.
2. **Curation logic lives in analysis code.** Clade assignment by a regular expression of snake genera, obstype collapsing, certainty overrides and subsampling are all in `x20`.
3. **Old and new fossil data coexist.** Until the rebuild replaces the old tables, results and the database disagree.
4. **The file-name-keyed intake manifest is now stale** for fossil PDFs (renamed in step 1).
5. **Things I noticed but did not verify.** `raw_data.csv` has a *Caiman crocodilus* record at 520,008.5 g (ref 164). Read Da Silveira & Magnusson 1999 before deciding whether it is a typo.

## Decisions needed from JM before implementation

1. **Unit of analysis. DECIDED 2026-11-11: keep all records, tagged by resolution tier.** The headline fossil comparison rests on tier A (individual pairs); tiers B, C and D are added cumulatively as a sensitivity check, and the tier-by-tier change is itself reported. *Revised 2026-11-11 (JM): there are now five tiers, set by where each mass comes from (A both measured, B predator measured and prey a taxon average, C the reverse, D both averages, E coarse or aggregate), and individual pairing is a separate column, `individual_pair`. The headline rests on individual pairs in tier A; B to E are added cumulatively. Definitions in `db/SCHEMA.md`.* Species-level evolutionary analysis may use all tiers.
2. **Taxonomic scope. DECIDED 2026-11-11: tetrapods only for now.** Fish and sharks as predators are out of scope; prompts 4 and 5 should restrict to tetrapod predators (prey may be any vertebrate).
3. **Species-mean ratio data (Glaudas). DECIDED: admit to the database as tier E (tier D before the 2026-11-11 tier revision), exclude from individual-level fits, use in the species-level evolutionary analysis.**
4. **Fossil evidence classes. DECIDED: database holds all; fossil pellets are compared with extant pellets/scat, and fossil combat associations with extant "follow" (direct observation) studies.** Cautions: keep bird pellets and mammal scat as separate levels unless the fit shows they are indistinguishable; with two combat fossils the comparison supports only an inside/outside-the-range statement. Follow studies record failed attacks as well as kills, so the schema carries an `interaction_outcome` field (consumed, killed_not_consumed, attempted_failed, unknown) and combat fossils are compared against all outcomes.

5. **Taxonomic reassignments. DECIDED 2026-11-11: treat them with great scepticism unless several papers support the new identification.** Working rule: keep the original describers' identification. Record any proposed reassignment, with its source, in the notes. A paper that only adopts a reassignment by citing it is not independent support (for example, Qiu 2025 calling GMV 2124 "a small tyrannosauroid" on the authority of Cau 2024). If the two identities would call for different mass equations, report the estimate under both.
6. **Priorities. DECIDED 2026-11-11: theropods and terrestrial tetrapods first.** Some marine and piscivorous data are needed, and records already found stay in the database, but they are not to be searched for intensively yet. Turtles are low priority. The porpoise leads are deprioritised; the Galician series is dropped.

All six decisions are made.

## Plan

Each phase has a prompt written to be pasted into a fresh Opus 5.5 session in this repository. Run them in order. Phases 1 and 1b can run in parallel. Phases 3 and 4 can run in parallel after phases 1 and 1b.

**Where we are (2026-11-11).** Phases 0 and 1 are done. Next: Prompt 1b (if not finished), then Prompt 2.

### Phase 0. Source intake (can run any time, in parallel with phase 1)

**Outcome:** every legally obtainable paper, supplement and dataset on disk under its fixed name; one request list for JM covering the rest.

**Status: done 2026-11-11.** The intake session got 31 items. JM supplied 27 more by hand the same day. One item remains open, Ji et al. 2007: it was not found, and it is now optional (see `docs/SOURCE_REQUESTS_FOR_JM.md`). Farlow et al. 2005 is image-only; an OCR text sits beside it. Six were closed without a file. Of those, two were checked and hold nothing we need (Xing 2013 SI, Yuan 2004). Two were set aside under decision 6 (Kear 2006 SI deferred, Galician porpoises dropped). The Dryad CarniDIET file was replaced by the GitHub copy, and Gelman & Hill exist only in print. Do not rerun this prompt; add new requests to the gap list in phase 4.

> **Prompt 0.** You are working in the DietProject repository. Your job is source intake only: obtain as many of the needed papers, supplements and datasets as you legitimately can, file them under fixed names, and finish by asking JM for the ones you could not get. Do not extract data, edit analysis code, or change any table other than the ones named here.
>
> **Read first:** `docs/ASSESSMENT_AND_PLAN_2026-11.md` (sections "Extant data leads" and Phase 4), `docs/methodology_index.csv`, `pdfs/methodology/README.md`, `data/fossil_rebuild/README.md` (the lists of missing supplements and of gut records cited but not in the folder), and `data/fossil_rebuild/step1_papers.csv` (column `supplement_files`).
>
> **Build the want list** in `docs/source_intake_log.csv` with columns: `item_id`, `kind` (method_paper, fossil_paper, fossil_supplement, extant_paper, extant_dataset), `citation`, `doi_or_url`, `needed_for`, `target_folder`, `target_file`, `status`, `obtained_from`, `date`, `note`. Seed it from: every `NEEDED` row of the methodology index; the ten missing supplements and the cited-but-absent fossil gut papers in the fossil rebuild README; and every "paper needed" or open dataset in "Extant data leads". Before searching, check whether each item is already somewhere under `pdfs/` or `data/` under another name (for example `pdfs/reference/Ostrom_1978.pdf`); if so, record where and do not fetch it again.
>
> **Verify citations before fetching.** Several index rows say "from memory" or "verify". Confirm authors, year, journal, volume, pages and DOI from the publisher or an indexing service, correct the row in `docs/methodology_index.csv`, and only then look for the file. If a remembered paper does not exist as cited, say so; do not substitute a similar one silently.
>
> **Where you may get files.** Only sources that are lawful to download from: the publisher's page when the article is open access; PubMed Central; arXiv, bioRxiv and similar preprint servers; institutional and funder repositories; pages hosted by an author or their lab; journal archives that are free to read (for example SORA for older ornithology journals, Biodiversity Heritage Library); and data repositories (Dryad, Zenodo, iDiv, Figshare, GitHub). Do not use shadow libraries, do not try to get around a paywall or login, and do not use JM's name, email address or institutional credentials in any request. If a service asks for a contact email, use a generic client string or skip that service.
>
> **Filing rules.** Method papers go in `pdfs/methodology/` under exactly the `method_file` name in the index. Fossil papers and their supplements go in `pdfs/fossil_cases/` using the existing pattern `Author_etal_Year_Journal_Taxon_topic.pdf`, with supplements named after their paper plus `_SI`. Extant papers go in `pdfs/extant_to_ingest/toadd/`. Datasets go in `data/structured/raw_downloads/<short_name>/`, each in its own new folder with a small `SOURCE.md` giving URL, DOI, version, licence and download date. Never rename a file that already exists in `pdfs/methodology/` or `pdfs/fossil_cases/`.
>
> **Check each download.** Open it and confirm it is the right item: title and authors match, page count is plausible, the text layer exists (note it if the file is image-only), and it is the published version or clearly labelled as a preprint or accepted manuscript. Reject and delete error pages saved as PDF. Compare checksums against files already on disk and record duplicates instead of keeping two copies. Treat downloaded files as untrusted: do not run anything from them.
>
> **Record as you go.** After each item, update its row in the intake log with `status` set to one of `obtained`, `already_on_disk`, `open_but_download_failed`, `paywalled`, `not_found`, or `citation_unresolved`, plus where it came from. For method papers, also set `status` in `docs/methodology_index.csv` to `on_disk`. Add any newly discovered method paper to the index with a properly formed file name.
>
> **If the network is blocked.** This environment sometimes blocks outbound downloads. Test with one small open-access file first. If downloads fail, do not keep retrying: finish the citation checks and the search for lawful locations, record the working URL for each item, and tell JM that the sandbox must be turned off for the downloads to run.
>
> **Effort.** Work through the whole list. For each item try at least the DOI landing page, an open-access lookup, the authors' pages, and one repository search before marking it unobtainable. Stop on an item once those are exhausted.
>
> **Finish by asking JM.** End with a single numbered request list containing only the items you could not obtain, most important first (items that block fossil mass estimates, then method papers that an equation already in use depends on, then the rest). For each give: the full verified citation, the DOI or best URL, one line on why it is needed, why you could not get it (paywalled, not found, image-only, network blocked), and the exact folder and file name to save it under. Put the same list in `docs/SOURCE_REQUESTS_FOR_JM.md`. Then give a short summary: counts by status, anything whose citation turned out to be wrong, and any item where a lawful copy exists but is a preprint or differs from the published version.

### Phase 1. Schema and validator

**Outcome:** `db/` with empty, documented tables and a script that refuses bad rows.

**Status: done 2026-11-11.** `db/SCHEMA.md` is the reference; it supersedes the lists below where they differ. JM revised two vocabularies the same day: `pair_resolution` has five tiers (A_both_measured, B_pred_measured_prey_average, C_prey_measured_pred_average, D_both_average, E_coarse_or_aggregate), and `mass_method` has `unknown`. `pairs.csv` gained an `individual_pair` column. Check with `Rscript code/db_validate.R`; test the validator with `Rscript db/tests/run_tests.R`.

Tables:
- `pairs.csv`: one row per predator–prey record. Key fields: `pair_id`, `source_id`, `pred_taxon_id`, `prey_taxon_id`, `pred_specimen`, `prey_specimen`, `observation_type`, `pair_resolution`, `pred_mass_g`, `pred_mass_lo_g`, `pred_mass_hi_g`, `pred_mass_method`, `pred_mass_basis`, the same six for prey, `prey_stage`, `n_items`, `interaction_outcome`, `is_fossil`, `age_ma`, `quote_or_locator`, `notes`.
- `taxa.csv`: `taxon_id`, accepted name, rank, class, order, family, `tree_label`, `phylopic_uuid`.
- `sources.csv`: `source_id`, citation, DOI, file name, data type, where it was extracted from.
- `measurements.csv`: long format raw measurements behind any regressed mass.
- `methods.csv`: one row per estimation method: `method_id`, the equation, its valid range, and `method_file`, which must be the exact file name of the paper in `pdfs/methodology/` as listed in `docs/methodology_index.csv`. Every regressed or ratio-derived mass in `pairs.csv` points to a `method_id`. Analysis write-ups cite method papers by the same file name.
- `vocab.yml`: allowed values.

Controlled vocabularies:
- `observation_type`: stomach_content, regurgitate, pellet, scat, prey_remains, direct_observation, combat_association, bite_trace, dna_detection.
- `interaction_outcome`: consumed, killed_not_consumed, attempted_failed, unknown.
- `pair_resolution`: A_individual_pair, B_species_link_measured_sizes, C_species_link_average_sizes, D_coarse_or_aggregate. (Replaced 2026-11-11 by five mass-source tiers; see Status above.)
- `mass_method`: measured, regressed_from_length, regressed_from_bone, species_average_database, species_average_literature, size_class_midpoint, ratio_derived, assigned_approximate. (`unknown` added 2026-11-11.)

> **Prompt 1.** You are working in the DietProject repository. Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` first, then `README.md`. Build the database skeleton described in Phase 1 under a new top-level `db/` folder. Create the five CSV tables with headers only (including `methods.csv`), `db/vocab.yml` holding the four controlled vocabularies exactly as listed, and `db/SCHEMA.md` defining every column, its type, whether it is required, and one worked example row for each `pair_resolution` tier. Write `code/db_validate.R` that checks: required fields present; vocabulary values legal; every `source_id` and taxon id resolves; masses positive; lo ≤ point ≤ hi; any `regressed_*` or `ratio_derived` mass has a `method_id` that resolves in `db/methods.csv`, whose `method_file` appears verbatim in `docs/methodology_index.csv`, plus a `mass_basis` naming the input measurement; any tier A row has a specimen identifier for the predator. The script must exit non-zero and print offending rows on failure. Do not migrate any data yet and do not modify existing files in `data/` or `code/`. Finish by running the validator on the empty tables and on a small deliberately broken fixture in `db/tests/` to show it fails correctly. Report what you built and any schema choices you had to make that the plan did not specify.

### Phase 1b. Fossil step 1 top-up (added 2026-11-11; runs in parallel with phase 1)

**Outcome:** the step 1 tables cover every fossil paper and supplement now on disk, so phase 3 starts from a complete compilation.

Step 1 was run before phase 0. Since then, 16 fossil papers and 13 supplements for step 1 papers have arrived; they are listed in the prompt.

> **Prompt 1b.** You are working in the DietProject repository. Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` (decisions 5 and 6 especially), then everything in `data/fossil_rebuild/`: `README.md`, `step1_notes.md`, and the three `step1_*.csv` tables, so you know their columns, vocabularies (`evidence_type`, `value_type`) and quote standard. Your job is to extend step 1 to the sources it has not yet read, using exactly the same schema, rules and quote standard. Do not estimate masses, do not resolve the 11 logged conflicts (that is phase 3), do not create `db/`, and do not use `data/fossil_predprey.csv`, `data/fossil_diet_extractions.csv` or `data/fossil_ppmr_points.csv` as inputs.
>
> **New papers** (in `pdfs/fossil_cases/` unless stated): Cau 2024 (BSPI; GMV 2124 identity only), Charig & Milner 1997 (*Baryonyx*), Currie & Chen 2001 (*Sinosauropteryx*), Hone et al. 2022 (*Microraptor zhaoianus* mammal; supersedes the Larsson et al. 2010 abstract, which you should still record as its own row), Hwang et al. 2004 (*Huaxiagnathus*), Peyer 2006 (*Compsognathus*, Canjuers), Ostrom 1978 (`pdfs/reference/Ostrom_1978.pdf`; *Compsognathus*, Solnhofen), Wang et al. 2020 (*Abitusavis*), Zheng et al. 2014 (*Yanornis*), Zhou et al. 2002 (*Yanornis* IVPP V13259), Zhou et al. 2004 (*Yanornis* gastroliths and fish), Zhou et al. 2009 (*Jianchangornis*; possibly chimaeric), Zhou & Wang 2010 (*Yabeinosaurus*; a review, so say whether it is the primary report), Zheng et al. 2011, Zheng et al. 2018 and Zhou & Zhang 2002 (seeds; record them as plant diet so they can be filtered out). **New supplements for papers already in step 1:** Nesbitt 2006 ESM, Qiu 2025 SI, O'Connor 2019 SI, Therrien 2023 SI and Data S1–S3, Han 2023 SI 1 and 2, White 2022 SI Figs S31–S34, Smithwick 2017 SI, Wang 2022 SI, Wilson 2010 Texts S1–S6, Godoy 2014 Text S1, Hu 2005 SI. Skip any supplement that `step1_notes.md` already quotes. The White 2022 files are interactive 3D PDFs; read only their text and do not run embedded content. Do not run anything inside a zip. Kear 2006 SI is deferred; leave it.
>
> For each new paper, add rows to `step1_papers.csv`, `step1_gut_entries.csv` and `step1_measurements.csv`, and a section to `step1_notes.md` with verbatim quotes for every row. For each supplement, add only the measurements and statements it contributes, citing the supplement in `source_ref`. Rename new PDFs only if they do not already follow `Author_etal_Year_Journal_Taxon_topic.pdf`, and log any rename in `step1_rename_log.csv`. Where a specimen already in step 1 appears in a new source (for example the *Sinocalliopteryx* holotype JMP-V-05-8-01, GMV 2124, *Yanornis* IVPP V13259), do not overwrite the old row. Add the new source's values as their own measurement rows and log any disagreement as a new numbered conflict in `README.md`. Apply decision 5 to identifications: record the original describers' identification and list any reassignment with its source. Two known facts to check rather than assume: Benson et al. 2014 S11 (`data/mass_references/journal.pbio.1001853.s011.XLS`) gives *Sinocalliopteryx* JMP-V-05-8-01 femur 250 mm, "measured from figures in Ji et al. 2007" (Ji 2007 itself is not on disk); and the Xing 2013 Fig. S1 holds no measurements, so the QM V1002 values in conflict 11 remain untraced. Finish by updating `README.md`: new counts of papers, entries and measurements, the conflict list, and the lists of missing supplements and cited-but-absent gut records with each item marked on disk or still missing.

### Phase 2. Migrate extant data

**Outcome:** every extant record in `db/pairs.csv` with a tier and mass methods; a reconciliation report.

> **Prompt 2.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` and `db/SCHEMA.md`. Migrate all extant predator–prey data into `db/` by writing one script per source under `code/db_migrate/`, each idempotent and each appending only its own `source_id`s. Sources: `data/longform.csv` with `data/raw_data.csv` and `data/longform_refs.csv`; `data/structured/aviandiet_gut_ppmr.csv`; `data/extant_new_observations.csv`; `data/structured/raptor_diets_long.csv`; `data/structured/tucker2016_mammal_ppmr.csv`; `data/structured/gator_jekyll_stomach.csv`; `data/structured/glaudas2019_snake_rpm.csv`; the tegu and Varanus tables. For each row assign `observation_type`, `pair_resolution`, `individual_pair` and both `mass_method` fields from evidence in the source, not by default. Read `code/x20_gutSlopesStan.R`, `code/x03_massData.R` and `data/mass_overrides.csv` to recover how each mass was actually produced. Rules: taxon-average masses are `species_average_*`; the alligator rows ref 207–251 are tier E with `size_class_midpoint` predators and `assigned_approximate` prey; Jekyll prey masses are remains weights, so record them in `measurements.csv` and leave `prey_mass_g` empty with a note; Glaudas rows are tier E with `ratio_derived` prey mass and need a documented snake length-to-mass equation with its citation, or stay without absolute masses if you cannot source one. Never invent a mass. Where a method cannot be determined, use an explicit `unknown` and list the rows. Do not change the old files. Deliver `db/MIGRATION_REPORT.md` with row counts in and out per source, counts by tier, method and observation type, every row dropped and why, and a comparison showing the migrated tier A–D vertebrate-prey subset reproduces the 15,260-row model input within a stated tolerance, or an explanation of each difference. Run `code/db_validate.R` and include its output.

### Phase 3. Fossil step 2: masses and conflicts

**Outcome:** fossil pairs in the database with mass intervals and methods.

**Status: done 2026-11-12.** `code/db_migrate/10_fossil_step2.R` writes 74 fossil pairs to `db/pairs.csv` and lists 32 excluded entries in `db/excluded_fossil_records.csv`. Fourteen pairs have both masses. Conflicts 1–27 are decided in `data/fossil_rebuild/step2_conflicts.md`, plus two new ones (28–29); five remain unresolved for want of a file. Estimates, intervals and the comparison with the old tables are in `data/fossil_rebuild/step2_mass_estimates.md`. Five method papers are `NEEDED` (listed in `docs/SOURCE_REQUESTS_FOR_JM.md`), and 25 pair-sides use methods marked `verified = FALSE` until they arrive.

> **Prompt 3.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md`, then everything in `data/fossil_rebuild/` including `step1_notes.md` and the conflicts listed in its `README.md`. Do not use `data/fossil_predprey.csv`, `data/fossil_diet_extractions.csv` or `data/fossil_ppmr_points.csv` as inputs; they may be consulted only to flag disagreements. Step A: resolve each of the 11 conflicts by re-reading the PDF in `pdfs/fossil_cases/` (render the page image when the text layer is ambiguous), record the decision and the evidence in `data/fossil_rebuild/step2_conflicts.md`, and leave unresolved any that need a supplement that is not on disk. Step B: for every entry in `step1_gut_entries.csv`, estimate predator and prey body mass with a point value and an interval. Use published equations only, and record the input measurement. Register each equation in `db/methods.csv` with its `method_file` set to the exact file name in `docs/methodology_index.csv`; if the paper is not in `pdfs/methodology/`, add a NEEDED row to the index with a file name following the folder's naming rule and list it for JM to obtain. Do not apply an equation from memory or from a secondary citation without marking the row as unverified. Preferred order: mass reported by the authors with their method; limb-bone circumference or length equations appropriate to the clade (the references in `data/mass_references/`); length-based equations; scaling from a named comparison specimen. Where prey is fragmentary, give a bracket and say what bounds it. Where no defensible estimate exists, leave it empty and say why. Step C: write the results into `db/pairs.csv`, `db/measurements.csv`, `db/taxa.csv` and `db/sources.csv` with `is_fossil` true, an `age_ma`, the correct `observation_type` (ingested prey, pellet, combat association, bite trace), and `individual_pair` TRUE for specimen-level pairs, with the tier set by the mass methods (tier A when both masses come from the specimens). Exclude rejected claims and no-contents specimens from `pairs.csv` but list them in a `db/excluded_fossil_records.csv`. Deliver `data/fossil_rebuild/step2_mass_estimates.md` showing each estimate, equation, input and interval, plus a table of every place the new values differ from the old fossil tables by more than 25 percent. Follow the repository rule: read the source before calling any value wrong. Start only after Prompt 1b has finished, and check that its conflicts were added to the README. Apply decision 5 wherever a specimen's identity decides which equation applies (GMV 2124 is the known case). `pdfs/methodology/Farlow_etal_2005_JVP_alligator_femur_body_size.pdf` is image-only. Search the OCR text beside it (`.ocr.txt`) to find things, but take every coefficient from the rendered page image, and quote the page.

### Phase 3b. Fossil mass top-up (added 2026-11-12)

**Outcome:** every fossil pair has the best mass estimate the evidence allows, however noisy, with an honest interval or bracket.

**Why.** Phase 3 left a mass empty wherever no published equation applied to the specimen, so 60 of 74 fossil pairs lack one or both masses. Roughly: 31 are fish prey with no published length; 16 have identified prey but no equation or taxon mass on disk (*Protoceratops*, *Confuciusornis*, *Zhangheotherium*, *Sinobaatar*, the *Scipionyx* and *Anchiornis* lizards); 5 have indeterminate prey; 10 lack a predator specimen or are out of scope. Extant prey masses are almost all species averages (tier D), so a taxon-average mass for an identified fossil prey item is no weaker than the reference it is compared with. The Phase 6 model accepts interval-censored prey masses, so a bracket with no point value is usable.

> **Prompt 3b.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md`, `data/fossil_rebuild/step2_mass_estimates.md` and `db/SCHEMA.md`. Goal: give every fossil pair in `db/pairs.csv` that lacks a predator or prey mass the best estimate the evidence allows, however noisy, with an honest interval. This relaxes the Phase 3 rule that left a mass empty when no published equation applied to the specimen. For each such pair, work down this list and stop at the first that applies. (1) A published equation applied to a published measurement; obtain missing equations (lizard, ceratopsian, hadrosaur and sauropod-hatchling length–mass; fish length–mass by body form), register them in `db/methods.csv` and `docs/methodology_index.csv`, and add any paper you cannot get to a numbered request list for JM. (2) A measurement you take from a published figure with a scale bar, recorded in `measurements.csv` with the figure, the scale bar and a note that it was measured from the figure, then an equation as in (1). (3) For prey identified to genus or species, a taxon-average mass from published estimates for that taxon, as `species_average_literature` with the citation and an interval spanning the published range or at least a factor of three either way; the pair becomes tier B. (4) A bracket with no point value: lower and upper bounds from element size, cluster size, a comparison specimen or a swallow-whole limit; fill `prey_mass_lo_g` and `prey_mass_hi_g`, leave the point empty, and state what sets each bound. (5) An upper bound only. For predators without limb bones, use a taxon range for the stated ontogenetic stage as an interval. Do not estimate masses for bite traces, isolated pellets with no producer specimen, invertebrate prey, or records marked WEAK. Never present a guess as a measurement: every row must say which rung it used. Follow decision 5 and the rule to read the source before calling a value wrong. Update `db/` through the existing `10_fossil_step2.R` path so the validator passes, extend the validator if bracket-only rows need it, and deliver `data/fossil_rebuild/step3b_topup.md` with a table of every pair: rung used, value, interval, source, and the pairs still empty with the reason.

### Phase 4. Completeness

**Outcome:** a written search protocol, a gap list with status, and new records.

> **Prompt 4.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` and `data/fossil_rebuild/README.md`. Produce `docs/SEARCH_PROTOCOL.md` and `db/gap_list.csv`. Part 1, fossils: using web search, compile every published case of a direct fossil predator–prey relationship among vertebrates where both parties are identifiable: stomach or intestinal contents, pellets and regurgitates attributable to a producer, preserved combat or predation in progress, and embedded teeth. Scope is TETRAPOD predators of any age, not only Mesozoic theropods; prey may be any vertebrate. Exclude fish and sharks as predators. Work in the priority order of decision 6. First, theropods including birds. Second, other terrestrial tetrapods: crocodyliforms, lizards, snakes, mammals, amphibians and pterosaurs. Third, marine reptiles and turtles. For the third group, record cases you meet in reviews or while searching the first two, but do not search for them exhaustively, and say so in the protocol. For each case record citation, DOI, predator, prey, specimen number, evidence class, whether the PDF is open access, and whether it is already in `data/fossil_rebuild/step1_papers.csv`. Start from the gut records cited but not on disk and the missing supplements listed in the rebuild README. Part 2, extant: tabulate records in `db/pairs.csv` by predator order, tier and observation type, identify orders with fewer than 20 tier A–B vertebrate-prey records, and for those search for datasets with individual predator sizes and measured or length-derived prey sizes (data repositories first, then papers). Start the extant half from the verified leads in the section "Extant data leads" of this document. Marine and piscivorous predators (porpoises, seabirds) are lower priority under decision 6: list leads but do not chase them. Do not download copyrighted PDFs. Whenever a paper, supplement or dataset is needed and is not on disk, STOP ADDING TO THAT ITEM AND ASK JM FOR IT: end the session with a single numbered request list giving, for each item, the citation, DOI or URL, why it is needed, and the exact file name and folder to save it under (`pdfs/fossil_cases/`, `pdfs/extant_to_ingest/toadd/`, or `pdfs/methodology/` with the name from `docs/methodology_index.csv`). Record the same items in `db/gap_list.csv` with status `requested_from_JM`. Method papers go in the methodology index, not the gap list. Do not add anything to `db/pairs.csv` in this phase. State the search terms, databases and dates used so the search can be repeated, and state plainly which groups you could not cover.

### Phase 4b. Extant expansion (added 2026-11-12)

**Outcome:** the extant side of `db/` is as broad as the sources on disk and the open datasets allow. Principle (JM, 2026-11-12): admit as much extant data as possible, tagged by tier and method, and test conclusions by narrowing in the analysis.

**Why.** Three blocks of data are not in the model input. (a) PDFs on disk that were never extracted: `data/intake_manifest.csv` lists 182 as queued, and some were skipped in June 2026 as belonging to "well-sampled clades". Shrikes are the example: `db/` holds only *Lanius excubitor* from two studies (Atkinson 1993, Brzeziński 2010; 141 pairs, 77 with both masses), two further *L. excubitor* PDFs sit in `pdfs/_processed/extant_pdf/herps_2026-06-04/skipped/`, and Tryjanowski et al. 2003 on *L. collurio* is filed only as a methods comparison. (b) 20,372 pairs have a predator mass but no prey mass, mostly prey identified above species level and invertebrates. (c) The open datasets in "Extant data leads" have not been ingested.

> **Prompt 4b.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` (this phase, "Extant data leads", decisions 1–6), `db/SCHEMA.md`, `db/MIGRATION_REPORT.md`, and `docs/SEARCH_PROTOCOL.md` and `db/gap_list.csv` if they exist. Goal: widen the extant data in `db/`. Breadth comes first: admit any record with an identifiable tetrapod predator, an identifiable prey and a defensible mass, assign its tier, observation type and mass methods from the source, and leave narrowing to the analysis. Follow every rule of Prompt 2: one idempotent script per source under `code/db_migrate/`, added to `run_all.R`; never invent a mass; read the source before coding anything; old files unchanged; fossil rows untouched. Part 1, PDFs on disk: work through the files with status `queued` in `data/intake_manifest.csv`, everything under `pdfs/extant_to_ingest/`, and the files under `pdfs/_processed/extant_pdf/herps_2026-06-04/skipped/`. Order: shrikes first (the two skipped *Lanius excubitor* PDFs; Tryjanowski et al. 2003 for *L. collurio* if it gives counts that can be turned into pairs); then other vertebrate-eating birds that are not raptors or owls; then lizards, snakes, crocodylians and small mammalian carnivores; marine and piscivorous sources last (decision 6). Record in a table each file, what it yielded, or why it yielded nothing. Part 2, prey masses for the pairs that have a predator mass and no prey mass: for prey identified to species use the same databases as Prompt 2; for prey identified to genus, family or order assign a taxon-level typical mass with a stated source and a wide interval, as `assigned_approximate` and tier E; for invertebrates use published length–mass equations where the source gives a length, otherwise an order-level typical mass with its source, also tier E. List the rows that remain without a mass. Part 3, open datasets: ingest the tetrapod-predator records of SquamataBase, then CarniDIET and GATEWAy, each as its own script, checking for publications already in `db/` and dropping duplicates by the rule in the migration report. If network access is unavailable, stop that part and list exactly which files JM must supply and where to put them. Deliver `db/EXPANSION_REPORT.md` with rows added per source, species added per clade, the change in the clade by observation type by tier table of species counts, every duplicate dropped, and the validator output.

### Phase 5. Phylogeny and evolutionary analysis

**Outcome:** a dated tree matched to the database and a first evolutionary analysis.

> **Prompt 5.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` and `db/SCHEMA.md`. Goal: analyse how predator–prey mass ratio evolves across tetrapods (tetrapod predators only; prey may be any vertebrate). Step A: define the species-level trait and write the definition in `docs/EVOLUTION_METHODS.md` before fitting anything. Fit a model of log prey mass on log predator mass with species-level intercepts, using tier A–D vertebrate-prey rows, observation type as a covariate, and report for each species the expected log ratio at a stated reference predator mass with its uncertainty and its sample size. Say how species with a single record and tier E species-mean data are handled. Step B: obtain a dated tree covering the species in `db/taxa.csv`. Use published time trees for each major group and a published backbone to join them, record every source and version in `db/tree/README.md`, fill `tree_label` in `db/taxa.csv`, and report the match rate. Place fossil tips by their published position and age with a documented rule; do not guess. If network access is unavailable, stop and list exactly which files JM must supply. Step C: estimate phylogenetic signal, fit Brownian, Ornstein–Uhlenbeck and early-burst models to the species trait with measurement error included, reconstruct along the tree, and test whether fossil taxa fall inside the range predicted for their position. Check sensitivity to tier, to observation type, and to dropping the best-sampled clades. Use `cores=1` for any Stan fit if chains hang at warm-up (see the sandbox note in the project memory). Deliver the methods file, a results file with effect sizes and intervals, figures, and a short list of conclusions the data do not support.

### Phase 6. Clade-nested regression and fossil similarity test (rewritten 2026-11-12, second revision)

**Outcome:** one hierarchical Bayesian regression of prey mass on predator mass, partially pooled across major clades and the minor clades inside them, fitted to extant data with no phylogeny; and a test of which major and minor extant clades each fossil group most resembles.

**What changed.** The tree-pooled model (node states, κ, α) is deferred, not dropped. Its text is in `docs/history/phase6_tree_deferred_2026-11-12.md` and it becomes a later phase that builds on this one. Reason (JM, 2026-11-12): a cleaner phylogeny-free regression is wanted first; branch lengths come later. The earlier four-step plan stays in `docs/history/phase6_superseded_2026-11-12.md`.

**Design decisions.**

- Two nested grouping levels fixed a priori. Major clades: Aves, Mammalia, Squamata (JM's "Lacertilia"; it contains Serpentes), Crocodylia. Minor clades are named groups inside each, of mixed rank (Strigiformes, Serpentes, Caniformia). Species sit inside minor clades. A family level between minor clade and species is a sensitivity fit only.
- Intercept, slope and residual spread all vary by major and minor clade, each partially pooled toward its parent. Species and source get intercepts only.
- The observation-type effect has a shared part and a major-clade deviation that is pooled toward zero.
- The model is fitted to extant data only. Fossils never inform the extant parameters. Each fossil group is scored against every major clade, every minor clade and a "none of the above" candidate by joint posterior predictive density.
- No likelihood weights, no subsampling. The Gaussian likelihood is evaluated on exact sufficient statistics so that all rows are used and the fit takes minutes.

**State of the data (counted from `db/` on 2026-11-12, extant, vertebrate prey, both masses present).** 76,162 rows; about 1,290 predator species. Species per major clade: Aves 325, Mammalia 184, Serpentes 889, other Squamata 53, Crocodylia 6. Rows by observation type:

| | Aves | Mammalia | Serpentes | other Squamata | Crocodylia |
|---|---|---|---|---|---|
| stomach_content | 1,056 | 2,234 | 5,109 | 380 | 165 |
| pellet | 38,364 | 0 | 0 | 0 | 0 |
| scat | 0 | 14,937 | 224 | 74 | 0 |
| prey_remains | 6,158 | 1,217 | 0 | 0 | 0 |
| direct_observation | 2,738 | 288 | 733 | 8 | 1 |
| regurgitate | 132 | 0 | 751 | 2 | 0 |
| dna_detection | 109 | 0 | 0 | 0 | 0 |
| unknown | 488 | 549 | 379 | 66 | 0 |

The median species has one distinct predator mass, so slopes are among-species relationships. 433 species have more than one observation type. Fossils: 74 pairs, of which roughly 29 non-avian theropod records (11 taxa) and 24 stem-bird records (4 taxa, all fish prey) have a predator mass and a prey point or bracket. Two crocodyliforms, *Repenomamus* and *Sanajeh* are also usable and serve as informal positive controls.

**Known limits to report against.** Pellets exist only in birds and scat almost only in mammals, so the clade-specific observation offsets for those cells come from the prior. Four major clades cannot estimate an among-major variance, so "none of the above" rests on a stated prior. Crocodylia has 6 species. Stem-bird fossils are all fish prey while the extant bird reference is mostly raptors and owls eating terrestrial vertebrates. The vertebrate-only filter truncates small predators' diets (M4). Fossil specimens, several of them juveniles, are scored against among-species lines (M3).

> **Prompt 6.** You are working in the DietProject repository. Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` (the section "Review of the statistical model", decisions 1–6, and all of Phase 6 including the data table and limits above this prompt), `db/SCHEMA.md`, `db/EXPANSION_REPORT.md` and `data/fossil_rebuild/step3b_topup.md`. Goal: a hierarchical Bayesian regression of ln prey mass on ln predator mass, partially pooled over major clades and the minor clades nested in them, with no phylogeny, fitted to extant data in Stan through `cmdstanr` (not brms); then a test of which major and minor extant clades each fossil group most resembles; then tidy exports that the Phase 7 site reads. Do not use a tree, branch lengths or `db/tree/` anywhere. Use only `db/` as data input. Do not edit `db/pairs.csv`, `db/sources.csv` or `db/measurements.csv`, do not rerun `code/db_migrate/` (`10_fossil_step2.R` is known to fail), and do not read the old tables in `data/`. Work in `code/db_analysis/` with scripts named `b01_…`, `b02_…` in run order; intermediates go in `results/6_work/`, the report in `results/6_model.md`, site exports in `results/site/`.
>
> **Order of work and checkpoints.** JM needs the headline quickly. Do Steps A, B and C first and write a first complete `results/6_model.md` and the Step D exports for the primary fit before starting Steps E and F. Update the report as later steps finish. If a later step runs out of time, say which fits were not run; never leave a table half-filled without a note.
>
> **Step A, clade map and analysis table (no model).**
> (1) Write `db/clade_map.csv` with one row per predator taxon that has data: `taxon_id`, `major_clade`, `minor_clade`, `family_clean`, `basis`. Major clades are Aves, Mammalia, Squamata and Crocodylia. Minor clades, fixed before any fit: in Aves, the order (merge Ardeiformes into Pelecaniformes); in Mammalia, Caniformia, Feliformia, Dasyuromorphia, Didelphimorphia; in Squamata, Serpentes, Varanidae, Teiioidea (Teiidae and Gymnophthalmidae), Iguania, and other Squamata; in Crocodylia, Alligatoridae and Crocodylidae (Gavialidae if present). Any minor clade with fewer than 5 predator species in the vertebrate-prey table is merged into "other <major clade>". Derive the map from `db/taxa.csv`, but its taxonomy has known faults that you must repair in the map, not in `taxa.csv`: about 90 predator species have a blank order and about 200 a blank family; "Acciptridae" is a misspelling; Dasyuridae appear under Didelphimorphia and under "Metatheria"; snakes appear under order "Serpentes" and under "Squamata". Resolve blanks from the trait tables on disk (`data/AVONET`, `data/pantheria`, SquamataBase taxonomy in `data/structured/raw_downloads/`), record what you used in `basis`, and list any species you cannot place; those are dropped and counted. Never assign a clade by matching part of a name.
> (2) Build the analysis table: extant rows with a tetrapod predator in the four major clades, a predator mass and a prey mass. Primary prey filter is vertebrate prey only (`is_vertebrate` of the prey taxon), which is the bone filter that matches fossil gut contents; an all-animal-prey version is built for a sensitivity fit. Keep all tiers. Flag fish prey, tier, `individual_pair`, source family (OS-Prey sources end in `_OSP`; also flag CarniDIET, GATEWAy, SquamataBase), and observation type. Observation types are used as coded in `db/`; keep `unknown` and `dna_detection` as their own levels. Response is ln prey mass in grams; predictor is m = ln predator mass − ln(1000 g).
> (3) Decide the unit and say so in the report: one row of `db/pairs.csv` is one record in the primary fit, unweighted. Tabulate how often `n_items` is filled, by source family, so the item-weighted sensitivity in Step E can be interpreted.
> (4) Report rows, species and sources by major clade × minor clade × observation type, the same by tier, the range of predator mass in each minor clade, and the number of species with more than one observation type in each major clade. Plot raw ln prey against ln predator by minor clade with 10/50/90 percent conditional quantiles.
> (5) Build the fossil table from `is_fossil` rows. Exclude bite traces, records with no predator mass or interval, and invertebrate prey. Classify every remaining record as: point with interval, bracket only, upper bound only, or excluded with the reason. Read `step3b_topup.md` to learn what each interval represents (a range, or a stated confidence level) and write down the rule you use to turn it into a standard deviation on the log scale. Map fossil observation types: stomach_content to stomach_content, pellet to pellet, combat_association to direct_observation. Fossil groups for the test: non-avian theropods (`analysis_clade` Theropoda), stem birds (Avialae), and each other fossil predator taxon on its own (the crocodyliforms, *Repenomamus*, *Sanajeh*). Also keep every fossil taxon as its own unit.
>
> **Step B, the extant model.** Write the model, every prior and the reasons in `docs/MODEL_6.md` before fitting. Indices: record i, species j, source r, minor clade k, major clade c(k), observation type o.
>
> Mean: μ_i = α0 + A_c + a_k + u_j + w_r + (β0 + B_c + b_k)·m_i + δ_o + d_{o,c}.
> Spread: ln s_i = γ0 + G_c + g_k + γ1·m_i + ζ_o.
> Likelihood: ln prey_i ~ Normal(μ_i, s_i).
>
> Pooling: A_c ~ Normal(0, τ_A); a_k ~ Normal(0, τ_a); u_j ~ Normal(0, τ_u[c]) with ln τ_u[c] partially pooled across major clades; w_r ~ Normal(0, τ_w); B_c ~ Normal(0, τ_B); b_k ~ Normal(0, τ_b); G_c ~ Normal(0, τ_G); g_k ~ Normal(0, τ_g); d_{o,c} ~ Normal(0, τ_d). Stomach contents is the baseline: δ and ζ are zero for it and d is zero for it in every clade. Use non-centred parameterisations throughout.
>
> Starting priors (adjust only on the evidence of the prior predictive check, and record any change): α0 ~ Normal(ln 50, 2); β0 ~ Normal(1, 0.5); γ0 ~ Normal(0, 1); γ1 ~ Normal(0, 0.25); δ_o ~ Normal(0, 1); ζ_o ~ Normal(0, 0.5); τ_A, τ_a, τ_u, τ_w ~ half-Normal(0, 1); τ_B, τ_b ~ half-Normal(0, 0.3); τ_G, τ_g ~ half-Normal(0, 0.5); τ_d ~ half-Normal(0, 0.5). With four major clades τ_A, τ_B and τ_G are set largely by their priors; say so, and show prior against posterior for each.
>
> Computation: all records in a cell defined by species × source × observation type × predator mass share μ and s, so the Gaussian log likelihood of the cell is exactly −n·ln s − [SS + n·(ȳ − μ)²]/(2s²) up to a constant, where n, ȳ and SS are the cell's count, mean and sum of squared deviations of ln prey. Fit on these sufficient statistics. Check on a subset of a few thousand rows that the cell version and a row-by-row version give the same log posterior. Run 4 chains; if parallel chains hang at warm-up, run them one core at a time.
>
> Checks before interpreting anything: prior predictive check; R-hat, effective sample size, divergences and tree depth; a posterior predictive check by minor clade and by major clade × observation type, reporting the median probability integral transform of the observed data (target 0.5) and the coverage of the 10–90 percent interval (target 0.8). The old model failed this for Crocodylia (M2); if any clade fails here, report it prominently and do not use that clade as a candidate without the warning. Report for each d_{o,c} whether its posterior differs from its prior; the cells with no data (pellets outside birds, scat in crocodylians) must be labelled as prior-only.
>
> Report: the line for each major and minor clade (intercept at 1 kg, slope, spread at 1 kg, each with an interval), the variance components, the observation offsets, and for each minor clade the no-pooling estimate next to the partially pooled one, so the amount of shrinkage is visible.
>
> **Step C, fossil similarity test.** Done in R from the posterior draws of the extant fit; no refit. For a fossil group with taxa j and records i, the candidates are every minor clade, every major clade, and "none of the above".
>
> - Minor-clade candidate k: the group's taxa are new species of clade k. Use that clade's intercept, slope and spread, with a new species effect u*_j ~ Normal(0, τ_u[c(k)]) shared by all records of a taxon and a new source effect per fossil source.
> - Major-clade candidate c: the group is one new minor clade of c. Draw a*, b* and g* from Normal(0, τ_a), Normal(0, τ_b) and Normal(0, τ_g), shared by the whole group, then species and source effects as above.
> - None of the above: the group is one new major clade. Draw A*, B* and G* from their hyper-distributions, then a new minor clade, species and sources.
>
> Observation offsets: δ_o + d_{o,c} for the candidate's major clade; for "none of the above" draw d from Normal(0, τ_d). Mark every fossil record whose observation type has no extant data in the candidate clade as extrapolated.
> Masses: the predator's true ln mass is drawn from a normal centred on the point estimate with the standard deviation from its interval, or uniformly on the log scale between the bounds when there is no point; records from one specimen share the draw. A prey point with an interval contributes Normal(ln prey | μ, sqrt(s² + se²)). A bracket contributes Φ((ln hi − μ)/s) − Φ((ln lo − μ)/s). An upper bound contributes Φ((ln hi − μ)/s).
> Score: for each posterior draw, average the group's joint likelihood (the product over its records) over at least 200 draws of the new effects and masses; then average over posterior draws. The log of that is the group's joint log predictive density under the candidate. Use log-sum-exp throughout. Report it per candidate with its Monte Carlo standard error (batch means over posterior draws), and convert to relative support with equal prior weight, separately for three comparisons: the four major clades plus none of the above; all minor clades; and the minor clades within the best major clade. Do this for each group and for each fossil taxon alone.
> Also report a tree-free second test: each record's predictive percentile against each major and minor clade line at its predator mass and observation type, with mass uncertainty propagated.
> Calibration of the test, which needs no refit: (a) simulate fossil data at the real fossil design (same taxa, records per taxon, predator masses, observation types, intervals and brackets) from each candidate clade in turn, 200 replicates each, run the test, and report the confusion matrix for major and for minor clades and how often "none of the above" wins. (b) For each extant minor clade with at least 15 species, draw 100 thinned samples that mimic the theropod design (about 11 species, 1–4 records each, stomach contents where the clade has them), score them against all candidates using the full-data posterior, and report how often the true major and minor clade ranks first. Note in the report that (b) is optimistic because the sample's own data helped fit its clade; Step E repeats it properly for a few clades.
> State plainly, group by group, when candidates are not distinguishable. Do not tune anything to change where theropods fall. Report the positive controls (*Repenomamus* against Mammalia, the crocodyliforms against Crocodylia, *Sanajeh* against Serpentes) whatever they show.
>
> **Step D, exports for the site.** One folder per fit, `results/site/<fit_id>/`, same files and columns in every folder, plus `results/site/fits.csv` listing `fit_id`, a short label, the ladder rung or narrowing it belongs to, row and species counts, and diagnostics. Files: `lines.csv` (major and minor clade, observation type, a grid of predator mass, posterior median and 80 percent interval of the mean, and the 10/50/90 percent predictive quantiles of prey mass); `species.csv` (one row per species: clade labels, predator mass, posterior median of expected ln prey, the 10–90 percent predictive prey interval used for the spindles, number of records, observation types present); `shrinkage.csv` (per minor clade: no-pooling and partially pooled intercept, slope and spread with intervals, and species and row counts); `obs_offsets.csv`; `fossils.csv` (one row per record: taxon, group, specimen, masses with intervals or brackets, record class, observation type, extrapolated flag, percentile against each major clade); `fossil_support.csv` (group or taxon, comparison, candidate, log predictive density, its standard error, relative support). Once, outside the fit folders: `results/site/points.csv` with every extant and fossil record used (`pair_id`, clade labels, masses, observation type, tier, mass methods, `source_id`, and the flags from Step A) so the site can filter points without model output. Write `results/site/README.md` defining every column.
>
> **Step E, ladder, validation and narrowing.** Every fit here reuses the Step B code, repeats Steps C and D, and gets its own `fit_id`.
> (1) Pooling ladder, five rungs: one line for all tetrapods (complete pooling); major clades only, unpooled; minor clades, unpooled; major and minor clades partially pooled (the primary model); the primary model plus a family level between minor clade and species. All rungs keep the species and source effects and the observation terms. Compare by 5-fold cross-validation that leaves out whole species, and again leaving out whole minor clades (one fold per major clade where possible); report the difference in expected log predictive density at each step with its standard error.
> (2) Proper calibration of the similarity test: for one large minor clade in each major clade and for Serpentes, refit without it, thin it to the theropod design, and score it. The right answer at the minor level is unavailable by construction; report whether its major clade ranks first and how much support "none of the above" receives.
> (3) Narrowing, each a full refit of the primary model: all animal prey; stomach contents only; fish prey removed (extant and fossil); juvenile and hatchling fossil predators removed (Step C only, no refit); bracket-only fossils removed (Step C only); tiers A–D; individual pairs only where a major clade keeps at least five species; OS-Prey removed; CarniDIET and GATEWAy removed; records weighted by `n_items` where it is filled (weighted sufficient statistics; say what was done with rows where it is empty); observation offsets without the major-clade deviation d; Student-t likelihood with ν ~ Gamma(2, 0.1) truncated below at 2, fitted row by row with within-chain threading, to test how far the Gaussian tails drive the fossil result (M5); the hyperprior scales on τ_A, τ_B and τ_G halved and doubled.
> (4) One recovery check: simulate data from the fitted primary model on the real design five times, refit, and report bias and interval coverage for the clade lines, the variance components and the observation offsets.
>
> **Step F, report.** `results/6_model.md`, written for a reader who has not seen this prompt: the clade map and counts; the model in one paragraph with the priors; diagnostics and predictive checks; clade lines; what the observation offsets rest on; the similarity result for each fossil group and taxon at major and minor level, with the calibration results beside it; the ladder table; a table of how the fossil result moves under each narrowing; agreement or disagreement with the Phase 5 results in `results/evolution/`; and a list of conclusions the data do not support. Lead with the answer to "which major and minor clades do non-avian theropods and stem birds most resemble, and how sure is that". Give numbers with intervals. Where a fit failed its diagnostics, show it as failed and do not report its estimates as results.

### Phase 7. Presentation site

**Outcome:** a static site on GitHub Pages that carries a talk of under 15 minutes.

> **Prompt 7.** Read `docs/ASSESSMENT_AND_PLAN_2026-11.md` `results/6_model.md` and the Phase 5 results. Take every model output from `results/site/`. Build a static site in `site/` deployable to GitHub Pages with no server. Write `code/site_export.R` that produces `site/data/*.json` from `db/` and the model outputs, so the site never reads analysis files directly. Structure the site as a linear talk of about 12 screens that can be advanced with the arrow keys and also scrolled. Individual slides should be EXTREMELY text-light. A short title conveying the main takeaway and then an interactive plot or key visualization per slide with simple legends & axis labels. No graph subtitles or sentence-length descriptions. Graphs & their interactivity or looping (if GIFs) should show the message with my narration rather than text on the screen. The outline roughly follows: 
(1) Intro discussing fossil diet data
(2) Difficulties connecting extinct to extant due to bias and taphonomy
(3) Connect to my Paleobiology paper: find extant data analogous to fossil data for comparison
(4) Mention gut contents & difficulty: bone found in a vertebrate is fossilizable & identifiable; fur found in a vertebrate is identifiable and **maybe** fossilizable (eg if mouse fur is found a chance for mouse bones--but also maybe not depending on digestion); bugs/other features found in stomach may not be preservable at all
(5) Brief visual explanation of pooling, connected directly to phylogenetic models audience familiar with (eg white noise = no pooling, OU = single shared pool, BM = pooling at every node proportional to divergence time)
(6) Show regression lines of extant predator/prey with toggles for group & observation type. Both points & spindles & regression lines that can all be toggled on/off to show how variance in prey increases along with mean size in prey as predator size increases
(7) Show where theropods fall; show what, if any, trends on line to birds; highlight ziphodont taxa (lizards, not snakes/crocs/birds/mammals, but some [not all] theropods)
(8) Show phylogeny of dinosaurs showing diet estimates; discuss how BM roughly means "concordant with phylogenetic characters" and OU roughly means "discordant with phylogenetic characters"
(9) Bring in the Alnashetri paper where we fit OU/BM to every most parsimonious tree, divided trees up by what model they supported, and made strict consensus of "MPTs that support BM" and also "MPTs that support OU" to have the phylo models make testable predictions (e.g., if BM is right you expect further studies support topology A over B)
(10) Teaser show hierarchical bayesian approach to growth curve histology data?
(11) Summary
(12) Acknowledgements: NSF, Coe College, PhyloPic, Faith Hysell, Mike Grundler (SquamataBase) & other major database references
For the plots: the database, as an interactive log–log plot of prey mass against predator mass with a prominent FIT SELECTOR that steps through the pooling ladder (one line → major clades unpooled → minor clades unpooled → major and minor partially pooled → plus family; the `fit_id`s in `results/site/fits.csv`) and the narrowing fits (prey filter, observation type, tier, fish prey, OS-Prey), redrawing the regime lines, species spindles and the fossil regime support from the matching fit, so the presenter can show what each step adds and whether the answer changes; plus filters for clade, observation type, mass method and fossil or extant, and a tooltip showing the source and how each mass was obtained; the tree with the ratio mapped on it; the fossil comparison by observation type. Use PhyloPic silhouettes via the `phylopic_uuid` field in `db/taxa.csv`; fetch through the PhyloPic API at build time, store the images locally, and generate a credits page with each image's author and licence, skipping any image whose licence does not permit reuse. Keep the plotting library to one dependency loaded from a pinned version. Dark theme is the default and every plot, silhouette and table must look right in it, with a light toggle for bright rooms. Provide a presenter mode: a key opens a second browser window, kept in step with the main one over `BroadcastChannel`, showing the current and next screen, speaker notes and a timer. The page must work at laptop projector resolution, and every plot must have a static fallback image. Add a GitHub Actions workflow that builds and deploys from the default branch.

## Order and effort

| Phase | Depends on | Rough size |
|---|---|---|
| 0 Source intake | none | medium (done 2026-11-11) |
| 1 Schema | decisions 1–4 | small |
| 1b Fossil step 1 top-up | 0 | small to medium |
| 2 Extant migration | 1 | large |
| 3 Fossil masses | 1, 1b | medium |
| 4 Completeness | 1 (2 for the extant half) | medium, then ongoing |
| 3b Fossil mass top-up | 3 | small to medium |
| 4b Extant expansion | 2 | large |
| 5 Evolution | 2, 3 | large |
| 6 Clade-nested regression and fossil similarity test | 3b, 4b | medium |
| 6-tree Tree-pooled model (deferred; `docs/history/phase6_tree_deferred_2026-11-12.md`) | 5, 6 | large, compute-heavy |
| 7 Site | 5, 6 | medium |

The first useful checkpoint is after phases 1–3: at that point goals 1, 2, 3 and 5 can be checked directly against the database, and the tier counts will show how much of the current model rests on confirmed pairs.
