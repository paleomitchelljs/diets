# Database schema (`db/`)

Written 2026-11-11 for Phase 1 of `docs/ASSESSMENT_AND_PLAN_2026-11.md`, and revised the same day after JM's review: the tiers are now five, set by where each mass comes from, whether the record is an individual pair is a separate column, and `mass_method` has `unknown`. The tables are empty. Phase 2 fills them with extant records and Phase 3 with fossil records.

The database is five CSV tables and one vocabulary file. Every analysis and the website read from here, so a rule that decides what counts belongs in these tables, not in a model script.

| File | One row is |
|---|---|
| `pairs.csv` | one predator–prey record: who ate (or attacked) whom, how we know, and how big each was |
| `taxa.csv` | one taxon, at whatever rank the records need |
| `sources.csv` | one publication or dataset |
| `methods.csv` | one mass-estimation equation |
| `measurements.csv` | one raw measurement (a bone, a body length, a remains weight) |
| `vocab.yml` | the four controlled vocabularies |

Run `Rscript code/db_validate.R` after every change. It exits 0 when the tables pass and 1 when they do not, and prints each offending row with its line number. `Rscript db/tests/run_tests.R` checks the validator itself, including that the column tables below agree with it.

## Conventions

- UTF-8, comma-separated, one header row. Quote any field that contains a comma.
- One record per line. A line break inside a field is an error (it would also throw off every line number the validator reports).
- A missing value is an empty cell. `NA` is read as missing too, since R writes it by default, but prefer empty.
- Booleans are `TRUE` or `FALSE`, upper case.
- Numbers are plain: `1500`, `0.45`, `1e+05`. No units, no `~`, no `>`, no thousands separators. If a source says "about 1.5 kg", the number is `1500` and the "about" goes in the basis or the notes.
- Masses are in grams. Ages are in millions of years (Ma).
- Ids use letters, digits and `_ . : -`, no spaces. An id never changes once rows point to it, even if the name it was built from changes.
- An interval is two columns, `_lo` and `_hi`. Give both or neither. The validator requires lo ≤ point ≤ hi wherever the values exist. A bracket with no point (lo and hi only) is allowed.

Types used below: *id* (unique key), *ref(table)* (must match a key in that table), *vocab(name)* (must be a value in `vocab.yml`), *text*, *number*, *integer*, *bool*. "Required" is `yes`, `no`, or a condition that the validator enforces.

## Tables

### `sources.csv`

| Column | Type | Required | Definition |
|---|---|---|---|
| `source_id` | id | yes | Short key for the publication or dataset. Use the step-1 `paper_id` form, `AuthorYear` with a letter if needed (`Hu2005`, `White2022`, `Delany1999a`). For a compiled database use its name and version (`AvianDietDB_2021`). |
| `citation` | text | yes | Full citation: authors, year, title, journal, volume, pages. |
| `doi` | text | no | DOI without the `https://doi.org/` prefix. A stable URL if there is no DOI. |
| `file_name` | text | no | Exact file name of the paper or dataset on disk. Separate several files with `; `. Empty if we do not hold a copy. |
| `data_type` | text | yes | Kind of source. Use `journal_article`, `supplement`, `book_or_chapter`, `thesis`, `report`, `dataset`, `compilation_database` or `unpublished`. Not yet a controlled vocabulary. |
| `extracted_from` | text | yes | Repository path of the file the rows were read from, plus a locator if useful (`data/raw_data.csv`; `data/fossil_rebuild/step1_gut_entries.csv`; `pdfs/fossil_cases/X.pdf, Table 2`). This records where we got the data, which can differ from the original publication. |
| `compiled_in` | ref(sources) | no | If the record reached us through a compilation (AvianDietDB, Tucker et al. 2016, GATEWAy), the `source_id` of that compilation. The row's own `source_id` is then the original study, so study-level effects group by the original study and not by the compilation. |
| `notes` | text | no | Anything else. |

### `taxa.csv`

| Column | Type | Required | Definition |
|---|---|---|---|
| `taxon_id` | id | yes | Stable key. Recommended form: the name with underscores (`Alligator_mississippiensis`, `Anolis`, `Ornithopoda`, `Vertebrata_indet`). |
| `accepted_name` | text | yes | The name used in analyses. For fossils this is the original describers' identification (decision 5). Record any proposed reassignment, with its source, in `notes`. |
| `rank` | text | yes | `species`, `genus`, `family`, `order`, `class`, `unranked_clade`, and so on. |
| `class` | text | no | Linnean class (`Aves`, `Mammalia`, `Reptilia`, `Amphibia`, `Actinopterygii`, `Insecta`). Non-avian dinosaurs and other extinct reptiles take `Reptilia`. |
| `order` | text | no | Linnean order, where one exists. |
| `family` | text | no | Linnean family, where one exists. |
| `analysis_clade` | text | no | The predator group used in models and on the site (now `Aves`, `Crocodylia`, `Lacertilia`, `Mammalia`, `Serpentes`; fossil groups such as `Theropoda`). Fill it for every predator taxon. Linnean ranks cannot separate snakes from lizards or theropods from birds, and the plan forbids assigning clades by matching genus names in code (M10, Phase 6a). |
| `is_vertebrate` | bool | no | `TRUE` for vertebrates, `FALSE` for invertebrates, empty if the taxon is too coarse to say. Vertebrate-prey filters use this column, not lists of class names. |
| `tree_label` | text | no | Tip label in the Phase 5 tree. |
| `phylopic_uuid` | text | no | PhyloPic image UUID for the site. |
| `notes` | text | no | Reassignments, synonyms, the taxonomic authority used. |

### `methods.csv`

| Column | Type | Required | Definition |
|---|---|---|---|
| `method_id` | id | yes | Key, e.g. `Campione2014_eqn7`. |
| `mass_method` | vocab(mass_method) | yes | The kind of mass this method produces. A `pairs.csv` row that cites the method must use the same value. |
| `description` | text | yes | One line on what the method is. |
| `equation` | text | yes | The equation as printed, with the log base: `log10(BM) = 2.754 * log10(C_femur) - 0.683`. |
| `response` | text | yes | What the equation predicts, and its unit (`body mass, g`). |
| `predictor` | text | yes | The input measurement or measurements, and their units (`femoral circumference, mm`). |
| `taxon_scope` | text | yes | The taxa the equation was fitted on or is meant for. |
| `valid_range` | text | yes | The calibration range as the source states it, or `not stated` plus a description of the calibration sample. |
| `input_min` | number | no | Lower end of the valid range, in predictor units, for automatic range checks later. |
| `input_max` | number | no | Upper end of the same range. Must be ≥ `input_min`. |
| `reported_error` | text | no | Error as the source reports it (%PE, SEE, R², a prediction interval). |
| `method_file` | text | yes | The exact `method_file` string from `docs/methodology_index.csv`. The validator requires a verbatim match: same case, same extension. If the paper is not in the index, add a `NEEDED` row there first. |
| `locator` | text | yes | Equation number, page and table in `method_file`. |
| `verified` | bool | yes | `TRUE` if the coefficients were transcribed from `method_file` itself, from the rendered page where the text layer is unreliable. `FALSE` if they came from memory or a secondary citation. The validator warns on every pair that uses an unverified method. |
| `notes` | text | no | Transcription problems, units the source leaves implicit, corrections. |

### `pairs.csv`

| Column | Type | Required | Definition |
|---|---|---|---|
| `pair_id` | id | yes | Key. Recommended form: `<source_id>_<record>`, where record is the upstream key (step-1 `entry_id`, a stomach number, a staged-table row) and a letter separates several prey in one predator (`White2022_01`, `Hurum2006_01b`). |
| `source_id` | ref(sources) | yes | The publication or dataset that reports this link. |
| `pred_taxon_id` | ref(taxa) | yes | Predator taxon. |
| `prey_taxon_id` | ref(taxa) | yes | Prey taxon, at the lowest rank the source supports. Coarse taxa (`Actinopterygii`, `Vertebrata_indet`) are allowed. |
| `pred_specimen` | text | if `individual_pair` is TRUE | Identifier of the predator individual: the catalogue number for fossils and museum specimens, or the source's own animal, stomach or sample number. If the source describes one individual without numbering it, use a label that picks it out unambiguously within the source (`female White-tailed Kite, 8 Oct 1961`). A species name or size class alone is not an identifier. |
| `prey_specimen` | text | no | Catalogue number or item label of the prey, if it has one. |
| `observation_type` | vocab(observation_type) | yes | How the link was observed. See the vocabulary below. |
| `pair_resolution` | vocab(pair_resolution) | yes | The tier: where the predator and prey masses come from. See below. |
| `individual_pair` | bool | yes | `TRUE` if the row is one identifiable predator individual and the prey it took (one stomach, one regurgitate, one observed kill, one fossil gut). `FALSE` for links reported per population or species, or pooled across individuals. Independent of the tier. The within-species slope and the fossil comparison need `TRUE` rows. |
| `pred_mass_g` | number | no | Predator body mass, g. Empty if not known. |
| `pred_mass_lo_g` | number | if `pred_mass_hi_g` is given | Lower bound of the predator mass interval, g. |
| `pred_mass_hi_g` | number | if `pred_mass_lo_g` is given | Upper bound of the predator mass interval, g. |
| `pred_mass_method` | vocab(mass_method) | if any predator mass is given | How the predator mass was obtained. Must be empty when no predator mass is given. |
| `pred_mass_method_id` | ref(methods) | if the method is regressed or ratio_derived | The equation used. Required for `regressed_from_length`, `regressed_from_bone` and `ratio_derived`. Optional for other methods, but it must resolve if filled. |
| `pred_mass_basis` | text | if the method is regressed or ratio_derived | The input measurement and its value, and what the interval means. Example: `femur length 250 mm (measurement Benson2014_m03); interval = 95% PI`. For other methods it is optional but useful (`AVONET Mass`). Placeholders such as `unknown` or `NA` do not count. |
| `prey_mass_g` | number | no | Prey body mass, g, as a whole animal. A weight of partly digested remains is not a body mass: put it in `measurements.csv` and leave this empty. |
| `prey_mass_lo_g` | number | if `prey_mass_hi_g` is given | Lower bound of the prey mass interval, g. |
| `prey_mass_hi_g` | number | if `prey_mass_lo_g` is given | Upper bound of the prey mass interval, g. |
| `prey_mass_method` | vocab(mass_method) | if any prey mass is given | As for the predator. |
| `prey_mass_method_id` | ref(methods) | if the method is regressed or ratio_derived | As for the predator. |
| `prey_mass_basis` | text | if the method is regressed or ratio_derived | As for the predator. If a species average was scaled for life stage, say so here. |
| `pred_stage` | text | no | Predator life stage: `adult`, `subadult`, `juvenile`, `hatchling`, `unknown`. Several fossil predators are juveniles, and that matters for placement. |
| `prey_stage` | text | no | Prey life stage: `adult`, `juvenile`, `neonate`, `hatchling`, `egg`, `unknown`. |
| `n_items` | integer | no | Number of prey individuals the row stands for, ≥ 1. Empty if the source does not give a count (for example when it reports percent volume). |
| `interaction_outcome` | vocab(interaction_outcome) | yes | What happened to the prey. |
| `is_fossil` | bool | yes | `TRUE` for fossil records. |
| `age_ma` | number | if `is_fossil` is TRUE | Age in Ma: the source's stated age, or the midpoint of its stated range. |
| `age_ma_lo` | number | if `age_ma_hi` is given | Younger bound, Ma. |
| `age_ma_hi` | number | if `age_ma_lo` is given | Older bound, Ma. |
| `quote_or_locator` | text | yes | A verbatim quote (fossils and hand-extracted papers), or a locator precise enough to find the record again (table and row, page, database record id). |
| `notes` | text | no | Anything else, including flags carried over from step 1. |

The plan lists "the same six for prey" without naming the sixth mass column. Here it is `*_mass_method_id`, which the validator's method rule needs.

### `measurements.csv`

| Column | Type | Required | Definition |
|---|---|---|---|
| `measurement_id` | id | yes | Key, e.g. `White2022_m01`. |
| `source_id` | ref(sources) | yes | Where the measurement was reported. |
| `pair_id` | ref(pairs) | no | The pair the measurement belongs to, when it belongs to one (a prey item's remains weight, a prey femur). Leave it empty for a predator measured once and used by several pairs, and link by `specimen` instead. |
| `specimen` | text | no | Specimen or animal identifier, written the same way as in `pairs.csv`. |
| `taxon_id` | ref(taxa) | yes | Taxon of the measured animal. When `pair_id` and a predator or prey `role` are given, it must equal that pair's predator or prey taxon. |
| `role` | text | yes | `predator`, `prey` or `comparison` (a specimen measured only for scaling). The validator enforces these three values. |
| `element` | text | yes | What was measured: `femur`, `skull`, `body`, `stomach remains`. |
| `dimension` | text | yes | Which dimension: `length`, `minimum circumference`, `SVL`, `total length`, `wet mass`. |
| `value` | number | yes | The value. Must be > 0 when the unit is a mass. |
| `unit` | text | yes | `mm`, `cm`, `m`, `mg`, `g`, `kg`, `cm3`, `deg`. |
| `value_type` | text | yes | As in step 1: `measured`, `estimated`, `preserved`, `derived`, `approximate`. |
| `locator` | text | yes | Page, table, figure, or a short quote. |
| `notes` | text | no | Anything else. |

## Controlled vocabularies

The values are copied from the plan without change. The definitions are new, and Phase 2 and Phase 3 assign values by them.

### `observation_type`

| Value | Use when |
|---|---|
| `stomach_content` | Prey was removed from the alimentary tract by dissection, flushing or palpation, or is preserved inside a fossil. Crop, gizzard, stomach and intestine all count. Fossil abdominal and intestinal contents go here. |
| `regurgitate` | The predator brought the prey up through the mouth, on its own or after handling or an emetic. Cast pellets are not regurgitates. |
| `pellet` | A cast pellet of indigestible remains (owls, raptors, other birds). A fossil regurgitalite attributed to a predator goes here. |
| `scat` | Faeces attributed to a predator, including coprolites. Decision 4 keeps bird pellets and mammal scat as separate levels. |
| `prey_remains` | Remains at a nest, den, kill site, cache or plucking post, attributed to the predator, with consumption not seen. GPS-cluster kill-site searches go here. |
| `direct_observation` | The predator was seen catching or eating the prey: field observation, follow studies, camera traps, video. |
| `combat_association` | A fossil predator and prey preserved together in an attack or struggle, with no ingestion. |
| `bite_trace` | Tooth marks, embedded teeth or healed bites on the prey that can be attributed to the predator taxon. |
| `dna_detection` | Prey identified only by DNA (from faeces, stomach contents or swabs). DNA gives no prey size, so these rows usually have no prey mass. |
| `unknown` | The source does not say how the link was observed: compilations that give only a species-level summary (Tucker et al. 2016), records whose compiler gives no method (Oliveira et al. 2025 Table S2; OS-Prey datasets with no method in the metadata). Keep these rows out of any observation-type-matched comparison; they enter only the least fossil-comparable stage of a serial analysis. Added by JM on 2026-11-12. |

### `interaction_outcome`

| Value | Use when |
|---|---|
| `consumed` | The prey was eaten, wholly or in part. The default for stomach contents, regurgitates, pellets and scat. |
| `killed_not_consumed` | The prey was killed and not eaten. |
| `attempted_failed` | The prey was attacked and escaped or survived (a healed bite, a failed chase). |
| `unknown` | The outcome cannot be seen, as in most combat fossils and many bite traces. |

### `pair_resolution`

The tier says where each of the two masses comes from. JM set this scheme on 2026-11-11, replacing the plan's four pairing-based tiers. The reason for it: a bird that was weighed, paired with a vole whose mass comes from a database, carries much less error than a record where both masses come from databases. The old scheme put those two records in the same tier.

A mass is *measured* if it comes from the animal itself: it was weighed, or estimated from that animal's own length or bones (`mass_method` = `measured`, `regressed_from_length`, `regressed_from_bone` or `reconstructed_from_remains`). That is what lets a fossil reach tier A. A mass is an *average* if it is a taxon mean from a database or the literature (`species_average_database`, `species_average_literature`).

| Value | Use when |
|---|---|
| `A_both_measured` | Both masses come from the animals in this record. |
| `B_pred_measured_prey_average` | The predator mass comes from the animal; the prey mass is a taxon average. The usual case is a weighed bird with prey sized from AVONET or PanTHERIA. |
| `C_prey_measured_pred_average` | The prey mass comes from the animal; the predator mass is a taxon average. The usual case is a museum snake whose prey was weighed but whose own size was not recorded. |
| `D_both_average` | Both masses are taxon averages. This is the usual diet-list record. |
| `E_coarse_or_aggregate` | Either mass is `size_class_midpoint`, `assigned_approximate`, `ratio_derived` or `unknown`, or the record summarises many individuals of different sizes (species-mean ratios such as Glaudas et al. 2019, prey pooled across predator size classes). Kept in the database, excluded from individual-level fits (decision 3). |

B ranks above C because a measured predator mass carries the within-species size variation that the within-species slope needs (improvement 2), while a taxon-average prey mass is close to the truth when the prey are adults of a species with little variation in size. That assumption fails for juvenile prey and for prey identified only to genus, so record `prey_stage` and use the taxon's real rank.

Rules the validator applies:

- The tier may be coarser than the mass methods allow, never finer. A study-level mean that is not specific to this predator can be treated as an average (Poulin et al. 2001 below), and the row then drops a tier.
- A side with no mass does not limit the tier. Assign the tier the source's measurements support. The row stays out of ratio analyses until the mass is filled.
- A coarse or `unknown` method on either side forces tier E.
- When the call is borderline, choose the coarser tier and say why in `notes`.

Whether the record is one predator individual is a separate question, answered by `individual_pair`. The two axes are independent. A weighed kite with an AVONET sparrow in its stomach is tier B and an individual pair. A Great Horned Owl from the same collecting trip that was not weighed, with a field mouse in its stomach, is tier D and still an individual pair. Decision 1's headline comparison uses individual pairs; the tier slider adds B, C, D and E to A cumulatively.

### `mass_method`

| Value | Use when |
|---|---|
| `measured` | Weighed on the animal itself: the predator at capture, or a whole prey item. Remains weights do not qualify. |
| `regressed_from_length` | From an external body length of the same animal or item (SVL, total length, fork length, carapace length) through a published equation. Needs `method_id` and `basis`. |
| `regressed_from_bone` | From a skeletal dimension (bone length or circumference, skull length) through a published equation. A femur length belongs here, not under `regressed_from_length`. Needs `method_id` and `basis`. |
| `reconstructed_from_remains` | Estimated by the source's authors from this prey item's own remains, without a published equation: remains that were weighed or measured and corrected for digestion loss or missing parts (Shine 1986 "reconstituted weights"). Counts as a mass from the animal for the tier, like a fossil prey mass estimated from its bones. Needs a `basis` saying what was measured and how it was corrected; `method_id` is optional. Added by JM on 2026-11-12. |
| `species_average_database` | A taxon mean from a compiled database (AVONET, PanTHERIA, FishBase). Name the database and field in `basis`. |
| `species_average_literature` | A taxon mean from a paper or handbook. Cite it in `basis` or `notes`. |
| `size_class_midpoint` | The midpoint of a size class the source reports, converted to mass. If an equation did the conversion, give its `method_id`. |
| `ratio_derived` | Computed from the other party's mass and a published ratio, for example a species-mean relative prey mass times predator mass. Needs `method_id` (the ratio's source) and `basis`. |
| `assigned_approximate` | A value a compiler assigned from general knowledge, with no measurement or cited figure behind it. The coarse crocodylian prey masses supplied at ingest are this. |
| `unknown` | A mass is given but how it was obtained cannot be determined from the source or the script that produced it. Needs no `method_id` or `basis`, forces tier E, and the validator warns on every one so the list stays visible. |

## What the validator checks

Each check has a name, and the validator prints that name beside every offending row.

| Check | Rule |
|---|---|
| `table_missing`, `unreadable`, `header` | All five tables exist and have exactly the columns above, in this order. |
| `newline_in_field`, `empty_row` | One record per line, and no blank records. |
| `required` | Every `yes` column is filled. |
| `id_format`, `duplicate_key` | Keys are unique and contain no spaces. |
| `vocab` | Every vocabulary column holds a value from `vocab.yml`. |
| `bool` | Boolean columns hold `TRUE` or `FALSE`. |
| `ref_source`, `ref_taxon`, `ref_pair` | Every `source_id`, `compiled_in`, taxon id and measurement `pair_id` resolves. |
| `mass_numeric`, `mass_positive`, `numeric` | Masses and measurement values are plain numbers. Masses are > 0. |
| `interval_pair`, `interval_order` | Bounds come in pairs, and lo ≤ point ≤ hi for masses and ages. |
| `bracket_basis` | A mass given only as a bracket (lo and hi, no point) has a `*_mass_basis` saying what sets each bound. Added in Phase 3b. |
| `method_missing`, `method_without_value` | A mass has a method, and a method has a mass. |
| `method_id_missing`, `method_id_unresolved` | A `regressed_*` or `ratio_derived` mass has a `method_id`, and every `method_id` resolves in `methods.csv`. |
| `method_file_not_in_index` | A method's `method_file` appears verbatim in `docs/methodology_index.csv`. Checked on `methods.csv` and on every pair that relies on the method. |
| `method_kind_mismatch` | The pair's `mass_method` equals the cited method's `mass_method`. |
| `basis_missing` | A `regressed_*`, `ratio_derived` or `reconstructed_from_remains` mass has a basis naming the input measurement. |
| `individual_specimen` | A row with `individual_pair` TRUE has a predator specimen identifier. |
| `tier_method_mismatch` | The tier is no finer than the two mass methods allow (see `pair_resolution`). |
| `fossil_age_missing`, `age_numeric` | A fossil row has an age, and ages are numbers ≥ 0. |
| `n_items_integer` | `n_items` is a whole number ≥ 1. |
| `role_value`, `measurement_taxon_mismatch`, `uuid_format`, `range_order` | Measurement roles, measurement-to-pair taxa, PhyloPic UUIDs, and method input ranges are consistent. |

Warnings print but do not fail the run: `unverified_method` (a pair uses a method with `verified = FALSE`), `index_status` (the method paper is not on disk), `duplicate_name` (one name under two taxon ids), and `unknown_method` (a mass whose method is `unknown`), and `bracket_only` (a mass given as a bracket with no point value; added in Phase 3b, so the list stays visible).

## Worked examples, one per tier

These are real records already in the repository. They are documentation, not migrated data, and none of them is in `pairs.csv`. Every value is taken from the file named in the right-hand column. A cell in ⟨angle brackets⟩ is work a later phase must do. I left it open rather than guess.

### Tier A: *Confractosuchus* AODF0890 with a juvenile ornithopod

Fossil individual pair. Both masses come from the two specimens' own bones, so the row is tier A once Phase 3 fills the predator mass.

| Column | Value | From |
|---|---|---|
| `pair_id` | `White2022_01` | step-1 `entry_id` |
| `source_id` | `White2022` | step-1 `paper_id` |
| `pred_taxon_id` | `Confractosuchus_sauroktonos` | step 1 `pred_taxon` |
| `prey_taxon_id` | `Ornithopoda` | step 1: "Ornithopoda indet. (early-diverging, non-iguanodontian; juvenile)" |
| `pred_specimen` | `AODF0890` | step 1 `pred_specimen` "AODF0890 (holotype)" |
| `observation_type` | `stomach_content` | "found concentrated in the anterior part of the abdominal cavity" |
| `pair_resolution` | `A_both_measured` | both masses from the specimens |
| `individual_pair` | `TRUE` | one specimen, one prey individual |
| `pred_mass_g`, `_lo_g`, `_hi_g`, `_method`, `_method_id`, `_basis` | ⟨Phase 3⟩ | The paper gives skull length 285 mm and "estimated to be around 2–2.5 m in length", but no mass. An equation applied to this specimen's skull or limbs keeps the row in tier A. |
| `prey_mass_g` | (empty) | the authors give a range, not a point |
| `prey_mass_lo_g`, `prey_mass_hi_g` | `1000`, `1700` | "The ornithopod's estimated body mass (1.0–1.7 kg; calculated using the bipedal formula of Campione et al. (2014))" |
| `prey_mass_method` | `regressed_from_bone` | femoral circumference equation |
| `prey_mass_method_id` | `Campione2014_eqn7` | the `methods.csv` row below |
| `prey_mass_basis` | `femoral circumference (authors' input value not in main text; locate in SI); interval = authors' range` | step-1 notes |
| `prey_stage` | `juvenile` | "remains of a juvenile ornithopod dinosaur" |
| `n_items` | `1` | "all presumably from a single individual" |
| `interaction_outcome` | `consumed` | ingested |
| `is_fossil` | `TRUE` | |
| `age_ma` | `93` | "upper Winton Formation (Cenomanian), ca 93 Ma" (main text) |
| `age_ma_lo`, `age_ma_hi` | `92.5`, `104` | "from the Cenomanian (92.5–104 Ma)" (abstract) |
| `quote_or_locator` | `"The partially-digested remains of a juvenile ornithopod dinosaur were found concentrated in the anterior part of the abdominal cavity" (p.287, 296-299; Figs 3e, 19)` | step-1 notes and `page_fig_refs` |
| `notes` | `Step-1 flag: only proximal femora preserved; tibia 43 mm long, 10 mm circumference reads small for 1-1.7 kg. Find the input value in SI before use.` | step-1 notes |

The method it cites. I checked eqn 7 against the rendered page, because the PDF text layer drops the minus signs.

| Column | Value |
|---|---|
| `method_id` | `Campione2014_eqn7` |
| `mass_method` | `regressed_from_bone` |
| `description` | corrected quadrupedal equation (cQE) for bipeds |
| `equation` | `log10(BM) = 2.754 * log10(C_femur) - 0.683` |
| `response` | body mass, g (unit not restated at eqn 7; inherited from Campione & Evans 2012; ⟨Phase 3: confirm against Table 3⟩) |
| `predictor` | femoral circumference, mm |
| `taxon_scope` | non-avian bipedal tetrapods |
| `valid_range` | ⟨Phase 3: transcribe from the paper⟩ |
| `method_file` | `Campione_etal_2014_MEE_biped_limb_mass.pdf` |
| `locator` | eqn 7, p. 915 |
| `verified` | `TRUE` |

### Tier B: a weighed White-tailed Kite with an Andean Sparrow

Extant individual pair. The bird was weighed; the prey mass is an AVONET mean. This is the common bird case the B tier exists for.

| Column | Value | From |
|---|---|---|
| `pair_id` | `Greer1966_ref84` | `data/extant_new_observations.csv` ref_id 84 |
| `source_id` | `Greer1966` | Greer JK, Bullock DS (1966) Auk 83:308-309, `pdfs/extant_to_ingest/ai_found/greer1966.pdf` |
| `pred_taxon_id` | `Elanus_leucurus` | |
| `prey_taxon_id` | `Zonotrichia_capensis` | |
| `pred_specimen` | `female White-tailed Kite, Collipulli, 8 Oct 1961` | one kite was collected: "The following were killed: 1 White-tailed Kite (Elanus leucurus)" |
| `observation_type` | `stomach_content` | "Analysis of the stomach contents revealed the following" |
| `pair_resolution` | `B_pred_measured_prey_average` | kite weighed, sparrow from AVONET |
| `individual_pair` | `TRUE` | one bird's stomach |
| `pred_mass_g` | `317` | "the female White-tailed Kite (317 g)" |
| `pred_mass_method` | `measured` | the note gives weights of the birds shot |
| `pred_mass_basis` | `weight reported for this bird` | |
| `prey_mass_g` | `20` | staged notes: "Rufous-collared Sparrow (AVONET 20g)" ⟨Phase 2: re-read from AVONET⟩ |
| `prey_mass_method` | `species_average_database` | |
| `prey_mass_basis` | `AVONET species mean` | |
| `n_items` | `1` | |
| `interaction_outcome` | `consumed` | |
| `is_fossil` | `FALSE` | |
| `quote_or_locator` | `"the female White-tailed Kite (317 g) had eaten a lizard (Liolaemus sp.) and an Andean Sparrow (Zonotrichia capensis)" (pp. 308-309)` | |
| `notes` | `The Liolaemus is a separate row. The same note's Great Horned Owl was not weighed (AVONET mass) and had eaten Akodon olivaceus: an individual pair in tier D.` | staged ref_id 89 |

A population-level B, to show a downgrade. Poulin et al. (2001) weighed every bird at capture ("Each bird captured was banded, weighed, ... forced to regurgitate, and released") and report *Micrastur ruficollis* at 165 g (Table 1, N = 2). Their prey size is "Anolis in diet samples averaged 33.5 mm SVL, ranging from 19 to 44 mm (n = 31)", pooled across every lizard-eating bird species. The prey mass would be `regressed_from_length` from that SVL, which on its own allows tier A. But the SVL is not specific to *Micrastur*'s prey, so the row is entered as `B_pred_measured_prey_average` with `individual_pair` FALSE, and `notes` says why. The staged prey mass of 1 g cites no equation; Phase 2 should recompute it with a registered lizard equation (Meiri 2010 is in the index).

### Tier C: a museum *Philodryas patagoniensis* with a weighed toad

Extant individual pair from SquamataBase. The prey was weighed, but the snake's size was not recorded.

| Column | Value | From |
|---|---|---|
| `pair_id` | ⟨Phase 2: `<source_id>_<record>`⟩ | |
| `source_id` | ⟨Phase 2: the original study⟩ | SquamataBase `reference` field: "Alimentación de los Ofidios de Uruguay. 2002. Asociación Herpetológica Española" |
| `pred_taxon_id` | `Philodryas_patagoniensis` | SquamataBase sb_v1.0.0 `inst/diet.csv`, data row 1009 |
| `prey_taxon_id` | `Rhinella_arenarum` | `prey` (verbatim "Bufo arenarum") |
| `pred_specimen` | `ZVC-R 4388` | `predator_voucher`, a museum catalogue number; `predator_count` 1 |
| `observation_type` | `stomach_content` | `event_basis` "dissected_gut_contents" |
| `pair_resolution` | `C_prey_measured_pred_average` | prey weighed, snake mass not recorded |
| `individual_pair` | `TRUE` | one voucher, one prey |
| `pred_mass_g`, `_method`, `_basis` | ⟨Phase 2⟩ | `predator_mass`, `predator_svl` and `predator_tl` are empty, so a cited species mean (`species_average_literature`). If the original source gives the snake's SVL, use `regressed_from_length` and the row becomes tier A. |
| `prey_mass_g` | `27.5` | `prey_mass` |
| `prey_mass_method` | `measured` | ⟨Phase 2: SquamataBase defines the field only as "The mass (in grams) of the specimen"; confirm in the original source that the toad was weighed, otherwise `unknown` and tier E⟩ |
| `n_items` | `1` | `prey_count` 1 |
| `interaction_outcome` | `consumed` | `event_outcome` "prey_eaten" |
| `is_fossil` | `FALSE` | |
| `quote_or_locator` | `SquamataBase sb_v1.0.0 inst/diet.csv: predator_voucher ZVC-R 4388; Cabo Polonio, Rocha, Uruguay; 1985-01-25` | |

In `sources.csv`, the original study gets `compiled_in = SquamataBase_1.0.0`. One warning for Phase 2: SquamataBase gives summarised museum tables "a unique randomly generated predator_voucher" per row ("a museum study of snake X may report that 12 specimens had eaten 14 individuals of prey Y", `man/diet.Rd`). A random voucher with `predator_count` above 1 is a group, not an individual, so those rows are `individual_pair` FALSE.

### Tier D: *Buteo platypterus* stomachs with *Sylvilagus audubonii*

Extant diet table. Both masses are database means, and the link is per species.

| Column | Value | From |
|---|---|---|
| `pair_id` | ⟨Phase 2: `<source_id>_<AvianDietDB record>`⟩ | |
| `source_id` | ⟨the original study, from AvianDietDB's source field⟩ | `aviandiet_gut_ppmr.csv` dropped that field. Phase 2 recovers it from the full AvianDietDB table. |
| `pred_taxon_id` | `Buteo_platypterus` | `data/structured/aviandiet_gut_ppmr.csv` |
| `prey_taxon_id` | `Sylvilagus_audubonii` | same row |
| `pred_specimen` | (empty) | 177 items pooled over 17 birds |
| `observation_type` | `stomach_content` | `study_type_raw` "stomach contents" |
| `pair_resolution` | `D_both_average` | both masses from databases |
| `individual_pair` | `FALSE` | items pooled across birds |
| `pred_mass_g` | `453.7` | |
| `pred_mass_method` | `species_average_database` | `code/x15_avianGutPPMR.R`: "Predator (always a bird) mass <- AVONET" |
| `pred_mass_basis` | `AVONET species mean` | |
| `prey_mass_g` | `883` | |
| `prey_mass_method` | `species_average_database` | x15: "PanTHERIA (mammal prey)" |
| `prey_mass_basis` | `PanTHERIA species mean` | |
| `n_items` | `3` | `fraction_diet` 0.0169 × `item_n` 177 = 2.99 |
| `interaction_outcome` | `consumed` | |
| `is_fossil` | `FALSE` | |
| `quote_or_locator` | `AvianDietDB (Hurlbert et al. 2021): Buteo platypterus, Iowa;Minnesota, 1933, summer, Sylvilagus audubonii` | |

In `sources.csv`, the original study gets `compiled_in = AvianDietDB_2021`, and `AvianDietDB_2021` has its own row.

### Tier E: *Alligator mississippiensis* with *Dorosoma*

Extant. One of the coarse alligator rows (ref 207–251): a size-class predator and an assigned prey mass.

| Column | Value | From |
|---|---|---|
| `pair_id` | `Delany1999_ref207` | `data/extant_new_observations.csv` ref_id 207 |
| `source_id` | `Delany1999` | staged `source` "Delany_etal_1999_SEAFWA_alligator_4FLlakes" |
| `pred_taxon_id` | `Alligator_mississippiensis` | |
| `prey_taxon_id` | `Dorosoma` | staged "Dorosoma sp" |
| `pred_specimen` | (empty) | "prey pooled across gator sizes" |
| `observation_type` | `stomach_content` | staged `obs_type` "Stomach" |
| `pair_resolution` | `E_coarse_or_aggregate` | coarse methods on both sides |
| `individual_pair` | `FALSE` | pooled across alligators |
| `pred_mass_g` | `100000` | staged `pred_mass_g` 1e+05 |
| `pred_mass_method` | `size_class_midpoint` | plan, Prompt 2 |
| `pred_mass_basis` | `representative adult ~2.7 m TL; no TL-to-mass equation cited` | staged `pred_meas_note` "representative adult ~2.7m TL" |
| `prey_mass_g` | `100` | staged `prey_mass_g` |
| `prey_mass_method` | `assigned_approximate` | staged notes: "prey mass=species-typical est" |
| `prey_mass_basis` | `species-typical estimate assigned at ingest` | |
| `prey_stage` | `unknown` | |
| `n_items` | (empty) | the source reports percent volume, not counts |
| `interaction_outcome` | `consumed` | |
| `is_fossil` | `FALSE` | |
| `quote_or_locator` | `staged note: "stomach %-volume occurrence, prey pooled across gator sizes" (PDF toadd/herps/DELANY-375-389.pdf) ⟨Phase 2: replace with a quote from the paper⟩` | |

## Decisions made here that the plan left open

1. The sixth mass column is `*_mass_method_id`.
2. Added columns: `individual_pair`; `pred_stage`; `age_ma_lo` and `age_ma_hi`; `taxa.analysis_clade` and `taxa.is_vertebrate`; `sources.compiled_in` and `sources.extracted_from` (the plan's "where it was extracted from", split in two); `measurements.pair_id`; and in `methods.csv`, `mass_method`, `response`, `predictor`, `taxon_scope`, `input_min`, `input_max`, `reported_error`, `locator` and `verified`. Each one answers a need stated elsewhere in the plan.
3. Tiers (JM, 2026-11-11): five tiers set by where each mass comes from, replacing the plan's four pairing-based tiers. Individual pairing moved to its own column, `individual_pair`, so that decision 1's "individual pairs" and the within-species slope can still be selected. A regressed mass from the animal's own length or bones counts as measured; without that, no fossil could be tier A.
4. The tier is checked against the mass methods: it may be coarser than they allow, never finer, and a missing mass does not limit it.
5. Masses are optional. A record with no usable mass still enters, so the link is not lost. A method without a mass is an error.
6. Intervals: both bounds or neither, and a bracket without a point is allowed. What the interval means (95% PI, author range, size-class bounds) is written in `*_mass_basis`, not in a separate column.
7. Only the four vocabularies are controlled. `data_type`, `rank`, `value_type`, `pred_stage` and `prey_stage` are free text with recommended values. `measurements.role` is the one extra enumeration the validator enforces.
8. `unknown` is a `mass_method` value (JM, 2026-11-11), so Prompt 2's instruction to "use an explicit `unknown`" now validates. An `unknown` mass forces tier E and raises a warning.
9. `unknown` is also an `observation_type` value (JM, 2026-11-12). It keeps records whose source gives no observation method available for the least comparable stage of a serial, most-to-least fossil-comparable analysis. Without it, records whose source does not state an observation method would have to be dropped or given a method nobody recorded. The rows that use it are listed in `db/MIGRATION_REPORT.md`.
10. `reconstructed_from_remains` is a `mass_method` value (JM, 2026-11-12) for masses the source's authors reconstructed from the remains of the item itself. It counts as from the animal for the tier, and its `basis` is required.
