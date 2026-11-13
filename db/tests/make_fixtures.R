###################################################################
# make_fixtures.R -- write the validator test fixtures.
#   db/tests/valid/          small, schema-legal tables (must PASS)
#   db/tests/broken/         the valid rows plus rows that each break one rule
#   db/tests/broken/EXPECTED.csv   every (table, key, check) the validator must report
#   db/tests/broken_header/  valid rows, but two tables have the wrong columns
#
# ALL ROWS ARE TEST DATA. Taxon names are real so the rows read naturally;
# sources, specimens and every number are made up. Nothing here is a record.
# Run: Rscript db/tests/make_fixtures.R   (then db/tests/run_tests.R)
###################################################################
args_all <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args_all, value = TRUE)
HERE <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else file.path(getwd(), "db", "tests")
ROOT <- normalizePath(file.path(HERE, "..", ".."))
local({ sys.source(file.path(ROOT, "code", "db_validate.R"), envir = environment()); SPEC <<- SPEC })

mk <- function(table, rows) {               # rows: list of named lists; missing columns -> ""
  cols <- names(SPEC[[table]]$cols)
  df <- as.data.frame(matrix("", nrow = length(rows), ncol = length(cols), dimnames = list(NULL, cols)),
                      stringsAsFactors = FALSE)
  for (i in seq_along(rows)) for (nm in names(rows[[i]])) {
    if (!nm %in% cols) stop(table, ": unknown column ", nm)
    df[i, nm] <- as.character(rows[[i]][[nm]])
  }
  df
}
put <- function(df, dir, table) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(df, file.path(dir, paste0(table, ".csv")), row.names = FALSE, na = "")
}
EXPECT <- list()
expect <- function(table, key, check, severity = "ERROR")
  EXPECT[[length(EXPECT) + 1]] <<- data.frame(severity = severity, table = paste0(table, ".csv"),
                                              key = key, check = check, stringsAsFactors = FALSE)
TF <- "TEST FIXTURE - not a real record"

## ================= valid tables =================
sources_ok <- list(
  list(source_id = "TEST_paper_fossil", citation = paste(TF, "(fossil paper)"), data_type = "journal_article",
       extracted_from = "db/tests (synthetic)"),
  list(source_id = "TEST_paper_extant", citation = paste(TF, "(extant paper)"), data_type = "journal_article",
       extracted_from = "db/tests (synthetic)"),
  list(source_id = "TEST_compilation", citation = paste(TF, "(compiled database)"), data_type = "compilation_database",
       extracted_from = "db/tests (synthetic)"),
  list(source_id = "TEST_study_in_compilation", citation = paste(TF, "(study reached through a compilation)"),
       data_type = "journal_article", extracted_from = "db/tests (synthetic)", compiled_in = "TEST_compilation"))

taxa_ok <- list(
  list(taxon_id = "Confractosuchus_sauroktonos", accepted_name = "Confractosuchus sauroktonos", rank = "species",
       class = "Reptilia", analysis_clade = "Crocodyliformes", is_vertebrate = "TRUE"),
  list(taxon_id = "Ornithopoda", accepted_name = "Ornithopoda", rank = "unranked_clade", class = "Reptilia",
       analysis_clade = "Ornithischia", is_vertebrate = "TRUE"),
  list(taxon_id = "Micrastur_ruficollis", accepted_name = "Micrastur ruficollis", rank = "species", class = "Aves",
       order = "Falconiformes", family = "Falconidae", analysis_clade = "Aves", is_vertebrate = "TRUE",
       phylopic_uuid = "00000000-0000-4000-8000-000000000000"),
  list(taxon_id = "Anolis", accepted_name = "Anolis", rank = "genus", class = "Reptilia", order = "Squamata",
       family = "Dactyloidae", analysis_clade = "Lacertilia", is_vertebrate = "TRUE"),
  list(taxon_id = "Buteo_platypterus", accepted_name = "Buteo platypterus", rank = "species", class = "Aves",
       order = "Accipitriformes", family = "Accipitridae", analysis_clade = "Aves", is_vertebrate = "TRUE"),
  list(taxon_id = "Sylvilagus_audubonii", accepted_name = "Sylvilagus audubonii", rank = "species", class = "Mammalia",
       order = "Lagomorpha", family = "Leporidae", analysis_clade = "Mammalia", is_vertebrate = "TRUE"),
  list(taxon_id = "Alligator_mississippiensis", accepted_name = "Alligator mississippiensis", rank = "species",
       class = "Reptilia", order = "Crocodylia", family = "Alligatoridae", analysis_clade = "Crocodylia",
       is_vertebrate = "TRUE"),
  list(taxon_id = "Actinopterygii", accepted_name = "Actinopterygii", rank = "class", class = "Actinopterygii",
       is_vertebrate = "TRUE"),
  list(taxon_id = "Insecta", accepted_name = "Insecta", rank = "class", class = "Insecta", is_vertebrate = "FALSE"),
  list(taxon_id = "Anilius_scytale", accepted_name = "Anilius scytale", rank = "species", class = "Reptilia",
       order = "Squamata", family = "Aniliidae", analysis_clade = "Serpentes", is_vertebrate = "TRUE"),
  list(taxon_id = "Vertebrata_indet", accepted_name = "Vertebrata indet.", rank = "unranked_clade",
       is_vertebrate = "TRUE"))

methods_ok <- list(
  list(method_id = "TEST_cqe_biped", mass_method = "regressed_from_bone",
       description = "corrected quadrupedal equation for bipeds (test copy)",
       equation = "log10(BM) = 2.754 * log10(C_femur) - 0.683", response = "body mass, g",
       predictor = "femoral circumference, mm", taxon_scope = "non-avian bipedal tetrapods",
       valid_range = "TEST: see source", method_file = "Campione_etal_2014_MEE_biped_limb_mass.pdf",
       locator = "eqn 7, p. 915", verified = "TRUE"),
  list(method_id = "TEST_lizard_svl", mass_method = "regressed_from_length",
       description = "lizard SVL to mass (test placeholder, coefficients not transcribed)",
       equation = "TEST: not transcribed", response = "body mass, g", predictor = "SVL, mm",
       taxon_scope = "Lacertilia", valid_range = "TEST", input_min = "10", input_max = "500",
       method_file = "Meiri_2010_JZool_lizard_length_mass.pdf", locator = "TEST", verified = "FALSE"),
  list(method_id = "TEST_rpm_ratio", mass_method = "ratio_derived",
       description = "prey mass = species-mean relative prey mass x predator mass (test)",
       equation = "prey_mass = rpm_mean * pred_mass", response = "prey body mass, g",
       predictor = "rpm_mean (unitless) and predator mass, g", taxon_scope = "Serpentes",
       valid_range = "TEST", method_file = "Glaudas_etal_2019_JAnimEcol_snake_relative_prey_mass.pdf",
       locator = "TEST", verified = "TRUE"))

pair_base <- list(source_id = "TEST_paper_extant", pred_taxon_id = "Buteo_platypterus",
  prey_taxon_id = "Sylvilagus_audubonii", observation_type = "stomach_content",
  pair_resolution = "D_both_average", individual_pair = "FALSE",
  pred_mass_g = "450", pred_mass_method = "species_average_database",
  prey_mass_g = "880", prey_mass_method = "species_average_database", interaction_outcome = "consumed",
  is_fossil = "FALSE", quote_or_locator = TF)
P <- function(...) { o <- modifyList(pair_base, list(...)); o }

pairs_ok <- list(
  P(pair_id = "OK_A_fossil", source_id = "TEST_paper_fossil", pred_taxon_id = "Confractosuchus_sauroktonos",
    prey_taxon_id = "Ornithopoda", pred_specimen = "TEST-0001", pair_resolution = "A_both_measured",
    individual_pair = "TRUE",
    pred_mass_g = "", pred_mass_method = "", prey_mass_g = "", prey_mass_lo_g = "1000", prey_mass_hi_g = "1700",
    prey_mass_method = "regressed_from_bone", prey_mass_method_id = "TEST_cqe_biped",
    prey_mass_basis = "femoral circumference (measurement TEST_m1)", prey_stage = "juvenile", n_items = "1",
    is_fossil = "TRUE", age_ma = "93", age_ma_lo = "92.5", age_ma_hi = "104"),
  P(pair_id = "OK_A_extant_remains", pred_taxon_id = "Alligator_mississippiensis", prey_taxon_id = "Actinopterygii",
    pred_specimen = "TEST-stomach-12", pair_resolution = "A_both_measured", individual_pair = "TRUE",
    pred_mass_g = "", pred_mass_method = "", prey_mass_g = "", prey_mass_method = "", n_items = "2",
    notes = "prey remains weight is in measurements.csv, not prey_mass_g"),
  P(pair_id = "OK_A_extant_invert", pred_taxon_id = "Alligator_mississippiensis", prey_taxon_id = "Insecta",
    pred_specimen = "TEST-stomach-12", pair_resolution = "A_both_measured", individual_pair = "TRUE",
    pred_mass_g = "", pred_mass_method = "", prey_mass_g = "", prey_mass_method = "", n_items = "6"),
  P(pair_id = "OK_B_individual", pred_taxon_id = "Micrastur_ruficollis", prey_taxon_id = "Anolis",
    pred_specimen = "TEST-bird-3", pair_resolution = "B_pred_measured_prey_average", individual_pair = "TRUE",
    pred_mass_g = "170", pred_mass_method = "measured", prey_mass_g = "1.5",
    prey_mass_method = "species_average_literature", prey_mass_basis = "TEST literature mean"),
  P(pair_id = "OK_B_downgraded", pred_taxon_id = "Micrastur_ruficollis", prey_taxon_id = "Anolis",
    observation_type = "regurgitate", pair_resolution = "B_pred_measured_prey_average",
    pred_mass_g = "165", pred_mass_method = "measured", pred_mass_basis = "mean of birds weighed at capture",
    prey_mass_g = "1.1", prey_mass_method = "regressed_from_length", prey_mass_method_id = "TEST_lizard_svl",
    prey_mass_basis = "SVL, mean of items pooled across predator species", n_items = "2",
    notes = "prey SVL not specific to this predator, so treated as an average (allowed downgrade)"),
  P(pair_id = "OK_C_individual", pred_taxon_id = "Anilius_scytale", prey_taxon_id = "Vertebrata_indet",
    pred_specimen = "TEST-snake-7", pair_resolution = "C_prey_measured_pred_average", individual_pair = "TRUE",
    pred_mass_g = "120", pred_mass_method = "species_average_literature", prey_mass_g = "27.5",
    prey_mass_method = "measured"),
  P(pair_id = "OK_D", source_id = "TEST_study_in_compilation"),
  P(pair_id = "OK_D_individual", pred_specimen = "TEST-owl-1", individual_pair = "TRUE",
    notes = "an individual pair whose masses both come from databases is tier D"),
  P(pair_id = "OK_E_sizeclass", pred_taxon_id = "Alligator_mississippiensis", prey_taxon_id = "Actinopterygii",
    pair_resolution = "E_coarse_or_aggregate", pred_mass_g = "1e+05", pred_mass_lo_g = "5e4", pred_mass_hi_g = "2e5",
    pred_mass_method = "size_class_midpoint", pred_mass_basis = "TL size class",
    prey_mass_g = "100", prey_mass_method = "assigned_approximate", prey_stage = "unknown"),
  P(pair_id = "OK_E_ratio", pred_taxon_id = "Anilius_scytale", prey_taxon_id = "Vertebrata_indet",
    pair_resolution = "E_coarse_or_aggregate", pred_mass_g = "250", pred_mass_method = "species_average_literature",
    prey_mass_g = "80", prey_mass_method = "ratio_derived", prey_mass_method_id = "TEST_rpm_ratio",
    prey_mass_basis = "rpm_mean 0.32 x pred_mass_g"),
  P(pair_id = "OK_E_unknown", pair_resolution = "E_coarse_or_aggregate", prey_mass_method = "unknown",
    notes = "mass given, method could not be determined"),
  P(pair_id = "OK_E_bracket", pair_resolution = "E_coarse_or_aggregate", prey_mass_g = "", prey_mass_lo_g = "1", prey_mass_hi_g = "50",
    prey_mass_method = "assigned_approximate", prey_mass_basis = "lower: smallest fish with this bone cluster; upper: swallow-whole limit of the predator"),
  P(pair_id = "OK_E_no_masses", pred_taxon_id = "Anilius_scytale", prey_taxon_id = "Vertebrata_indet",
    pair_resolution = "E_coarse_or_aggregate", pred_mass_g = "", pred_mass_method = "", prey_mass_g = "",
    prey_mass_method = "", notes = "no absolute masses: masses are optional"))

meas_ok <- list(
  list(measurement_id = "TEST_m1", source_id = "TEST_paper_fossil", pair_id = "OK_A_fossil", specimen = "TEST-0001 gut",
       taxon_id = "Ornithopoda", role = "prey", element = "femur", dimension = "circumference", value = "30",
       unit = "mm", value_type = "measured", locator = "TEST"),
  list(measurement_id = "TEST_m2", source_id = "TEST_paper_extant", pair_id = "OK_A_extant_remains",
       specimen = "TEST-stomach-12", taxon_id = "Actinopterygii", role = "prey", element = "stomach remains",
       dimension = "wet mass", value = "15.9", unit = "g", value_type = "measured", locator = "TEST"),
  list(measurement_id = "TEST_m3", source_id = "TEST_paper_extant", specimen = "TEST-stomach-12",
       taxon_id = "Alligator_mississippiensis", role = "predator", element = "body", dimension = "total length",
       value = "152", unit = "cm", value_type = "measured", locator = "TEST"),
  list(measurement_id = "TEST_m4", source_id = "TEST_paper_extant", specimen = "TEST comparison",
       taxon_id = "Anolis", role = "comparison", element = "body", dimension = "SVL", value = "33.5",
       unit = "mm", value_type = "measured", locator = "TEST"))

V <- file.path(HERE, "valid")
put(mk("sources", sources_ok), V, "sources"); put(mk("taxa", taxa_ok), V, "taxa")
put(mk("methods", methods_ok), V, "methods"); put(mk("pairs", pairs_ok), V, "pairs")
put(mk("measurements", meas_ok), V, "measurements")
## the valid set should raise exactly four warnings
write.csv(data.frame(severity = "WARN", table = "pairs.csv", key = c("OK_B_downgraded", "OK_E_unknown", "OK_E_bracket", "OK_A_fossil"),
                     check = c("unverified_method", "unknown_method", "bracket_only", "bracket_only")),
          file.path(V, "EXPECTED.csv"), row.names = FALSE)

## ================= broken tables =================
## each bad row breaks one rule; its id says which; expect() records the check it must trip
src_bad <- list(
  list(source_id = "BAD_no_citation", citation = "", data_type = "journal_article", extracted_from = "x"),
  list(source_id = "BAD_compiled_in", citation = TF, data_type = "journal_article", extracted_from = "x",
       compiled_in = "TEST_no_such_compilation"),
  list(source_id = "BAD_newline", citation = TF, data_type = "journal_article", extracted_from = "x",
       notes = "first line\nsecond line"))                       # keep last: it shifts later line numbers
expect("sources", "BAD_no_citation", "required")
expect("sources", "BAD_compiled_in", "ref_source")
expect("sources", "BAD_newline", "newline_in_field")

tax_bad <- list(
  list(taxon_id = "BAD_uuid", accepted_name = "Bad uuid", rank = "species", phylopic_uuid = "not-a-uuid"),
  list(taxon_id = "BAD_no_rank", accepted_name = "Bad norank", rank = ""),
  list(taxon_id = "BAD_vert_flag", accepted_name = "Bad vertflag", rank = "species", is_vertebrate = "yes"),
  list(taxon_id = "BAD_dup_name_1", accepted_name = "Duplicated name", rank = "species"),
  list(taxon_id = "BAD_dup_name_2", accepted_name = "Duplicated name", rank = "species"),
  list())                                                         # an all-empty row
expect("taxa", "BAD_uuid", "uuid_format")
expect("taxa", "BAD_no_rank", "required")
expect("taxa", "BAD_vert_flag", "bool")
expect("taxa", "BAD_dup_name_1", "duplicate_name", "WARN")
expect("taxa", "BAD_dup_name_2", "duplicate_name", "WARN")
expect("taxa", "", "empty_row")

met_template <- methods_ok[[1]]
M <- function(...) modifyList(met_template, list(...))
met_bad <- list(
  M(method_id = "BAD_method_file", method_file = "campione_etal_2014_MEE_biped_limb_mass.pdf"),  # case differs
  M(method_id = "BAD_verified", verified = "maybe"),
  M(method_id = "BAD_range", input_min = "50", input_max = "10"),
  M(method_id = "BAD_kind", mass_method = "regression"))
expect("methods", "BAD_method_file", "method_file_not_in_index")
expect("methods", "BAD_verified", "bool")
expect("methods", "BAD_range", "range_order")
expect("methods", "BAD_kind", "vocab")

bone <- list(prey_mass_g = "500", prey_mass_method = "regressed_from_bone", prey_mass_method_id = "TEST_cqe_biped",
             prey_mass_basis = "femoral circumference")
PB <- function(...) do.call(P, modifyList(bone, list(...)))
pairs_bad <- list(
  P(pair_id = "BAD_vocab_obs", observation_type = "Stomach"),
  P(pair_id = "BAD_individual_no_specimen", individual_pair = "TRUE"),
  P(pair_id = "BAD_ref_source", source_id = "TEST_no_such_source"),
  P(pair_id = "BAD_ref_taxon", prey_taxon_id = "No_such_taxon"),
  P(pair_id = "BAD_mass_negative", pred_mass_g = "-5"),
  P(pair_id = "BAD_mass_text", prey_mass_g = "~100"),
  P(pair_id = "BAD_lo_above_point", pred_mass_g = "100", pred_mass_lo_g = "200", pred_mass_hi_g = "300"),
  P(pair_id = "BAD_point_above_hi", prey_mass_g = "900", prey_mass_lo_g = "100", prey_mass_hi_g = "300"),
  P(pair_id = "BAD_hi_only", prey_mass_hi_g = "1000"),
  PB(pair_id = "BAD_method_id_missing", prey_mass_method_id = ""),
  PB(pair_id = "BAD_method_id_unresolved", prey_mass_method_id = "TEST_no_such_method"),
  PB(pair_id = "BAD_method_file_chain", prey_mass_method_id = "BAD_method_file"),
  PB(pair_id = "BAD_basis_missing", prey_mass_basis = "unknown"),
  PB(pair_id = "BAD_basis_empty", prey_mass_basis = ""),
  P(pair_id = "BAD_method_kind", pred_mass_method = "regressed_from_length", pred_mass_method_id = "TEST_cqe_biped",
    pred_mass_basis = "femoral circumference"),
  P(pair_id = "BAD_ratio_no_method", pair_resolution = "E_coarse_or_aggregate", prey_mass_method = "ratio_derived",
    prey_mass_basis = "rpm_mean x predator mass"),
  P(pair_id = "BAD_method_missing", prey_mass_method = ""),
  P(pair_id = "BAD_method_without_value", pred_mass_g = ""),
  P(pair_id = "BAD_vocab_mass_method", pred_mass_method = "estimated"),
  P(pair_id = "BAD_required_outcome", interaction_outcome = ""),
  P(pair_id = "BAD_vocab_outcome", interaction_outcome = "eaten"),
  P(pair_id = "BAD_vocab_tier", pair_resolution = "A"),
  P(pair_id = "BAD_bool_fossil", is_fossil = "yes"),
  P(pair_id = "BAD_fossil_no_age", is_fossil = "TRUE"),
  P(pair_id = "BAD_age_order", is_fossil = "TRUE", age_ma = "120", age_ma_lo = "92.5", age_ma_hi = "104"),
  P(pair_id = "BAD_n_items", n_items = "2.5"),
  P(pair_id = "BAD_duplicate"),
  P(pair_id = "BAD_duplicate"),
  P(pair_id = "BAD id with spaces"),
  P(pair_id = "BAD_bool_individual", individual_pair = "yes"),
  P(pair_id = "BAD_tierA_with_average", pair_resolution = "A_both_measured"),
  P(pair_id = "BAD_tierA_coarse", pair_resolution = "A_both_measured", pred_mass_method = "measured",
    prey_mass_method = "assigned_approximate"),
  P(pair_id = "BAD_tierB_pred_average", pair_resolution = "B_pred_measured_prey_average"),
  P(pair_id = "BAD_tierC_prey_average", pair_resolution = "C_prey_measured_pred_average"),
  P(pair_id = "BAD_tierD_unknown", pred_mass_method = "unknown"),
  P(pair_id = "BAD_bracket_no_basis", pair_resolution = "E_coarse_or_aggregate", prey_mass_g = "", prey_mass_lo_g = "1", prey_mass_hi_g = "50",
    prey_mass_method = "assigned_approximate", prey_mass_basis = ""))
for (k in c("BAD_tierA_with_average", "BAD_tierA_coarse", "BAD_tierB_pred_average", "BAD_tierC_prey_average",
            "BAD_tierD_unknown")) expect("pairs", k, "tier_method_mismatch")
expect("pairs", "BAD_bool_individual", "bool")
expect("pairs", "BAD_tierD_unknown", "unknown_method", "WARN")
for (e in list(
  c("BAD_vocab_obs", "vocab"), c("BAD_individual_no_specimen", "individual_specimen"), c("BAD_ref_source", "ref_source"),
  c("BAD_ref_taxon", "ref_taxon"), c("BAD_mass_negative", "mass_positive"), c("BAD_mass_text", "mass_numeric"),
  c("BAD_lo_above_point", "interval_order"), c("BAD_point_above_hi", "interval_order"),
  c("BAD_hi_only", "interval_pair"), c("BAD_method_id_missing", "method_id_missing"),
  c("BAD_method_id_unresolved", "method_id_unresolved"), c("BAD_method_file_chain", "method_file_not_in_index"),
  c("BAD_basis_missing", "basis_missing"), c("BAD_basis_empty", "basis_missing"),
  c("BAD_method_kind", "method_kind_mismatch"), c("BAD_ratio_no_method", "method_id_missing"),
  c("BAD_method_missing", "method_missing"), c("BAD_method_without_value", "method_without_value"),
  c("BAD_vocab_mass_method", "vocab"), c("BAD_required_outcome", "required"), c("BAD_vocab_outcome", "vocab"),
  c("BAD_bracket_no_basis", "bracket_basis"), c("BAD_vocab_tier", "vocab"), c("BAD_bool_fossil", "bool"), c("BAD_fossil_no_age", "fossil_age_missing"),
  c("BAD_age_order", "interval_order"), c("BAD_n_items", "n_items_integer"),
  c("BAD_duplicate", "duplicate_key"), c("BAD id with spaces", "id_format")))
  expect("pairs", e[1], e[2])
expect("pairs", "OK_B_downgraded", "unverified_method", "WARN")   # inherited from the valid rows
expect("pairs", "OK_E_unknown", "unknown_method", "WARN")
expect("pairs", "BAD_bracket_no_basis", "bracket_only", "WARN")
expect("pairs", "OK_A_fossil", "bracket_only", "WARN")   # inherited from the valid rows
expect("pairs", "OK_E_bracket", "bracket_only", "WARN")

meas_bad <- list(
  modifyList(meas_ok[[3]], list(measurement_id = "BAD_role", role = "gut")),
  modifyList(meas_ok[[3]], list(measurement_id = "BAD_m_taxon", taxon_id = "No_such_taxon")),
  modifyList(meas_ok[[3]], list(measurement_id = "BAD_m_value", value = "152 cm")),
  modifyList(meas_ok[[1]], list(measurement_id = "BAD_m_taxon_mismatch", taxon_id = "Actinopterygii")),
  modifyList(meas_ok[[1]], list(measurement_id = "BAD_m_pair", pair_id = "No_such_pair")),
  modifyList(meas_ok[[2]], list(measurement_id = "BAD_m_zero_mass", value = "0")))
expect("measurements", "BAD_role", "role_value")
expect("measurements", "BAD_m_taxon", "ref_taxon")
expect("measurements", "BAD_m_value", "numeric")
expect("measurements", "BAD_m_taxon_mismatch", "measurement_taxon_mismatch")
expect("measurements", "BAD_m_pair", "ref_pair")
expect("measurements", "BAD_m_zero_mass", "mass_positive")

B <- file.path(HERE, "broken")
put(mk("sources", c(sources_ok, src_bad)), B, "sources"); put(mk("taxa", c(taxa_ok, tax_bad)), B, "taxa")
put(mk("methods", c(methods_ok, met_bad)), B, "methods"); put(mk("pairs", c(pairs_ok, pairs_bad)), B, "pairs")
put(mk("measurements", c(meas_ok, meas_bad)), B, "measurements")
ex <- unique(do.call(rbind, EXPECT))
write.csv(ex, file.path(B, "EXPECTED.csv"), row.names = FALSE)

## ================= broken header =================
H <- file.path(HERE, "broken_header"); dir.create(H, showWarnings = FALSE)
for (tb in c("sources", "methods", "measurements"))
  file.copy(file.path(V, paste0(tb, ".csv")), file.path(H, paste0(tb, ".csv")), overwrite = TRUE)
pp <- mk("pairs", pairs_ok); pp$interaction_outcome <- NULL; put(pp, H, "pairs")
tt <- mk("taxa", taxa_ok); tt$synonyms <- ""; put(tt, H, "taxa")
write.csv(data.frame(severity = "ERROR", table = c("pairs.csv", "taxa.csv"), key = "", check = "header"),
          file.path(H, "EXPECTED.csv"), row.names = FALSE)

cat("fixtures written:\n ", V, "\n ", B, sprintf("(%d expected issues)", nrow(ex)), "\n ", H, "\n")
