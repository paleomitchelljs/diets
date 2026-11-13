###################################################################
# code/db_migrate/_common.R -- shared helpers for the Phase 2 migration.
#
# Ownership rule (what makes each script idempotent and limited to its own rows):
#   A migration script OWNS every sources.csv row whose `extracted_from` begins with one of
#   the input paths it declares (OWNS). On each run it deletes those sources, every pairs.csv
#   and measurements.csv row that cites them, and then appends its freshly built rows.
#   It refuses to run if a source_id or pair_id it is about to write already belongs to a
#   row it does not own. taxa.csv and methods.csv are shared: rows are upserted by key
#   (a taxon row is never deleted by a script; run_all.R prunes unreferenced taxa).
#
# Nothing here reads or writes the old files in data/ except to read them.
###################################################################

args_all <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", args_all, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa[1]))), "../..")) else normalizePath(getwd())
DB   <- file.path(ROOT, "db")
EVID <- file.path(ROOT, "code", "db_migrate", "evidence")
MIG  <- file.path(DB, "migration")          # migration side outputs (dropped rows, method lists, comparison)
dir.create(MIG, showWarnings = FALSE)

## column order comes from the validator's SPEC so the two cannot drift apart
local({ src <- file.path(ROOT, "code", "db_validate.R"); e <- new.env(); sys.source(src, envir = e); SPEC <<- e$SPEC })
COLS <- lapply(SPEC, function(s) names(s$cols))

num   <- function(x) suppressWarnings(as.numeric(x))
blank <- function(x) is.na(x) | trimws(as.character(x)) %in% c("", "NA")

## plain number formatting accepted by the validator (no thousands separators, no units)
fnum <- function(x, sig = 8) {
  out <- rep("", length(x)); ok <- is.finite(x)
  out[ok] <- vapply(signif(x[ok], sig), function(v) format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE), "")
  out
}

source(file.path(ROOT, "code", "db_migrate", "_ids.R"))
one_line <- function(x) gsub("[\r\n]+", " ", ifelse(is.na(x), "", as.character(x)))

read_db <- function(tb) {
  p <- file.path(DB, paste0(tb, ".csv"))
  x <- read.csv(p, colClasses = "character", na.strings = character(0), check.names = FALSE, fileEncoding = "UTF-8-BOM")
  x[, COLS[[tb]], drop = FALSE]
}
write_db <- function(tb, x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  miss <- setdiff(COLS[[tb]], names(x)); for (m in miss) x[[m]] <- ""
  x <- x[, COLS[[tb]], drop = FALSE]
  for (cl in names(x)) { v <- as.character(x[[cl]]); v[is.na(v) | v == "NA"] <- ""; x[[cl]] <- one_line(v) }
  write.csv(x, file.path(DB, paste0(tb, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(nrow(x))
}
blank_frame <- function(tb, n = 0) {
  x <- as.data.frame(matrix("", nrow = n, ncol = length(COLS[[tb]])), stringsAsFactors = FALSE)
  names(x) <- COLS[[tb]]; x
}
## build a table from named columns; unnamed schema columns are blank
mk <- function(tb, ...) {
  l <- list(...); n <- max(vapply(l, length, 1L))
  x <- blank_frame(tb, n)
  for (k in names(l)) { if (!k %in% COLS[[tb]]) stop("unknown column ", k, " for ", tb); v <- l[[k]]; x[[k]] <- if (length(v) == 1) rep(as.character(v), n) else as.character(v) }
  for (cl in names(x)) x[[cl]][is.na(x[[cl]]) | x[[cl]] == "NA"] <- ""
  x
}

## ---------------------------------------------------------------
## shared registries
## ---------------------------------------------------------------
METHOD_REG <- function() read.csv(file.path(EVID, "methods_registry.csv"), colClasses = "character", check.names = FALSE)
SQUAMATES  <- function() read.csv(file.path(EVID, "squamate_genera.csv"), colClasses = "character", check.names = FALSE, comment.char = "#")

## evaluate a registered log10-linear length equation: log10(mass g) = a + b*log10(length mm)
eq_mass <- function(method_id, len_mm) {
  r <- METHOD_REG(); i <- match(method_id, r$method_id)
  if (anyNA(i)) stop("method not registered: ", paste(unique(method_id[is.na(i)]), collapse = ", "))
  a <- num(r$coef_a[i]); b <- num(r$coef_b[i]); form <- r$form[i]
  out <- rep(NA_real_, length(len_mm))
  ll <- form == "log10_log10"; out[ll] <- 10^(a[ll] + b[ll] * log10(len_mm[ll]))
  cj <- form == "cj1979_cubic_kg_cm";  L <- len_mm[cj] / 10          # TL cm -> kg -> g
  out[cj] <- 1000 * (1.35 - 0.0378 * L + 0.0000046 * L^3)
  out
}

## ---------------------------------------------------------------
## taxon classification (one function so every script writes identical taxa rows)
## ---------------------------------------------------------------
VERT_RE   <- "Actinopter|Acinopter|Chondrich|Sarcopter|Osteichthy|Pisces|Teleost|Mammal|Aves|Reptil|Amphib|Squamat|Anura|Chelon|Testud|Serpent|Lacert|Crocodyl|Amniota|Carrion|Egg|fish|vertebr"
INVERT_RE <- "Insect|Arachn|Crustac|Malacostr|Myriapod|Chilopod|Diplopod|Gastropod|Mollusc|Annelid|Hexapod|Arthropod|Bivalv|Oligochaet|Clitellat|Branchiopod|Collembol|Decapod|Isopod|Orthopter|Coleopter|Hymenopt|Lepidopt|Hemipter|Cephalopod|Scorpion|Solifug|invert"
## the same rule x00/x20 use to type prey; returns "vert", "invert" or NA (non-animal)
prey_type <- function(cls) ifelse(grepl(VERT_RE, cls, ignore.case = TRUE), "vert", ifelse(grepl(INVERT_RE, cls, ignore.case = TRUE), "invert", NA))

CLASS_NORM <- c(Acinopterygii = "Actinopterygii", Actinoptergii = "Actinopterygii", Actinoptergyii = "Actinopterygii",
                Actinopterygii = "Actinopterygii", Teleostei = "Actinopterygii", Osteichthyes = "Actinopterygii",
                Amphibia = "Amphibia", Aves = "Aves", Mammalia = "Mammalia", Reptilia = "Reptilia", reptilia = "Reptilia",
                Squamata = "Reptilia", Testudines = "Reptilia", Crocodylia = "Reptilia", Chondrichthyes = "Chondrichthyes",
                Insecta = "Insecta", Arachnida = "Arachnida", Malacostraca = "Malacostraca", Gastropoda = "Gastropoda")
REPT_ORDER <- c(Squamata = "Squamata", Testudines = "Testudines", Crocodylia = "Crocodylia")

guess_rank <- function(name) {
  n <- trimws(name)
  ifelse(grepl("_or_|/", n), "unranked_alternatives",
  ifelse(grepl("^[A-Z][a-z]+[ _][a-z]+[ _][a-z]+$", n), "subspecies",
  ifelse(grepl("^[A-Z][a-z]+[ _](x[ _])?[a-z-]+$", n) & !grepl("[ _](sp|spp|indet)$", n), "species",
  ifelse(grepl("idae$", n), "family", ifelse(grepl("inae$", n), "subfamily", ifelse(grepl("ini$", n), "tribe",
  ifelse(grepl("formes$|^(Squamata|Testudines|Crocodylia|Rodentia|Carnivora|Chiroptera|Lagomorpha|Anura|Urodela|Coleoptera|Orthoptera|Lepidoptera|Hymenoptera|Hemiptera|Diptera|Odonata|Araneae|Blattodea|Mantodea|Decapoda|Isopoda|Artiodactyla|Primates|Eulipotyphla|Serpentes|Lacertilia|Passeriformes)$", n), "order",
  ifelse(grepl("^(Aves|Mammalia|Reptilia|Amphibia|Actinopterygii|Insecta|Arachnida|Malacostraca|Gastropoda|Chilopoda|Diplopoda|Bivalvia|Clitellata|Chondrichthyes)$", n), "class",
  ifelse(grepl("^[A-Z][a-z]+$", n), "genus", "unranked")))))))))
}

## make a taxa row. cls = Linnean class (normalised here), ord/fam optional, clade = analysis_clade (predators only)
taxon_row <- function(id, name, rank = guess_rank(name), cls = "", ord = "", fam = "", clade = "", vert = NA, notes = "") {
  n <- length(id); rec <- function(v) if (length(v) == 1) rep(v, n) else v
  cls <- rec(cls); ord <- rec(ord)
  craw <- sub("[_ ].*$", "", cls); cl2 <- ifelse(craw %in% names(CLASS_NORM), CLASS_NORM[craw], craw)
  ord  <- ifelse(blank(ord) & craw %in% names(REPT_ORDER), REPT_ORDER[craw], ord)
  vv <- rec(vert); vv <- ifelse(is.na(vv), "", ifelse(vv, "TRUE", "FALSE"))
  mk("taxa", taxon_id = id, accepted_name = gsub("_", " ", name), rank = rec(rank), class = cl2, order = ord,
     family = rec(fam), analysis_clade = rec(clade), is_vertebrate = vv, notes = rec(notes))
}

## predator analysis clade from Linnean class + the curated squamate genus table (no genus regex in code)
pred_clade <- function(cls, genus, sq = SQUAMATES()) {
  c0 <- sub("[_ ].*$", "", cls)
  ifelse(c0 == "Aves", "Aves", ifelse(c0 == "Mammalia", "Mammalia", ifelse(c0 == "Crocodylia", "Crocodylia",
  ifelse(c0 %in% c("Squamata", "Reptilia"), sq$analysis_clade[match(genus, sq$genus)], NA))))
}

## ---------------------------------------------------------------
## write one script's rows under the ownership rule
## ---------------------------------------------------------------
commit <- function(owner, owns, sources, pairs, measurements = NULL, taxa = NULL, methods = NULL, pair_tag, delete_pairs_by_source = TRUE) {
  ## pair_tag: regex matched by every pair_id this script writes and by no other script's pair_ids.
  ## Pairs are owned by tag (so a merged publication's source row, owned by 00_merged_sources.R, can be
  ## cited by several scripts); sources are owned by extracted_from prefix.
  if (nrow(pairs) && !all(grepl(pair_tag, pairs$pair_id))) stop(owner, ": pair_ids not matching its pair_tag: ", paste(head(pairs$pair_id[!grepl(pair_tag, pairs$pair_id)]), collapse = ", "))
  pairs$pred_taxon_id <- norm_case(pairs$pred_taxon_id); pairs$prey_taxon_id <- norm_case(pairs$prey_taxon_id)
  if (!is.null(measurements) && nrow(measurements)) measurements$taxon_id <- norm_case(measurements$taxon_id)
  if (!is.null(taxa) && nrow(taxa)) { taxa$taxon_id <- norm_case(taxa$taxon_id); an <- norm_case(gsub(" ", "_", taxa$accepted_name)); taxa$accepted_name <- gsub("_", " ", an) }
  S <- read_db("sources"); P <- read_db("pairs"); M <- read_db("measurements"); X <- read_db("taxa"); ME <- read_db("methods")
  mine <- Reduce(`|`, lapply(owns, function(o) startsWith(S$extracted_from, o)), init = rep(FALSE, nrow(S)))
  old_ids <- S$source_id[mine]
  dp <- grepl(pair_tag, P$pair_id) | (delete_pairs_by_source & P$source_id %in% old_ids)
  dm <- (delete_pairs_by_source & M$source_id %in% old_ids) | (M$pair_id != "" & grepl(pair_tag, M$pair_id))
  S <- S[!mine, , drop = FALSE]; P <- P[!dp, , drop = FALSE]; M <- M[!dm, , drop = FALSE]
  ## every new source must be one this script owns, and must not collide with another script's rows
  bad_own <- !Reduce(`|`, lapply(owns, function(o) startsWith(sources$extracted_from, o)), init = rep(FALSE, nrow(sources)))
  if (any(bad_own)) stop(owner, ": sources whose extracted_from is not one of its inputs: ", paste(head(sources$source_id[bad_own]), collapse = ", "))
  clash <- intersect(sources$source_id, S$source_id)
  if (length(clash)) stop(owner, ": source_id already owned by another script: ", paste(head(clash, 10), collapse = ", "))
  if (any(duplicated(sources$source_id))) stop(owner, ": duplicate source_id in its own output: ", paste(head(sources$source_id[duplicated(sources$source_id)]), collapse = ", "))
  pc <- intersect(pairs$pair_id, P$pair_id); if (length(pc)) stop(owner, ": pair_id already used: ", paste(head(pc), collapse = ", "))
  if (any(duplicated(pairs$pair_id))) stop(owner, ": duplicate pair_id: ", paste(head(pairs$pair_id[duplicated(pairs$pair_id)]), collapse = ", "))
  ext <- setdiff(unique(pairs$source_id), sources$source_id)          # sources owned elsewhere: only merged publications
  merged <- read.csv(file.path(EVID, "duplicate_studies.csv"), colClasses = "character")$merged_source_id
  if (length(setdiff(ext, merged))) stop(owner, ": pairs cite sources it does not write: ", paste(head(setdiff(ext, merged)), collapse = ", "))
  if (length(setdiff(ext, S$source_id))) stop(owner, ": merged sources missing from db/sources.csv (run 00_merged_sources.R first): ", paste(head(setdiff(ext, S$source_id)), collapse = ", "))
  S <- rbind(S, sources[, COLS$sources]); P <- rbind(P, pairs[, COLS$pairs])
  if (!is.null(measurements) && nrow(measurements)) {
    mc <- intersect(measurements$measurement_id, M$measurement_id); if (length(mc)) stop(owner, ": measurement_id already used: ", paste(head(mc), collapse = ", "))
    M <- rbind(M, measurements[, COLS$measurements])
  }
  if (!is.null(taxa) && nrow(taxa)) {                   # upsert: keep existing rows, fill their blank fields
    taxa <- taxa[!duplicated(taxa$taxon_id), COLS$taxa]
    taxa <- taxa[taxa$taxon_id %in% c(pairs$pred_taxon_id, pairs$prey_taxon_id, if (!is.null(measurements)) measurements$taxon_id), , drop = FALSE]   # only taxa this script uses
    new <- taxa[!taxa$taxon_id %in% X$taxon_id, , drop = FALSE]
    j <- match(taxa$taxon_id, X$taxon_id); k <- which(!is.na(j))
    for (cl in setdiff(COLS$taxa, "taxon_id")) { e <- X[[cl]][j[k]]; f <- blank(e) & !blank(taxa[[cl]][k]); X[[cl]][j[k][f]] <- taxa[[cl]][k][f] }
    X <- rbind(X, new)
  }
  if (!is.null(methods) && nrow(methods)) {             # upsert by key: the registry is the single definition
    ME <- ME[!ME$method_id %in% methods$method_id, , drop = FALSE]; ME <- rbind(ME, methods[, COLS$methods])
  }
  ## rows sorted by key, so the tables are byte-identical whatever order the scripts run in
  S <- S[order(S$source_id), ]; P <- P[order(P$pair_id), ]; M <- M[order(M$measurement_id), ]; X <- X[order(X$taxon_id), ]; ME <- ME[order(ME$method_id), ]
  write_db("sources", S); write_db("pairs", P); write_db("measurements", M); write_db("taxa", X); write_db("methods", ME)
  cat(sprintf("%s: wrote %d sources, %d pairs, %d measurements (db totals: %d sources, %d pairs, %d measurements, %d taxa, %d methods)\n",
              owner, nrow(sources), nrow(pairs), if (is.null(measurements)) 0L else nrow(measurements), nrow(S), nrow(P), nrow(M), nrow(X), nrow(ME)))
}

## ---------------------------------------------------------------
## tier from the two mass methods (SCHEMA.md, pair_resolution)
## A side with no mass is treated as a taxon average (the coarser reading, since a later fill
## would normally come from a database), unless the caller says the source records a length or
## bone of that very animal (*_none_from_animal = TRUE). `aggregate` forces E (species-mean
## ratios, prey pooled across predator size classes).
## ---------------------------------------------------------------
FROM_ANIMAL <- c("measured", "regressed_from_length", "regressed_from_bone", "reconstructed_from_remains")
AVERAGE     <- c("species_average_database", "species_average_literature")
COARSE      <- c("size_class_midpoint", "assigned_approximate", "ratio_derived", "unknown")
tier_of <- function(pm, ym, pred_none_from_animal = FALSE, prey_none_from_animal = FALSE, aggregate = FALSE) {
  n <- max(length(pm), length(ym)); pm <- rep_len(pm, n); ym <- rep_len(ym, n)
  pna <- rep_len(pred_none_from_animal, n); yna <- rep_len(prey_none_from_animal, n); ag <- rep_len(aggregate, n)
  pf <- pm %in% FROM_ANIMAL | (blank(pm) & pna); yf <- ym %in% FROM_ANIMAL | (blank(ym) & yna)
  ifelse(ag | pm %in% COARSE | ym %in% COARSE, "E_coarse_or_aggregate",
  ifelse(pf & yf, "A_both_measured", ifelse(pf, "B_pred_measured_prey_average", ifelse(yf, "C_prey_measured_pred_average", "D_both_average"))))
}

## methods.csv rows for the given ids, from the registry
methods_rows <- function(ids) {
  r <- METHOD_REG(); r <- r[r$method_id %in% unique(ids[!blank(ids)]), , drop = FALSE]
  r[, COLS$methods]
}

## per-script side logs (dropped rows, unknown-method rows) under db/migration/
log_dropped <- function(owner, df) {                    # df: source_file, source_row, reason, detail
  f <- file.path(MIG, paste0("dropped_", owner, ".csv")); write.csv(df, f, row.names = FALSE); invisible(f)
}
log_input <- function(owner, n_in, n_unit, note = "") {
  f <- file.path(MIG, paste0("rowcount_", owner, ".csv"))
  write.csv(data.frame(script = owner, input_rows = n_in, input_unit = n_unit, note = note), f, row.names = FALSE)
}

## merged publications (evidence/duplicate_studies.csv) and the record-level duplicate decisions
MERGED  <- function() read.csv(file.path(EVID, "duplicate_studies.csv"), colClasses = "character", na.strings = character(0))
DUPRECS <- function() read.csv(file.path(EVID, "duplicate_records.csv"), colClasses = "character", na.strings = character(0))
