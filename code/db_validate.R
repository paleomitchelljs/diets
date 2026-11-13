###################################################################
# db_validate.R -- refuse bad rows in the db/ tables.
#
# Usage (from anywhere; ROOT is found from this script's path):
#   Rscript code/db_validate.R                 # validates db/
#   Rscript code/db_validate.R db/tests/broken # validates another folder
# Options:
#   --vocab=PATH      controlled vocabularies   (default db/vocab.yml)
#   --index=PATH      methodology index         (default docs/methodology_index.csv)
#   --report=PATH     write every error and warning to a CSV
#   --max-print=N     offending rows printed per check (default 50; Inf = all)
#
# Exit status: 0 = no errors (warnings allowed); 1 = one or more errors;
#              2 = could not run (folder, vocab or index missing/unreadable).
#
# The column spec below (SPEC) must agree with db/SCHEMA.md;
# db/tests/run_tests.R checks that they do.
###################################################################

args_all <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", args_all, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa[1]))), "..")) else normalizePath(getwd())

## ---------------------------------------------------------------
## Column spec. req: "yes" = always required; "cond" = required under
## a rule checked below (see SCHEMA.md); "no" = optional.
## ---------------------------------------------------------------
SPEC <- list(
  sources = list(key = "source_id", cols = c(
    source_id = "yes", citation = "yes", doi = "no", file_name = "no",
    data_type = "yes", extracted_from = "yes", compiled_in = "no", notes = "no")),
  taxa = list(key = "taxon_id", cols = c(
    taxon_id = "yes", accepted_name = "yes", rank = "yes", class = "no", order = "no",
    family = "no", analysis_clade = "no", is_vertebrate = "no", tree_label = "no",
    phylopic_uuid = "no", notes = "no")),
  methods = list(key = "method_id", cols = c(
    method_id = "yes", mass_method = "yes", description = "yes", equation = "yes",
    response = "yes", predictor = "yes", taxon_scope = "yes", valid_range = "yes",
    input_min = "no", input_max = "no", reported_error = "no", method_file = "yes",
    locator = "yes", verified = "yes", notes = "no")),
  pairs = list(key = "pair_id", cols = c(
    pair_id = "yes", source_id = "yes", pred_taxon_id = "yes", prey_taxon_id = "yes",
    pred_specimen = "cond", prey_specimen = "no",
    observation_type = "yes", pair_resolution = "yes", individual_pair = "yes",
    pred_mass_g = "no", pred_mass_lo_g = "cond", pred_mass_hi_g = "cond",
    pred_mass_method = "cond", pred_mass_method_id = "cond", pred_mass_basis = "cond",
    prey_mass_g = "no", prey_mass_lo_g = "cond", prey_mass_hi_g = "cond",
    prey_mass_method = "cond", prey_mass_method_id = "cond", prey_mass_basis = "cond",
    pred_stage = "no", prey_stage = "no", n_items = "no",
    interaction_outcome = "yes", is_fossil = "yes",
    age_ma = "cond", age_ma_lo = "cond", age_ma_hi = "cond",
    quote_or_locator = "yes", notes = "no")),
  measurements = list(key = "measurement_id", cols = c(
    measurement_id = "yes", source_id = "yes", pair_id = "no", specimen = "no",
    taxon_id = "yes", role = "yes", element = "yes", dimension = "yes", value = "yes",
    unit = "yes", value_type = "yes", locator = "yes", notes = "no"))
)
VOCAB_KEYS     <- c("observation_type", "interaction_outcome", "pair_resolution", "mass_method")
NEEDS_METHOD   <- c("regressed_from_length", "regressed_from_bone", "ratio_derived")
## tiers are set by where each mass comes from (SCHEMA.md, pair_resolution)
FROM_ANIMAL    <- c("measured", "regressed_from_length", "regressed_from_bone", "reconstructed_from_remains")
NEEDS_BASIS    <- c(NEEDS_METHOD, "reconstructed_from_remains")   # a basis naming what was measured
AVERAGE        <- c("species_average_database", "species_average_literature")
COARSE         <- c("size_class_midpoint", "assigned_approximate", "ratio_derived", "unknown")
ROLES          <- c("predator", "prey", "comparison")      # measurements.role (structural)
MASS_UNITS     <- c("mg", "g", "kg")
INDEX_OK       <- c("on_disk", "on_disk_elsewhere")         # other statuses -> warning
PLACEHOLDERS   <- c("unknown", "na", "n/a", "none", "?", "-", "tbd", "todo")
ID_RE          <- "^[A-Za-z0-9][A-Za-z0-9_.:-]*$"
NUM_RE         <- "^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$"
UUID_RE        <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

## ---------------------------------------------------------------
## helpers
## ---------------------------------------------------------------
blank   <- function(x) is.na(x) | trimws(x) == "" | x == "NA"
filled  <- function(x) !blank(x)
is_num  <- function(x) filled(x) & grepl(NUM_RE, trimws(x))
as_num  <- function(x) ifelse(is_num(x), suppressWarnings(as.numeric(trimws(x))), NA_real_)

ISSUES <- list()
issue <- function(sev, table, df, bad, column, check, msg) {
  bad[is.na(bad)] <- FALSE
  if (!any(bad)) return(invisible())
  key <- SPEC[[table]]$key
  ISSUES[[length(ISSUES) + 1]] <<- data.frame(
    severity = sev, table = paste0(table, ".csv"), line = df$.line[bad],
    key = if (key %in% names(df)) df[[key]][bad] else "",
    column = column, check = check,
    value = if (column %in% names(df)) df[[column]][bad] else "",
    message = if (length(msg) == 1) msg else msg[bad],
    stringsAsFactors = FALSE)
}
err  <- function(...) issue("ERROR", ...)
warn <- function(...) issue("WARN", ...)
tab_err <- function(table, check, msg)
  ISSUES[[length(ISSUES) + 1]] <<- data.frame(severity = "ERROR", table = paste0(table, ".csv"),
    line = NA_integer_, key = "", column = "", check = check, value = "", message = msg,
    stringsAsFactors = FALSE)

read_tab <- function(path) {
  x <- tryCatch(read.csv(path, colClasses = "character", na.strings = character(0),
                         check.names = FALSE, fileEncoding = "UTF-8-BOM",
                         strip.white = FALSE, blank.lines.skip = FALSE),
                error = function(e) e)
  if (inherits(x, "error")) return(x)
  x$.line <- seq_len(nrow(x)) + 1L      # header is line 1; valid because newlines in fields are refused
  x
}

## ---------------------------------------------------------------
## main
## ---------------------------------------------------------------
db_validate <- function(argv = commandArgs(trailingOnly = TRUE)) {
  ISSUES <<- list()
  getopt <- function(name, default) {
    hit <- grep(paste0("^--", name, "="), argv, value = TRUE)
    if (length(hit)) sub(paste0("^--", name, "="), "", hit[length(hit)]) else default
  }
  pos <- argv[!grepl("^--", argv)]
  DB     <- if (length(pos)) pos[1] else file.path(ROOT, "db")
  VOCABF <- getopt("vocab", file.path(ROOT, "db", "vocab.yml"))
  INDEXF <- getopt("index", file.path(ROOT, "docs", "methodology_index.csv"))
  REPORT <- getopt("report", "")
  MAXP   <- suppressWarnings(as.numeric(getopt("max-print", "50"))); if (is.na(MAXP)) MAXP <- 50

  fatal <- function(msg) { cat("CANNOT RUN:", msg, "\n"); return(2L) }
  if (!dir.exists(DB)) return(fatal(paste("no such folder:", DB)))
  if (!requireNamespace("yaml", quietly = TRUE)) return(fatal("R package 'yaml' is not installed"))
  if (!file.exists(VOCABF)) return(fatal(paste("vocab file not found:", VOCABF)))
  vocab <- tryCatch(yaml::read_yaml(VOCABF), error = function(e) NULL)
  if (is.null(vocab) || !all(VOCAB_KEYS %in% names(vocab)))
    return(fatal(paste("vocab file must define:", paste(VOCAB_KEYS, collapse = ", "))))
  if (!file.exists(INDEXF)) return(fatal(paste("methodology index not found:", INDEXF)))
  idx <- tryCatch(read.csv(INDEXF, colClasses = "character", check.names = FALSE,
                           fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
  if (is.null(idx) || !"method_file" %in% names(idx)) return(fatal(paste("cannot read method_file column of", INDEXF)))

  cat("=== db_validate ===\n")
  cat("  tables :", normalizePath(DB), "\n  vocab  :", VOCABF, "\n  index  :", INDEXF, "\n\n")

  ## ---- read tables, check headers and generic row structure ----
  T <- list(); usable <- list()
  for (tb in names(SPEC)) {
    path <- file.path(DB, paste0(tb, ".csv"))
    exp_cols <- names(SPEC[[tb]]$cols)
    if (!file.exists(path)) { tab_err(tb, "table_missing", "file not found"); usable[[tb]] <- FALSE; next }
    x <- read_tab(path)
    if (inherits(x, "error")) { tab_err(tb, "unreadable", conditionMessage(x)); usable[[tb]] <- FALSE; next }
    got <- setdiff(names(x), ".line")
    miss <- setdiff(exp_cols, got); extra <- setdiff(got, exp_cols)
    if (length(miss))  tab_err(tb, "header", paste("missing column(s):", paste(miss, collapse = ", ")))
    if (length(extra)) tab_err(tb, "header", paste("unexpected column(s):", paste(extra, collapse = ", ")))
    if (!length(miss) && !length(extra) && !identical(got, exp_cols))
      tab_err(tb, "header", "columns present but not in SCHEMA.md order")
    usable[[tb]] <- !length(miss)
    T[[tb]] <- x
    cat(sprintf("  %-17s %6d rows%s\n", paste0(tb, ".csv"), nrow(x), if (usable[[tb]]) "" else "  (header broken: row checks skipped)"))
    if (!usable[[tb]] || nrow(x) == 0) next
    vals <- as.matrix(x[, got, drop = FALSE])
    nl <- apply(vals, 1, function(r) any(grepl("[\r\n]", r)))
    err(tb, x, nl, "", "newline_in_field", "a field contains a line break (put notes on one line)")
    empty <- apply(vals, 1, function(r) all(blank(r)))
    err(tb, x, empty, "", "empty_row", "row is entirely empty")
    T[[tb]] <- x[!empty, , drop = FALSE]
  }
  cat("\n")
  ok <- function(tb) isTRUE(usable[[tb]]) && nrow(T[[tb]]) > 0
  keys_of <- function(tb) if (!is.null(T[[tb]]) && SPEC[[tb]]$key %in% names(T[[tb]])) T[[tb]][[SPEC[[tb]]$key]] else character(0)

  ## ---- generic: required fields, key format, duplicate keys ----
  for (tb in names(SPEC)) {
    if (!ok(tb)) next
    x <- T[[tb]]; spec <- SPEC[[tb]]$cols; key <- SPEC[[tb]]$key
    for (cl in names(spec)[spec == "yes"])
      err(tb, x, blank(x[[cl]]), cl, "required", paste0("required field '", cl, "' is empty"))
    k <- x[[key]]
    err(tb, x, filled(k) & !grepl(ID_RE, k), key, "id_format", "id has spaces or illegal characters")
    err(tb, x, filled(k) & (duplicated(k) | duplicated(k, fromLast = TRUE)), key, "duplicate_key",
        paste0("'", key, "' is not unique"))
  }
  SRC <- keys_of("sources"); TAX <- keys_of("taxa"); MET <- keys_of("methods"); PAI <- keys_of("pairs")

  ## ---- sources ----
  if (ok("sources")) {
    s <- T$sources
    err("sources", s, filled(s$compiled_in) & !s$compiled_in %in% SRC, "compiled_in", "ref_source",
        "compiled_in does not resolve in sources.csv")
    err("sources", s, filled(s$compiled_in) & s$compiled_in == s$source_id, "compiled_in", "ref_source",
        "source cannot be compiled in itself")
  }

  ## ---- taxa ----
  if (ok("taxa")) {
    t <- T$taxa
    err("taxa", t, filled(t$is_vertebrate) & !t$is_vertebrate %in% c("TRUE", "FALSE"), "is_vertebrate", "bool",
        "is_vertebrate must be TRUE, FALSE or empty")
    err("taxa", t, filled(t$phylopic_uuid) & !grepl(UUID_RE, t$phylopic_uuid), "phylopic_uuid", "uuid_format",
        "phylopic_uuid is not a UUID")
    nm <- tolower(trimws(t$accepted_name))
    warn("taxa", t, filled(nm) & (duplicated(nm) | duplicated(nm, fromLast = TRUE)), "accepted_name",
         "duplicate_name", "same accepted_name under more than one taxon_id")
  }

  ## ---- methods ----
  in_index <- function(f) f %in% idx$method_file           # verbatim: exact, case-sensitive, untrimmed
  if (ok("methods")) {
    m <- T$methods
    err("methods", m, filled(m$mass_method) & !m$mass_method %in% vocab$mass_method, "mass_method", "vocab",
        "mass_method not in vocab.yml")
    err("methods", m, filled(m$method_file) & !in_index(m$method_file), "method_file", "method_file_not_in_index",
        "method_file does not appear verbatim in docs/methodology_index.csv")
    st <- idx$status[match(m$method_file, idx$method_file)]
    if (!is.null(st)) warn("methods", m, in_index(m$method_file) & !st %in% INDEX_OK, "method_file", "index_status",
        paste0("methodology index status is '", st, "' (paper not on disk)"))
    err("methods", m, filled(m$verified) & !m$verified %in% c("TRUE", "FALSE"), "verified", "bool",
        "verified must be TRUE or FALSE")
    for (cl in c("input_min", "input_max"))
      err("methods", m, filled(m[[cl]]) & !is_num(m[[cl]]), cl, "numeric", paste(cl, "is not a number"))
    lo <- as_num(m$input_min); hi <- as_num(m$input_max)
    err("methods", m, !is.na(lo) & !is.na(hi) & lo > hi, "input_min", "range_order", "input_min > input_max")
  }

  ## ---- pairs ----
  if (ok("pairs")) {
    p <- T$pairs
    for (v in c("observation_type", "pair_resolution", "interaction_outcome"))
      err("pairs", p, filled(p[[v]]) & !p[[v]] %in% vocab[[v]], v, "vocab", paste(v, "not in vocab.yml"))
    for (cl in c("is_fossil", "individual_pair"))
      err("pairs", p, filled(p[[cl]]) & !p[[cl]] %in% c("TRUE", "FALSE"), cl, "bool", paste(cl, "must be TRUE or FALSE"))
    err("pairs", p, filled(p$source_id) & !p$source_id %in% SRC, "source_id", "ref_source",
        "source_id does not resolve in sources.csv")
    for (cl in c("pred_taxon_id", "prey_taxon_id"))
      err("pairs", p, filled(p[[cl]]) & !p[[cl]] %in% TAX, cl, "ref_taxon", paste(cl, "does not resolve in taxa.csv"))

    ## an individual pair needs a predator specimen
    err("pairs", p, p$individual_pair %in% "TRUE" & blank(p$pred_specimen), "pred_specimen",
        "individual_specimen", "individual_pair is TRUE but there is no predator specimen identifier")

    ## the tier may not claim better masses than the mass methods show.
    ## A side with no mass does not limit the tier; a coarse side forces E.
    pm <- p$pred_mass_method; ym <- p$prey_mass_method
    pred_avg <- pm %in% AVERAGE; prey_avg <- ym %in% AVERAGE
    coarse   <- pm %in% COARSE | ym %in% COARSE
    tr <- p$pair_resolution
    why <- rep(NA_character_, nrow(p))
    why[tr != "E_coarse_or_aggregate" & coarse] <- "a coarse or unknown mass method puts the row in tier E"
    why[is.na(why) & tr == "A_both_measured" & (pred_avg | prey_avg)] <- "tier A needs neither mass to be a species average"
    why[is.na(why) & tr == "B_pred_measured_prey_average" & pred_avg] <- "tier B needs a predator mass from the animal itself"
    why[is.na(why) & tr == "C_prey_measured_pred_average" & prey_avg] <- "tier C needs a prey mass from the animal itself"
    bad <- !is.na(why) & tr %in% vocab$pair_resolution     # an illegal tier is already reported by 'vocab'
    err("pairs", p, bad, "pair_resolution", "tier_method_mismatch",
        paste0(why, " (pred: '", pm, "', prey: '", ym, "')"))
    for (me in c("pred_mass_method", "prey_mass_method"))
      warn("pairs", p, p[[me]] %in% "unknown", me, "unknown_method", paste0(me, " is 'unknown'"))

    ## masses, intervals, methods (same rules for predator and prey)
    for (who in c("pred", "prey")) {
      pt <- paste0(who, "_mass_g"); lo <- paste0(who, "_mass_lo_g"); hi <- paste0(who, "_mass_hi_g")
      me <- paste0(who, "_mass_method"); mi <- paste0(who, "_mass_method_id"); ba <- paste0(who, "_mass_basis")
      for (cl in c(pt, lo, hi)) {
        err("pairs", p, filled(p[[cl]]) & !is_num(p[[cl]]), cl, "mass_numeric", paste(cl, "is not a plain number"))
        v <- as_num(p[[cl]])
        err("pairs", p, !is.na(v) & v <= 0, cl, "mass_positive", paste(cl, "must be > 0"))
      }
      P <- as_num(p[[pt]]); L <- as_num(p[[lo]]); H <- as_num(p[[hi]])
      err("pairs", p, xor(filled(p[[lo]]), filled(p[[hi]])), lo, "interval_pair",
          paste0(lo, " and ", hi, " must both be given or both be empty"))
      err("pairs", p, !is.na(L) & !is.na(H) & L > H, lo, "interval_order", paste0(lo, " > ", hi))
      err("pairs", p, !is.na(L) & !is.na(P) & L > P, lo, "interval_order", paste0(lo, " > ", pt))
      err("pairs", p, !is.na(H) & !is.na(P) & P > H, hi, "interval_order", paste0(pt, " > ", hi))

      ## a bracket without a point value (lo and hi only) is allowed, but it must say what sets the bounds
      brk <- blank(p[[pt]]) & filled(p[[lo]]) & filled(p[[hi]])
      warn("pairs", p, brk, lo, "bracket_only", paste0(who, " mass is a bracket with no point value"))
      err("pairs", p, brk & (blank(p[[ba]]) | tolower(trimws(p[[ba]])) %in% PLACEHOLDERS), ba, "bracket_basis",
          paste0(who, " mass is a bracket with no point value, so ", ba, " must state what sets each bound"))
      anyval <- filled(p[[pt]]) | filled(p[[lo]]) | filled(p[[hi]])
      err("pairs", p, anyval & blank(p[[me]]), me, "method_missing", paste0("a ", who, " mass is given but ", me, " is empty"))
      err("pairs", p, !anyval & filled(p[[me]]), me, "method_without_value", paste0(me, " is set but no ", who, " mass is given"))
      err("pairs", p, filled(p[[me]]) & !p[[me]] %in% vocab$mass_method, me, "vocab", paste(me, "not in vocab.yml"))

      needs <- p[[me]] %in% NEEDS_METHOD
      err("pairs", p, needs & blank(p[[mi]]), mi, "method_id_missing",
          paste0(p[[me]], " mass needs ", mi, " pointing to methods.csv"))
      err("pairs", p, filled(p[[mi]]) & !p[[mi]] %in% MET, mi, "method_id_unresolved",
          paste(mi, "does not resolve in methods.csv"))
      if (ok("methods")) {
        j <- match(p[[mi]], T$methods$method_id)
        mf <- T$methods$method_file[j]; mk <- T$methods$mass_method[j]; mv <- T$methods$verified[j]
        err("pairs", p, needs & !is.na(j) & !in_index(mf), mi, "method_file_not_in_index",
            paste0("method '", p[[mi]], "' cites method_file '", mf, "', which is not in docs/methodology_index.csv"))
        err("pairs", p, !is.na(j) & filled(p[[me]]) & mk != p[[me]], mi, "method_kind_mismatch",
            paste0(me, " is '", p[[me]], "' but method '", p[[mi]], "' is '", mk, "'"))
        warn("pairs", p, !is.na(j) & mv %in% "FALSE", mi, "unverified_method",
             paste0("method '", p[[mi]], "' is marked verified = FALSE"))
      }
      bt <- tolower(trimws(p[[ba]]))
      err("pairs", p, p[[me]] %in% NEEDS_BASIS & (blank(p[[ba]]) | bt %in% PLACEHOLDERS), ba, "basis_missing",
          paste0(p[[me]], " mass needs ", ba, " naming the input measurement"))
    }

    ## n_items
    err("pairs", p, filled(p$n_items) & !grepl("^[0-9]+$", trimws(p$n_items)), "n_items", "n_items_integer",
        "n_items must be a whole number")
    err("pairs", p, grepl("^[0-9]+$", trimws(p$n_items)) & suppressWarnings(as.numeric(p$n_items)) < 1,
        "n_items", "n_items_integer", "n_items must be >= 1")

    ## fossil age
    fos <- p$is_fossil %in% "TRUE"
    err("pairs", p, fos & blank(p$age_ma), "age_ma", "fossil_age_missing", "fossil row has no age_ma")
    for (cl in c("age_ma", "age_ma_lo", "age_ma_hi")) {
      err("pairs", p, filled(p[[cl]]) & !is_num(p[[cl]]), cl, "age_numeric", paste(cl, "is not a number"))
      err("pairs", p, !is.na(as_num(p[[cl]])) & as_num(p[[cl]]) < 0, cl, "age_numeric", paste(cl, "must be >= 0"))
    }
    A <- as_num(p$age_ma); AL <- as_num(p$age_ma_lo); AH <- as_num(p$age_ma_hi)
    err("pairs", p, xor(filled(p$age_ma_lo), filled(p$age_ma_hi)), "age_ma_lo", "interval_pair",
        "age_ma_lo and age_ma_hi must both be given or both be empty")
    err("pairs", p, (!is.na(AL) & !is.na(AH) & AL > AH) | (!is.na(AL) & !is.na(A) & AL > A) |
                    (!is.na(AH) & !is.na(A) & A > AH), "age_ma", "interval_order",
        "ages must satisfy age_ma_lo <= age_ma <= age_ma_hi")
  }

  ## ---- measurements ----
  if (ok("measurements")) {
    x <- T$measurements
    err("measurements", x, filled(x$source_id) & !x$source_id %in% SRC, "source_id", "ref_source",
        "source_id does not resolve in sources.csv")
    err("measurements", x, filled(x$taxon_id) & !x$taxon_id %in% TAX, "taxon_id", "ref_taxon",
        "taxon_id does not resolve in taxa.csv")
    err("measurements", x, filled(x$pair_id) & !x$pair_id %in% PAI, "pair_id", "ref_pair",
        "pair_id does not resolve in pairs.csv")
    err("measurements", x, filled(x$role) & !x$role %in% ROLES, "role", "role_value",
        paste("role must be one of:", paste(ROLES, collapse = ", ")))
    err("measurements", x, filled(x$value) & !is_num(x$value), "value", "numeric", "value is not a plain number")
    v <- as_num(x$value)
    err("measurements", x, !is.na(v) & v <= 0 & x$unit %in% MASS_UNITS, "value", "mass_positive", "mass must be > 0")
    if (ok("pairs")) {
      j <- match(x$pair_id, T$pairs$pair_id)
      want <- ifelse(x$role == "predator", T$pairs$pred_taxon_id[j],
              ifelse(x$role == "prey", T$pairs$prey_taxon_id[j], NA))
      err("measurements", x, !is.na(j) & !is.na(want) & x$taxon_id != want, "taxon_id", "measurement_taxon_mismatch",
          paste0("taxon_id differs from the linked pair's ", x$role, " taxon (", want, ")"))
    }
  }

  ## ---- report ----
  I <- if (length(ISSUES)) do.call(rbind, ISSUES) else
    data.frame(severity = character(0), table = character(0), line = integer(0), key = character(0),
               column = character(0), check = character(0), value = character(0), message = character(0))
  if (nrow(I)) I <- I[order(match(I$table, paste0(names(SPEC), ".csv")), I$line, na.last = FALSE), , drop = FALSE]
  if (nzchar(REPORT)) write.csv(I, REPORT, row.names = FALSE)
  for (sev in c("ERROR", "WARN")) {
    S <- I[I$severity == sev, , drop = FALSE]
    if (!nrow(S)) next
    grp <- unique(S[, c("table", "check")])
    grp <- grp[order(match(grp$table, paste0(names(SPEC), ".csv"))), , drop = FALSE]
    for (g in seq_len(nrow(grp))) {
      G <- S[S$table == grp$table[g] & S$check == grp$check[g], , drop = FALSE]
      cat(sprintf("%-5s %s [%s]  %d row%s\n", if (sev == "ERROR") "FAIL" else "WARN",
                  grp$table[g], grp$check[g], nrow(G), if (nrow(G) == 1) "" else "s"))
      shown <- utils::head(G, MAXP)
      for (r in seq_len(nrow(shown))) {
        loc <- if (is.na(shown$line[r])) "      (table)" else sprintf("      line %-5d", shown$line[r])
        kv  <- if (nzchar(shown$key[r])) sprintf(" %s", shown$key[r]) else ""
        val <- if (nzchar(shown$column[r])) sprintf("  %s='%s'", shown$column[r], shown$value[r]) else ""
        cat(sprintf("%s%s%s  -- %s\n", loc, kv, val, shown$message[r]))
      }
      if (nrow(G) > MAXP) cat(sprintf("      ... and %d more (use --max-print=Inf or --report=FILE)\n", nrow(G) - MAXP))
    }
  }
  ne <- sum(I$severity == "ERROR"); nw <- sum(I$severity == "WARN")
  cat(sprintf("\nRESULT: %s  (%d error%s in %d check%s; %d warning%s)\n",
              if (ne) "FAIL" else "PASS", ne, if (ne == 1) "" else "s",
              length(unique(paste(I$table, I$check)[I$severity == "ERROR"])),
              if (length(unique(paste(I$table, I$check)[I$severity == "ERROR"])) == 1) "" else "s",
              nw, if (nw == 1) "" else "s"))
  if (ne) 1L else 0L
}

if (sys.nframe() == 0L) quit(status = db_validate(), save = "no")
