###################################################################
# run_tests.R -- show that code/db_validate.R passes good tables and
# fails bad ones for the right reasons.
#   1. db/ itself                 -> exit 0, no ERROR-level issues (warnings allowed since Phase 2)
#   2. db/tests/valid             -> exit 0, issues == valid/EXPECTED.csv
#   3. db/tests/broken            -> exit 1, issues == broken/EXPECTED.csv (no more, no fewer)
#   4. db/tests/broken_header     -> exit 1, issues == broken_header/EXPECTED.csv
#   5. db/SCHEMA.md column tables agree with the validator's SPEC
# Run: Rscript db/tests/run_tests.R   (exit 0 = all tests pass)
###################################################################
args_all <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args_all, value = TRUE)
HERE <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else file.path(getwd(), "db", "tests")
ROOT <- normalizePath(file.path(HERE, "..", ".."))
VAL  <- file.path(ROOT, "code", "db_validate.R")
RS   <- file.path(R.home("bin"), "Rscript")
fails <- 0L
say <- function(ok, msg) { cat(if (ok) "  ok    " else "  FAIL  ", msg, "\n", sep = ""); if (!ok) fails <<- fails + 1L }

run <- function(dir) {
  rep <- tempfile(fileext = ".csv")
  out <- suppressWarnings(system2(RS, c(shQuote(VAL), shQuote(dir), paste0("--report=", shQuote(rep))),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status"); if (is.null(status)) status <- 0L
  list(status = status, out = out,
       rep = if (file.exists(rep)) read.csv(rep, colClasses = "character", na.strings = character(0)) else NULL)
}
sig <- function(d) if (is.null(d) || !nrow(d)) character(0) else
  sort(unique(paste(d$severity, d$table, d$key, d$check, sep = " | ")))
compare <- function(label, r, expected_status, expected_file) {
  say(r$status == expected_status, sprintf("%s: exit status %s (expected %s)", label, r$status, expected_status))
  got  <- sig(r$rep)
  want <- if (is.null(expected_file)) character(0) else
    sig(read.csv(expected_file, colClasses = "character", na.strings = character(0)))
  miss <- setdiff(want, got); extra <- setdiff(got, want)
  say(!length(miss), sprintf("%s: all %d expected issues reported", label, length(want)))
  for (m in miss) cat("          missing : ", m, "\n", sep = "")
  say(!length(extra), sprintf("%s: no unexpected issues", label))
  for (m in extra) cat("          extra   : ", m, "\n", sep = "")
}

cat("=== db validator tests ===\n")
## db/ itself: since Phase 2 it holds data, and the validator's warnings (unknown_method etc.) are expected;
## the test now requires exit 0 and no ERROR-level issue (it required zero issues while the tables were empty)
rd <- run(file.path(ROOT, "db"))
say(rd$status == 0L, sprintf("db/: exit status %s (expected 0)", rd$status))
say(is.null(rd$rep) || !any(rd$rep$severity == "ERROR"), "db/: no ERROR-level issues (warnings allowed)")
compare("tests/valid", run(file.path(HERE, "valid")), 0L, file.path(HERE, "valid", "EXPECTED.csv"))
rb <- run(file.path(HERE, "broken"))
compare("tests/broken", rb, 1L, file.path(HERE, "broken", "EXPECTED.csv"))
compare("tests/broken_header", run(file.path(HERE, "broken_header")), 1L,
        file.path(HERE, "broken_header", "EXPECTED.csv"))

## ---- SCHEMA.md vs SPEC ----
env <- new.env(); sys.source(VAL, envir = env); SPEC <- env$SPEC
md <- readLines(file.path(ROOT, "db", "SCHEMA.md"), warn = FALSE)
for (tb in names(SPEC)) {
  h <- grep(paste0("^### `", tb, "\\.csv`"), md)
  if (length(h) != 1) { say(FALSE, sprintf("SCHEMA.md: one '### `%s.csv`' heading", tb)); next }
  nxt <- grep("^#{2,3} ", md); nxt <- nxt[nxt > h]; end <- if (length(nxt)) nxt[1] - 1 else length(md)
  rows <- grep("^\\| *`[a-z_]+` *\\|", md[h:end], value = TRUE)
  cells <- strsplit(rows, "\\|")
  doc_col <- gsub("[` ]", "", vapply(cells, `[`, "", 2))
  doc_req <- tolower(trimws(vapply(cells, `[`, "", 4)))
  doc_req <- ifelse(grepl("^yes", doc_req), "yes", ifelse(grepl("^no", doc_req), "no", "cond"))
  spec <- SPEC[[tb]]$cols
  say(identical(doc_col, names(spec)), sprintf("SCHEMA.md %s.csv: columns and order match the validator (%d)", tb, length(spec)))
  if (!identical(doc_col, names(spec))) cat("          doc : ", paste(doc_col, collapse = ","), "\n          spec: ",
                                            paste(names(spec), collapse = ","), "\n", sep = "")
  if (identical(doc_col, names(spec))) {
    bad <- doc_col[doc_req != unname(spec)]
    say(!length(bad), sprintf("SCHEMA.md %s.csv: 'Required' matches the validator", tb))
    if (length(bad)) cat("          differ: ", paste(bad, collapse = ", "), "\n", sep = "")
  }
  hdr <- names(read.csv(file.path(ROOT, "db", paste0(tb, ".csv")), check.names = FALSE))
  say(identical(hdr, names(spec)), sprintf("db/%s.csv header matches the validator", tb))
}

cat(sprintf("\n%s (%d failing test%s)\n", if (fails) "TESTS FAILED" else "ALL TESTS PASS", fails, if (fails == 1) "" else "s"))
quit(status = if (fails) 1L else 0L, save = "no")
