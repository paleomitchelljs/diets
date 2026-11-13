###################################################################
# run_all.R -- build db/ end to end: the Phase 2 extant scripts (00-09) and the Phase 3 fossil
# script (10_fossil_step2.R), in file-name order.
#   Rscript code/db_migrate/run_all.R              # migrate, prune unreferenced taxa, compare, count, validate
#   Rscript code/db_migrate/run_all.R --evidence   # first rebuild the evidence tables from the PDFs/docx
# Each NN_*.R script replaces only the rows it owns (see _common.R), so the scripts can also be run
# one at a time. 00_merged_sources.R must run before 01-09. Nothing here empties db/. The fossil
# script runs here too (and so do the Phase 4b expansion scripts 20-32 and 90_prey_mass_fill.R, which must stay last among the NN_ scripts), so a build that starts from an empty db/ still ends with the fossil rows
# (they were lost once, on 2026-11-12, when only 0N_*.R was run).
###################################################################
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
HERE <- dirname(normalizePath(sub("^--file=", "", fa[1]))); ROOT <- normalizePath(file.path(HERE, "..", ".."))
RS <- file.path(R.home("bin"), "Rscript")
run <- function(f) { cat("\n>>>", f, "\n"); st <- system2(RS, shQuote(f)); if (!identical(st, 0L)) stop(f, " failed (status ", st, ")") }
if ("--evidence" %in% commandArgs(TRUE)) for (f in c("build_rawdata_evidence.R", "parse_oliveira2025.R", "build_duplicate_evidence.R")) run(file.path(HERE, "evidence", f))
for (f in sort(list.files(HERE, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) run(f)
## prune taxa no pair or measurement refers to (left behind when a script renames a taxon)
tx <- read.csv(file.path(ROOT, "db", "taxa.csv"), colClasses = "character", na.strings = character(0), check.names = FALSE)
pr <- read.csv(file.path(ROOT, "db", "pairs.csv"), colClasses = "character", na.strings = character(0), check.names = FALSE)
me <- read.csv(file.path(ROOT, "db", "measurements.csv"), colClasses = "character", na.strings = character(0), check.names = FALSE)
keep <- tx$taxon_id %in% c(pr$pred_taxon_id, pr$prey_taxon_id, me$taxon_id)
cat(sprintf("\npruned %d unreferenced taxa (kept %d)\n", sum(!keep), sum(keep)))
tx <- tx[keep, ]; tx <- tx[order(tx$taxon_id), ]
write.csv(tx, file.path(ROOT, "db", "taxa.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
for (f in c("compare_model_input.R", "migration_counts.R", "expansion_report.R")) if (file.exists(file.path(HERE, f))) run(file.path(HERE, f))
## validator: print it and keep a copy for MIGRATION_REPORT.md (default --max-print=50)
cat("\n>>> validator\n")
vout <- suppressWarnings(system2(RS, shQuote(file.path(ROOT, "code", "db_validate.R")), stdout = TRUE, stderr = TRUE))
writeLines(vout); writeLines(vout, file.path(ROOT, "db", "migration", "validator_output.txt"))
