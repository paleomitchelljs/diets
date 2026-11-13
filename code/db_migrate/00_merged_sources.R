###################################################################
# 00_merged_sources.R -- one sources.csv row per publication that reached us through more than one
# upstream table (evidence/duplicate_studies.csv, hand-checked; JM asked for the merge on 2026-11-12).
# The scripts that read those upstream tables (01, 02, 03, 04, 09) cite these source_ids instead of
# writing their own, and drop the duplicate records listed in evidence/duplicate_records.csv.
# Writes no pairs. Owns: sources with extracted_from starting "merged:". Run before 01-09.
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "00_merged_sources"; OWNS <- "merged:"
DS <- MERGED()
ST <- read.csv(file.path(EVID, "rawdata_studies.csv"), colClasses = "character")
first_ref <- sub(";.*", "", DS$raw_data_refno)
upstreams <- function(m) paste(c(
  if (nzchar(m$raw_data_refno)) paste0("raw_data RefNo ", gsub(";", ", ", m$raw_data_refno)),
  if (nzchar(m$adb_citation)) "AvianDietDB",
  if (nzchar(m$osprey_study)) paste0("OS-Prey study '", gsub(";", "' and '", m$osprey_study), "'", if (nzchar(m$osprey_predators)) paste0(" (predators ", gsub(";", ", ", m$osprey_predators), ")") else ""),
  if (nzchar(m$extant_source_id)) "extant_new_observations",
  if (nzchar(m$varanus_key)) "varanus_varius_summary.csv"), collapse = "; ")
up <- vapply(seq_len(nrow(DS)), function(i) upstreams(DS[i, ]), "")
src <- mk("sources", source_id = DS$merged_source_id, citation = DS$citation,
  file_name = ifelse(nzchar(first_ref), ST$pdf[match(first_ref, ST$refno)], ""),
  data_type = ifelse(DS$merged_source_id == "Auffenberg1981", "book_or_chapter", "journal_article"),
  extracted_from = paste0("merged: ", up, " (code/db_migrate/evidence/duplicate_studies.csv)"),
  compiled_in = ifelse(nzchar(DS$adb_citation), "AvianDietDB_2021", ifelse(nzchar(DS$raw_data_refno) & DS$merged_source_id != "Auffenberg1981", "DietMatrix2019", "")),
  notes = paste0("one publication found in several upstream tables; duplicate records dropped by code/db_migrate/evidence/duplicate_records.csv (kept copy: AvianDietDB > raw_data > OS-Prey). Evidence: ", DS$evidence))
src$file_name[is.na(src$file_name)] <- ""
log_input(OWNER, nrow(DS), "merged publications in evidence/duplicate_studies.csv")
log_dropped(OWNER, data.frame(source_file = character(0), source_row = character(0), reason = character(0), detail = character(0)))
commit(OWNER, OWNS, src, blank_frame("pairs"), NULL, NULL, NULL, pair_tag = "^$", delete_pairs_by_source = FALSE)
