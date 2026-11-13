###################################################################
# 21_nikolov2004.R -- Nikolov, Kodzhabashev & Popov 2004, wintering Great Grey Shrikes in Bulgaria (Phase 4b, Part 1).
# Table 1 totals (201 prey from caches 51.7%, pellets 43.3%, food remains 5%; pooled, no per-item method).
# Coded prey_remains (caches + remains = 56.7%), pooled note. Predator mass: AVONET species mean.
# Owns: sources with extracted_from starting with the PDF path; pairs "Nikolov2004_*".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "21_nikolov2004"; PDF <- "pdfs/_processed/extant_pdf/herps_2026-06-04/skipped/Lanius_excubitordiet-.pdf"
ev <- EXP_EVID("nikolov2004")
ev <- with_defaults(ev, pred_name = "Lanius excubitor", pred_class = "Aves", pred_clade = "Aves", pred_family = "Laniidae", pred_order = "Passeriformes",
  pred_mass_method = "lookup", individual_pair = "FALSE", observation_type = "prey_remains", interaction_outcome = "consumed")
ev$prey_order <- ""; ev$prey_family <- ""
ev$quote <- paste0(ev$quote, " (Biol. Lett. 41:121)")
ev$notes <- paste0(ev$notes, "; Bulgaria, Nov-Mar 1998-2002; observation types pooled (caches 51.7%, pellets 43.3%, food remains 5%), coded by the majority class of cache plus remains")
h <- hand_pairs(ev, "Nikolov2004", "")
src <- mk("sources", source_id = "Nikolov2004", citation = "Nikolov BP, Kodzhabashev ND, Popov VV (2004) Diet composition and spatial patterns of food caching in wintering Great Grey Shrikes (Lanius excubitor) in Bulgaria. Biological Letters 41(2):119-133",
  file_name = "Lanius_excubitordiet-.pdf", data_type = "journal_article", extracted_from = paste0(PDF, ", Table 1"),
  notes = "Phase 4b. Filed as skipped in June 2026 (well-sampled clade).")
commit(OWNER, PDF, src, h$pairs, NULL, h$taxa, pair_tag = "^Nikolov2004_n[0-9]+$")
