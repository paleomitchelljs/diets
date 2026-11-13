###################################################################
# 20_gronlund1970.R -- Grönlund, Itämies & Mikkola 1970, Great Grey Shrike diet in Finland (Phase 4b, Part 1).
# Read from pdfs/_processed/extant_pdf/herps_2026-06-04/skipped/Lanius_excubitor.pdf (pp. 168-170).
# Breeding season: 251 prey from pellets at one nest, given as percentages -> n_items = % x 251 (rounded).
# Autumn: 100 prey, counts printed. Winter: 85 prey, counts printed, from larder finds and observations
# (Leivo 1942; Siivonen unpublished) -> prey_remains. Predator mass: AVONET species mean (no bird weighed).
# Owns: sources with extracted_from starting with the PDF path; pairs "Gronlund1970_*".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "20_gronlund1970"; PDF <- "pdfs/_processed/extant_pdf/herps_2026-06-04/skipped/Lanius_excubitor.pdf"
ev <- EXP_EVID("gronlund1970")
ev <- with_defaults(ev, pred_name = "Lanius excubitor", pred_class = "Aves", pred_clade = "Aves", pred_family = "Laniidae", pred_order = "Passeriformes",
  pred_mass_method = "lookup", individual_pair = "FALSE", interaction_outcome = "consumed")
ev$prey_order <- ""; ev$prey_family <- ""
ev$notes <- paste0(ev$notes, "; ", c(breeding = "breeding season 1969, one nest at Alajoki, Lapua (61 whole pellets and fragments)",
  autumn = "post-breeding pellets from several territories at Alajoki, 1969", winter = "winter larder finds 1936-39 (Leivo 1942) and observations Feb 1948 (Siivonen, unpublished), Helsinki; larders and observations pooled in the source")[ev$season])
ev$quote <- paste0(ev$quote, " (Ornis Fennica 47:", c(breeding = "168-169", autumn = "169-170", winter = "170")[ev$season], ")")
ev$rec <- ev$rec
h <- hand_pairs(ev, "Gronlund1970", "")
src <- mk("sources", source_id = "Gronlund1970", citation = "Grönlund S, Itämies J, Mikkola H (1970) On the food and feeding habits of the Great Grey Shrike Lanius excubitor in Finland. Ornis Fennica 47:167-171",
  file_name = "Lanius_excubitor.pdf", data_type = "journal_article", extracted_from = paste0(PDF, ", pp. 168-170"),
  notes = "Phase 4b. Filed as skipped in June 2026 (well-sampled clade). Percentages converted to counts, see each pair's notes.")
commit(OWNER, PDF, src, h$pairs, NULL, h$taxa, pair_tag = "^Gronlund1970_[abw][0-9]+$")
