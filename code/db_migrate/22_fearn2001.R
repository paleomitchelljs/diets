###################################################################
# 22_fearn2001.R -- Fearn, Robinson, Sambono & Shine 2001, "Pythons in the pergola: the ecology of 'nuisance' carpet pythons
# (Morelia spilota) from suburban habitats in south-eastern Queensland", Wildlife Research 28:573-579 (Phase 4b, Part 1).
# Read from pdfs/extant_to_ingest/ai_found/302pythonsinthepergola.pdf, Table 1 (prey identified from gut contents of carpet
# pythons; 249 snakes collected; snakes weighed and palpated, some regurgitated, some defecated; aviculturalists supplied bird
# lists). Methods: "Masses for partly digested birds were estimated from published literature (MacWhirter 1987; Marchant and
# Higgins 1993)". Table 1 does not say how each prey mass was obtained, so the prey masses are `unknown` (tier E). Where Table 1
# gives a range ("500-4000") only the bracket is stored. Predator mass: species mean from the Amniote database (the table is
# pooled over juveniles and adults and not linked to individual snakes). Pooled over snakes: individual_pair FALSE. Observation
# type stomach_content (gut contents obtained by palpation, regurgitation and faeces).
# Owns: sources with extracted_from starting with the PDF path; pairs "Fearn2001_*".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "22_fearn2001"; PDF <- "pdfs/extant_to_ingest/ai_found/302pythonsinthepergola.pdf"
ev <- EXP_EVID("fearn2001")
ev <- with_defaults(ev, pred_name = "Morelia spilota", pred_class = "Reptilia", pred_clade = "Serpentes", pred_family = "Pythonidae", pred_order = "Squamata",
  pred_mass_method = "lookup", observation_type = "stomach_content", individual_pair = "FALSE", prey_mass_method = "unknown")
ev$prey_mass_method[ev$prey_mass_g == "" & ev$prey_mass_lo_g == ""] <- ""
ev$quote <- paste0(ev$quote, " (Wildl. Res. 28:574)")
ev$notes <- paste0(ev$notes, ifelse(ev$notes != "", "; ", ""), "table pooled over juvenile and adult snakes; prey mass as printed in Table 1, method not stated (masses of partly digested birds estimated from literature per Methods)")
ev$prey_order <- ""; ev$prey_family <- ""
h <- hand_pairs(ev, "Fearn2001", "")
h$pairs$prey_mass_basis[h$pairs$prey_mass_lo_g != ""] <- "range printed in Table 1 for this prey species; what sets the bounds is not stated"
src <- mk("sources", source_id = "Fearn2001", citation = "Fearn S, Robinson B, Sambono J, Shine R (2001) Pythons in the pergola: the ecology of 'nuisance' carpet pythons (Morelia spilota) from suburban habitats in south-eastern Queensland. Wildlife Research 28:573-579",
  doi = "10.1071/WR00106", file_name = "302pythonsinthepergola.pdf", data_type = "journal_article", extracted_from = paste0(PDF, ", Table 1"),
  notes = "Phase 4b. Queued in the intake manifest; not previously extracted.")
commit(OWNER, PDF, src, h$pairs, NULL, h$taxa, pair_tag = "^Fearn2001_p[0-9]+$")
