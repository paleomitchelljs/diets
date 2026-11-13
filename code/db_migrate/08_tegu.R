###################################################################
# 08_tegu.R -- migrate the Salvator merianae GI-contents dataset (Harman et al. 2025, USGS
# ScienceBase; data/structured/tegu_diet_individuals.csv + tegu_diet_dietcategories.csv) into db/.
# One pairs row per tegu x diet category recorded present (Found = 1). Each tegu was weighed and
# measured, so these are individual pairs with a measured predator mass. The diet is category
# presence only: no prey counts or masses (x23: "we do NOT fabricate them"), so prey mass is empty
# and n_items is empty. Plant and man-made categories are not prey and are dropped.
# tegu_salvator_summary.csv is a summary of these two tables and adds no records.
# Owns: sources with extracted_from starting "data/structured/tegu_diet_".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "08_tegu"; OWNS <- "data/structured/tegu_diet_"
ind <- read.csv(file.path(ROOT, "data", "structured", "tegu_diet_individuals.csv"), colClasses = "character", na.strings = character(0))
dc  <- read.csv(file.path(ROOT, "data", "structured", "tegu_diet_dietcategories.csv"), colClasses = "character", na.strings = character(0))
dc$k <- seq_len(nrow(dc)) + 1L
SID <- "Harman2025_tegu"
src <- mk("sources", source_id = SID, citation = "Harman MEA, Yackel Adams AA, Falk BG, Romagosa CM (2025) Gastrointestinal contents and morphometrics of Argentine black and white tegus (Salvator merianae) in Florida. U.S. Geological Survey data release, ScienceBase",
  doi = "10.5066/P1CJ29AG", file_name = "tegu_diet_individuals.csv; tegu_diet_dietcategories.csv", data_type = "dataset",
  extracted_from = "data/structured/tegu_diet_individuals.csv + data/structured/tegu_diet_dietcategories.csv",
  notes = "authors and DOI from code/x23_teguDiet.R; title paraphrased (not checked against the ScienceBase record)")
MAP <- c(Amphibian = "Amphibia", Bird = "Aves", Bird.Egg = "Aves", Fish = "Actinopterygii", Insectivore = "Eulipotyphla", Mesomammal = "Mammalia", Mustelid = "Mustelidae",
  Rabbit = "Leporidae", Reptile.Egg = "Reptilia", Rodent = "Rodentia", Squamate = "Squamata", Turtle = "Testudines",
  Arachnida = "Arachnida", Blattodea = "Blattodea", Chilopoda = "Chilopoda", Coleoptera = "Coleoptera", Decapoda = "Decapoda", Dermaptera = "Dermaptera", Diplopoda = "Diplopoda",
  Diptera = "Diptera", Ephemeroptera = "Ephemeroptera", Gastropod = "Gastropoda", Hemiptera = "Hemiptera", Hymenoptera.Formicidae = "Formicidae", Hymenoptera.Other = "Hymenoptera",
  Lepidoptera = "Lepidoptera", Neuroptera = "Neuroptera", Odonata = "Odonata", Orthoptera = "Orthoptera", Phasmatodea = "Phasmatodea", Zygentoma = "Zygentoma")
VERT <- c("Amphibia", "Aves", "Actinopterygii", "Eulipotyphla", "Mammalia", "Mustelidae", "Leporidae", "Reptilia", "Rodentia", "Squamata", "Testudines")
NONPREY <- c("Fruit", "Man.made", "Misc.Veg", "Non.Fruit.Seeds")
f <- dc[dc$Found == "1", ]
drop <- f$DietCategory %in% NONPREY
dropped <- data.frame(source_file = "data/structured/tegu_diet_dietcategories.csv", source_row = paste0("line ", f$k[drop]),
  reason = "non-animal or man-made item (fruit, vegetation, seeds, man-made material); not a predator-prey link", detail = paste0("tegu ", f$ID[drop], ": ", f$DietCategory[drop]))
unm <- !f$DietCategory %in% c(names(MAP), NONPREY); if (any(unm)) stop("unmapped diet categories: ", paste(unique(f$DietCategory[unm]), collapse = ", "))
f <- f[!drop, ]
i <- match(f$ID, ind$ID); w <- num(ind$weight[i]); svl <- num(ind$svl[i])
spec <- paste0("tegu ", f$ID, " (", ind$source[i], ", ", ind$sex[i], ")")
pid <- paste0(SID, "_", make_id(f$ID), "_", make_id(f$DietCategory))
pairs <- mk("pairs", pair_id = pid, source_id = SID, pred_taxon_id = "Salvator_merianae", prey_taxon_id = MAP[f$DietCategory], pred_specimen = spec,
  observation_type = "stomach_content", pair_resolution = tier_of(ifelse(is.finite(w), "measured", ""), "", pred_none_from_animal = TRUE), individual_pair = "TRUE",
  pred_mass_g = fnum(w), pred_mass_method = ifelse(is.finite(w), "measured", ""), pred_mass_basis = ifelse(is.finite(w), "field 'weight' (g) of this tegu", ""),
  prey_stage = ifelse(grepl("Egg", f$DietCategory), "egg", ""), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("tegu_diet_dietcategories.csv line %d: ID %s, DietCategory %s, Found 1; tegu_diet_individuals.csv ID %s", f$k, f$ID, f$DietCategory, f$ID),
  notes = paste0("GI-tract category presence only (no count, no prey size); category '", f$DietCategory, "'",
                 ifelse(f$DietCategory == "Mesomammal", "; 'Mesomammal' is a size category, taxon set to Mammalia", ""),
                 ifelse(f$DietCategory == "Insectivore", "; 'Insectivore' taken as Eulipotyphla (shrews and moles)", ""),
                 ifelse(is.finite(w), "", "; weight missing in the source")))
g <- !duplicated(f$ID)
meas <- mk("measurements", measurement_id = paste0(SID, "_", make_id(f$ID[g]), "_SVL"), source_id = SID, specimen = spec[g], taxon_id = "Salvator_merianae", role = "predator",
  element = "body", dimension = "SVL", value = fnum(svl[g]), unit = "cm", value_type = "measured", locator = paste0("tegu_diet_individuals.csv ID ", f$ID[g], ", column svl"))
meas <- meas[!blank(meas$value), ]
up <- unique(MAP[f$DietCategory])
taxa <- rbind(taxon_row("Salvator_merianae", "Salvator merianae", "species", cls = "Squamata", fam = "Teiidae", clade = "Lacertilia", vert = TRUE),
  taxon_row(up, up, guess_rank(up), vert = up %in% VERT))
log_dropped(OWNER, dropped)
log_input(OWNER, nrow(dc), "rows of tegu_diet_dietcategories.csv (275 tegus x 35 categories)", sprintf("%d with Found = 1", sum(dc$Found == "1")))
commit(OWNER, OWNS, src, pairs, meas, taxa, pair_tag = "^Harman2025_tegu_")
