###################################################################
# 05_tucker2016.R -- migrate data/structured/tucker2016_mammal_ppmr.csv (x13: Tucker, Ord & Rogers
# 2016 supplement, species-mean predator mass and species-mean prey mass for 107 carnivorous
# mammals) into db/. One pairs row per predator species. The prey mass is the mean mass of the
# prey in that predator's diet, compiled from the literature (Prey Mass Sources), so the row
# summarises many prey and predator individuals: tier E, prey taxon Animalia_pooled. The source
# gives no observation method per row: observation_type `unknown`.
# Owns: sources with extracted_from starting "data/structured/tucker2016_mammal_ppmr.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "05_tucker2016"; OWNS <- "data/structured/tucker2016_mammal_ppmr.csv"
t <- read.csv(file.path(ROOT, "data", "structured", "tucker2016_mammal_ppmr.csv"), colClasses = "character")
t$k <- seq_len(nrow(t)) + 1L
src <- mk("sources", source_id = "Tucker2016", citation = "Tucker MA, Ord TJ, Rogers TL (2016) Revisiting the cost of carnivory in mammals. Journal of Evolutionary Biology 29:2181-2190 (supplement TuckerDatabaseJEB.xlsx)",
  doi = "10.1111/jeb.12936", file_name = "TuckerDatabaseJEB.xlsx; new_J of Evolutionary Biology - 2016 - Tucker - Revisiting the cost of carnivory in mammals.pdf", data_type = "supplement",
  extracted_from = "data/structured/tucker2016_mammal_ppmr.csv (code/x13 from TuckerDatabaseJEB.xlsx, sheet Data; log10 kg converted to g)",
  notes = "same species data as Tucker & Rogers 2014 PLoS ONE (data/INTEGRATED_SOURCES.md); per-row original sources are kept in pair notes")
sp <- gsub("_", " ", t$species)
qm <- num(t$prey_mass_g); zero <- is.finite(qm) & qm <= 0
qm[zero] <- num(t$pred_mass_g[zero]) * exp(num(t$ppmr_ln[zero]))   # x13 rounded prey mass to 3 decimals; recover it from its own ln ratio
pairs <- mk("pairs", pair_id = paste0("Tucker2016_", make_id(t$species)), source_id = "Tucker2016", pred_taxon_id = make_id(t$species), prey_taxon_id = "Animalia_pooled",
  observation_type = "unknown", pair_resolution = tier_of("species_average_literature", "species_average_literature", aggregate = TRUE), individual_pair = "FALSE",
  pred_mass_g = fnum(num(t$pred_mass_g)), pred_mass_method = "species_average_literature", pred_mass_basis = paste0("species mean compiled by Tucker et al. 2016 from: ", t$pred_sources),
  prey_mass_g = fnum(qm), prey_mass_method = "species_average_literature",
  prey_mass_basis = paste0("mean mass of the prey in this predator's diet, compiled by Tucker et al. 2016 from: ", t$prey_sources, "; an aggregate over prey taxa, not one prey species",
    ifelse(zero, sprintf("; staged prey_mass_g is 0 (x13 rounds to 3 decimals), recomputed as pred_mass_g x exp(ppmr_ln = %s)", t$ppmr_ln), "")),
  interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("TuckerDatabaseJEB.xlsx, sheet Data: %s (%s); tucker2016_mammal_ppmr.csv line %d", sp, t$habitat, t$k),
  notes = paste0("habitat ", t$habitat, "; species-level summary (x13: 'SPECIES MEANS from mixed methods, NOT per-item gut/scat samples'); the source does not give an observation method per species"))
taxa <- rbind(taxon_row(make_id(t$species), t$species, "species", cls = "Mammalia", clade = "Mammalia", vert = TRUE),
              taxon_row("Animalia_pooled", "Animalia pooled", "unranked_pool", notes = "pooled diet of many prey taxa (mean prey mass reported over the whole diet)"))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
log_dropped(OWNER, data.frame(source_file = character(0), source_row = character(0), reason = character(0), detail = character(0)))
log_input(OWNER, nrow(t), "rows of tucker2016_mammal_ppmr.csv", "x13 already dropped 7 of 114 species that lack a prey mass")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "^Tucker2016_")
