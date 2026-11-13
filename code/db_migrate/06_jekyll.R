###################################################################
# 06_jekyll.R -- migrate data/structured/gator_jekyll_stomach.csv (x09: 27 wild Alligator
# mississippiensis from Jekyll Island, Georgia, stomach contents by prey category) into db/.
# One pairs row per alligator x prey category with a count. Each row is one animal's stomach:
# individual_pair TRUE, specimen = tail-notch code.
# Predator mass: the weight in the source where the alligator was weighed (8 of 27); otherwise
# Chabreck & Joanen 1979 on the measured total length when TL >= 100 cm (the range the authors call
# reliable); otherwise empty. TL and SVL go to measurements.csv.
# Prey: the source's per-category masses are weights of stomach contents (remains), not body
# masses (Prompt 2), so they go to measurements.csv and prey_mass_g stays empty.
# Seeds are not prey and are dropped.
# Owns: sources with extracted_from starting "data/structured/gator_jekyll_stomach.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "06_jekyll"; OWNS <- "data/structured/gator_jekyll_stomach.csv"
j <- read.csv(file.path(ROOT, "data", "structured", "gator_jekyll_stomach.csv"), colClasses = "character", na.strings = character(0))
j$k <- seq_len(nrow(j)) + 1L
SID <- "JekyllIsland_Dryad2023"
src <- mk("sources", source_id = SID, citation = "Body size measurements and stomach contents of Alligator mississippiensis on Jekyll Island, Georgia. Dryad dataset, version 2023-09-20 (authors not recorded in the staged table)",
  doi = "10.5061/dryad.sxksn0387", file_name = "Body_size_measurements_and_stomach_contents_of_Alligator_mississippiensis_on_Jekyll_Island__Georgia.xlsx", data_type = "dataset",
  extracted_from = "data/structured/gator_jekyll_stomach.csv (code/x09 from the xlsx, sheet 'RAW JI GC 180')", notes = "DOI read from the download folder name in pdfs/_processed/extant_pdf/toadd_2026-11-11/duplicate_already_ingested/")
TAX <- c(Fishes = "Actinopterygii", Birds = "Aves", Mammals = "Mammalia", Reptiles = "Reptilia", Crustaceans = "Crustacea", Gastropods = "Gastropoda", "Insects/Arachnids" = "Insecta_or_Arachnida")
drop <- j$prey_category == "Seeds"
dropped <- data.frame(source_file = OWNS, source_row = paste0("line ", j$k[drop]), reason = "seeds are not prey (non-animal food item)",
                      detail = sprintf("alligator %s; count %s; mass %s g", j$specimen[drop], j$count[drop], j$prey_total_mass_g[drop]))
j <- j[!drop, ]
TL <- num(j$pred_total_length_cm); SVL <- num(j$pred_svl_cm); W <- num(j$pred_mass_g)
eqm <- eq_mass("ChabreckJoanen1979_Fig2", TL * 10)
pm <- ifelse(is.finite(W), W, ifelse(TL >= 100, eqm, NA))
pmeth <- ifelse(is.finite(W), "measured", ifelse(TL >= 100, "regressed_from_length", ""))
pmid <- ifelse(pmeth == "regressed_from_length", "ChabreckJoanen1979_Fig2", "")
pbasis <- ifelse(pmeth == "measured", sprintf("Weight (kg) %s in the source for this alligator", fnum(W / 1000)),
          ifelse(pmeth == "regressed_from_length", sprintf("total length %s cm of this alligator (source column 'Total Length (cm)')", fnum(TL)), ""))
spec <- paste0("Jekyll Island alligator, tail notch ", j$specimen)
pid <- paste0(SID, "_", make_id(j$specimen), "_", make_id(TAX[j$prey_category]))
cnt <- num(j$count); tot <- num(j$prey_total_mass_g)
pairs <- mk("pairs", pair_id = pid, source_id = SID, pred_taxon_id = "Alligator_mississippiensis", prey_taxon_id = TAX[j$prey_category], pred_specimen = spec,
  observation_type = "stomach_content", pair_resolution = tier_of(pmeth, "", pred_none_from_animal = TRUE), individual_pair = "TRUE",
  pred_mass_g = fnum(pm), pred_mass_method = pmeth, pred_mass_method_id = pmid, pred_mass_basis = pbasis,
  n_items = ifelse(is.finite(cnt) & cnt >= 1, as.character(floor(cnt)), ""), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("Jekyll Island xlsx, sheet 'RAW JI GC 180', tail notch %s, columns '1%s' (count %s) and '2%s' (mass %s g); gator_jekyll_stomach.csv line %d",
    j$specimen, j$prey_category, j$count, j$prey_category, ifelse(blank(j$prey_total_mass_g), "NA", j$prey_total_mass_g), j$k),
  notes = paste0("prey category '", j$prey_category, "'; prey mass in the source is the weight of stomach contents of this category (remains), kept in measurements.csv, not a body mass",
                 ifelse(TL < 100 & !is.finite(W), "; TL below 100 cm and not weighed: no predator mass (Chabreck & Joanen 1979 call their equation reliable above 1 m)", "")))
## measurements: remains weight per category (linked to the pair), TL and SVL per alligator (linked by specimen)
mr <- which(is.finite(tot))
meas <- mk("measurements", measurement_id = paste0(pid[mr], "_rem"), source_id = SID, pair_id = pid[mr], specimen = spec[mr], taxon_id = TAX[j$prey_category[mr]],
  role = "prey", element = "stomach contents", dimension = "wet mass, total of category", value = fnum(tot[mr]), unit = "g", value_type = "measured",
  locator = sprintf("column '2%s', tail notch %s", j$prey_category[mr], j$specimen[mr]), notes = sprintf("%s items; remains weight, not a body mass", j$count[mr]))
g <- !duplicated(j$specimen)
meas <- rbind(meas,
  mk("measurements", measurement_id = paste0(SID, "_", make_id(j$specimen[g]), "_TL"), source_id = SID, specimen = spec[g], taxon_id = "Alligator_mississippiensis", role = "predator",
     element = "body", dimension = "total length", value = fnum(TL[g]), unit = "cm", value_type = "measured", locator = paste0("column 'Total Length (cm)', tail notch ", j$specimen[g])),
  mk("measurements", measurement_id = paste0(SID, "_", make_id(j$specimen[g]), "_SVL"), source_id = SID, specimen = spec[g], taxon_id = "Alligator_mississippiensis", role = "predator",
     element = "body", dimension = "SVL", value = fnum(SVL[g]), unit = "cm", value_type = "measured", locator = paste0("column 'Snout Vent Length (cm)', tail notch ", j$specimen[g])))
meas <- meas[!blank(meas$value), ]
taxa <- rbind(taxon_row("Alligator_mississippiensis", "Alligator mississippiensis", "species", cls = "Crocodylia", fam = "Alligatoridae", clade = "Crocodylia", vert = TRUE),
  taxon_row(c("Actinopterygii", "Aves", "Mammalia", "Reptilia"), c("Actinopterygii", "Aves", "Mammalia", "Reptilia"), "class", cls = c("Actinopterygii", "Aves", "Mammalia", "Reptilia"), vert = TRUE),
  taxon_row(c("Crustacea", "Gastropoda", "Insecta_or_Arachnida"), c("Crustacea", "Gastropoda", "Insecta or Arachnida"), c("subphylum", "class", "unranked_alternatives"), vert = FALSE))
log_dropped(OWNER, dropped)
log_input(OWNER, nrow(j) + sum(drop), "rows of gator_jekyll_stomach.csv (alligator x prey category)", "27 alligators")
commit(OWNER, OWNS, src, pairs, meas, taxa, methods_rows(pmid), pair_tag = "^JekyllIsland_Dryad2023_")
