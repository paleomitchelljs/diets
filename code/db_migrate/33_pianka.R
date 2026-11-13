###################################################################
# 33_pianka.R -- Pianka lizard data, individual animals with stomach contents (Phase 4b, Part 1; JM asked on 2026-11-12 to
# incorporate it). Files supplied by JM in pdfs/extant_to_ingest/toadd/pianka/: Australia/OZ_all.csv (17,336 animals) and
# NorthAmerica/NA_all.csv (3,770 animals). Kalahari/KL_all.csv (4,902 animals) is NOT ingested: its prey are numeric codes 1-68
# and no key to them is in the folder, so no vertebrate prey can be picked out (see EXPANSION_REPORT.md).
# One row of the source is one animal, with prey counts (N) and volumes (V, ml) per prey category.
# Admitted: animals with vertebrate prey (OZ `VertebratesN` > 0; NA `Lizards` > 0), predator identified to species. Invertebrate
# categories are not ingested: they would add about 50,000 rows with order-level prey and no use for the vertebrate-prey fits,
# and need a category-to-taxon table (not written).
# Prey taxon: OZ "Vertebrata_indet" (the category is "Vertebrates": lizards, eggs, birds, mammals as the Notes show); NA "Lacertilia".
# The volume of the category is a remains measurement, not a body mass: it goes to measurements.csv (cm3, wet volume of stomach
# contents) and prey_mass_g stays empty. Predator mass: the weight recorded for the animal (`measured`), else its SVL through
# the registered Meiri 2010 (lizards) or Feldman & Meiri 2013 (snakes) equation inside its range, else the Amniote species mean.
# Observation type stomach_content (animals were collected and their stomachs examined; Pianka 1986). individual_pair TRUE.
# Species decoding: evidence/expansion/pianka_species.csv (OZ names as in OZ_all.csv; NA four-letter codes decoded by me, since the
# folder holds no key: flagged in notes). Predators of ambiguous identity (e.g. 'Ctenotus_grandis/hanloni', 'Phrynosoma',
# 'Uma', unidentified 'Snake_mouse_kill') are dropped and logged.
# Owns: sources with extracted_from starting "pdfs/extant_to_ingest/toadd/pianka"; pairs "Pianka*".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "33_pianka"; OWNS <- "pdfs/extant_to_ingest/toadd/pianka";
SP <- read.csv(file.path(EVID, "expansion", "pianka_species.csv"), colClasses = "character", na.strings = character(0))
rd <- function(f) { d <- read.csv(file.path(ROOT, OWNS, f), colClasses = "character", check.names = FALSE, fileEncoding = "latin1", na.strings = character(0)); d }
oz <- rd("Australia/OZ_all.csv"); na <- rd("NorthAmerica/NA_all.csv")
drop <- data.frame(source_file = character(), source_row = character(), reason = character(), detail = character())
## unified frame
o <- data.frame(region = "OZ", file = "Australia/OZ_all.csv", row = seq_len(nrow(oz)), id = oz$Number, raw = trimws(oz$Genus_species), wt = oz$Wt, svl = oz$SVL, n = oz$VertebratesN, v = oz$VertebratesV,
                site = oz$Area, yr = oz$Year, notes = oz$Notes, prey = "Vertebrata_indet", stringsAsFactors = FALSE)
nn <- data.frame(region = "NA", file = "NorthAmerica/NA_all.csv", row = seq_len(nrow(na)), id = na[[1]], raw = trimws(na$Species), wt = na$Wt, svl = na$SVL, n = na$Lizards, v = na$LizardsV,
                 site = na$Site, yr = "", notes = "", prey = "Lacertilia", stringsAsFactors = FALSE)
d <- rbind(o, nn); n_in <- nrow(d)
nv <- suppressWarnings(as.numeric(d$n)); d <- d[is.finite(nv) & nv > 0, ]; d$nn <- nv[is.finite(nv) & nv > 0]
mp <- SP[match(paste(d$region, d$raw), paste(SP$region, SP$raw)), ]
bad <- is.na(mp$accepted)
if (any(bad)) drop <- rbind(drop, data.frame(source_file = d$file[bad], source_row = paste0("data row ", d$row[bad]), reason = "predator not identified to a species, ambiguous, or not in the species table",
  detail = sprintf("%s (Number %s)", d$raw[bad], d$id[bad])))
d <- d[!bad, ]; mp <- mp[!bad, ]
## predator mass
wt <- suppressWarnings(as.numeric(d$wt)); svl <- suppressWarnings(as.numeric(d$svl)); REG <- METHOD_REG(); rg <- REG[match(mp$mass_method_id, REG$method_id), ]
if (anyNA(rg$method_id)) stop("method not registered: ", paste(unique(mp$mass_method_id[is.na(rg$method_id)]), collapse = ", "))
svl_ok <- is.finite(svl) & svl >= num(rg$input_min) & svl <= num(rg$input_max)
pm <- rep(NA_real_, nrow(d)); pmeth <- rep("", nrow(d)); pmid <- rep("", nrow(d)); pbas <- rep("", nrow(d))
a <- is.finite(wt) & wt > 0; pm[a] <- wt[a]; pmeth[a] <- "measured"; pbas[a] <- "Wt recorded for this animal (g) in the Pianka file"
b <- !a & svl_ok; pm[b] <- eq_mass(mp$mass_method_id[b], svl[b]); pmeth[b] <- "regressed_from_length"; pmid[b] <- mp$mass_method_id[b]
pbas[b] <- sprintf("snout-vent length %s mm (Pianka file); %s; equation estimate, no interval", fnum(svl[b]), mp$mass_method_id[b])
r <- !a & !b; cls <- ifelse(mp$analysis_clade == "Serpentes" | mp$analysis_clade == "Lacertilia", "Reptilia", "Reptilia")
pl <- mass_lookup(mp$accepted, ifelse(r, cls, "")); f <- r & is.finite(pl$v); pm[f] <- pl$v[f]; pmeth[f] <- "species_average_database"; pbas[f] <- pl$b[f]
tier <- tier_of(pmeth, rep("", nrow(d)))
sid <- ifelse(d$region == "OZ", "Pianka_OZ", "Pianka_NA"); pid <- taxon_id_of(mp$accepted)
vol <- suppressWarnings(as.numeric(d$v)); egg <- grepl("egg", tolower(d$notes))
prey_id <- d$prey
pairs <- mk("pairs", pair_id = paste0(sid, "_", d$id, "_", d$row), source_id = sid, pred_taxon_id = pid, prey_taxon_id = prey_id,
  pred_specimen = paste0("Pianka ", d$region, " animal number ", d$id), observation_type = "stomach_content", pair_resolution = tier, individual_pair = "TRUE",
  pred_mass_g = fnum(pm), pred_mass_method = pmeth, pred_mass_method_id = pmid, pred_mass_basis = pbas,
  n_items = as.character(as.integer(d$nn)), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("%s row %d: Number %s, %s, site %s%s; %s %s", d$file, d$row + 1L, d$id, d$raw, ifelse(d$site == "", "?", trimws(d$site)), ifelse(d$yr == "", "", paste0(", ", d$yr)),
                             ifelse(d$region == "OZ", "VertebratesN", "Lizards"), d$n),
  notes = paste0("individual lizard or snake collected and dissected; prey category volume ", ifelse(is.finite(vol), paste0(vol, " ml (remains volume, in measurements.csv)"), "not given"),
                 ifelse(d$notes != "", paste0("; field note: '", d$notes, "'"), ""), ifelse(egg, "; note mentions eggs, which may be part of the category", ""),
                 ifelse(mp$note != "", paste0("; ", mp$note), ""), ifelse(d$raw != mp$accepted & d$raw != gsub(" ", "_", mp$accepted), paste0("; name in file: ", d$raw), "")))
meas <- NULL
mi <- is.finite(vol) & vol > 0
if (any(mi)) meas <- mk("measurements", measurement_id = paste0(pairs$pair_id[mi], "_vol"), source_id = sid[mi], pair_id = pairs$pair_id[mi], specimen = pairs$pred_specimen[mi], taxon_id = prey_id[mi], role = "prey",
  element = "stomach contents", dimension = "volume", value = fnum(vol[mi]), unit = "cm3", value_type = "measured", locator = pairs$quote_or_locator[mi],
  notes = "volume of the vertebrate prey category in this stomach (ml = cm3); remains volume, not a body mass")
taxa <- rbind(taxon_row(pid, mp$accepted, "species", cls = "Reptilia", ord = "Squamata", fam = mp$family, clade = mp$analysis_clade, vert = TRUE),
              taxon_row(unique(prey_id), c(Vertebrata_indet = "Vertebrata indet", Lacertilia = "Lacertilia")[unique(prey_id)], c(Vertebrata_indet = "unranked", Lacertilia = "order")[unique(prey_id)], cls = c(Vertebrata_indet = "", Lacertilia = "Reptilia")[unique(prey_id)], vert = TRUE))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
src <- rbind(
  mk("sources", source_id = "Pianka_OZ", citation = "Pianka ER, Australian desert lizard field data 1966-2008: individual lizards, snakes and mammals with stomach contents (OZ_all.csv). Data files supplied by JM from the Pianka archive; public-release status not confirmed (assessment plan, extant data leads). See Pianka ER (1986) Ecology and Natural History of Desert Lizards, Princeton University Press",
     file_name = "OZ_all.csv; OZ_Species.csv; OZ_key.csv", data_type = "dataset", extracted_from = paste0(OWNS, "/Australia/OZ_all.csv"), notes = "Phase 4b. Vertebrate-prey records only; invertebrate categories not ingested."),
  mk("sources", source_id = "Pianka_NA", citation = "Pianka ER, North American desert lizard field data: individual lizards with stomach contents (NA_all.csv). Data files supplied by JM from the Pianka archive; public-release status not confirmed. See Pianka ER (1986) Ecology and Natural History of Desert Lizards, Princeton University Press",
     file_name = "NA_all.csv; NA_key.csv", data_type = "dataset", extracted_from = paste0(OWNS, "/NorthAmerica/NA_all.csv"), notes = "Phase 4b. Vertebrate-prey records only (the Lizards category); species decoded from four-letter codes."))
src <- src[src$source_id %in% sid, ]
log_dropped(OWNER, drop); log_input(OWNER, n_in, "animals in OZ_all.csv and NA_all.csv")
commit(OWNER, OWNS, src, pairs, meas, taxa, methods = methods_rows(pmid), pair_tag = "^Pianka_(OZ|NA)_")
