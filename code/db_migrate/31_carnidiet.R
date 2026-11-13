###################################################################
# 31_carnidiet.R -- CarniDIET 1.0 (Middleton et al. 2021, Glob Ecol Biogeogr 30:1175; Dryad 10.5061/dryad.2v6wwpzmr, CC0)
# via the GitHub CSV JM chose on 2026-11-11 (data/structured/raw_downloads/carnidiet/SOURCE.md). Phase 4b, Part 3.
# A CarniDIET row is a population-level diet composition: one predator species, one prey taxon, a percentage, at one site and
# period, from one publication. It is not an individual pair (individual_pair FALSE), and has no counts, so n_items is empty
# and the percentage, its basis, the sample sizes and the site go into notes.
# Admitted: prey that is an animal identified to at least class (foodType Mammal, Bird, Reptile, Amphibian, Herpetofauna, Fish,
# Invertebrate, Vertebrate, Reptile/Fish, Animal); eggs, plants, fungi, anthropogenic items, "Other" and kingdom- or phylum-level
# prey are dropped (logged), as are rows from stable-isotope or tissue studies (no prey item was recorded) and rows whose prey
# is given only as a percentage of another source ("Data compilation" is kept as `unknown` observation type).
# Predator mass: PanTHERIA species mean (species_average_database), genus mean if the species is not found. Prey masses are
# left to 90_prey_mass_fill.R. Tier D (E where a mass method forces it). Observation type from samplingProtocol (table in code).
# Duplicates: a block of CarniDIET rows (one publication, one predator species, one evidence kind) is dropped when db/ already
# holds, from another source, a publication with the same first-author surname and year, the same predator species and the same
# evidence kind (gut = stomach_content; other = everything else). Every dropped block is listed in the log with both citations.
# Owns: sources whose extracted_from starts with the CSV path; pairs "*_cd<recordID>".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "31_carnidiet"; CSV <- "data/structured/raw_downloads/carnidiet/CarniDIET_1.0_github-0301492.csv"; OWNS <- CSV
d <- read.csv(file.path(ROOT, CSV), colClasses = "character", na.strings = c("NA", ""), check.names = FALSE)
for (cl in names(d)) { v <- d[[cl]]; v[is.na(v)] <- ""; bad <- !validUTF8(v); if (any(bad)) v[bad] <- iconv(v[bad], "latin1", "UTF-8"); d[[cl]] <- v }   # a few fields hold latin-1 bytes
d$k <- as.integer(d$recordID); n_in <- nrow(d)
drop <- data.frame(source_file = character(), source_row = character(), reason = character(), detail = character())
keep <- rep(TRUE, n_in)
mark <- function(i, reason) { i <- i & keep; if (any(i)) drop <<- rbind(drop, data.frame(source_file = CSV, source_row = paste0("recordID ", d$recordID[i]), reason = reason,
  detail = sprintf("%s x %s (%s) [%s %s]", d$scientificNameCarni[i], d$scientificNamePrey[i], d$taxonRankPrey[i], d$sourceAuthor[i], d$sourceYear[i]))); keep[i] <<- FALSE }
mark(d$methodQuantification == "Stable isotope analysis" | d$samplingProtocol %in% c("Blood", "Bone collagen", "Tissue samples"), "stable-isotope or tissue study: no prey item recorded")
mark(d$foodType == "Egg", "prey is eggs")
mark(d$foodType %in% c("Plant", "Fungi", "Anthropogenic", "Other", ""), "prey is not an animal item (plant, fungus, anthropogenic, other) or foodType missing")
mark(d$taxonRankPrey %in% c("Kingdom", "Phylum", "Subphylum", "Other"), "prey identified only to kingdom or phylum")
## ---- observation type ----
OB <- c(Kills = "prey_remains", `Feeding observations` = "direct_observation", Intestine = "stomach_content", Stomach = "stomach_content", `Prey items in stomach` = "stomach_content",
  Scat = "scat", `Prey items in scats` = "scat", Latrines = "scat", `Prey items in dens` = "prey_remains", `Scat/Stomach` = "stomach_content", `Scats/Kills` = "scat",
  `Scats/Kills/Stomach` = "stomach_content", `Prey items in scat/stomach` = "stomach_content", `Kills/Food items` = "unknown", `Prey items` = "unknown",
  `Prey items in multiple sources` = "unknown", `Data compilation` = "unknown")
otype <- unname(OB[d$samplingProtocol]); ok <- keep; if (anyNA(otype[ok])) stop("unmapped samplingProtocol: ", paste(unique(d$samplingProtocol[ok & is.na(otype)]), collapse = ", "))
otype[is.na(otype)] <- "unknown"
## ---- duplicates against db/ ----
norm_s <- function(x) gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
P0 <- read_db("pairs"); S0 <- read_db("sources"); P0 <- P0[P0$is_fossil == "FALSE" & !grepl("_CD_cd[0-9]+$", P0$pair_id), ]
S0$yr <- sub("^[^0-9]*((1[89]|20)[0-9]{2}).*$", "\\1", S0$source_id); S0$yr[!grepl("^(1[89]|20)[0-9]{2}$", S0$yr)] <- ""
S0$sur <- norm_s(sub("^([^0-9_]*)[0-9].*$", "\\1", S0$source_id))
kind_of <- function(ot) ifelse(ot %in% c("stomach_content", "regurgitate"), "gut", "other")
P0$key <- paste(S0$sur[match(P0$source_id, S0$source_id)], S0$yr[match(P0$source_id, S0$source_id)], gsub("_", " ", sub("^([A-Za-z]+_[A-Za-z-]+).*$", "\\1", P0$pred_taxon_id)), kind_of(P0$observation_type))
dsur <- norm_s(sub(",.*$", "", d$sourceAuthor)); dsp <- gsub("_", " ", d$scientificNameCarni)
d$key <- paste(dsur, d$sourceYear, dsp, kind_of(otype))
hit <- keep & d$key %in% P0$key
if (any(hit)) {
  blk <- unique(d[hit, c("key", "sourceAuthor", "sourceYear", "sourceJournal", "scientificNameCarni")])
  db_src <- vapply(d$key[hit], function(k) paste(unique(P0$source_id[P0$key == k]), collapse = "+"), "")
  drop <- rbind(drop, data.frame(source_file = CSV, source_row = paste0("recordID ", d$recordID[hit]), reason = "duplicate record: same first author, year, predator species and evidence kind already in db/",
    detail = sprintf("%s x %s [%s %s, %s]; kept copy: %s", d$scientificNameCarni[hit], d$scientificNamePrey[hit], d$sourceAuthor[hit], d$sourceYear[hit], d$sourceJournal[hit], db_src)))
  keep[hit] <- FALSE
}
sel <- which(keep); d <- d[sel, , drop = FALSE]; otype <- otype[sel]
## ---- ids ----
au1 <- make_id(gsub("[ '.-]", "", tools::toTitleCase(sub(",.*$", "", d$sourceAuthor)))); au1[au1 == ""] <- "Anon"
base <- paste0(au1, ifelse(d$sourceYear == "", "nd", d$sourceYear)); pubkey <- paste(base, d$sourceTitle)
ub <- unique(pubkey); ubase <- sub(" .*$", "", ub); sfx <- ave(ubase, ubase, FUN = function(v) if (length(v) == 1) "" else letters[seq_along(v)])
sidmap <- setNames(paste0(ubase, sfx, "_CD"), ub); sid <- unname(sidmap[pubkey])
if (any(grepl("[0-9]{2}$", sub("_CD$", "", sidmap)))) NULL
## ---- taxa and masses ----
pred_name <- gsub("_", " ", d$scientificNameCarni); pred_id <- taxon_id_of(pred_name)
pl <- mass_lookup(pred_name, rep("Mammalia", nrow(d))); pm <- ifelse(is.finite(pl$v), "species_average_database", "")
rank <- tolower(d$taxonRankPrey); prey_sp <- rank == "species"
prey_name <- ifelse(prey_sp, gsub("_", " ", d$scientificNamePrey), ifelse(rank == "genus", d$genusPrey, ifelse(rank == "family", d$familyPrey, ifelse(rank == "order", d$orderPrey, ifelse(d$commonNamePrey != "", d$foodType, d$foodType)))))
CL <- c(Mammal = "Mammalia", Bird = "Aves", Reptile = "Reptilia", Herpetofauna = "Reptilia_or_Amphibia", Amphibian = "Amphibia", Fish = "Actinopterygii", Invertebrate = "Invertebrata", Vertebrate = "Vertebrata", `Reptile/Fish` = "Reptilia_or_Actinopterygii", Animal = "Animalia")
pcls <- unname(CL[d$foodType]); prey_name <- ifelse(rank == "class", gsub("_", " ", pcls), prey_name)
bad <- prey_name == "" | is.na(prey_name); prey_name[bad] <- gsub("_", " ", pcls[bad])
prey_id <- taxon_id_of(prey_name)
vert <- ifelse(d$foodType %in% c("Mammal", "Bird", "Reptile", "Amphibian", "Herpetofauna", "Fish", "Vertebrate", "Reptile/Fish"), TRUE, ifelse(d$foodType == "Invertebrate", FALSE, NA))
taxa <- rbind(taxon_row(pred_id, pred_name, "species", cls = "Mammalia", ord = d$orderCarni, fam = d$familyCarni, clade = "Mammalia", vert = TRUE),
  taxon_row(prey_id, prey_name, ifelse(rank %in% c("species", "genus", "family", "order", "class"), rank, guess_rank(prey_name)), cls = ifelse(d$foodType %in% c("Mammal", "Bird", "Reptile", "Amphibian", "Fish"), pcls, ""),
            ord = ifelse(rank %in% c("species", "genus", "family", "order"), d$orderPrey, ""), fam = ifelse(rank %in% c("species", "genus", "family"), d$familyPrey, ""), vert = vert))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
## ---- pairs ----
tier <- tier_of(pm, rep("", nrow(d)))
note <- paste0("population-level diet record, not an individual pair; ", d$percentage, "% (", d$methodQuantification, ")",
  ifelse(d$sampleSizeScatStomachTissue != "", paste0("; n samples ", d$sampleSizeScatStomachTissue), ""), ifelse(d$sampleSizeKillsPreyItems != "", paste0("; n prey items/kills ", d$sampleSizeKillsPreyItems), ""),
  ifelse(d$sexCarni != "Both", paste0("; predator sex ", d$sexCarni), ""), ifelse(d$lifeStageCarni != "Unspecified", paste0("; predator stage ", d$lifeStageCarni), ""),
  ifelse(d$season != "", paste0("; season ", d$season), ""), "; ", ifelse(d$startYear != "", paste0(d$startYear, ifelse(d$endYear != "" & d$endYear != d$startYear, paste0("-", d$endYear), "")), "years not given"),
  "; ", trimws(paste(d$country, d$protectedAreaHigher)), ifelse(d$domesticOrAgricultural == "1", "; domestic or agricultural prey", ""),
  ifelse(d$samplingProtocol %in% c("Scat/Stomach", "Scats/Kills", "Scats/Kills/Stomach", "Prey items in scat/stomach"), paste0("; evidence kinds pooled (", d$samplingProtocol, ")"), ""),
  ifelse(d$samplingProtocol == "Kills", "; kill records coded prey_remains (consumption not seen)", ""),
  ifelse(rank == "species" & d$scientificNamePrey != "", "", ifelse(rank %in% c("genus", "family", "order"), paste0("; prey identified to ", rank), "")),
  ifelse(d$foodType %in% c("Herpetofauna", "Reptile/Fish", "Animal", "Vertebrate") & rank == "class", paste0("; prey group '", d$foodType, "' is coarser than a Linnean class"), ""))
pairs <- mk("pairs", pair_id = paste0(sid, "_cd", d$recordID), source_id = sid, pred_taxon_id = pred_id, prey_taxon_id = prey_id, observation_type = otype,
  pair_resolution = tier, individual_pair = "FALSE", pred_mass_g = fnum(pl$v), pred_mass_method = pm, pred_mass_basis = ifelse(pm == "", "", pl$b),
  pred_stage = ifelse(d$lifeStageCarni %in% c("Adult", "Juvenile", "Subadult"), tolower(d$lifeStageCarni), ""), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("CarniDIET 1.0 (GitHub CSV) recordID %s: %s; %s (%s); data basis: %s", d$recordID, paste(d$sourceAuthor, d$sourceYear), substr(d$sourceTitle, 1, 80), d$sourceJournal, ifelse(d$dataBasisFromSource == "", "not given", d$dataBasisFromSource)),
  notes = note)
pairs$notes <- gsub("; ; ", "; ", gsub("; $", "", pairs$notes))
srcid <- unname(sidmap)
srcs <- unique(data.frame(sid = sid, pub = pubkey, a = d$sourceAuthor, y = d$sourceYear, t = d$sourceTitle, j = d$sourceJournal, doi = ""))
src <- rbind(
  mk("sources", source_id = "CarniDIET_1.0", citation = "Middleton OS, Svensson H, Scharlemann JPW, Faurby S, Sandom CJ (2021) CarniDIET 1.0: a database of terrestrial carnivorous mammal diets. Global Ecology and Biogeography 30(6):1175-1182 (data: Dryad 10.5061/dryad.2v6wwpzmr, CC0)",
     doi = "10.1111/geb.13296", file_name = "CarniDIET_1.0_github-0301492.csv; CarniDIET_1.0_Metadata.xlsx; Middleton_etal_2021_GEB_CarniDIET.pdf", data_type = "compilation_database",
     extracted_from = paste0(CSV, " (all studies)"), notes = "Phase 4b. GitHub CSV commit 0301492, chosen by JM on 2026-11-11 (see raw_downloads/carnidiet/SOURCE.md)."),
  mk("sources", source_id = srcs$sid, citation = sprintf("%s (%s) %s. %s", srcs$a, srcs$y, srcs$t, srcs$j), data_type = "journal_article",
     extracted_from = paste0(CSV, " (study-level fields)"), compiled_in = "CarniDIET_1.0"))
log_dropped(OWNER, drop); log_input(OWNER, n_in, "rows of CarniDIET 1.0")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "_CD_cd[0-9]+$")
