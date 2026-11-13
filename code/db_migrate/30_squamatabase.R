###################################################################
# 30_squamatabase.R -- SquamataBase v1.0.0 (Grundler 2020, Biodivers Data J 8:e49943), tetrapod-predator records
# (Phase 4b, Part 3). Input: data/structured/raw_downloads/squamatabase/squamatabase-sb_v1.0.0.zip, inst/diet.csv
# (10,940 records, 38 columns, CC0; Zenodo 10.5281/zenodo.3667777).
#
# What a row is: "a snake specimen eating or attempting to eat a prey specimen" (man/diet.Rd). predator_count > 1 means the
# row summarises several predators (individual_pair FALSE).
# Admission (breadth first): predator identified to species (or subspecies, collapsed to the species) and a tetrapod; prey
# identified to at least class; wild setting. Dropped and logged: genus-level predators, non-tetrapod predator (a scorpion),
# amphibian predators (no analysis clade exists), prey identified only to phylum, captive-setting events, and records whose
# publication is already in db/ (evidence/expansion/squamatabase_duplicates.csv, rule below).
# Masses: predator = recorded mass (count 1) `measured`; else snout-vent length through the registered Feldman & Meiri 2013
# family equation when the SVL lies in its range; else total length (Colubridae TL equation); else the species mean in the
# Amniote database (genus mean if the species is absent). Prey = the recorded prey_mass (prey_count 1) is `measured`: SquamataBase defines the field as "the mass
# (in grams) of the specimen"; the original sources were not checked, JM instructed on 2026-11-12 to assume the records are correct. Other prey masses are left to 90_prey_mass_fill.R.
# Observation type from event_basis (table below). No row is dropped for lacking a mass; rows with neither a predator mass
# nor a recorded prey mass stay (the link is not lost; 90_prey_mass_fill.R may fill the prey).
# Duplicate rule (same as MIGRATION_REPORT.md): a block of records is a duplicate when db/ already holds, from another source,
# the same publication (matched by year, journal and title in the hand-checked map) with the same predator species and the
# same kind of evidence (gut = stomach_content, regurgitate; other = everything else). The db copy is kept.
# Owns: sources whose extracted_from starts with the zip path; pairs "SqB*".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "30_squamatabase"
ZIP <- "data/structured/raw_downloads/squamatabase/squamatabase-sb_v1.0.0.zip"; OWNS <- ZIP
td <- tempfile("sb"); dir.create(td)
inner <- grep("inst/diet\\.csv$", unzip(file.path(ROOT, ZIP), list = TRUE)$Name, value = TRUE)
unzip(file.path(ROOT, ZIP), files = inner, exdir = td, junkpaths = TRUE)
d <- read.csv(file.path(td, "diet.csv"), colClasses = "character", na.strings = "", check.names = FALSE)
for (cl in names(d)) d[[cl]][is.na(d[[cl]])] <- ""
d$k <- seq_len(nrow(d))
n_in <- nrow(d)
drop <- data.frame(source_file = character(), source_row = character(), reason = character(), detail = character())
add_drop <- function(i, reason) { if (any(i)) drop <<- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("data row ", d$k[i]), reason = reason,
  detail = sprintf("%s x %s; %s", d$predator[i], d$prey[i], substr(d$reference[i], 1, 80)))) ; i }
keep <- rep(TRUE, n_in)
## --- predator ---
ptax <- strsplit(d$predator_taxon, ";", fixed = TRUE)
pcls <- vapply(ptax, function(v) if (length(v) >= 3) v[length(v) - 2] else "", ""); pord <- vapply(ptax, function(v) if (length(v) >= 4) v[length(v) - 3] else "", "")
pfam <- vapply(ptax, function(v) { x <- v[grepl("idae$", v)]; if (length(x)) x[1] else "" }, "")
is_snake <- grepl("Serpentes", d$predator_taxon, fixed = TRUE)
i <- d$predator_rank == "genus"; keep[add_drop(i & keep, "predator identified only to genus")] <- FALSE
i <- !is_snake & pcls %in% c("Arachnida", "Amphibia"); keep[add_drop(i & keep, "predator is not a tetrapod with an analysis clade (scorpion or amphibian)")] <- FALSE
i <- d$event_setting == "captive_setting"; keep[add_drop(i & keep, "captive setting")] <- FALSE
## --- prey ---
qtax <- strsplit(d$prey_taxon, ";", fixed = TRUE)
qcls <- vapply(qtax, function(v) if (length(v) >= 3) v[length(v) - 2] else "", ""); qord <- vapply(qtax, function(v) if (length(v) >= 4) v[length(v) - 3] else "", "")
qfam <- vapply(qtax, function(v) { x <- v[grepl("idae$", v)]; if (length(x)) x[1] else "" }, "")
qvert <- vapply(qtax, function(v) "Chordata" %in% v, TRUE)
i <- d$prey_rank == "phylum" | d$prey == ""; keep[add_drop(i & keep, "prey identified only to phylum or missing")] <- FALSE
## --- duplicates against db/ ---
DUPMAP <- read.csv(file.path(EVID, "expansion", "squamatabase_duplicates.csv"), colClasses = "character")
P0 <- read_db("pairs"); S0 <- read_db("sources")
sp_of <- function(x) sub("^(\\S+ \\S+).*$", "\\1", gsub("_", " ", x))
d$psp <- sp_of(d$predator)
kind_of <- function(ot) ifelse(ot %in% c("stomach_content", "regurgitate"), "gut", "other")
ot_map <- c(direct_observation = "direct_observation", observed_hunting = "direct_observation", dissected_gut_contents = "stomach_content",
  gut_contents_of_roadkill = "stomach_content", radiograph_of_gut_contents = "stomach_content", `radio-transmitter_of_prey_found_in_predator` = "stomach_content",
  regurgitated_from_specimen = "regurgitate", `regurgitated_from_specimen/dissected_gut_contents` = "stomach_content", observed_in_scat = "scat",
  `observed_in_scat/dissected_gut_contents` = "stomach_content", observed_in_nest_refuse = "prey_remains")
otype <- unname(ot_map[d$event_basis]); otype[d$event_basis == ""] <- "unknown"
if (anyNA(otype)) stop("unmapped event_basis: ", paste(unique(d$event_basis[is.na(otype)]), collapse = ", "))
dup <- rep(FALSE, n_in); dup_to <- rep("", n_in)
for (j in seq_len(nrow(DUPMAP))) {
  hit <- startsWith(d$reference, DUPMAP$sb_ref_prefix[j]); if (!any(hit)) stop("duplicate map entry matches no SquamataBase reference: ", DUPMAP$sb_ref_prefix[j])
  pj <- P0[P0$source_id == DUPMAP$db_source_id[j], ]; if (!nrow(pj)) stop("db source has no pairs: ", DUPMAP$db_source_id[j])
  have <- paste(sp_of(pj$pred_taxon_id), kind_of(pj$observation_type))
  m <- hit & paste(d$psp, kind_of(otype)) %in% have; dup <- dup | m; dup_to[m] <- DUPMAP$db_source_id[j]
}
keep2 <- keep & !dup
if (any(dup & keep)) drop <- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("data row ", d$k[dup & keep]),
  reason = "duplicate record: the same publication, predator species and evidence kind is already in db/ (evidence/expansion/squamatabase_duplicates.csv)",
  detail = sprintf("%s x %s; kept copy: %s", d$predator[dup & keep], d$prey[dup & keep], dup_to[dup & keep])))
d <- d[keep2, , drop = FALSE]; ptax <- NULL
sel <- which(keep2); pcls <- pcls[sel]; pfam <- pfam[sel]; qcls <- qcls[sel]; qord <- qord[sel]; qfam <- qfam[sel]; qvert <- qvert[sel]; otype <- otype[sel]; is_snake <- is_snake[sel]
## --- identifiers ---
yr <- sub(".*[ .](1[89][0-9]{2}|20[0-9]{2})[a-z]?\\.\\s+[A-Z].*$", "\\1", d$reference); yr[!grepl("^(1[89]|20)[0-9]{2}$", yr)] <- "nd"
short_hash <- function(x) vapply(x, function(z) { u <- utf8ToInt(z); h <- 0; for (a in u) h <- (h * 131 + a) %% 16777213; sprintf("%06x", as.integer(h)) }, "")
refs <- sort(unique(d$reference)); rh <- short_hash(refs)
if (anyDuplicated(rh)) stop("hash collision among references")
ryr <- yr[match(refs, d$reference)]
sid_of <- setNames(paste0("SqB", ryr, "_", rh), refs); sid <- unname(sid_of[d$reference])
## --- masses: predator ---
FMROW <- c(Colubridae = "Colubridae", Elapidae = "Elapidae", Viperidae = "Viperidae", Boidae = "Boidae", Pythonidae = "Pythonidae", Homalopsidae = "Homalopsidae", Lamprophiidae = "Lamprophiidae")
REG <- METHOD_REG()
pcount <- num(d$predator_count); one <- is.finite(pcount) & pcount == 1
pm_rec <- ifelse(one, num(d$predator_mass), NA); svl <- ifelse(one, num(d$predator_svl), NA); tl <- ifelse(one, num(d$predator_tl), NA)
fam_row <- ifelse(pfam %in% names(FMROW), FMROW[pfam], "All"); mid_svl <- paste0("FeldmanMeiri2013_T1_", fam_row, "_SVL")
rng <- REG[match(mid_svl, REG$method_id), ]; svl_ok <- is_snake & is.finite(svl) & svl >= num(rng$input_min) & svl <= num(rng$input_max)
tl_rng <- REG[REG$method_id == "FeldmanMeiri2013_T1_Colubridae_TL", ]
tl_ok <- is_snake & !svl_ok & pfam == "Colubridae" & is.finite(tl) & tl >= num(tl_rng$input_min) & tl <= num(tl_rng$input_max)
pmass <- rep(NA_real_, nrow(d)); pmeth <- rep("", nrow(d)); pmid <- rep("", nrow(d)); pbas <- rep("", nrow(d))
a <- is.finite(pm_rec) & pm_rec > 0
pmass[a] <- pm_rec[a]; pmeth[a] <- "measured"; pbas[a] <- "SquamataBase predator_mass (g) for this specimen (predator_count 1)"
b <- !a & svl_ok
pmass[b] <- eq_mass(mid_svl[b], svl[b]); pmeth[b] <- "regressed_from_length"; pmid[b] <- mid_svl[b]
pbas[b] <- sprintf("snout-vent length %s mm (SquamataBase predator_svl); Feldman & Meiri 2013 Table 1 %s row; equation estimate, no interval", fnum(svl[b]), fam_row[b])
cc <- !a & !b & tl_ok
pmass[cc] <- eq_mass("FeldmanMeiri2013_T1_Colubridae_TL", tl[cc]); pmeth[cc] <- "regressed_from_length"; pmid[cc] <- "FeldmanMeiri2013_T1_Colubridae_TL"
pbas[cc] <- sprintf("total length %s mm (SquamataBase predator_tl); Feldman & Meiri 2013 Table 1 Colubridae TL row; equation estimate, no interval", fnum(tl[cc]))
rest <- !a & !b & !cc
cls_pred <- ifelse(is_snake | pcls == "Reptilia", "Reptilia", ifelse(pcls == "Aves", "Aves", "Reptilia"))
pl <- mass_lookup(d$psp, ifelse(rest, cls_pred, ""))
f <- rest & is.finite(pl$v); pmass[f] <- pl$v[f]; pmeth[f] <- "species_average_database"; pbas[f] <- pl$b[f]
## --- prey mass: recorded value only; method unknown (see header) ---
qcount <- num(d$prey_count); qm <- ifelse(is.finite(qcount) & qcount == 1, num(d$prey_mass), NA)
ymass <- ifelse(is.finite(qm) & qm > 0, qm, NA); ymeth <- ifelse(is.finite(ymass), "measured", "")
tier <- tier_of(pmeth, ymeth, pred_none_from_animal = FALSE)
## --- taxa ---
pred_name <- d$psp; pred_id <- taxon_id_of(pred_name)
pred_clade <- ifelse(is_snake, "Serpentes", ifelse(pcls == "Aves", "Aves", "Lacertilia"))
prey_sp <- d$prey_rank %in% c("species", "infraspecies"); prey_name <- ifelse(prey_sp, sp_of(d$prey), d$prey); prey_id <- taxon_id_of(prey_name)
prey_rank <- ifelse(prey_sp, "species", d$prey_rank)
pcl_norm <- ifelse(pcls == "", cls_pred, pcls)
taxa <- rbind(taxon_row(pred_id, pred_name, "species", cls = pcl_norm, ord = ifelse(is_snake, "Squamata", ""), fam = pfam, clade = pred_clade, vert = TRUE),
              taxon_row(prey_id, prey_name, prey_rank, cls = qcls, ord = ifelse(d$prey_rank %in% c("class", "superfamily"), "", qord), fam = ifelse(d$prey_rank %in% c("species", "infraspecies", "genus", "family"), qfam, ""), vert = qvert))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
## --- pairs ---
OUT <- c(prey_eaten = "consumed", predation_interrupted_by_observer = "attempted_failed", predator_abandoned_attempt = "attempted_failed",
         prey_escaped = "attempted_failed", prey_refused = "attempted_failed", predator_died_in_attempt = "attempted_failed", outcome_not_observed = "unknown")
outc <- unname(OUT[d$event_outcome]); outc[d$event_outcome == ""] <- ifelse(otype[d$event_outcome == ""] == "unknown", "unknown", "consumed")
if (anyNA(outc)) stop("unmapped event_outcome")
hexv <- grepl("^[0-9a-f]{32}$", d$predator_voucher)
spec <- ifelse(one & d$predator_voucher != "", ifelse(hexv, paste0("SquamataBase voucher ", d$predator_voucher), d$predator_voucher), "")
indiv <- one & spec != ""
loc <- trimws(gsub(" +,", ",", paste(d$locality_misc, d$locality_adm2_name, d$locality_adm1_name, d$locality_adm0_name, sep = ", ")))
loc <- gsub("(, )+", ", ", gsub("^(, )+|(, )+$", "", gsub(", ,", ",", loc)))
nit <- ifelse(is.finite(qcount) & qcount >= 1 & qcount == floor(qcount), as.character(qcount), "")
pairs <- mk("pairs", pair_id = paste0(sid, "_r", d$k), source_id = sid, pred_taxon_id = pred_id, prey_taxon_id = prey_id, pred_specimen = spec,
  observation_type = otype, pair_resolution = tier, individual_pair = ifelse(indiv, "TRUE", "FALSE"),
  pred_mass_g = fnum(pmass), pred_mass_method = pmeth, pred_mass_method_id = pmid, pred_mass_basis = pbas,
  prey_mass_g = fnum(ymass), prey_mass_method = ymeth,
  pred_stage = ifelse(d$predator_age == "", "", d$predator_age), prey_stage = ifelse(d$prey_age == "", "", d$prey_age),
  n_items = nit, interaction_outcome = outc, is_fossil = "FALSE",
  quote_or_locator = sprintf("SquamataBase sb_v1.0.0 inst/diet.csv data row %d: predator_voucher %s; %s; %s; event_basis %s", d$k, ifelse(d$predator_voucher == "", "none", d$predator_voucher), ifelse(loc == "", "no locality", loc), ifelse(d$event_date == "", "no date", d$event_date), ifelse(d$event_basis == "", "not given", d$event_basis)),
  notes = paste0(ifelse(!one, sprintf("predator_count %s: row summarises several predators; ", ifelse(d$predator_count == "", "not given", d$predator_count)), ""),
                 ifelse(is.finite(ymass), "prey_mass is SquamataBase's value for this prey specimen, taken as measured on JM's instruction (2026-11-12: SquamataBase records assumed correct); the original source was not checked; ", ""),
                 ifelse(is.finite(qcount) & qcount > 1 & d$prey_mass != "", "prey_mass given for a row with prey_count above 1: not used; ", ""),
                 ifelse(d$predator_rank == "infraspecies", paste0("predator recorded as ", d$predator, ", collapsed to species; "), ""),
                 ifelse(prey_sp & d$prey_rank == "infraspecies", paste0("prey recorded as ", d$prey, ", collapsed to species; "), ""),
                 ifelse(d$prey_verbatim != "" & d$prey_verbatim != d$prey, paste0("prey as published: ", d$prey_verbatim, "; "), ""),
                 ifelse(d$predator_verbatim != "" & d$predator_verbatim != d$predator, paste0("predator as published: ", d$predator_verbatim, "; "), ""),
                 ifelse(d$event_basis == "observed_in_scat/dissected_gut_contents" | d$event_basis == "regurgitated_from_specimen/dissected_gut_contents", "two evidence kinds pooled in the source; coded stomach_content; ", ""),
                 ifelse(d$event_basis == "radio-transmitter_of_prey_found_in_predator", "radio-tagged prey found inside the predator; ", ""),
                 ifelse(d$event_basis == "", "event_basis not given in SquamataBase; ", ""),
                 ifelse(d$event_outcome == "predator_died_in_attempt", "predator died during the attempt; ", ""),
                 ifelse(d$event_remark != "", paste0("remark: ", d$event_remark, "; "), ""), "SquamataBase provides the reference as title, year and journal; authors are not given"))
pairs$notes <- sub("; $", "", pairs$notes)
## --- sources ---
src <- rbind(
  mk("sources", source_id = "SquamataBase_1.0.0", citation = "Grundler MC (2020) SquamataBase: a natural history database and R package for comparative biology of snake feeding habits. Biodiversity Data Journal 8:e49943 (data: sb_v1.0.0, Zenodo 10.5281/zenodo.3667777, CC0)",
     doi = "10.3897/BDJ.8.e49943", file_name = "squamatabase-sb_v1.0.0.zip; Grundler_2020_BDJ_SquamataBase.pdf", data_type = "compilation_database",
     extracted_from = paste0(ZIP, " (inst/diet.csv)"), notes = "Phase 4b. The post-1.0 GitHub master (commit 97cf7d7, about 100 more records) is on disk and not ingested."),
  mk("sources", source_id = unname(sid_of), citation = paste0(names(sid_of), " (reference string as given in SquamataBase: title, year, journal; authors not recorded)"),
     data_type = "journal_article", extracted_from = paste0(ZIP, " (inst/diet.csv, reference field)"), compiled_in = "SquamataBase_1.0.0"))
log_dropped(OWNER, drop); log_input(OWNER, n_in, "records of SquamataBase diet.csv (inst/diet.csv, v1.0.0)")
commit(OWNER, OWNS, src, pairs, NULL, taxa, methods = methods_rows(pmid), pair_tag = "^SqB[0-9a-z]+_[0-9a-f]{6}_r[0-9]+$")
