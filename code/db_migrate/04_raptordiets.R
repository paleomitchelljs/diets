###################################################################
# 04_raptordiets.R -- migrate data/structured/raptor_diets_long.csv (x10 harmonisation of the
# OS-Prey raptor diet database, new_RaptorDiets.csv + metadata) into db/.
# One pairs row per input row (dataset x prey taxon).
# Masses (JM, 2026-11-12): x10 left both empty "for the mass layer"; they are filled here the way
# x15 filled AvianDietDB: predator from AVONET1_BirdLife (species mean, else genus mean); prey from
# AVONET (Aves) or PanTHERIA (Mammalia; column 5, read by position), species mean, else genus mean,
# only for prey identified to species, subspecies or genus. Other prey classes stay without a mass.
# Observation type: x10's per-dataset primary method (priority Stomach > Pellet > PreyRemains >
# Follow > DNA > Photos); pooled methods are kept in notes; datasets with no method -> `unknown`.
# Merged publications (evidence/duplicate_studies.csv) cite the merged source; their duplicate
# records (same publication, predator and evidence kind as a kept raw_data or AvianDietDB copy) are
# dropped as listed in evidence/duplicate_records.csv.
# Owns: sources with extracted_from starting "data/structured/raptor_diets_long.csv"; pairs ending "_osp<line>".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "04_raptordiets"; OWNS <- "data/structured/raptor_diets_long.csv"
r <- read.csv(file.path(ROOT, "data", "structured", "raptor_diets_long.csv"), colClasses = "character", na.strings = c("", "NA"))
for (cl in names(r)) r[[cl]][is.na(r[[cl]])] <- ""
r$k <- seq_len(nrow(r)) + 1L                              # file line
n_in <- nrow(r)

## ---------------- sources, merges, duplicates ----------------
st <- unique(r$study)
au <- make_id(gsub("[ '.-]", "", tools::toTitleCase(sub("^([^ ,]+).*$", "\\1", st))))
yr <- ifelse(grepl("(18|19|20)[0-9]{2}", st), sub(".*((18|19|20)[0-9]{2}).*", "\\1", st), "nd")
base <- paste0(au, yr, "_OSP"); sid <- ave(base, base, FUN = function(v) if (length(v) == 1) v else paste0(v, letters[seq_along(v)]))
sid <- ave(sid, sid, FUN = function(v) if (length(v) == 1) v else paste0(v, seq_along(v)))   # >26 collisions
sid_of <- setNames(sid, st)
pred_id <- taxon_id_of(r$pred_species)
kind <- ifelse(r$obstype == "Stomach", "gut", "other")
DS <- MERGED(); DS <- DS[nzchar(DS$osprey_study), ]
row_sid <- unname(sid_of[r$study]); merged <- rep(FALSE, nrow(r))
for (i in seq_len(nrow(DS))) {
  hit <- r$study %in% strsplit(DS$osprey_study[i], ";")[[1]]
  if (nzchar(DS$osprey_predators[i])) hit <- hit & pred_id %in% strsplit(DS$osprey_predators[i], ";")[[1]]
  row_sid[hit] <- DS$merged_source_id[i]; merged[hit] <- TRUE
}
DR <- DUPRECS(); DR <- DR[DR$upstream == "os_prey" & DR$action == "drop", ]
dup <- merged & paste(r$study, pred_id, kind) %in% paste(DR$key, DR$pred, DR$kind)
kept_in <- DR$kept_upstream[match(paste(r$study, pred_id, kind), paste(DR$key, DR$pred, DR$kind))]
dropped <- data.frame(source_file = OWNS, source_row = paste0("line ", r$k[dup]),
  reason = "duplicate record: the same publication, predator species and evidence kind is kept from another upstream table (evidence/duplicate_records.csv)",
  detail = sprintf("OS-Prey study '%s' (merged source %s), dataset %s: %s x %s; kept copy: %s", r$study[dup], row_sid[dup], r$dataset[dup], r$pred_species[dup], r$prey_species[dup], kept_in[dup]))
r <- r[!dup, ]; row_sid <- row_sid[!dup]; pred_id <- pred_id[!dup]; merged <- merged[!dup]
own_st <- unique(r$study[!merged])
src <- rbind(
  mk("sources", source_id = "OSPrey_2026", citation = "Uiterwaal SF, La Sorte FA, Coblentz KE, DeLong JP (2026) The OS-Prey (Omnibus Study of Prey) database: a compilation of diet records for birds of prey. bioRxiv preprint (posted 31 Mar 2026)",
     doi = "10.64898/2026.03.28.714998", file_name = "new_RaptorDiets.csv; new_RaptorDiets_Metadata.csv; new_2026.03.28.714998v1.full.pdf", data_type = "compilation_database",
     extracted_from = "data/structured/raptor_diets_long.csv (built by code/x10 from new_RaptorDiets.csv and its metadata)", notes = "preprint, not peer reviewed; the data files are labelled RaptorDiets"),
  mk("sources", source_id = sid_of[own_st], citation = paste0(own_st, " (study string as given in OS-Prey; full citation not in the staged table)"), data_type = "journal_article",
     extracted_from = "data/structured/raptor_diets_long.csv (field study)", compiled_in = "OSPrey_2026"))

## ---------------- masses: AVONET / PanTHERIA, as x15 ----------------
normn <- function(x) tolower(gsub("[ _]+", " ", trimws(x)))
av <- read.csv(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), check.names = FALSE, stringsAsFactors = FALSE)
avm <- num(av$Mass); ok <- is.finite(avm) & avm > 0; avs <- normn(av$Species1[ok]); avm <- avm[ok]
av_sp <- tapply(avm, avs, mean); av_gen <- tapply(avm, sub(" .*$", "", avs), mean)
pan <- read.delim(file.path(ROOT, "data", "pantheria"), header = FALSE, skip = 1, quote = "", colClasses = "character")
pmm <- num(pan[[5]]); pmm[pmm == -999] <- NA
pan_sp <- tapply(pmm, normn(paste(pan[[3]], pan[[4]])), function(z) mean(z, na.rm = TRUE)); pan_sp <- pan_sp[is.finite(pan_sp)]
pan_gen <- tapply(pmm, normn(pan[[3]]), function(z) mean(z, na.rm = TRUE)); pan_gen <- pan_gen[is.finite(pan_gen)]
look <- function(name, sp_tab, gen_tab, db, binomial_ok) {
  nm <- normn(name); two <- sub("^(\\S+ \\S+).*$", "\\1", nm); gen <- sub(" .*$", "", nm)
  v <- ifelse(binomial_ok & two %in% names(sp_tab), sp_tab[two], ifelse(gen %in% names(gen_tab), gen_tab[gen], NA))
  b <- ifelse(binomial_ok & two %in% names(sp_tab), paste0(db, " species mean for ", tools::toTitleCase(two)),
       ifelse(gen %in% names(gen_tab), paste0(db, " genus mean for ", tools::toTitleCase(gen)), ""))
  list(v = unname(v), b = b) }
pl <- look(r$pred_species, av_sp, av_gen, "AVONET1_BirdLife (Mass)", grepl(" ", trimws(r$pred_species)))
lvl <- r$prey_id_level; idok <- lvl %in% c("Species", "Subspecies", "Genus"); binom <- lvl %in% c("Species", "Subspecies")
pc <- tolower(trimws(r$prey_class))
qa <- look(r$prey_species, av_sp, av_gen, "AVONET1_BirdLife (Mass)", binom)
qp <- look(r$prey_species, pan_sp, pan_gen, "PanTHERIA (data/pantheria column 5, read by position)", binom)
qm <- ifelse(idok & pc == "aves", qa$v, ifelse(idok & pc == "mammalia", qp$v, NA))
qb <- ifelse(idok & pc == "aves", qa$b, ifelse(idok & pc == "mammalia", qp$b, ""))
qb <- ifelse(is.finite(qm) & lvl == "Genus", paste0(qb, " (prey identified to genus)"), qb)
qb <- ifelse(is.finite(qm) & binom & grepl("genus mean", qb), paste0(qb, " (species not found under this name)"), qb)
pm <- pl$v; pb <- ifelse(is.finite(pm) & !grepl(" species mean ", pl$b), paste0(pl$b, " (species not found under this name)"), pl$b)
pmeth <- ifelse(is.finite(pm), "species_average_database", ""); qmeth <- ifelse(is.finite(qm), "species_average_database", "")

## ---------------- observation type ----------------
OB <- c(Stomach = "stomach_content", Pellet = "pellet", PreyRemains = "prey_remains", Follow = "direct_observation", DNA = "dna_detection", Photos = "direct_observation", unknown = "unknown")
otype <- unname(OB[r$obstype]); if (anyNA(otype)) stop("unmapped obstype: ", paste(unique(r$obstype[is.na(otype)]), collapse = ","))
pooled <- grepl("\\+", r$methods_all)

## ---------------- taxa ----------------
prey_name <- ifelse(blank(r$prey_species), ifelse(blank(r$prey_order), r$prey_class, r$prey_order), r$prey_species)
prey_id <- make_id(prey_name); prey_id[blank(prey_id)] <- "Animalia_indet"
pt <- prey_type(r$prey_class)
taxa <- rbind(
  taxon_row(pred_id, r$pred_species, "species", cls = "Aves", ord = r$pred_order, fam = r$pred_family, clade = "Aves", vert = TRUE),
  taxon_row(prey_id, prey_name, ifelse(lvl %in% c("Species", "Genus", "Family", "Order", "Class", "Subspecies"), tolower(lvl), guess_rank(prey_name)),
            cls = r$prey_class, ord = r$prey_order, vert = ifelse(is.na(pt), NA, pt == "vert")))
taxa <- taxa[!duplicated(taxa$taxon_id), ]

## ---------------- pairs ----------------
cnt <- num(r$count)
nit <- ifelse(is.finite(cnt) & cnt >= 1, floor(cnt), NA)
pairs <- mk("pairs", pair_id = paste0(row_sid, "_osp", r$k), source_id = row_sid, pred_taxon_id = pred_id, prey_taxon_id = prey_id,
  observation_type = otype, pair_resolution = tier_of(pmeth, qmeth), individual_pair = "FALSE",
  pred_mass_g = fnum(pm), pred_mass_method = pmeth, pred_mass_basis = ifelse(pmeth == "", "", pb),
  prey_mass_g = fnum(qm), prey_mass_method = qmeth, prey_mass_basis = ifelse(qmeth == "", "", paste0(qb, "; adult mean")),
  n_items = ifelse(is.na(nit), "", as.character(nit)), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("OS-Prey (new_RaptorDiets.csv) dataset %s, study '%s': %s x %s (ID level %s), count %s%s; %s %s; raptor_diets_long.csv line %d",
    r$dataset, r$study, r$pred_species, prey_name, lvl, ifelse(blank(r$count), "NA", r$count), ifelse(blank(r$percent), "", paste0(", percent ", r$percent)), r$country, r$year, r$k),
  notes = paste0("dataset methods: ", r$methods_all, ifelse(pooled, " (methods pooled within the dataset; coded by x10's priority Stomach > Pellet > PreyRemains > Follow > DNA > Photos)", ""),
                 ifelse(r$obstype == "Photos", "; photos or video of prey deliveries coded direct_observation", ""),
                 ifelse(r$obstype == "unknown", "; no method recorded in the OS-Prey metadata", ""),
                 ifelse(merged, sprintf("; OS-Prey study '%s', merged with the other copy of this publication", r$study), ""),
                 "; masses looked up in AVONET/PanTHERIA at migration (2026-11-12), as x15 did for AvianDietDB",
                 ifelse(is.finite(cnt) & cnt != floor(cnt), sprintf("; fractional count %s, n_items = floor", r$count), "")))
log_dropped(OWNER, dropped)
log_input(OWNER, n_in, "rows of raptor_diets_long.csv")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "_osp[0-9]+$")
