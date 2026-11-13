###################################################################
# 02_aviandietdb.R -- migrate data/structured/aviandiet_gut_ppmr.csv (x15: the AvianDietDB gut
# subset with AVONET predator masses and AVONET/PanTHERIA prey masses) into db/.
# One pairs row per input row. The original study for each row is recovered from the full
# AvianDietDB export (aviandietdb_dietdb.csv, field Source) by matching every field x14 kept.
# Masses are re-derived exactly as x15 did (species mean, else genus mean) so the basis can say which.
# Owns: sources with extracted_from starting "data/structured/aviandiet_gut_ppmr.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
suppressPackageStartupMessages(library(data.table))
OWNER <- "02_aviandietdb"; OWNS <- "data/structured/aviandiet_gut_ppmr.csv"

g <- fread(file.path(ROOT, "data", "structured", "aviandiet_gut_ppmr.csv"), colClasses = "character"); g[, k := .I]
db <- fread(file.path(ROOT, "data", "structured", "aviandietdb_dietdb.csv"), colClasses = "character", showProgress = FALSE)
K <- c("Scientific_Name", "Prey_Class", "Prey_Order", "Prey_Family", "Prey_Genus", "Prey_Scientific_Name", "Prey_Stage", "Fraction_Diet", "Diet_Type",
       "Location_Region", "Observation_Year_Begin", "Observation_Season", "Item_Sample_Size", "Bird_Sample_Size", "Study_Type")
G <- g[, .(Scientific_Name = bird_species, Prey_Class = prey_class, Prey_Order = prey_order, Prey_Family = prey_family, Prey_Genus = prey_genus,
           Prey_Scientific_Name = prey_scientific, Prey_Stage = prey_stage, Fraction_Diet = fraction_diet, Diet_Type = diet_type, Location_Region = location_region,
           Observation_Year_Begin = year, Observation_Season = season, Item_Sample_Size = item_n, Bird_Sample_Size = bird_n, Study_Type = study_type_raw, k)]
norm_f <- function(x) { x[is.na(x) | x == "NA"] <- ""; x }
for (cc in K) { G[[cc]] <- norm_f(G[[cc]]); db[[cc]] <- norm_f(db[[cc]]) }
G$Fraction_Diet <- as.character(num(G$Fraction_Diet)); db$Fraction_Diet <- as.character(num(db$Fraction_Diet))
m <- merge(G, unique(db[, c(K, "Source", "Analysis_Number", "Location_Specific", "Notes"), with = FALSE]), by = K, all.x = TRUE)
m <- m[order(k)][!duplicated(k)]
if (anyNA(m$Source)) stop("AvianDietDB rows without a matching Source: ", sum(is.na(m$Source)))
g <- g[order(k)]; g$Source <- m$Source; g$Analysis_Number <- m$Analysis_Number; g$Location_Specific <- m$Location_Specific

## ---------------- sources ----------------
cit <- unique(g$Source)
au <- make_id(gsub("[ '-]", "", sub(",.*$", "", cit))); yr <- sub(".*?\\b((18|19|20)[0-9]{2})\\b.*", "\\1", cit, perl = TRUE)
base <- paste0(au, yr, "_ADB"); sid <- ave(base, base, FUN = function(v) if (length(v) == 1) v else paste0(v, letters[seq_along(v)]))
src <- rbind(
  mk("sources", source_id = "AvianDietDB_2021", citation = "Hurlbert AH, Olsen AM, Sawyer MM, Winner PM (2021) The Avian Diet Database as a source of quantitative information on bird diets. Scientific Data 8:260",
     doi = "10.1038/s41597-021-01049-9", file_name = "aviandietdb_dietdb.csv; new_The_Avian_Diet_Database_as_a_source_of_quantitativ.pdf", data_type = "compilation_database",
     extracted_from = "data/structured/aviandiet_gut_ppmr.csv (built by code/x14 and x15 from data/structured/aviandietdb_dietdb.csv)",
     notes = "export of the aviandietdb R package; x14 kept Study_Type stomach/esophagus/crop/emetic; x15 added AVONET and PanTHERIA masses"),
  mk("sources", source_id = sid, citation = cit, data_type = "journal_article",
     extracted_from = "data/structured/aviandiet_gut_ppmr.csv (Source field recovered from data/structured/aviandietdb_dietdb.csv)",
     compiled_in = "AvianDietDB_2021", notes = "citation as given in AvianDietDB field Source; PDF not checked"))
sid_of <- setNames(sid, cit)
## publications also found in another upstream table cite the merged source written by 00_merged_sources.R
DS <- MERGED(); mc <- strsplit(DS$adb_citation, " \\|\\| ")
merged_of <- setNames(rep(DS$merged_source_id, lengths(mc)), unlist(mc))
src <- src[!src$source_id %in% sid_of[names(merged_of)], ]
sid_of[intersect(names(merged_of), names(sid_of))] <- merged_of[intersect(names(merged_of), names(sid_of))]

## ---------------- masses: redo x15's lookup to record species vs genus ----------------
normn <- function(x) tolower(gsub("[ _]+", " ", trimws(x)))
av <- fread(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), showProgress = FALSE)
av <- av[is.finite(Mass) & Mass > 0]; av[, sp := normn(Species1)]; av[, gen := sub(" .*$", "", sp)]
avo_sp <- av[, .(m = mean(Mass), ord = Order1[1], fam = Family1[1]), by = sp]; avo_gen <- av[, .(m = mean(Mass)), by = gen]
pan <- read.delim(file.path(ROOT, "data", "pantheria"), header = FALSE, skip = 1, quote = "", colClasses = "character")
pmm <- num(pan[[5]]); pmm[pmm == -999] <- NA
pan_sp <- tapply(pmm, normn(paste(pan[[3]], pan[[4]])), function(z) mean(z, na.rm = TRUE)); pan_sp <- pan_sp[is.finite(pan_sp)]
pan_gen <- tapply(pmm, normn(pan[[3]]), function(z) mean(z, na.rm = TRUE)); pan_gen <- pan_gen[is.finite(pan_gen)]
how <- function(cl, sp, gen) {
  if (cl == "aves") { if (sp %in% avo_sp$sp) return("AVONET1_BirdLife species mean (Mass)"); if (gen %in% avo_gen$gen) return("AVONET1_BirdLife genus mean (Mass) for the prey genus") }
  if (cl == "mammalia") { if (sp %in% names(pan_sp)) return("PanTHERIA species mean (data/pantheria column 5, read by position)"); if (gen %in% names(pan_gen)) return("PanTHERIA genus mean (data/pantheria column 5) for the prey genus") }
  "not reproduced" }
bsp <- normn(g$bird_species)
pred_how <- ifelse(bsp %in% avo_sp$sp, "AVONET1_BirdLife species mean (Mass)", "AVONET1_BirdLife genus mean (Mass) for the predator genus")
prey_how <- mapply(how, normn(g$prey_class), normn(g$prey_scientific), normn(g$prey_genus))

## ---------------- pairs ----------------
st <- tolower(g$study_type_raw)
otype <- ifelse(grepl("emetic", st), "regurgitate", "stomach_content")
onote <- ifelse(grepl("emetic", st), "AvianDietDB Study_Type 'emetic': prey brought up after an emetic",
         ifelse(grepl("pellet", st), "AvianDietDB Study_Type 'stomach contents; pellet contents': stomach and pellet samples pooled by the source", "AvianDietDB Study_Type 'stomach contents'"))
prey_id <- make_id(ifelse(!blank(g$prey_scientific), g$prey_scientific, ifelse(!blank(g$prey_genus), g$prey_genus, ifelse(!blank(g$prey_family), g$prey_family, g$prey_order))))
pred_id <- make_id(g$bird_species)
fr <- num(g$fraction_diet); itn <- num(g$item_n); bn <- num(g$bird_n)
nit <- ifelse(g$diet_type == "Items" & is.finite(fr * itn) & round(fr * itn) >= 1, round(fr * itn), NA)
indiv <- bn %in% 1
spec <- ifelse(indiv, sprintf("single %s, %s, %s %s (AvianDietDB Analysis_Number %s)", g$bird_species, sid_of[g$Source], g$location_region, g$year, g$Analysis_Number), "")
stage <- ifelse(blank(g$prey_stage), "", g$prey_stage)
pairs <- mk("pairs", pair_id = paste0(sid_of[g$Source], "_r", g$k), source_id = sid_of[g$Source], pred_taxon_id = pred_id, prey_taxon_id = prey_id,
  pred_specimen = spec, observation_type = otype, pair_resolution = tier_of("species_average_database", "species_average_database"),
  individual_pair = ifelse(indiv, "TRUE", "FALSE"),
  pred_mass_g = fnum(num(g$pred_mass_g)), pred_mass_method = "species_average_database", pred_mass_basis = pred_how,
  prey_mass_g = fnum(num(g$prey_mass_g)), prey_mass_method = "species_average_database",
  prey_mass_basis = paste0(prey_how, ifelse(stage != "", sprintf("; adult mean, not scaled for prey_stage '%s'", stage), "")),
  prey_stage = stage, n_items = ifelse(is.na(nit), "", as.character(nit)), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("AvianDietDB (Hurlbert et al. 2021): %s, %s, %s, %s, prey %s; Fraction_Diet %s (%s), Item_Sample_Size %s, Bird_Sample_Size %s; aviandiet_gut_ppmr.csv row %d",
    g$bird_species, g$location_region, g$year, g$season, ifelse(blank(g$prey_scientific), g$prey_genus, g$prey_scientific), g$fraction_diet, g$diet_type,
    ifelse(blank(g$item_n), "NA", g$item_n), ifelse(blank(g$bird_n), "NA", g$bird_n), g$k),
  notes = paste0(onote, ifelse(g$diet_type != "Items", sprintf("; Diet_Type %s, so no item count", g$diet_type), ""),
                 "; x20 used this row once regardless of item count"))

## ---------------- taxa ----------------
ao <- avo_sp[match(bsp, avo_sp$sp)]
tx_pred <- taxon_row(pred_id, g$bird_species, "species", cls = "Aves", ord = ifelse(is.na(ao$ord), "", ao$ord), fam = g$bird_family, clade = "Aves", vert = TRUE)
tx_prey <- taxon_row(prey_id, ifelse(!blank(g$prey_scientific), g$prey_scientific, ifelse(!blank(g$prey_genus), g$prey_genus, g$prey_family)),
                     cls = g$prey_class, ord = g$prey_order, fam = g$prey_family, vert = TRUE)
taxa <- rbind(tx_pred, tx_prey); taxa <- taxa[!duplicated(taxa$taxon_id), ]

log_dropped(OWNER, data.frame(source_file = character(0), source_row = character(0), reason = character(0), detail = character(0)))
log_input(OWNER, nrow(g), "rows of aviandiet_gut_ppmr.csv")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "_r[0-9]+$")
