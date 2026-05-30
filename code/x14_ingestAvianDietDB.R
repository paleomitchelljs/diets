###################################################################
# x14_ingestAvianDietDB.R
# Harmonise the Avian Diet Database (Hurlbert et al. 2021) export
#   data/structured/aviandietdb_dietdb.csv   (from the `aviandietdb` R pkg)
# into a tidy long table, mapping Study_Type -> obstype and splitting out the
# GUT subset (the fossil-comparable bird records the project was missing).
#
# Data are diet COMPOSITION (Fraction_Diet as Items / Wt_or_Vol / Occurrence),
# NOT paired masses. Bird (predator) + prey masses are left NA -> fill from
# AVONET (birds) + PanTHERIA/FishBase/etc (prey) in the mass layer.
#
# Outputs:
#   data/structured/aviandiet_long.csv   all 73k records, harmonised + obstype
#   data/structured/aviandiet_gut.csv    the Stomach/crop/esophagus/emetic subset
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
SRC <- file.path(ROOT, "data", "structured", "aviandietdb_dietdb.csv")
d <- fread(SRC, showProgress = FALSE)

## Study_Type (often combined with ';') -> a single obstype, gut-prioritised.
## emetic/crop/esophagus/stomach = gut-equivalent (recently ingested, retrieved
## from the digestive tract); pellet/fecal/remains/observation are distinct modes.
map_obstype <- function(st) {
  s <- tolower(ifelse(is.na(st), "", st))
  fifelse(grepl("stomach|esophagus|crop|emetic", s), "Stomach",
  fifelse(grepl("pellet", s),                        "Pellet",
  fifelse(grepl("fecal|faecal", s),                  "Scat",
  fifelse(grepl("prey remains", s),                  "PreyRemains",
  fifelse(grepl("behavior|behaviour|observation", s),"Follow",
  fifelse(grepl("nest debris", s),                   "NestDebris",
  fifelse(grepl("dna", s),                           "DNA", "unknown")))))))
}
out <- data.table(
  source_file  = "aviandietdb (Hurlbert et al. 2021, R pkg)",
  bird_species = d$Scientific_Name, bird_common = d$Common_Name, bird_family = d$Family,
  obstype      = map_obstype(d$Study_Type), study_type_raw = d$Study_Type,
  prey_class = d$Prey_Class, prey_order = d$Prey_Order, prey_family = d$Prey_Family,
  prey_genus = d$Prey_Genus, prey_scientific = d$Prey_Scientific_Name, prey_stage = d$Prey_Stage,
  fraction_diet = d$Fraction_Diet, diet_type = d$Diet_Type,
  pred_mass_g = NA_real_, prey_mass_g = NA_real_,            # -> mass layer (AVONET etc.)
  location_region = d$Location_Region, year = d$Observation_Year_Begin,
  season = d$Observation_Season, item_n = d$Item_Sample_Size, bird_n = d$Bird_Sample_Size)

fwrite(out, file.path(ROOT, "data", "structured", "aviandiet_long.csv"))
gut <- out[obstype == "Stomach"]
fwrite(gut, file.path(ROOT, "data", "structured", "aviandiet_gut.csv"))

cat(sprintf("Avian Diet DB: %d records -> aviandiet_long.csv\n", nrow(out)))
cat("obstype distribution (records):\n"); print(sort(table(out$obstype), decreasing = TRUE))
cat(sprintf("\nGUT subset: %d records, %d bird species, %d prey taxa (to species) -> aviandiet_gut.csv\n",
            nrow(gut), uniqueN(gut$bird_species), uniqueN(gut$prey_scientific[gut$prey_scientific != ""])))
cat("  (vs the old extant bird GUT reference: 602 items / 7 species)\n")
cat("\nDiet_Type within gut subset:\n"); print(table(gut$diet_type))

## --- update manifest: Hurlbert 2021 PDF row now points to ingested data ---
MAN <- file.path(ROOT, "data", "intake_manifest.csv")
man <- read.csv(MAN, stringsAsFactors = FALSE, colClasses = "character")
r <- which(man$file == "new_The_Avian_Diet_Database_as_a_source_of_quantitativ.pdf")
if (length(r) == 1) {
  man$status[r] <- "structured"; man$obstype_hint[r] <- "stomach-dominant(mixed)"
  man$target_table[r] <- "data/structured/aviandiet_long.csv"; man$n_obs[r] <- nrow(out)
  man$date_processed[r] <- format(Sys.Date())
  man$notes[r] <- "Avian Diet Database INGESTED (aviandietdb R export): 73049 records; ~50k GUT-equivalent (stomach/crop/esophagus/emetic) across many bird spp; gut subset in aviandiet_gut.csv; masses pending AVONET"
  write.csv(man, MAN, row.names = FALSE, quote = TRUE)
  message("manifest: Avian Diet Database now 'structured' (ingested).")
}
