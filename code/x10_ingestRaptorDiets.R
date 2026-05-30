###################################################################
# x10_ingestRaptorDiets.R
# Harmonise the RaptorDiets database drop (new_RaptorDiets.csv +
# new_RaptorDiets_Metadata.csv) into a tidy long staging table.
#
# 58,533 prey records across ~3,538 datasets, 191 raptor species. The
# metadata flags the OBSERVATION METHOD per dataset (Pellets / Prey remains /
# Direct obs / DNA / Stomach-Gullet / Photos). Pellets dominate (owl-heavy) --
# a THIRD size-biased channel beyond scat and gut. We map method -> obstype,
# keep ALL records (for the broad PPMR map + the future observation-process
# model), and flag the gut/gullet subset that is directly fossil-comparable.
#
# Masses are NOT looked up here (raptor + prey masses need AVONET/PanTHERIA;
# network-blocked in-sandbox) -- left NA for the mass layer (P5).
#
# Output:
#   data/structured/raptor_diets_long.csv   harmonised long table (+ source_file, obstype)
#   data/structured/raptor_diets_gut.csv    the Stomach/Gullet subset (use now)
###################################################################

args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

find_src <- function(nm) {
	for (d in c("pdfs/extant_to_ingest/ai_found", "pdfs/_processed/extant_data"))
		if (file.exists(file.path(ROOT, d, nm))) return(file.path(ROOT, d, nm))
	stop("not found: ", nm)
}
DATA_FILE <- "new_RaptorDiets.csv"; META_FILE <- "new_RaptorDiets_Metadata.csv"
d  <- read.csv(find_src(DATA_FILE), check.names = FALSE, stringsAsFactors = FALSE)
md <- read.csv(find_src(META_FILE), check.names = FALSE, stringsAsFactors = FALSE)

## --- method -> obstype, per dataset -----------------------------
flag <- function(col) col %in% names(md) & nzchar(trimws(ifelse(is.na(md[[col]]), "", as.character(md[[col]]))))
methcols <- c(Stomach = "Stomach/Gullet contents", Pellet = "Pellets",
              PreyRemains = "Prey remains", Follow = "Direct observations",
              DNA = "DNA", Photos = "Photos/ Videos")
M <- sapply(methcols, function(c) {
	v <- md[[c]]; !is.na(v) & nzchar(trimws(as.character(v)))
})                                            # logical matrix: datasets x methods
colnames(M) <- names(methcols)
# primary obstype by priority (gut most fossil-comparable); also keep full list
priority <- c("Stomach","Pellet","PreyRemains","Follow","DNA","Photos")
prim <- apply(M, 1, function(r) { hit <- priority[priority %in% colnames(M)[r]]; if (length(hit)) hit[1] else "unknown" })
methods_all <- apply(M, 1, function(r) paste(colnames(M)[r], collapse = "+"))
md$obstype_primary <- prim
md$methods_all <- ifelse(methods_all == "", "unknown", methods_all)

## --- join records to dataset metadata ---------------------------
mm <- match(d$DataSet, md$DataSet)
out <- data.frame(
	source_file = DATA_FILE,
	dataset = d$DataSet, study = d$Study,
	obstype = md$obstype_primary[mm], methods_all = md$methods_all[mm],
	pred_order = md$`Raptor order`[mm], pred_family = md$`Raptor family`[mm],
	pred_genus = md$`Raptor genus`[mm], pred_species = d$RaptorScientificName,
	pred_mass_g = NA_real_,                       # -> mass layer (AVONET)
	prey_class = d$PreyClass, prey_order = d$PreyOrder,
	prey_species = d$PreyScientificName, prey_id_level = d$IDLevel,
	prey_mass_g = NA_real_,                       # -> mass layer
	count = suppressWarnings(as.numeric(d$Count)),
	percent = suppressWarnings(as.numeric(d$`Percent (out of 100)`)),
	country = md$Country[mm], lat = md$`Latitude (decimal degree)`[mm],
	lon = md$`Longitude (decimal degree)`[mm], year = md$Year[mm],
	stringsAsFactors = FALSE)

dir.create(file.path(ROOT, "data", "structured"), showWarnings = FALSE)
write.csv(out, file.path(ROOT, "data", "structured", "raptor_diets_long.csv"), row.names = FALSE)
gut <- out[out$obstype == "Stomach", ]
write.csv(gut, file.path(ROOT, "data", "structured", "raptor_diets_gut.csv"), row.names = FALSE)

## --- report -----------------------------------------------------
cat(sprintf("Harmonised %d records -> data/structured/raptor_diets_long.csv\n", nrow(out)))
cat("primary obstype distribution (records):\n"); print(sort(table(out$obstype), decreasing = TRUE))
cat(sprintf("\nGUT/GULLET subset (use now): %d records, %d raptor spp, %d prey spp -> raptor_diets_gut.csv\n",
            nrow(gut), length(unique(gut$pred_species)), length(unique(gut$prey_species[gut$prey_id_level=="Species"]))))

## --- stamp manifest (both source files) -------------------------
MAN <- file.path(ROOT, "data", "intake_manifest.csv")
if (file.exists(MAN)) {
	man <- read.csv(MAN, stringsAsFactors = FALSE, colClasses = "character")
	for (nm in c(DATA_FILE, META_FILE)) {
		r <- which(man$file == nm); if (length(r) != 1) next
		man$status[r] <- "structured"; man$obstype_hint[r] <- "pellet+gut(mixed)"
		man$target_table[r] <- "data/structured/raptor_diets_long.csv"
		man$n_obs[r] <- if (nm == DATA_FILE) nrow(out) else nrow(md)
		man$date_processed[r] <- format(Sys.Date())
		man$notes[r] <- "RaptorDiets DB; mostly pellets; gut subset in raptor_diets_gut.csv; masses pending AVONET"
	}
	write.csv(man, MAN, row.names = FALSE, quote = TRUE)
	message("intake_manifest.csv: marked RaptorDiets files structured.")
}
