###################################################################
# x09_ingestGatorJekyll.R
# Harmonise the structured Alligator stomach-contents drop
#   pdfs/extant_to_ingest/ai_found/new_Body_size_..._Jekyll_Island__Georgia.xlsx
# into a tidy long staging table with provenance.
#
# Source: 27 wild A. mississippiensis, each with body measurements and
# stomach contents broken into prey categories: per-category COUNT (1<cat>)
# and total MASS in g (2<cat>). This is real CROCODILIAN GUT data with
# predator body size -- high value because the extant croc gut reference
# currently has only 3 species.
#
# Output: data/structured/gator_jekyll_stomach.csv  (one row per gator x prey
# category with count, total & mean-item prey mass, predator size, obstype=Stomach).
# Predator MASS is left NA: these gators were measured by length (Total Length
# cm), not weighed (Weight kg is blank). Fill via an A. mississippiensis
# length->mass regression in the mass layer (note in mass_gaps).
###################################################################

suppressWarnings(suppressPackageStartupMessages(library(readxl)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

SRC_FILE <- "new_Body_size_measurements_and_stomach_contents_of_Alligator_mississippiensis_on_Jekyll_Island__Georgia.xlsx"
src <- file.path(ROOT, "pdfs", "extant_to_ingest", "ai_found", SRC_FILE)
if (!file.exists(src)) src <- file.path(ROOT, "pdfs", "_processed", "extant_data", SRC_FILE)  # if already moved
x <- suppressMessages(as.data.frame(read_excel(src)))

cats <- c("Birds","Crustaceans","Fishes","Gastropods","Insects/Arachnids","Mammals","Reptiles","Seeds")
clade_of <- c(Birds = "Aves", Crustaceans = "Invertebrate", Fishes = "Actinopterygii",
              Gastropods = "Invertebrate", "Insects/Arachnids" = "Invertebrate",
              Mammals = "Mammalia", Reptiles = "Reptilia", Seeds = "Plant")

rows <- list()
for (i in seq_len(nrow(x))) {
	for (cat in cats) {
		cnt  <- suppressWarnings(as.numeric(x[[paste0("1", cat)]][i]))
		mass <- suppressWarnings(as.numeric(x[[paste0("2", cat)]][i]))
		if (is.na(cnt) || cnt <= 0) next
		rows[[length(rows) + 1]] <- data.frame(
			source_file   = SRC_FILE,
			study         = "Jekyll_Island_GA_2019",
			obs_type      = "Stomach",
			pred_class    = "Crocodylia",
			pred_genus    = "Alligator", pred_species = "mississippiensis",
			specimen      = as.character(x[["Tail Notch"]][i]),
			pred_total_length_cm = suppressWarnings(as.numeric(x[["Total Length (cm)"]][i])),
			pred_svl_cm   = suppressWarnings(as.numeric(x[["Snout Vent Length (cm)"]][i])),
			pred_sex      = as.character(x[["Sex"]][i]),
			pred_mass_g   = suppressWarnings(as.numeric(x[["Weight (kg)"]][i])) * 1000,  # mostly NA -> NA
			lat = suppressWarnings(as.numeric(x[["y"]][i])), lon = suppressWarnings(as.numeric(x[["x"]][i])),
			prey_category = cat, prey_clade = unname(clade_of[cat]),
			count         = cnt,
			prey_total_mass_g = ifelse(is.finite(mass), mass, NA_real_),
			prey_mean_item_mass_g = ifelse(is.finite(mass) && cnt > 0, mass / cnt, NA_real_),
			certainty = 1, stringsAsFactors = FALSE)
	}
}
out <- do.call(rbind, rows)

dir.create(file.path(ROOT, "data", "structured"), showWarnings = FALSE)
OUT <- file.path(ROOT, "data", "structured", "gator_jekyll_stomach.csv")
write.csv(out, OUT, row.names = FALSE)

## --- record provenance back into the intake manifest ------------
# (the reusable pattern: every structured-ingest script stamps its own
#  manifest row done, so status never has to be hand-edited.)
MAN <- file.path(ROOT, "data", "intake_manifest.csv")
if (file.exists(MAN)) {
	man <- read.csv(MAN, stringsAsFactors = FALSE, colClasses = "character")
	r <- which(man$file == SRC_FILE)
	if (length(r) == 1) {
		man$status[r]        <- "done"
		man$obstype_hint[r]  <- "gut"
		man$target_table[r]  <- "data/structured/gator_jekyll_stomach.csv"
		man$n_obs[r]         <- nrow(out)
		man$date_processed[r] <- format(Sys.Date())
		man$notes[r]         <- "croc GUT; 27 gators; predator mass needs TL->mass regression"
		write.csv(man, MAN, row.names = FALSE, quote = TRUE)
		message("intake_manifest.csv: marked '", SRC_FILE, "' done.")
	}
}

cat(sprintf("Tidied %d gators -> %d gator x prey-category rows -> %s\n",
            nrow(x), nrow(out), sub(paste0(ROOT, "/"), "", OUT)))
cat(sprintf("Predator total length: %.0f-%.0f cm (n=%d weighed: %d)\n",
            min(out$pred_total_length_cm, na.rm = TRUE), max(out$pred_total_length_cm, na.rm = TRUE),
            length(unique(out$specimen)), sum(!is.na(unique(out$pred_mass_g)))))
cat("prey categories by record count:\n"); print(sort(table(out$prey_category), decreasing = TRUE))
cat("\nNOTE: predator mass NA (length-only) -> needs A. mississippiensis TL->mass regression before PPMR.\n")
