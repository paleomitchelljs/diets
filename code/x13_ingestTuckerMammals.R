###################################################################
# x13_ingestTuckerMammals.R
# Harmonise the Tucker, Ord & Rogers 2016 (J Evol Biol) supplement
# (TuckerDatabaseJEB.xlsx) -> a tidy mammalian predator-prey mass table.
#
# Source: 114 carnivorous mammal species (terrestrial + marine), species-level
# MEAN predator & prey mass compiled from the literature. Columns are log10(kg).
#
# IMPORTANT framing:
#  * Units: the file is log10(kg). The project's longform PPMR is natural-log
#    (ln) of the ratio. ppmr_ln = (preyMass_log10 - predMass_log10) * ln(10).
#  * obstype = "compiled_literature_mean": these are SPECIES MEANS from mixed
#    methods, NOT per-item gut/scat samples. Use for the broad clade-level PPMR
#    map; do NOT pool naively with per-item gut data without accounting for the
#    aggregation/obstype difference.
#
# Output: data/structured/tucker2016_mammal_ppmr.csv
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(readxl)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

SF <- "TuckerDatabaseJEB.xlsx"
src <- NULL
for (d in c("pdfs/extant_to_ingest/ai_found", "pdfs/_processed/extant_data"))
  if (file.exists(file.path(ROOT, d, SF))) src <- file.path(ROOT, d, SF)
stopifnot(!is.null(src))
x <- suppressMessages(as.data.frame(read_excel(src, sheet = "Data")))

pred_log10 <- suppressWarnings(as.numeric(x[["Predator Mass (log10)"]]))
prey_log10 <- suppressWarnings(as.numeric(x[["Prey Mass (log10)"]]))
out <- data.frame(
  source_file = SF, dataset = "Tucker_et_al_2016_JEB",
  species = gsub("[ _]+", "_", trimws(x[["Species"]])),
  clade   = "Mammalia",
  habitat = x[["Habitat"]],
  obs_type = "compiled_literature_mean",
  pred_mass_g = round(10^pred_log10 * 1000, 1),
  prey_mass_g = round(10^prey_log10 * 1000, 3),
  ppmr_ln = round((prey_log10 - pred_log10) * log(10), 3),
  predmass_ln = round(pred_log10 * log(10) + log(1000), 3),    # ln(g), matches longform 'predmass'
  prey_sources = x[["Prey Mass Sources"]],
  pred_sources = x[["Predator Mass Sources"]],
  stringsAsFactors = FALSE)
out <- out[is.finite(out$ppmr_ln), ]            # drop rows lacking prey mass

dir.create(file.path(ROOT, "data", "structured"), showWarnings = FALSE)
OUT <- file.path(ROOT, "data", "structured", "tucker2016_mammal_ppmr.csv")
write.csv(out, OUT, row.names = FALSE)

cat(sprintf("Tucker mammals: %d species with PPMR (of 114) -> %s\n", nrow(out), sub(paste0(ROOT,"/"),"",OUT)))
cat("by habitat:\n"); print(table(out$habitat))
agg <- do.call(rbind, lapply(split(out, out$habitat), function(d)
  data.frame(habitat=d$habitat[1], n=nrow(d),
             median_ppmr_ln=round(median(d$ppmr_ln),2),
             range=sprintf("%.2f..%.2f", min(d$ppmr_ln), max(d$ppmr_ln)))))
cat("\nPPMR (ln) by habitat:\n"); print(agg, row.names=FALSE)
cat(sprintf("\nFor context: extant terrestrial-carnivore gut band ~ -2.5 (Mammalia); large terr carnivores eat relatively large prey (ppmr_ln near -0.8..-1.5).\n"))

## --- manifest: add the XLSX as a structured source -------------
MAN <- file.path(ROOT, "data", "intake_manifest.csv")
man <- read.csv(MAN, stringsAsFactors = FALSE, colClasses = "character")
r <- which(man$file == SF)
if (length(r) == 1) {
  man$status[r] <- "structured"; man$obstype_hint[r] <- "compiled_mean"
  man$target_table[r] <- "data/structured/tucker2016_mammal_ppmr.csv"
  man$n_obs[r] <- nrow(out); man$date_processed[r] <- format(Sys.Date())
  man$notes[r] <- "Tucker et al 2016 supplement: 114 carnivorous mammal spp (terr+marine), species-mean predator/prey mass (log10 kg -> ppmr_ln); compiled means, not gut-specific"
  write.csv(man, MAN, row.names = FALSE, quote = TRUE)
  message("manifest: TuckerDatabaseJEB.xlsx marked structured.")
} else message("NOTE: run x08_intake.R first to register the new XLSX, then re-run.")
