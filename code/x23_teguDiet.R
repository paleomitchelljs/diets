###################################################################
# x23_teguDiet.R
# Tegu (Salvator merianae) GI-contents + morphometrics DB
#   Harman, Yackel Adams, Falk & Romagosa 2025, USGS ScienceBase, DOI 10.5066/P1CJ29AG
#   data/structured/tegu_diet_individuals.csv (275 tegus; weight g, svl, sex, site)
#   data/structured/tegu_diet_dietcategories.csv (ID x 35 DietCategory, Found 0/1)
#
# PER JM: incorporate the PREDATOR-MASS side + diet BREADTH only. The diet is
# category PRESENCE/ABSENCE (no prey species/counts/masses), so it CANNOT yield
# per-item PPMR points -- we do NOT fabricate them. Salvator is a large teiid =>
# strengthens the thin Lacertilia predator-mass reference; tegus are extreme
# generalists (relevant to the foraging-mode/breadth axis).
# Output: data/structured/tegu_salvator_summary.csv (mass + breadth) + console.
###################################################################
args <- commandArgs(trailingOnly=FALSE); fa <- grep("^--file=", args, value=TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
ind  <- read.csv(file.path(ROOT,"data","structured","tegu_diet_individuals.csv"), stringsAsFactors=FALSE)
diet <- read.csv(file.path(ROOT,"data","structured","tegu_diet_dietcategories.csv"), stringsAsFactors=FALSE)
ind$weight <- suppressWarnings(as.numeric(ind$weight)); diet$Found <- suppressWarnings(as.numeric(diet$Found))

## --- (a) Salvator merianae PREDATOR-MASS reference (the gold) ---
w <- ind$weight[is.finite(ind$weight)]
mass_overall <- data.frame(species="Salvator_merianae", n=length(w), mean_g=round(mean(w)),
                           median_g=round(median(w)), sd_g=round(sd(w)), min_g=min(w), max_g=max(w))
bysex <- aggregate(weight~sex, ind[is.finite(ind$weight),], function(x) c(n=length(x), mean=round(mean(x)), median=round(median(x))))
cat("=== Salvator merianae predator mass (g), n=275 individuals ===\n"); print(mass_overall, row.names=FALSE)
cat("by sex:\n"); print(bysex)

## --- (b) diet BREADTH: vertebrate-prey categories (occurrence only) ---
vert <- c("Rodent","Mesomammal","Rabbit","Insectivore","Mustelid","Bird","Squamate",
          "Amphibian","Fish","Turtle","Reptile.Egg","Bird.Egg")
dv <- diet[diet$DietCategory %in% vert,]
occ <- aggregate(Found~DietCategory, dv, function(x) sum(x,na.rm=TRUE))
occ <- occ[order(-occ$Found),]; names(occ)[2] <- "n_tegus_present"
# per-tegu breadth: how many of the 35 categories each tegu hit (generalism)
br <- aggregate(Found~ID, diet, function(x) sum(x,na.rm=TRUE))
cat("\n=== diet breadth: categories per tegu (of 35; generalism) ===\n")
print(round(summary(br$Found)))
nvert <- aggregate(Found~ID, dv, function(x) sum(x,na.rm=TRUE))
cat(sprintf("tegus with >=1 VERTEBRATE prey category: %d of %d (%.0f%%)\n",
            sum(nvert$Found>0), nrow(nvert), 100*mean(nvert$Found>0)))
cat("\n=== vertebrate-prey category occurrence (presence only; NOT PPMR) ===\n"); print(occ, row.names=FALSE)

## --- write a compact summary (mass reference + breadth); NOT model PPMR input ---
out <- rbind(
  data.frame(metric="pred_mass_g_mean",   value=mass_overall$mean_g),
  data.frame(metric="pred_mass_g_median", value=mass_overall$median_g),
  data.frame(metric="pred_mass_g_min",    value=mass_overall$min_g),
  data.frame(metric="pred_mass_g_max",    value=mass_overall$max_g),
  data.frame(metric="n_individuals",      value=length(w)),
  data.frame(metric="pct_with_vertebrate_prey", value=round(100*mean(nvert$Found>0))),
  setNames(data.frame(paste0("occ_",occ$DietCategory), occ$n_tegus_present), c("metric","value")))
write.csv(out, file.path(ROOT,"data","structured","tegu_salvator_summary.csv"), row.names=FALSE)
cat("\nWrote data/structured/tegu_salvator_summary.csv (Salvator mass reference + vert-prey breadth).\n")
cat("NOTE: predator-mass + breadth only; NO PPMR points (diet is category presence/absence). Per JM.\n")
