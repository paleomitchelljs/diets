###################################################################
# x15_avianGutPPMR.R
# Build a REAL extant bird GUT predator-prey mass-ratio distribution from the
# Avian Diet Database gut subset (x14), now that AVONET supplies bird masses.
#
# Predator (always a bird) mass <- AVONET. Prey mass <- AVONET (bird prey) or
# PanTHERIA (mammal prey); other prey (fish/herp/invert) left for later sources.
# So this is the VERTEBRATE-PREY bird gut distribution -- the subset comparable
# to fossil theropod gut contents (which preserve vertebrate prey).
#
# This REPLACES the old extant bird-gut reference (602 items / 7 heron spp).
# Output: data/structured/aviandiet_gut_ppmr.csv + console summary.
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

norm <- function(x) tolower(gsub("[ _]+", " ", trimws(x)))

## --- AVONET (birds) ---------------------------------------------
av <- fread(file.path(ROOT,"data","AVONET","TraitData","AVONET1_BirdLife.csv"), showProgress=FALSE)
av <- av[is.finite(Mass) & Mass > 0]; av[, sp := norm(Species1)]; av[, gen := tstrsplit(sp," ",keep=1)[[1]]]
avo_sp  <- av[, .(m=mean(Mass)), by=sp];  avo_sp  <- setNames(avo_sp$m,  avo_sp$sp)
avo_gen <- av[, .(m=mean(Mass)), by=gen]; avo_gen <- setNames(avo_gen$m, avo_gen$gen)

## --- PanTHERIA (mammals; malformed -> read by position) ----------
pan <- read.delim(file.path(ROOT,"data","pantheria"), header=FALSE, skip=1, quote="",
                  check.names=FALSE, colClasses="character")
pm <- suppressWarnings(as.numeric(pan[[5]])); pm[pm==-999] <- NA
psp <- norm(paste(pan[[3]], pan[[4]])); pgen <- norm(pan[[3]])
pan_sp  <- tapply(pm, psp,  function(z) mean(z,na.rm=TRUE)); pan_sp  <- pan_sp[is.finite(pan_sp)]
pan_gen <- tapply(pm, pgen, function(z) mean(z,na.rm=TRUE)); pan_gen <- pan_gen[is.finite(pan_gen)]

## --- gut subset -------------------------------------------------
g <- fread(file.path(ROOT,"data","structured","aviandiet_gut.csv"), showProgress=FALSE)
# predator mass (bird) <- AVONET
bsp <- norm(g$bird_species); bgen <- sub(" .*$","",bsp)
g[, pred_mass_g := fifelse(bsp %in% names(avo_sp), avo_sp[bsp],
                    fifelse(bgen %in% names(avo_gen), avo_gen[bgen], NA_real_))]
# prey mass <- AVONET (Aves) or PanTHERIA (Mammalia)
psp_q <- norm(g$prey_scientific); pgen_q <- norm(g$prey_genus); pcl <- norm(g$prey_class)
lookup <- function(cl, sp, gen) {
  spm <- if (cl=="aves") avo_sp else if (cl=="mammalia") pan_sp else return(NA_real_)
  gnm <- if (cl=="aves") avo_gen else pan_gen
  if (sp %in% names(spm)) return(unname(spm[sp]))
  if (gen %in% names(gnm)) return(unname(gnm[gen]))
  NA_real_
}
g[, prey_mass_g := mapply(lookup, pcl, psp_q, pgen_q)]
g[, ppmr_ln := log(prey_mass_g) - log(pred_mass_g)]

res <- g[is.finite(ppmr_ln)]
fwrite(res, file.path(ROOT,"data","structured","aviandiet_gut_ppmr.csv"))

cat(sprintf("=== Avian Diet GUT subset: %d records; predator mass filled for %d (%.0f%%) ===\n",
            nrow(g), sum(is.finite(g$pred_mass_g)), 100*mean(is.finite(g$pred_mass_g))))
cat(sprintf("Records with BOTH masses (vertebrate prey: bird+mammal): %d, across %d bird spp\n",
            nrow(res), uniqueN(res$bird_species)))
cat("\nprey-class composition of the PPMR-computable records:\n"); print(sort(table(res$prey_class), decreasing=TRUE))
cat(sprintf("\nBIRD GUT PPMR (ln) — per trophic link: median=%.2f  mean=%.2f\n",
            median(res$ppmr_ln), mean(res$ppmr_ln)))
print(round(quantile(res$ppmr_ln, c(.05,.1,.25,.5,.75,.9,.95)),2))
# per-species median (independent-unit view)
spm <- res[, .(m=median(ppmr_ln)), by=bird_species]
cat(sprintf("\nper-bird-species median PPMR: median-of-species=%.2f (n=%d spp)\n", median(spm$m), nrow(spm)))
cat("\n--- context ---\n")
cat("old bird-gut ref: 602 items / 7 heron spp, median ~ -3.5\n")
cat("fossil theropod gut points: -1.7 to -5.5\n")
cat("Serpentes gut -2.5 | terr mammal carnivores (Tucker) -1.99 | Croc/Lacertilia ~ -8\n")
