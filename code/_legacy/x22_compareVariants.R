###################################################################
# x22_compareVariants.R  — compare the Lacertilia model variants
#
# After x20 has been run 4x (one per variant, distinct GUT_SUFFIX), this:
#   * LOO-compares the 4 fits (out-of-sample predictive; the honest model-selection criterion)
#   * collates per-clade slopes across variants (does prey_type change the scaling?)
#   * tabulates the fossil-relevant Lacertilia diagnostic captured by x20 in the logs
# Writes data/lacertilia_loo_compare.csv + lacertilia_slope_compare.csv.
#
# Variants (see decisions_lacertilia.md):
#   _v0   baseline (no prey_type, no obstype:clade) -- plant-stripped data only
#   _vpt  + prey_type MAIN effect (global vert/invert shift)
#   _vptx + prey_type * clade (clade-specific vert/invert gap = the synthesis for bimodal Lacertilia)
#   _voc  + obstype:clade (Option A: pure-model clade-specific observation bias, no prey_type)
###################################################################
suppressWarnings(suppressPackageStartupMessages({ library(brms) }))
args <- commandArgs(trailingOnly=FALSE); fa <- grep("^--file=", args, value=TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

variants <- c("V0"="_v0", "prey_type(main)"="_vpt", "prey_type*clade"="_vptx", "obstype:clade"="_voc")
fits <- list()
for (nm in names(variants)) {
  f <- file.path(ROOT,"data",paste0("fit_gutslopes",variants[[nm]],"_cfix.rds"))
  if (file.exists(f)) {
    fit <- readRDS(f)
    if (!("loo" %in% names(fit$criteria)))
      fit <- tryCatch(add_criterion(fit,"loo"), error=function(e){ cat("(loo failed for",nm,")\n"); fit })
    fits[[nm]] <- fit
  } else cat("MISSING fit:", f, "\n")
}
cat("loaded variants:", paste(names(fits), collapse=", "), "\n")

## ---- LOO compare (higher elpd_diff = better; |elpd_diff| > ~4 & > 2*se = meaningful) ----
have_loo <- names(fits)[vapply(fits, function(x) "loo" %in% names(x$criteria), logical(1))]
if (length(have_loo) >= 2) {
  cat("\n=== LOO compare (out-of-sample predictive; top = best) ===\n")
  lc <- loo_compare(setNames(lapply(have_loo, function(n) fits[[n]]$criteria$loo), have_loo))
  print(lc)
  write.csv(data.frame(variant=rownames(lc), lc), file.path(ROOT,"data","lacertilia_loo_compare.csv"), row.names=FALSE)
  cat("\nwrote data/lacertilia_loo_compare.csv\n")
} else cat("\n(need >=2 fits with loo for a comparison)\n")

## ---- per-clade slope across variants ----
sl <- do.call(rbind, lapply(names(variants), function(nm){
  f <- file.path(ROOT,"data",paste0("gut_slopes_posterior",variants[[nm]],".csv"))
  if (!file.exists(f)) return(NULL); d <- read.csv(f); data.frame(variant=nm, d) }))
if (!is.null(sl)) {
  cat("\n=== per-clade prey~pred SLOPE across variants ===\n")
  print(reshape(sl[,c("variant","clade","slope")], idvar="clade", timevar="variant", direction="wide"), row.names=FALSE)
  write.csv(sl, file.path(ROOT,"data","lacertilia_slope_compare.csv"), row.names=FALSE)
  cat("wrote data/lacertilia_slope_compare.csv\n")
}

## ---- per-clade sigma (breadth) across variants ----
sg <- do.call(rbind, lapply(names(variants), function(nm){
  f <- file.path(ROOT,"data",paste0("gut_sigma_by_clade",variants[[nm]],".csv"))
  if (!file.exists(f)) return(NULL); d <- read.csv(f); data.frame(variant=nm, d) }))
if (!is.null(sg)) { cat("\n=== per-clade BREADTH (sigma) across variants ===\n")
  print(reshape(sg[,c("variant","clade","sigma")], idvar="clade", timevar="variant", direction="wide"), row.names=FALSE) }

cat("\nx22 DONE. See logs for the per-variant fossil-relevant Lacertilia percentile diagnostic.\n")
