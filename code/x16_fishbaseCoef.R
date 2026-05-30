###################################################################
# x16_fishbaseCoef.R
# Condense the raw FishBase length-weight export (data/structured/fishbase.csv,
# 25,730 population-level records) into a per-SPECIES length-weight coefficient
# lookup: W(g) = a * L(cm)^b.
#
# FishBase gives the RELATIONSHIP, not a mass -- you apply a,b to a RECORDED
# prey length. So the primary use is converting length-bearing prey to mass:
#   - fossil fish prey (Microraptor fin-ray, Scipionyx fish total length)
#   - Acrochordus filesnake fish prey (SL 35-480 mm)
# (Unmeasured "fish" diet records still need a length assumption.)
#
# Output: data/structured/fishbase_lw_coef.csv  (species, a, b, type, n, Lmin/max)
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
d <- fread(file.path(ROOT, "data", "structured", "fishbase.csv"), showProgress = FALSE)

d[, a := as.numeric(a)][, b := as.numeric(b)]
d <- d[is.finite(a) & is.finite(b) & a > 0 & b > 0]
# one row per species: median coefficients (robust to multi-population entries)
coef <- d[, .(
  a = median(a), b = median(b),
  n_records = .N,
  length_type = names(sort(table(Type), decreasing = TRUE))[1],   # TL/SL/FL most common
  Lmin_cm = suppressWarnings(min(LengthMin, na.rm = TRUE)),
  Lmax_cm = suppressWarnings(max(LengthMax, na.rm = TRUE)),
  family = Family[1], order = Order[1], class = Class[1]
), by = .(species = Species, genus = Genus)]
coef[!is.finite(Lmin_cm), Lmin_cm := NA][!is.finite(Lmax_cm), Lmax_cm := NA]

fwrite(coef, file.path(ROOT, "data", "structured", "fishbase_lw_coef.csv"))
cat(sprintf("FishBase LW coefficients condensed: %d species (from %d population records)\n",
            nrow(coef), nrow(d)))
cat("length-type distribution:\n"); print(table(coef$length_type))
# sanity: a typical 20 cm TL fish -> mass via median a,b
ex <- coef[species %in% c("Oreochromis niloticus","Gambusia affinis","Esox lucius")]
if (nrow(ex)) { ex[, mass_at_20cm_g := round(a * 20^b, 1)]; cat("\nsanity (mass at 20 cm TL):\n"); print(ex[, .(species, a, b, mass_at_20cm_g)]) }
cat("\nUse: prey_mass_g = a * (recorded_length_cm)^b. Coefficients assume the matching length TYPE.\n")
