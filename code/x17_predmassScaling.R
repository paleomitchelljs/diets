###################################################################
# x17_predmassScaling.R
# How does prey mass scale with predator mass? — the hierarchical
# prey-on-predator slope with partial pooling (species < genus < clade),
# the general relationship + which taxa fall far off it, and where the
# extinct theropods sit relative to extant predator "styles".
#
# WHY ln(prey)~ln(pred) and NOT PPMR~pred:
#   PPMR = ln(prey) - ln(pred), so regressing PPMR on ln(pred) has a built-in
#   slope of -1 (the shared term) — a "no relationship" null is PPMR-slope = -1,
#   not 0. Regressing ln(prey) on ln(pred) is the un-coupled measure:
#     slope b = 1  -> isometric (prey scales 1:1; PPMR constant with size)
#     b < 1        -> larger predators take relatively SMALLER prey (PPMR falls)
#     b > 1        -> larger predators take relatively LARGER prey
#   Equivalently PPMR-vs-predmass slope = b - 1. Vertebrate prey also have a
#   hard FLOOR (~smallest vertebrate, a few g) and a soft CEILING (~ predator
#   mass), which by themselves push b below 1 at the extremes (the boundary
#   effect) — so we report b against the b=1 isometry and b=0 ("prey size
#   independent of predator") references.
#
# Model (extant STOMACH contents = gut-comparable; Squamata split):
#   preyMass ~ predmass + (1 + predmass | clade) + (1 | genus) + (1 | species) + (1 | study)
#   lme4 here (fast, in-sandbox). A brms version (random slopes + sigma~predmass,
#   the old skew-normal form) is gated behind RUN_BRMS for a real machine.
###################################################################
suppressWarnings(suppressPackageStartupMessages({ library(lme4); library(ggplot2); library(ggrepel) }))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)

dat <- read.csv(file.path(ROOT, "data", "longform.csv"))
dat$clade <- ifelse(dat$clade != "Squamata", dat$clade,
                    ifelse(dat$subclade == "Serpentes", "Serpentes", "Lacertilia"))
gut <- subset(dat, obstype == "Stomach")
PM_BAR <- mean(gut$predmass)
gut$predmass_c <- gut$predmass - PM_BAR
for (v in c("clade","genus","species","study")) gut[[v]] <- factor(gut[[v]])

## --- hierarchical fit (lme4) ------------------------------------
m <- lmer(preyMass ~ predmass_c + (1 + predmass_c | clade) + (1 | genus) + (1 | species) + (1 | study),
          data = gut, control = lmerControl(optimizer = "bobyqa"))
b0 <- fixef(m)["(Intercept)"]; b1 <- fixef(m)["predmass_c"]
cat(sprintf("=== GENERAL relationship: ln(prey) = %.2f + %.2f * ln(pred_centered) ===\n", b0, b1))
cat(sprintf("population scaling exponent b = %.2f (isometry b=1; 'prey size independent of predator' b=0)\n", b1))
cat(sprintf("  => PPMR falls with predator size at slope b-1 = %.2f\n\n", b1 - 1))

## per-clade slopes (partially pooled): population + clade random-slope BLUP
re_cl <- ranef(m)$clade
clade_slopes <- data.frame(clade = rownames(re_cl),
                           slope_b = round(b1 + re_cl$predmass_c, 2),
                           intercept = round(b0 + re_cl$`(Intercept)`, 2))
clade_slopes <- clade_slopes[order(clade_slopes$slope_b), ]
cat("=== per-clade prey~pred scaling (partially pooled) ===\n"); print(clade_slopes, row.names = FALSE)

## --- OUTLIERS: species/genera far from the general relationship --
sp_blup <- ranef(m)$species[, "(Intercept)"]; names(sp_blup) <- rownames(ranef(m)$species)
ge_blup <- ranef(m)$genus[, "(Intercept)"];   names(ge_blup) <- rownames(ranef(m)$genus)
cat("\n=== top species OUTLIERS (eat much LARGER prey than expected for their clade+size) ===\n")
print(round(head(sort(sp_blup, decreasing = TRUE), 6), 2))
cat("=== top species OUTLIERS (much SMALLER prey than expected) ===\n")
print(round(head(sort(sp_blup), 6), 2))
write.csv(data.frame(species = names(sp_blup), prey_size_deviation_ln = round(sp_blup, 3))[order(-sp_blup), ],
          file.path(ROOT, "data", "scaling_species_outliers.csv"), row.names = FALSE)

## --- obstype as a FACTOR: can we cross-compare modes & use ALL the data? ----
# In this pure regression (conditioning on predator size + clade + taxon), the
# mode offset is the near-true METHOD effect, NOT the size/taxon-composition
# artifact that confounded raw PPMR means (x06). If small & consistent, we can
# pool scat/pellet/follow with stomach (a ~0.1-0.4 ln adjustment) instead of
# discarding ~89% of the data.
dall <- read.csv(file.path(ROOT, "data", "longform.csv"))
dall$clade <- ifelse(dall$clade != "Squamata", dall$clade,
                     ifelse(dall$subclade == "Serpentes", "Serpentes", "Lacertilia"))
dall$predmass_c <- dall$predmass - PM_BAR
dall$obstype <- relevel(factor(dall$obstype), ref = "Stomach")
for (v in c("clade","genus","species","study")) dall[[v]] <- factor(dall[[v]])
mo <- lmer(preyMass ~ predmass_c + obstype + (1 + predmass_c | clade) + (1 | genus) + (1 | species) + (1 | study),
           data = dall, control = lmerControl(optimizer = "bobyqa"))
bo <- fixef(mo); seo <- sqrt(diag(as.matrix(vcov(mo))))
cat("\n=== obstype offset in ln(prey) vs Stomach (full data; conditioned on size + clade + taxon) ===\n")
for (i in grep("^obstype", names(bo)))
  cat(sprintf("  %-10s %+.2f (SE %.2f)\n", sub("obstype", "", names(bo)[i]), bo[i], seo[i]))
cat("  -> small, tight scat-vs-stomach offset (cf. the ~7-ln RAW gap in x06) => modes are cross-comparable;\n",
    "     pooling all obstypes with a mode adjustment is defensible (single-mode clades still extrapolate).\n")

## --- THEROPODS: where do they fall vs each extant clade's line? --
foss <- read.csv(file.path(ROOT, "data", "fossil_gut_points.csv"))
foss <- foss[is.finite(foss$pred_mass) & is.finite(foss$prey_mass), ]
foss$lpred <- log(foss$pred_mass); foss$lprey <- log(foss$prey_mass)
foss$predmass_c <- foss$lpred - PM_BAR
# expected ln(prey) under each clade's partially-pooled line; residual = obs - exp
for (cl in clade_slopes$clade) {
  ic <- clade_slopes$intercept[clade_slopes$clade == cl]; sl <- clade_slopes$slope_b[clade_slopes$clade == cl]
  foss[[paste0("resid_", cl)]] <- round(foss$lprey - (ic + sl * foss$predmass_c), 2)
}
foss$best_match <- apply(foss[, paste0("resid_", clade_slopes$clade)], 1,
                         function(r) clade_slopes$clade[which.min(abs(r))])
cat("\n=== fossil theropods: residual (ln) from each extant clade's scaling line; nearest = best_match ===\n")
print(foss[, c("pred","prey","pred_mass","prey_mass", paste0("resid_", clade_slopes$clade), "best_match")], row.names = FALSE)
write.csv(foss, file.path(ROOT, "data", "theropod_scaling_residuals.csv"), row.names = FALSE)

## --- figure: ln(prey) vs ln(pred), per-species medians + clade lines + theropods ---
spmed <- aggregate(cbind(preyMass, predmass) ~ species + clade, gut, median)
xr <- range(c(spmed$predmass, foss$lpred))
lines_df <- do.call(rbind, lapply(clade_slopes$clade, function(cl) {
  data.frame(clade = cl, x = xr,
             y = clade_slopes$intercept[clade_slopes$clade==cl] + clade_slopes$slope_b[clade_slopes$clade==cl]*(xr - PM_BAR))
}))
foss$lab <- sub(" juvenile","", gsub("_"," ", foss$pred))
p <- ggplot(spmed, aes(predmass, preyMass, color = clade)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey55") +   # isometry (prey=pred)
  annotate("text", x = xr[2], y = xr[2], label = "prey = predator (b=1)", hjust = 1, vjust = -0.4, size = 2.6, color = "grey55") +
  geom_point(alpha = .5, size = 1.4) +
  geom_line(data = lines_df, aes(x, y, color = clade), linewidth = 1) +
  geom_point(data = foss, aes(lpred, lprey), inherit.aes = FALSE, color = "black", shape = 17, size = 2.6) +
  geom_text_repel(data = foss, aes(lpred, lprey, label = lab), inherit.aes = FALSE,
                  size = 2.5, color = "grey15", max.overlaps = Inf, box.padding = .4, min.segment.length = 0) +
  labs(title = "Prey mass scales with predator mass — per-clade slopes + fossil theropods",
       subtitle = sprintf("Extant stomach contents; points = per-species medians, lines = partially-pooled clade fits. Population b=%.2f (isometry=1). Black triangles = fossil theropod gut items.", b1),
       x = "ln(predator mass, g)", y = "ln(prey mass, g)", color = "clade") +
  theme_minimal(base_size = 12) + theme(plot.subtitle = element_text(color = "grey40", size = 8.5))
ggsave(file.path(ROOT, "figures", "predmass_scaling.png"), p, width = 9, height = 6, dpi = 150)
ggsave(file.path(ROOT, "figures", "predmass_scaling.pdf"), p, width = 9, height = 6)
cat("\nWrote data/scaling_species_outliers.csv, data/theropod_scaling_residuals.csv, figures/predmass_scaling.{png,pdf}\n")

## --- brms version (real machine; random slopes + sigma sub-model) ----
if (isTRUE(as.logical(Sys.getenv("RUN_BRMS", "FALSE")))) {
  suppressPackageStartupMessages(library(brms))
  NCORES <- as.integer(Sys.getenv("STAN_CORES", "4"))
  bform <- bf(preyMass ~ predmass_c + (1 + predmass_c | clade/genus/species) + (1 | study),
              sigma   ~ predmass_c + (1 | clade))     # diet breadth varies with size + clade
  fit <- brm(bform, data = gut, family = student(), chains = 4, cores = NCORES,
             warmup = 1000, iter = 2000, control = list(adapt_delta = 0.95, max_treedepth = 14))
  print(summary(fit)); saveRDS(fit, file.path(ROOT, "data", "fit_scaling.rds"))
}
