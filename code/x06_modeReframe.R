###################################################################
# x06_modeReframe.R
# OBSERVATION-MODE / PRESERVATION-BIAS REFRAME of the extant PPMR data.
#
# Motivation
#   Fossil diet records are GUT CONTENTS (+ coprolites, bite marks). The
#   extant database, by contrast, is 88.5% scat/follow and only 11.4%
#   stomach. Worse, observation mode is almost perfectly CONFOUNDED with
#   clade: Crocodylia is observed only via stomach, Aves almost only via
#   scat. So a fossil theropod gut content compared against an extant
#   "bird line" is being compared against a SCAT line, not a gut line.
#
#   A single random intercept for obstype (as in x01/x02) cannot fix a
#   confound this severe. This script:
#     1. Documents the confound (clade x obstype design table).
#     2. Estimates the gut(Stomach)-vs-Scat MODE OFFSET *within* the two
#        clades that have both modes (Mammalia, Squamata), to test whether
#        a single global correction is even defensible.
#     3. Re-expresses each clade's PPMR on a GUT-CONTENT-EQUIVALENT basis
#        where identifiable, and FLAGS clades where it is not (Aves, Croc).
#     4. Repeats the key fit on an independent-unit aggregation
#        (study x species x obstype cell means) to show how much the
#        171k pseudo-replicated "items" are driven by a few huge studies.
#
# This is the offline (lme4) reference step. The Bayesian version with a
# Student-t response and full uncertainty propagation comes later (x02 +
# the fossil bridge x07).
#
# Outputs:
#   data/mode_offset_by_clade.csv      Stomach-Scat offset per clade (+CI, identifiability)
#   data/clade_lines_gut_equiv.csv     clade PPMR: observed vs gut-equivalent
#   figures/mode_offset.png/.pdf       the offset-heterogeneity figure
#   figures/clade_lines_by_mode.png/.pdf
###################################################################

suppressWarnings(suppressPackageStartupMessages({
	library(lme4)
	library(ggplot2)
}))

## --- locate repo root --------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	ROOT <- normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", file_arg))), ".."))
} else {
	ROOT <- normalizePath(getwd())
}
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)

dat <- read.csv(file.path(ROOT, "data", "longform.csv"), stringsAsFactors = FALSE)
for (v in c("clade","obstype","study","species","subclade")) dat[[v]] <- factor(dat[[v]])

## center predmass once (shared across all fits => comparable intercepts)
PM_BAR <- mean(dat$predmass)
dat$predmass_c <- dat$predmass - PM_BAR
cat(sprintf("Centering predmass at mean log-g = %.3f (= %.0f g)\n\n", PM_BAR, exp(PM_BAR)))

###################################################################
# 1. The confound, made explicit
###################################################################
cat("=== clade x obstype : N prey-items ===\n")
print(addmargins(table(dat$clade, dat$obstype)))
cat("\n=== clade x obstype : mean PPMR ===\n")
print(round(tapply(dat$ppreymass, list(dat$clade, dat$obstype),
                   function(x) if (length(x)) mean(x) else NA_real_), 2))

# which clades actually have BOTH scat and stomach in usable numbers?
tab <- table(dat$clade, dat$obstype)
both_modes <- rownames(tab)[tab[, "Scat"] >= 50 & tab[, "Stomach"] >= 50]
cat("\nClades with >=50 items in BOTH Scat and Stomach (mode offset *raw counts* present):",
    paste(both_modes, collapse = ", "), "\n")
cat("Clades observed in only one mode (gut line NOT identifiable from extant data):",
    paste(setdiff(levels(dat$clade), both_modes), collapse = ", "), "\n")

# CRUCIAL: a mode offset is only trustworthy to the extent the SAME taxa are
# observed both ways. Count species/studies seen via BOTH Scat and Stomach.
overlap_counts <- function(cl) {
	d <- dat[dat$clade == cl, ]
	sp <- tapply(d$obstype, d$species, function(x) all(c("Scat","Stomach") %in% x))
	st <- tapply(d$obstype, d$study,   function(x) all(c("Scat","Stomach") %in% x))
	c(n_species_both = sum(sp, na.rm = TRUE), n_studies_both = sum(st, na.rm = TRUE))
}
ov <- t(sapply(levels(dat$clade), overlap_counts))
cat("\n=== within-taxon Scat<->Stomach overlap (the ONLY basis for a mode correction) ===\n")
print(ov)
cat("  -> offsets from <3 shared species/studies are fragile; flagged below.\n\n")

###################################################################
# 2. Mode offset within the clades that have both modes
#    (the test of whether one global gut-correction is defensible)
###################################################################
sub <- droplevels(dat[dat$clade %in% both_modes & dat$obstype %in% c("Scat","Stomach"), ])
sub$obstype <- relevel(factor(sub$obstype), ref = "Scat")
sub$clade   <- factor(sub$clade)
ref_clade   <- levels(sub$clade)[1]
sub$clade   <- relevel(sub$clade, ref = ref_clade)

m_offset <- lmer(
	ppreymass ~ predmass_c + clade * obstype + (1 | study/species),
	data = sub, control = lmerControl(optimizer = "nloptwrap"))

b <- fixef(m_offset); V <- as.matrix(vcov(m_offset))
# Stomach-Scat offset for the reference clade = coef "obstypeStomach"
# for clade X = obstypeStomach + cladeX:obstypeStomach
offset_rows <- lapply(levels(sub$clade), function(cl) {
	if (cl == ref_clade) {
		nm <- "obstypeStomach"; cvec <- setNames(numeric(length(b)), names(b)); cvec[nm] <- 1
	} else {
		nm <- c("obstypeStomach", paste0("clade", cl, ":obstypeStomach"))
		cvec <- setNames(numeric(length(b)), names(b)); cvec[intersect(nm, names(b))] <- 1
	}
	est <- sum(cvec * b); se <- sqrt(drop(t(cvec) %*% V %*% cvec))
	data.frame(clade = cl, stomach_minus_scat = est, se = se,
	           ci_low = est - 1.96*se, ci_high = est + 1.96*se,
	           n_scat = sum(sub$clade==cl & sub$obstype=="Scat"),
	           n_stom = sum(sub$clade==cl & sub$obstype=="Stomach"),
	           n_species_both = ov[cl, "n_species_both"],
	           n_studies_both = ov[cl, "n_studies_both"],
	           offset_quality = ifelse(ov[cl, "n_species_both"] >= 3, "ok", "FRAGILE (<3 shared taxa)"))
})
offset_tab <- do.call(rbind, offset_rows)

cat("=== MODE OFFSET: mean log(prey/pred) in Stomach minus Scat, by clade ===\n")
cat("   (a single global gut-correction is only safe if these are similar)\n")
print(transform(offset_tab,
	stomach_minus_scat = round(stomach_minus_scat,2), se = round(se,2),
	ci_low = round(ci_low,2), ci_high = round(ci_high,2)), row.names = FALSE)

# heterogeneity test: does the clade:obstype interaction matter?
m_noint <- update(m_offset, . ~ predmass_c + clade + obstype + (1 | study/species))
lr <- anova(m_noint, m_offset)
cat(sprintf("\nLikelihood-ratio test for heterogeneous mode offset (clade x obstype):\n  chisq=%.1f df=%d p=%.3g\n",
            lr$Chisq[2], lr$Df[2], lr$`Pr(>Chisq)`[2]))

# add the clades where the offset is NOT identifiable, flagged
non_ident <- setdiff(levels(dat$clade), both_modes)
offset_full <- rbind(
	offset_tab,
	if (length(non_ident)) data.frame(clade = non_ident, stomach_minus_scat = NA, se = NA,
	           ci_low = NA, ci_high = NA,
	           n_scat = as.integer(tab[non_ident, "Scat"]),
	           n_stom = as.integer(tab[non_ident, "Stomach"]),
	           n_species_both = ov[non_ident, "n_species_both"],
	           n_studies_both = ov[non_ident, "n_studies_both"],
	           offset_quality = "NOT IDENTIFIABLE (single-mode clade)")
)
write.csv(offset_full, file.path(ROOT, "data", "mode_offset_by_clade.csv"), row.names = FALSE)

###################################################################
# 3. Clade lines: observed-mode vs gut-content-equivalent
#    Model: full data, predmass_c + clade + obstype + REs.
#    The Stomach coefficient is the *global* gut offset (borrowed from
#    the both-mode clades). We report each clade's PPMR at mean predmass
#    in (a) its observed dominant mode and (b) projected to Stomach.
###################################################################
m_main <- lmer(
	ppreymass ~ predmass_c + clade + obstype + (1 | study/species),
	data = dat, control = lmerControl(optimizer = "nloptwrap"))
bm <- fixef(m_main); Vm <- as.matrix(vcov(m_main))

# dominant observed mode per clade (by item count)
dom_mode <- apply(tab, 1, function(r) colnames(tab)[which.max(r)])

clade_line <- function(cl, mode) {
	cvec <- setNames(numeric(length(bm)), names(bm))
	cvec["(Intercept)"] <- 1                       # predmass_c=0 => at mean predator size
	if (cl != levels(dat$clade)[1]) cvec[paste0("clade", cl)] <- 1
	if (mode != "Follow") {                        # Follow is the reference obstype level? check
		key <- paste0("obstype", mode)
		if (key %in% names(bm)) cvec[key] <- 1
	}
	est <- sum(cvec*bm); se <- sqrt(drop(t(cvec) %*% Vm %*% cvec))
	c(est = est, se = se)
}
# determine reference obstype level (the one without a coefficient)
ob_levels <- levels(dat$obstype)
ob_ref <- ob_levels[!paste0("obstype", ob_levels) %in% names(bm)][1]

rows <- lapply(levels(dat$clade), function(cl) {
	obs <- clade_line(cl, dom_mode[cl])
	gut <- clade_line(cl, "Stomach")
	data.frame(clade = cl,
	           dominant_mode = dom_mode[cl],
	           ppmr_observed_mode = obs["est"], se_obs = obs["se"],
	           ppmr_gut_equiv = gut["est"], se_gut = gut["se"],
	           gut_identifiable = ifelse(cl %in% both_modes, "yes",
	                                     "NO - relies on global offset transfer"))
})
clade_lines <- do.call(rbind, rows)
rownames(clade_lines) <- NULL
cat(sprintf("\n(obstype reference level = %s; global Stomach offset = %.2f)\n", ob_ref,
            if ("obstypeStomach" %in% names(bm)) bm["obstypeStomach"] else NA))
cat("\n=== Clade PPMR at mean predator size: observed mode vs gut-equivalent ===\n")
print(transform(clade_lines,
	ppmr_observed_mode = round(ppmr_observed_mode,2), se_obs = round(se_obs,2),
	ppmr_gut_equiv = round(ppmr_gut_equiv,2), se_gut = round(se_gut,2)), row.names = FALSE)
write.csv(clade_lines, file.path(ROOT, "data", "clade_lines_gut_equiv.csv"), row.names = FALSE)

###################################################################
# 4. Independent-unit robustness: collapse to study x species x obstype
#    cell means, refit. If big studies dominate, item-level and
#    cell-level estimates will disagree.
###################################################################
cell <- aggregate(ppreymass ~ study + species + obstype + clade, data = dat, FUN = mean)
cell_n <- aggregate(ppreymass ~ study + species + obstype + clade, data = dat, FUN = length)
cell$n_items <- cell_n$ppreymass
cell$predmass_c <- aggregate(predmass_c ~ study + species + obstype + clade, data = dat, FUN = mean)$predmass_c
cat(sprintf("\n=== Independent-unit check: %d study x species x obstype cells (vs %d items) ===\n",
            nrow(cell), nrow(dat)))
m_cell <- lmer(ppreymass ~ predmass_c + clade + obstype + (1 | study),
               data = cell, control = lmerControl(optimizer = "nloptwrap"))
cmp <- data.frame(
	term = names(fixef(m_main)),
	item_level = round(fixef(m_main), 3),
	cell_level = round(fixef(m_cell)[names(fixef(m_main))], 3))
rownames(cmp) <- NULL
cat("Fixed effects: item-level (pseudo-replicated) vs cell-level (1 row per study x species x mode)\n")
print(cmp, row.names = FALSE)

###################################################################
# 5. Figures
###################################################################
## (a) mode-offset heterogeneity
off_plot <- offset_tab
off_plot$clade <- factor(off_plot$clade, levels = off_plot$clade[order(off_plot$stomach_minus_scat)])
p1 <- ggplot(off_plot, aes(clade, stomach_minus_scat)) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
	geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .15, linewidth = .9) +
	geom_point(size = 4, color = "#b2182b") +
	geom_text(aes(label = sprintf("scat n=%d / stomach n=%d", n_scat, n_stom)),
	          vjust = -1.1, size = 3, color = "grey30") +
	coord_flip() +
	labs(title = "Gut(stomach) vs scat shift in log(prey/predator), by clade",
	     subtitle = "If a single global gut-correction were valid these would coincide. They do not.",
	     x = NULL, y = "Stomach mean - Scat mean  (log mass-ratio units)") +
	theme_minimal(base_size = 12)
ggsave(file.path(ROOT, "figures", "mode_offset.png"), p1, width = 8.5, height = 4.2, dpi = 150)
ggsave(file.path(ROOT, "figures", "mode_offset.pdf"), p1, width = 8.5, height = 4.2)

## (b) clade lines: observed vs gut-equivalent
cl_long <- rbind(
	data.frame(clade = clade_lines$clade, basis = "observed mode",
	           ppmr = clade_lines$ppmr_observed_mode, se = clade_lines$se_obs,
	           ident = clade_lines$gut_identifiable),
	data.frame(clade = clade_lines$clade, basis = "gut-equivalent",
	           ppmr = clade_lines$ppmr_gut_equiv, se = clade_lines$se_gut,
	           ident = clade_lines$gut_identifiable))
p2 <- ggplot(cl_long, aes(clade, ppmr, color = basis, shape = ident)) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
	geom_errorbar(aes(ymin = ppmr - 1.96*se, ymax = ppmr + 1.96*se),
	              width = .15, position = position_dodge(.4)) +
	geom_point(size = 3.4, position = position_dodge(.4)) +
	coord_flip() +
	labs(title = "Clade PPMR at mean predator size: observed mode vs gut-equivalent",
	     subtitle = "Triangles = gut line leans on transferring the global offset (Aves, Crocodylia): treat with caution",
	     x = NULL, y = "log(prey mass / predator mass) at mean predator size") +
	scale_shape_manual(values = c("yes" = 16, "NO - relies on global offset transfer" = 17)) +
	theme_minimal(base_size = 12)
ggsave(file.path(ROOT, "figures", "clade_lines_by_mode.png"), p2, width = 9, height = 4.5, dpi = 150)
ggsave(file.path(ROOT, "figures", "clade_lines_by_mode.pdf"), p2, width = 9, height = 4.5)

cat("\nWrote: data/mode_offset_by_clade.csv, data/clade_lines_gut_equiv.csv,\n",
    "      figures/mode_offset.{png,pdf}, figures/clade_lines_by_mode.{png,pdf}\n")
