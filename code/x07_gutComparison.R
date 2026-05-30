###################################################################
# x07_gutComparison.R
# The ROBUST fossil<->extant comparison: GUT-to-GUT.
#
# Rationale (see x06): observation mode is confounded with clade, and the
# extant data cannot support a reliable scat->gut correction. The one
# comparison that sidesteps the confound is to compare fossil GUT CONTENTS
# only against extant STOMACH (gut) contents only -- same observation mode
# on both sides, so no mode offset is needed.
#
# Two caveats kept explicit:
#   (a) Even gut-to-gut, FOSSIL guts over-preserve large/robust prey (small
#       soft prey digest and are not described), so fossil PPMR is likely an
#       UPPER bound on the true ratio. We treat it as such, not as a point.
#   (b) Extant gut samples are pseudo-replicated (few studies/species dominate),
#       so we report BOTH per-item and per-species-median distributions.
#
# This is a first cut: it uses fossil cases that ALREADY have both predator
# and prey mass. Cases with prey known only by length feed in once the mass-
# estimation layer (Benson/Campione, P2 proper) is built.
#
# Outputs:
#   data/extant_gut_ppmr_by_clade.csv   per-clade gut PPMR summary + quantiles
#   data/fossil_gut_points.csv          usable fossil gut PPMR points
#   data/fossil_clade_resemblance.csv   per fossil: percentile + likelihood-posterior over extant clades
#   figures/gut_comparison.png/.pdf
###################################################################

suppressWarnings(suppressPackageStartupMessages({
	library(ggplot2)
}))
args <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)

## --- EXTANT gut (stomach) data ----------------------------------
dat <- read.csv(file.path(ROOT, "data", "longform.csv"))

# SPLIT SQUAMATA into snakes vs lizards: the clade label pools two very
# different feeding modes (snakes take large prey; teiid/monitor lizards take
# small prey), which is bimodal and misleading. subclade already separates them
# (Serpentes vs Teiidae/Varanidae). Reusable derivation -> use clade_fine everywhere.
split_squamata <- function(clade, subclade) {
	ifelse(clade != "Squamata", clade,
	       ifelse(subclade == "Serpentes", "Serpentes", "Lacertilia"))
}
dat$clade <- split_squamata(dat$clade, dat$subclade)

gut <- subset(dat, obstype == "Stomach")
clades <- sort(unique(gut$clade))

qs <- c(.05, .10, .25, .50, .75, .90, .95)
ext_summ <- do.call(rbind, lapply(clades, function(cl) {
	d <- gut[gut$clade == cl, ]
	# per-species medians = an independent-unit view (1 value per species)
	sp_med <- tapply(d$ppreymass, d$species, median)
	q <- quantile(d$ppreymass, qs)
	data.frame(clade = cl, n_items = nrow(d),
	           n_species = length(unique(d$species)),
	           n_studies = length(unique(d$study)),
	           mean = round(mean(d$ppreymass), 2),
	           median_item = round(median(d$ppreymass), 2),
	           median_of_species = round(median(sp_med), 2),
	           t(round(q, 2)))
}))
names(ext_summ) <- sub("^X", "q", names(ext_summ))
cat("=== EXTANT gut (stomach) PPMR by clade ===\n")
print(ext_summ, row.names = FALSE)
write.csv(ext_summ, file.path(ROOT, "data", "extant_gut_ppmr_by_clade.csv"), row.names = FALSE)

## --- FOSSIL gut points (cases with both masses) -----------------
fp <- read.csv(file.path(ROOT, "data", "fossil_predprey.csv"), check.names = TRUE)
fp$mp <- suppressWarnings(as.numeric(fp$mass_pred)); fp$mq <- suppressWarnings(as.numeric(fp$mass_prey))
f1 <- subset(fp, is.finite(mp) & is.finite(mq))
f1 <- data.frame(pred = f1$name_pred, prey = f1$name_prey, prey_clade = f1$clade_prey,
                 pred_mass = f1$mp, prey_mass = f1$mq, source = f1$source,
                 ppmr = log(f1$mq) - log(f1$mp), evidence = "gut/assoc")

fd <- read.csv(file.path(ROOT, "data", "fossil_diet_extractions.csv"), check.names = TRUE)
fd$mp <- suppressWarnings(as.numeric(fd$pred_mass_g)); fd$mq <- suppressWarnings(as.numeric(fd$prey_mass_g))
f2 <- subset(fd, is.finite(mp) & is.finite(mq))
f2 <- data.frame(pred = f2$name_pred, prey = f2$name_prey, prey_clade = f2$clade_prey,
                 pred_mass = f2$mp, prey_mass = f2$mq, source = f2$paper,
                 ppmr = log(f2$mq) - log(f2$mp), evidence = f2$evidence_type)
foss <- unique(rbind(f1, f2))
foss$ppmr <- round(foss$ppmr, 2)
foss <- foss[order(foss$ppmr), ]
cat("\n=== FOSSIL gut PPMR points (all Theropoda predators) ===\n")
print(foss[, c("pred","prey","pred_mass","prey_mass","ppmr","evidence")], row.names = FALSE)
write.csv(foss, file.path(ROOT, "data", "fossil_gut_points.csv"), row.names = FALSE)

## --- "Which extant clade's gut does each fossil resemble?" -------
# For each fossil point: percentile under each clade's per-item gut ECDF, and a
# crude equal-prior likelihood posterior using each clade's gut mean & sd.
mu <- tapply(gut$ppreymass, gut$clade, mean); sg <- tapply(gut$ppreymass, gut$clade, sd)
resemb <- do.call(rbind, lapply(seq_len(nrow(foss)), function(i) {
	x <- foss$ppmr[i]
	pct <- sapply(clades, function(cl) round(100 * mean(gut$ppreymass[gut$clade == cl] <= x), 0))
	lik <- dnorm(x, mu[clades], sg[clades]); post <- lik / sum(lik)
	data.frame(pred = foss$pred[i], prey = foss$prey[i], ppmr = x,
	           t(setNames(pct, paste0("pct_in_", clades))),
	           t(setNames(round(post, 2), paste0("P_", clades))))
}))
cat("\n=== Fossil resemblance to extant GUT distributions ===\n")
cat("   pct_in_X = fossil's percentile within clade X's gut PPMR (50 = typical; >90 = unusually large prey for that clade)\n")
cat("   P_X      = equal-prior posterior that the fossil item came from clade X's gut size-distribution\n")
print(resemb, row.names = FALSE)
write.csv(resemb, file.path(ROOT, "data", "fossil_clade_resemblance.csv"), row.names = FALSE)

## --- figure -----------------------------------------------------
gut$clade <- factor(gut$clade, levels = clades)
foss_plot <- data.frame(clade = "Theropoda\n(fossil gut)", ppmr = foss$ppmr, label = foss$pred)
p <- ggplot(gut, aes(clade, ppreymass)) +
	geom_violin(fill = "grey85", color = "grey55", scale = "width") +
	geom_boxplot(width = .15, outlier.size = .3, outlier.alpha = .15) +
	geom_jitter(data = foss_plot, aes(clade, ppmr), width = .07, height = 0,
	            color = "#b2182b", size = 3) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
	annotate("text", x = 0.7, y = 0.3, label = "prey = predator mass", hjust = 0, size = 3, color = "grey40") +
	coord_cartesian(ylim = c(-16, 4)) +
	labs(title = "Gut-to-gut: extant stomach-content PPMR vs fossil theropod gut contents",
	     subtitle = "Same observation mode on both sides (no scat correction). Red = fossil cases with measured prey+predator mass.\nFossil guts over-preserve large prey, so red points are likely an UPPER bound. Aves gut = 602 items / 7 spp (mostly herons): thin.",
	     x = NULL, y = "log(prey mass / predator mass)") +
	theme_minimal(base_size = 12) +
	theme(plot.subtitle = element_text(color = "grey40", size = 9))
ggsave(file.path(ROOT, "figures", "gut_comparison.png"), p, width = 9, height = 5.2, dpi = 150)
ggsave(file.path(ROOT, "figures", "gut_comparison.pdf"), p, width = 9, height = 5.2)
cat("\nWrote data/extant_gut_ppmr_by_clade.csv, data/fossil_gut_points.csv,\n",
    "      data/fossil_clade_resemblance.csv, figures/gut_comparison.{png,pdf}\n")
