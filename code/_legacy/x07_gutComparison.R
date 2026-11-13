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
	library(ggrepel)
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

# Replace the thin 7-heron longform Aves STOMACH sample with the real bird-gut
# reference (Avian Diet DB, vertebrate prey, AVONET-massed; x15): 214 records /
# 20 bird spp. Far more representative of vertebrate-eating birds than 7 herons.
adb_f <- file.path(ROOT, "data", "structured", "aviandiet_gut_ppmr.csv")
if (file.exists(adb_f)) {
	adb <- read.csv(adb_f)
	aves_new <- data.frame(preyMass = NA_real_, ppreymass = adb$ppmr_ln, predmass = NA_real_,
	                       species = adb$bird_species, genus = NA, subclade = NA,
	                       clade = "Aves", obstype = "Stomach", study = "AvianDietDB")
	gut <- rbind(gut[gut$clade != "Aves", names(aves_new)], aves_new)
	message("Aves gut reference: swapped 7-heron longform stomach -> Avian Diet DB gut (214 rec / 20 spp)")
}
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
                 pred_clade = f1$clade_pred,
                 pred_mass = f1$mp, prey_mass = f1$mq, source = f1$source,
                 ppmr = log(f1$mq) - log(f1$mp), evidence = "gut/assoc")

fd <- read.csv(file.path(ROOT, "data", "fossil_diet_extractions.csv"), check.names = TRUE)
fd$mp <- suppressWarnings(as.numeric(fd$pred_mass_g)); fd$mq <- suppressWarnings(as.numeric(fd$prey_mass_g))
f2 <- subset(fd, is.finite(mp) & is.finite(mq))
f2 <- data.frame(pred = f2$name_pred, prey = f2$name_prey, prey_clade = f2$clade_prey,
                 pred_clade = f2$clade_pred,
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

## --- figures: extant gut (stomach) violins with fossils placed at their OWN extant clade ----
clean <- function(x) sub(" juvenile", " juv.", gsub("_", " ", x))
foss$class <- ifelse(grepl("attack", foss$evidence), "attack",
              ifelse(grepl("assoc", foss$evidence) | grepl("Hone", foss$source), "association", "gut (measured)"))
THEROCAT <- "Theropoda\n(fossil)"
# map a fossil predator's clade -> the x-axis column it plots in: its extant kin, or the
# dedicated Theropoda column (no extant analog). Lets the fossil croc sit on the extant
# Crocodylia violin, the fossil snake on Serpentes, etc.
foss_col <- function(pc) ifelse(grepl("Theropoda", pc), THEROCAT,
                         ifelse(grepl("Crocodyl", pc), "Crocodylia",
                         ifelse(grepl("Serpentes|Madtsoiidae", pc), "Serpentes",
                         ifelse(grepl("Lacert|Varan|Teiid|Squamata", pc), "Lacertilia",
                         ifelse(grepl("Mammal", pc), "Mammalia",
                         ifelse(grepl("Aves", pc), "Aves", THEROCAT))))))
mpts <- data.frame(ppmr = foss$ppmr, lo = NA_real_, hi = NA_real_, class = foss$class,
                   col_clade = foss_col(foss$pred_clade),
                   label = paste0(clean(foss$pred), " -> ", clean(foss$prey)))
# ESTIMATED gut points (x19) are all Theropoda; label only the extremes to limit clutter
est <- tryCatch(read.csv(file.path(ROOT, "data", "theropod_gut_ppmr_estimated.csv")), error = function(e) NULL)
ept <- if (!is.null(est)) {
	e <- data.frame(ppmr = est$ppmr_ln, lo = est$ppmr_lo, hi = est$ppmr_hi, class = "gut (estimated)",
	                col_clade = THEROCAT,
	                label = ifelse(est$ppmr_ln > -1 | est$ppmr_ln < -7, clean(sub("_.*$", "", est$case)), NA))
	e[is.finite(e$ppmr), ]
} else mpts[0, ]

xlev <- c(clades, THEROCAT)
gut$clade <- factor(gut$clade, levels = clades)
shp <- c("gut (measured)" = 17, "gut (estimated)" = 2, "attack" = 8, "association" = 1)
col <- c("gut (measured)" = "#b2182b", "gut (estimated)" = "grey35", "attack" = "#c8860d", "association" = "#b2182b")

plot_gut <- function(fall, stem, subtitle, h = 6) {
	fall$clade <- factor(fall$col_clade, levels = xlev)
	fall <- fall[!is.na(fall$clade), ]
	th  <- fall[fall$col_clade == THEROCAT, ]   # theropod column (many pts): labels nudged right
	ext <- fall[fall$col_clade != THEROCAT, ]   # fossils on their extant clade: labels repelled in place
	p <- ggplot(gut, aes(clade, ppreymass)) +
		geom_violin(fill = "grey85", color = "grey55", scale = "width") +
		geom_boxplot(width = .15, outlier.size = .3, outlier.alpha = .12) +
		geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
		annotate("text", x = 0.6, y = 0.7, label = "prey = predator mass", hjust = 0, size = 2.6, color = "grey45") +
		geom_errorbar(data = fall[is.finite(fall$lo), ], aes(x = clade, ymin = lo, ymax = hi),
		              inherit.aes = FALSE, width = 0, color = "grey65", linewidth = .4) +
		geom_point(data = fall, aes(x = clade, y = ppmr, shape = class, color = class),
		           inherit.aes = FALSE, size = 2.8) +
		geom_text_repel(data = th, aes(x = clade, y = ppmr, label = label), inherit.aes = FALSE,
		                size = 2.4, color = "grey20", segment.color = "grey75", direction = "y",
		                nudge_x = 0.5, hjust = 0, box.padding = .25, max.overlaps = Inf, na.rm = TRUE) +
		geom_text_repel(data = ext, aes(x = clade, y = ppmr, label = label), inherit.aes = FALSE,
		                size = 2.4, color = "grey20", segment.color = "grey75", fontface = "italic",
		                point.padding = .4, box.padding = .5, min.segment.length = 0,
		                nudge_y = -1.2, max.overlaps = Inf, na.rm = TRUE, seed = 1) +
		scale_shape_manual(values = shp, name = "fossil", drop = FALSE) +
		scale_color_manual(values = col, name = "fossil", drop = FALSE) +
		scale_x_discrete(limits = xlev, expand = expansion(add = c(0.6, 2.4))) +
		coord_cartesian(ylim = c(-16, 4)) +
		labs(title = "PPMR: extant gut (stomach) violins, fossils placed at their OWN clade",
		     subtitle = subtitle, x = NULL, y = "log(prey mass / predator mass)") +
		theme_minimal(base_size = 12) + theme(plot.subtitle = element_text(color = "grey40", size = 8))
	ggsave(file.path(ROOT, "figures", paste0(stem, ".png")), p, width = 10, height = h, dpi = 150)
	ggsave(file.path(ROOT, "figures", paste0(stem, ".pdf")), p, width = 10, height = h)
}
plot_gut(rbind(mpts, ept), "gut_comparison",
         "Extant = stomach gut (Aves = Avian Diet DB). FOSSILS PLACED AT THEIR OWN CLADE (croc->Crocodylia, snake->Serpentes); Theropoda has no extant analog (own column). Symbols: solid tri=measured gut, open tri=estimated gut (x19; bars=prey-mass range), star=attack, open circ=association. Fossil guts over-preserve large prey -> likely UPPER bound.")
plot_gut(mpts[mpts$class == "gut (measured)", ], "gut_comparison_strict",
         "STRICT: measured fossil GUT CONTENTS only (no estimates/attack/association), each at its own clade.", h = 5.6)
cat("\nWrote figures/gut_comparison.{png,pdf} + gut_comparison_strict.* (fossils at their own clade) + the data CSVs\n")
