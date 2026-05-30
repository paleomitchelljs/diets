###################################################################
# x05_cladeSlopes.R
# Per-clade slope of log(prey mass / predator mass) on log(predator mass),
# from STOMACH-content observations only (filters out scat / follow / larder),
# with 95% CIs and a publication-style plot.
#
# Model: ppreymass ~ predmass * clade + (1 | species:subclade:clade) + (1 | study)
# - predmass        : slope for reference clade
# - predmass:cladeX : deviation of clade X's slope from reference
# Per-clade slope is built by contrast (slope[ref] + slope[X] = predmass + predmass:cladeX).
###################################################################

suppressWarnings(suppressPackageStartupMessages({
	library(lme4)
	library(ggplot2)
}))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	ROOT <- normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", file_arg))), ".."))
} else {
	ROOT <- normalizePath(getwd())
}

dat <- read.csv(file.path(ROOT, "data", "longform.csv"), stringsAsFactors = FALSE)
gut <- subset(dat, obstype == "Stomach")
gut$clade <- factor(gut$clade)
gut$study <- factor(gut$study)
gut$species <- factor(gut$species)
gut$subclade <- factor(gut$subclade)

## --- sample sizes per clade --------------------------------------
cat("=== Stomach-content sample sizes by clade ===\n")
ntab <- data.frame(
	clade   = levels(gut$clade),
	n_items   = as.integer(table(gut$clade)),
	n_studies = as.integer(tapply(gut$study,   gut$clade, function(x) length(unique(x)))),
	n_species = as.integer(tapply(gut$species, gut$clade, function(x) length(unique(x))))
)
print(ntab, row.names = FALSE)
keep <- ntab$clade[ntab$n_items >= 100 & ntab$n_studies >= 2]
cat("\nClades with >=100 prey-items and >=2 studies (analysed):", paste(keep, collapse=", "), "\n")
gut <- droplevels(gut[gut$clade %in% keep, ])

## --- fit ---------------------------------------------------------
fit <- lmer(
	ppreymass ~ predmass * clade + (1 | species:subclade:clade) + (1 | study),
	data    = gut,
	control = lmerControl(optimizer = "nloptwrap")
)
cat("\n=== Fixed effects ===\n")
print(round(summary(fit)$coefficients, 4))

## --- per-clade slopes via contrasts ------------------------------
b <- fixef(fit); V <- vcov(fit)
ref <- levels(gut$clade)[1]
others <- levels(gut$clade)[-1]

slopes <- setNames(numeric(nlevels(gut$clade)), levels(gut$clade))
ses    <- slopes
slopes[ref] <- b["predmass"]
ses[ref]    <- sqrt(V["predmass", "predmass"])
for (cl in others) {
	inter <- paste0("predmass:clade", cl)
	if (!inter %in% names(b)) next
	slopes[cl] <- b["predmass"] + b[inter]
	v <- V["predmass","predmass"] + V[inter, inter] + 2 * V["predmass", inter]
	ses[cl] <- sqrt(v)
}

est <- data.frame(
	clade     = names(slopes),
	slope     = round(slopes, 4),
	se        = round(ses, 4),
	ci_low    = round(slopes - 1.96 * ses, 4),
	ci_high   = round(slopes + 1.96 * ses, 4),
	n_items   = ntab$n_items[match(names(slopes), ntab$clade)],
	n_species = ntab$n_species[match(names(slopes), ntab$clade)],
	n_studies = ntab$n_studies[match(names(slopes), ntab$clade)],
	stringsAsFactors = FALSE
)
est <- est[order(est$slope), ]
cat("\n=== Per-clade slopes (stomach contents) ===\n"); print(est, row.names = FALSE)

write.csv(est, file.path(ROOT, "data", "clade_slopes_stomach.csv"), row.names = FALSE)

## --- plot --------------------------------------------------------
est$clade <- factor(est$clade, levels = est$clade)   # preserve sort order
p <- ggplot(est, aes(x = clade, y = slope)) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
	geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.18, linewidth = 0.9) +
	geom_point(size = 4.5, color = "#1f5f9c") +
	geom_text(aes(label = sprintf("n=%d items / %d sp / %d studies",
	                              n_items, n_species, n_studies)),
	          hjust = -0.08, vjust = -0.7, size = 3.2, color = "grey30") +
	coord_flip() +
	labs(
		title    = "Per-clade slope of PPMR vs. predator mass",
		subtitle = "Stomach-content observations only; lme4 mixed-model fixed-effect slope ± 1.96·SE",
		x = NULL,
		y = expression("Slope:  d[log(prey mass / predator mass)] / d[log(predator mass)]")
	) +
	theme_minimal(base_size = 12) +
	theme(plot.subtitle = element_text(color = "grey40"))

dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)
ggsave(file.path(ROOT, "figures", "clade_slopes_stomach.pdf"), p, width = 8.5, height = 5)
ggsave(file.path(ROOT, "figures", "clade_slopes_stomach.png"), p, width = 8.5, height = 5, dpi = 150)
cat("\nWrote data/clade_slopes_stomach.csv and figures/clade_slopes_stomach.{pdf,png}\n")

###################################################################
# Within-Aves: subclade (order) slopes
#   (a) Stomach-only — reveals heron-vs-hawk/eagle heterogeneity, no Strigiformes
#   (b) All obstypes — pulls in Strigiformes from Scat/pellets
###################################################################

aves_subclade_slopes <- function(dat_sub, label, file_stem, min_items = 100, min_studies = 2) {
	dat_sub$subclade <- factor(dat_sub$subclade)
	dat_sub$study    <- factor(dat_sub$study)
	dat_sub$species  <- factor(dat_sub$species)
	tab <- data.frame(
		subclade  = levels(dat_sub$subclade),
		n_items   = as.integer(table(dat_sub$subclade)),
		n_species = as.integer(tapply(dat_sub$species, dat_sub$subclade, function(x) length(unique(x)))),
		n_studies = as.integer(tapply(dat_sub$study,   dat_sub$subclade, function(x) length(unique(x))))
	)
	cat(sprintf("\n=== Aves subclade sample sizes (%s) ===\n", label)); print(tab, row.names=FALSE)
	keep <- tab$subclade[tab$n_items >= min_items & tab$n_studies >= min_studies]
	cat("Analysed (>=", min_items, "items, >=", min_studies, "studies):", paste(keep, collapse=", "), "\n", sep="")
	if (length(keep) < 2) { cat("  -> need >=2 subclades to fit; skipping.\n"); return(invisible(NULL)) }
	dat_sub <- droplevels(dat_sub[dat_sub$subclade %in% keep, ])

	fit <- lmer(ppreymass ~ predmass * subclade + (1 | species) + (1 | study),
	            data = dat_sub, control = lmerControl(optimizer = "nloptwrap"))
	b <- fixef(fit); V <- vcov(fit)
	ref <- levels(dat_sub$subclade)[1]; others <- levels(dat_sub$subclade)[-1]
	slopes <- setNames(numeric(nlevels(dat_sub$subclade)), levels(dat_sub$subclade))
	ses    <- slopes
	slopes[ref] <- b["predmass"]; ses[ref] <- sqrt(V["predmass","predmass"])
	for (cl in others) {
		inter <- paste0("predmass:subclade", cl)
		if (!inter %in% names(b)) next
		slopes[cl] <- b["predmass"] + b[inter]
		v <- V["predmass","predmass"] + V[inter, inter] + 2 * V["predmass", inter]
		ses[cl] <- sqrt(v)
	}
	est <- data.frame(
		subclade  = names(slopes),
		slope     = round(slopes, 4),
		se        = round(ses, 4),
		ci_low    = round(slopes - 1.96 * ses, 4),
		ci_high   = round(slopes + 1.96 * ses, 4),
		n_items   = tab$n_items[match(names(slopes), tab$subclade)],
		n_species = tab$n_species[match(names(slopes), tab$subclade)],
		n_studies = tab$n_studies[match(names(slopes), tab$subclade)]
	)
	est <- est[order(est$slope), ]
	cat(sprintf("\n=== Aves subclade slopes (%s) ===\n", label)); print(est, row.names=FALSE)

	write.csv(est, file.path(ROOT, "data", paste0(file_stem, ".csv")), row.names=FALSE)
	est$subclade <- factor(est$subclade, levels = est$subclade)
	p2 <- ggplot(est, aes(x = subclade, y = slope)) +
		geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
		geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.18, linewidth = 0.9) +
		geom_point(size = 4.5, color = "#1f5f9c") +
		geom_text(aes(label = sprintf("n=%d / %d sp / %d studies", n_items, n_species, n_studies)),
		          hjust = -0.08, vjust = -0.7, size = 3.2, color = "grey30") +
		coord_flip() +
		labs(title = "Aves subclade slopes of PPMR vs. predator mass",
		     subtitle = label,
		     x = NULL, y = "Slope") +
		theme_minimal(base_size = 12) +
		theme(plot.subtitle = element_text(color = "grey40"))
	ggsave(file.path(ROOT, "figures", paste0(file_stem, ".pdf")), p2, width = 8.5, height = 5)
	ggsave(file.path(ROOT, "figures", paste0(file_stem, ".png")), p2, width = 8.5, height = 5, dpi = 150)
	cat(sprintf("Wrote data/%s.csv and figures/%s.{pdf,png}\n", file_stem, file_stem))
}

aves_stom <- subset(dat, obstype == "Stomach" & clade == "Aves")
aves_all  <- subset(dat,                          clade == "Aves")
aves_subclade_slopes(aves_stom, "Stomach contents only (no Strigiformes in DB)",
                     "aves_subclade_slopes_stomach", min_items = 50, min_studies = 2)
aves_subclade_slopes(aves_all,  "All obstypes (Scat included, brings in Strigiformes)",
                     "aves_subclade_slopes_all",      min_items = 100, min_studies = 2)
