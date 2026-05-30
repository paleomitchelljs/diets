###################################################################
# x02_phyloModel.R
# Hierarchical, partially-pooled, phylogenetic model of the per-item
# predator-prey mass ratio, fit with Stan via brms.
#
# Pipeline position:  x00 -> x01 (lme4 baseline) -> x02 (this: phylo Bayesian)
#
# What this script does
#   1. Loads data/longform.csv (one row per prey item; see x00).
#   2. Builds a PHYLOGENY over predator species.
#        - Default: a TAXONOMY tree (Class/Order/Genus/Species) with Grafen
#          branch lengths, constructed from the data itself. This is a
#          deliberate, clearly-labelled PLACEHOLDER so the full modelling
#          machinery runs end-to-end offline.
#        - To use a real time-calibrated tree, drop a Newick/Nexus file at
#          data/tree.tre  (tips = "Genus_species", matching longform$species).
#          It will be picked up automatically; no other change needed.
#   3. Derives the phylogenetic correlation matrix A = vcv(tree, corr=TRUE).
#   4. Specifies a brms model with:
#        - population slope of predmass, plus a clade-VARYING slope
#          (how clade modulates the ratio's dependence on predator size);
#        - a PHYLOGENETIC species effect      (1 | gr(species, cov = A));
#        - a NON-phylogenetic species effect   (1 | species)   [Lynch's lambda
#          decomposition: separates heritable signal from species-specific
#          deviation];
#        - nested taxonomic pooling on subclade and clade;
#        - method/effort effects: study, obstype.
#   5. VALIDATES the specification offline by generating Stan code and the
#      Stan data object (make_stancode / make_standata) -- these run in-process
#      and need no compiler, so the model is checked even where Stan cannot
#      sample.
#   6. Fits with brm() ONLY when RUN_BRMS=TRUE (sampling needs a C++ toolchain
#      / subprocess, unavailable in some sandboxes).
###################################################################

suppressWarnings(suppressPackageStartupMessages({
	library(ape)
	library(brms)
}))

## --- locate repo root --------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	ROOT <- normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", file_arg))), ".."))
} else {
	ROOT <- normalizePath(getwd())
}
LONGFORM <- file.path(ROOT, "data", "longform.csv")
RAW      <- file.path(ROOT, "data", "raw_data.csv")
TREEFILE <- file.path(ROOT, "data", "tree.tre")     # optional real tree

## --- data --------------------------------------------------------
dat <- read.csv(LONGFORM, stringsAsFactors = FALSE)
dat$study    <- factor(dat$study)
dat$species  <- factor(dat$species)
dat$genus    <- factor(dat$genus)
dat$subclade <- factor(dat$subclade)
dat$clade    <- factor(dat$clade)
dat$obstype  <- factor(dat$obstype)
dat$phylo    <- dat$species   # separate name for the phylogenetic effect (see below)

# center predmass (see x01): puts the intercept at the mean predator size and
# decorrelates intercept/slope, which removes the warmup funnel on few-level
# groups. The clade-varying slope below is on the centered predictor.
dat$predmass_c <- dat$predmass - mean(dat$predmass)

###################################################################
# Build / load the phylogeny.
###################################################################
build_taxonomy_tree <- function(raw_path, species_levels) {
	T <- read.csv(raw_path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
	h <- as.character(unlist(T[3, ])); d <- T[4:nrow(T), ]
	gc_ <- function(n) which(h == n)
	tax <- data.frame(
		class   = d[[gc_("Class")]],
		order   = d[[gc_("Clade")]],
		genus   = d[[gc_("Genus")]],
		species = paste(d[[gc_("Genus")]], d[[gc_("Species")]], sep = "_"),
		stringsAsFactors = FALSE
	)
	# keep only species that appear in the modelling data, and complete taxonomy
	tax <- tax[tax$species %in% species_levels, ]
	tax <- tax[stats::complete.cases(tax) &
	           tax$order != "" & tax$genus != "" & !grepl("_$", tax$species), ]
	tax <- unique(tax)
	# de-duplicate any species mapped to >1 taxonomy (keep first)
	tax <- tax[!duplicated(tax$species), ]
	for (col in c("class", "order", "genus", "species")) tax[[col]] <- factor(tax[[col]])

	frm <- ~class/order/genus/species
	tree <- ape::as.phylo(frm, data = tax, collapse = FALSE)
	tree <- ape::compute.brlen(tree, method = "Grafen")  # ultrametric branch lengths
	tree <- ape::multi2di(tree)                           # resolve polytomies -> binary
	tree$edge.length[tree$edge.length <= 0] <- 1e-8       # guard against zero lengths
	tree
}

if (file.exists(TREEFILE)) {
	message("Using real tree: ", TREEFILE)
	tree <- ape::read.tree(TREEFILE)
	tree_source <- "file"
} else {
	message("No data/tree.tre found -> building TAXONOMY placeholder tree from data.")
	tree <- build_taxonomy_tree(RAW, levels(dat$species))
	tree_source <- "taxonomy-placeholder"
}

## restrict data + tree to their shared species, keep both in sync
common <- intersect(levels(droplevels(dat$species)), tree$tip.label)
message(sprintf("Species: data=%d, tree=%d, shared=%d",
                nlevels(droplevels(dat$species)), length(tree$tip.label), length(common)))
dat  <- droplevels(dat[dat$species %in% common, ])
tree <- ape::keep.tip(tree, common)

## phylogenetic correlation matrix (rownames must match the grouping factor)
A <- ape::vcv(tree, corr = TRUE)
stopifnot(all(levels(dat$species) %in% rownames(A)))

saveRDS(tree, file.path(ROOT, "data", "phylo_tree.rds"))
saveRDS(A,    file.path(ROOT, "data", "phylo_A.rds"))

###################################################################
# Model specification
###################################################################
bform <- bf(
	ppreymass ~ predmass_c +
		(1 + predmass_c | clade) +        # clade-varying intercept & size-slope
		(1 | subclade) +
		(1 | gr(phylo, cov = A)) +        # phylogenetic species effect (cov = A)
		(1 | species) +                   # non-phylogenetic species deviation
		(1 | study) +
		(1 | obstype)
)

Prior <- c(
	prior(student_t(3, 0, 5), class = "Intercept"),
	prior(student_t(3, 0, 5), class = "b"),
	prior(student_t(3, 0, 2.5), class = "sd"),   # tighter; few-level clade/obstype groups funnel under wider priors
	prior(lkj(2),             class = "cor")
)

###################################################################
# Offline validation: generate Stan code + data (no compiler needed)
###################################################################
sc <- make_stancode(bform, data = dat, family = student(),
                    prior = Prior, data2 = list(A = A))
writeLines(sc, file.path(ROOT, "data", "phylo_model.stan"))

sd <- make_standata(bform, data = dat, family = student(),
                    prior = Prior, data2 = list(A = A))
message(sprintf("Stan data built OK: N=%d obs; groups -> species=%d, study=%d, clade=%d, subclade=%d, obstype=%d",
                sd$N, nlevels(dat$species), nlevels(dat$study),
                nlevels(dat$clade), nlevels(dat$subclade), nlevels(dat$obstype)))
message("Tree source: ", tree_source,
        " | wrote data/phylo_model.stan, data/phylo_tree.rds, data/phylo_A.rds")

###################################################################
# Fit (only when explicitly enabled; needs a working Stan toolchain)
###################################################################
if (isTRUE(as.logical(Sys.getenv("RUN_BRMS", "FALSE")))) {
	# cores: parallel chains FORK and hang in the Claude sandbox; set
	# STAN_CORES=1 there. Defaults to 4 for a normal terminal. (See x01.)
	NCORES <- as.integer(Sys.getenv("STAN_CORES", "4"))
	fit <- brm(
		bform, data = dat, family = student(),   # heavy-tailed PPMR; see x01
		data2   = list(A = A),
		prior   = Prior,
		chains  = 4, cores = NCORES,
		warmup  = 1000, iter = 2000,
		control = list(adapt_delta = 0.95, max_treedepth = 14)
	)
	print(summary(fit))
	saveRDS(fit, file.path(ROOT, "data", "fit_phylo.rds"))
	message("Wrote data/fit_phylo.rds")
}
