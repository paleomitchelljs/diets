###################################################################
# x03_massData.R
# Begin integrating more extant body-mass data into the diet database.
#
# What this does
#   * Inventories every PREDATOR and every observed PREY taxon, with its
#     current mass and how many diet records it appears in.
#   * Classifies each taxon by broad clade using the prey "species"
#     annotation (which encodes MAMMALIA / AVES / SQUAMATA / ACTINOPTERYGII /
#     AMPHIBIA / TESTUDINES / INVERT / PLANT / EGGS ...), normalising the
#     handful of spelling variants.
#   * Fills MISSING masses where a defensible source exists:
#       - PanTHERIA (mammals): binomial match first, else genus-mean.
#     Existing masses are never overwritten.
#   * Emits two reviewable artifacts (raw_data.csv is left untouched):
#       data/mass_overrides.csv  - key, mass_g, source   (consumed by x00)
#       data/mass_gaps.csv       - still-missing taxa, classified + prioritised
#         by observation count, with a suggested data source per clade.
#
# NOTE on architecture: this script does NOT edit raw_data.csv. It writes a
# side table (mass_overrides.csv) that x00_reformatData.R applies to fill
# missing masses. Delete that file to revert. Fills only ever touch NAs.
###################################################################

suppressWarnings(suppressPackageStartupMessages(library(data.table)))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	ROOT <- normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", file_arg))), ".."))
} else {
	ROOT <- normalizePath(getwd())
}
RAW <- file.path(ROOT, "data", "raw_data.csv")
PAN <- file.path(ROOT, "data", "pantheria")

## --- normalise broad-clade tags ---------------------------------
norm_clade <- function(x) {
	t <- toupper(sub("[_ ].*$", "", trimws(x)))
	t[t %in% c("ACTINOPTERGII", "ACINOPTERYGII", "ACTINOPTERGYII")] <- "ACTINOPTERYGII"
	t
}
NONANIMAL <- c("PLANT", "FUNGUS", "EGGS", "FRUIT", "FRUITS", "GRASS",
               "LEAVES", "SEEDS", "CARRION", "DUMMY")
VERT      <- c("MAMMALIA", "AVES", "SQUAMATA", "ACTINOPTERYGII",
               "AMPHIBIA", "TESTUDINES", "CROCODYLIA", "VERTEBRATA", "AMNIOTA")
SRC <- c(MAMMALIA = "PanTHERIA / EltonTraits-Mammal",
         AVES = "AVONET / EltonTraits-Bird",
         ACTINOPTERYGII = "FishBase",
         SQUAMATA = "Feldman et al. / Meiri squamate mass",
         AMPHIBIA = "AmphiBIO",
         TESTUDINES = "literature / TraitBank",
         CROCODYLIA = "literature",
         INVERT = "invertebrate length-mass regressions",
         VERTEBRATA = "resolve to finer clade",
         AMNIOTA = "resolve to finer clade")

## --- read the wide table ----------------------------------------
T <- read.csv(RAW, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
h <- as.character(unlist(T[3, ])); d <- T[4:nrow(T), ]
gcol <- function(n) which(h == n)
StartCol <- 31

## --- predators ---------------------------------------------------
pred <- data.frame(
	key   = paste(d[[gcol("Genus")]], d[[gcol("Species")]]),
	clade = toupper(d[[gcol("Class")]]),
	genus = d[[gcol("Genus")]],
	epithet = d[[gcol("Species")]],
	mass  = suppressWarnings(as.numeric(d[[gcol("Mass")]])),
	stringsAsFactors = FALSE)
pred <- unique(pred)

## --- prey (columns >= StartCol) ----------------------------------
pg <- as.character(T[1, StartCol:ncol(T)])
ps <- as.character(T[2, StartCol:ncol(T)])
pm <- suppressWarnings(as.numeric(as.character(T[3, StartCol:ncol(T)])))
cells <- suppressWarnings(matrix(as.numeric(as.matrix(d[, StartCol:ncol(T)])), nrow = nrow(d)))
nrec  <- colSums(cells > 0, na.rm = TRUE)   # how many records each prey appears in
prey <- data.frame(
	key   = paste(pg, ps),
	clade = norm_clade(ps),
	genus = pg,
	epithet = ps,
	mass  = pm,
	n_rec = nrec,
	stringsAsFactors = FALSE)
prey <- prey[prey$n_rec > 0, ]                       # only observed prey
# collapse duplicate prey columns sharing a key; sum their record counts.
# (aggregate() with a formula drops NA-mass rows, so do it by hand to keep gaps)
nr <- tapply(prey$n_rec, prey$key, sum)
prey <- prey[!duplicated(prey$key), c("key", "clade", "genus", "epithet", "mass")]
prey$n_rec <- as.integer(nr[prey$key])

## --- PanTHERIA lookups (mammals) ---------------------------------
# NOTE: the local data/pantheria file is malformed -- every data row has 53
# fields against a 52-field header, so the column NAMES are shifted relative to
# the data and cannot be trusted. Read by POSITION instead. Verified
# empirically against known masses (lion 159 kg, elephant 3.94 t, mouse 19 g):
#   col 3 = Genus, col 4 = Species, col 5 = adult body mass (g, -999 = missing).
pan <- read.delim(PAN, header = FALSE, skip = 1, quote = "", check.names = FALSE,
                  colClasses = "character", stringsAsFactors = FALSE)
pan_g <- pan[[3]]; pan_s <- pan[[4]]
pmass <- suppressWarnings(as.numeric(pan[[5]])); pmass[pmass == -999] <- NA
pan_binom <- tapply(pmass, paste(pan_g, pan_s), function(z) mean(z, na.rm = TRUE))
pan_binom <- pan_binom[is.finite(pan_binom)]
pan_genus <- tapply(pmass, pan_g, function(z) mean(z, na.rm = TRUE))
pan_genus <- pan_genus[is.finite(pan_genus)]
message("PanTHERIA (read by position) | binomials w/ mass: ", length(pan_binom),
        " | genera w/ mass: ", length(pan_genus))

## --- AVONET lookups (birds) --------------------------------------
# AVONET1 (BirdLife taxonomy; Tobias et al. 2022): Species1 + Mass (body mass, g),
# ~11,000 spp. Verified vs known masses (sparrow 26g, bald eagle 4.7kg, ostrich 111kg).
AVO <- file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv")
avo_binom <- avo_genus <- setNames(numeric(0), character(0))
if (file.exists(AVO)) {
	av  <- read.csv(AVO, stringsAsFactors = FALSE, check.names = FALSE)
	avm <- suppressWarnings(as.numeric(av$Mass)); keep <- is.finite(avm) & avm > 0
	avsp <- gsub("[ _]+", " ", trimws(av$Species1[keep])); avm <- avm[keep]
	avo_binom <- tapply(avm, avsp, mean);            avo_binom <- avo_binom[is.finite(avo_binom)]
	avo_genus <- tapply(avm, sub(" .*$", "", avsp), mean); avo_genus <- avo_genus[is.finite(avo_genus)]
	message("AVONET (birds) | binomials w/ mass: ", length(avo_binom),
	        " | genera w/ mass: ", length(avo_genus))
} else message("AVONET not found at ", AVO, " -> bird masses NOT filled")

## --- fill missing masses -----------------------------------------
# MAMMALIA -> PanTHERIA, AVES -> AVONET; binomial match first, else genus-mean.
fill_mass <- function(df) {
	df$fill_src  <- NA_character_
	df$fill_mass <- NA_real_
	need <- which(is.na(df$mass))
	for (i in need) {
		cl <- df$clade[i]
		if      (cl == "MAMMALIA") { bmap <- pan_binom; gmap <- pan_genus; src <- "pantheria" }
		else if (cl == "AVES")     { bmap <- avo_binom; gmap <- avo_genus; src <- "avonet"    }
		else next
		g <- df$genus[i]
		# some prey carry the full binomial (underscored) in the genus field,
		# e.g. "Ovis_aries" -> split to recover Genus + epithet.
		bin_cands <- c(paste(g, df$epithet[i]),                 # Genus + tag
		               if (grepl("_", g)) gsub("_", " ", g))     # "Ovis_aries" -> "Ovis aries"
		gen_cand  <- sub("_.*$", "", g)                          # leading token as genus
		hit <- bin_cands[bin_cands %in% names(bmap)]
		if (length(hit)) {
			df$fill_mass[i] <- bmap[[hit[1]]]; df$fill_src[i] <- paste0(src, "_binomial")
		} else if (gen_cand %in% names(gmap)) {
			df$fill_mass[i] <- gmap[[gen_cand]]; df$fill_src[i] <- paste0(src, "_genus_mean")
		}
	}
	df
}
pred <- fill_mass(pred)
prey <- fill_mass(prey)

## --- write overrides (only successful fills) ---------------------
mk_ovr <- function(df) {
	f <- df[!is.na(df$fill_mass), ]
	if (!nrow(f)) return(NULL)
	data.frame(key = f$key, mass_g = round(f$fill_mass, 2), source = f$fill_src,
	           clade = f$clade, stringsAsFactors = FALSE)
}
ovr <- unique(rbind(mk_ovr(pred), mk_ovr(prey)))
ovr <- ovr[!duplicated(ovr$key), ]
write.csv(ovr, file.path(ROOT, "data", "mass_overrides.csv"), row.names = FALSE, quote = TRUE)

## --- write gaps (still missing after fill), classified -----------
gap_rows <- function(df, role) {
	g <- df[is.na(df$mass) & is.na(df$fill_mass), ]
	if (!nrow(g)) return(NULL)
	type <- ifelse(g$clade %in% NONANIMAL, "non-animal (exclude?)",
	        ifelse(g$clade %in% VERT, "vertebrate (look up)",
	        ifelse(g$clade %in% c("INVERT"), "invertebrate", "indet/unknown")))
	data.frame(role = role, key = g$key, clade = g$clade, type = type,
	           n_rec = if ("n_rec" %in% names(g)) g$n_rec else NA_integer_,
	           suggested_source = ifelse(g$clade %in% names(SRC), SRC[g$clade], ""),
	           stringsAsFactors = FALSE)
}
gaps <- rbind(gap_rows(pred, "predator"), gap_rows(prey, "prey"))
gaps <- gaps[order(-ifelse(is.na(gaps$n_rec), 0, gaps$n_rec),
                   gaps$type, gaps$clade), ]
write.csv(gaps, file.path(ROOT, "data", "mass_gaps.csv"), row.names = FALSE, quote = TRUE)

## --- report ------------------------------------------------------
cat("\n================ mass integration summary ================\n")
cat(sprintf("Predators: %d taxa, %d missing mass -> %d filled, %d still missing\n",
            nrow(pred), sum(is.na(pred$mass)), sum(!is.na(pred$fill_mass)),
            sum(is.na(pred$mass) & is.na(pred$fill_mass))))
cat(sprintf("Prey:      %d observed taxa, %d missing mass -> %d filled, %d still missing\n",
            nrow(prey), sum(is.na(prey$mass)), sum(!is.na(prey$fill_mass)),
            sum(is.na(prey$mass) & is.na(prey$fill_mass))))
cat(sprintf("Records recoverable by these fills (prey side): %d\n",
            sum(prey$n_rec[!is.na(prey$fill_mass)])))
cat("\nRemaining prey gaps by clade (records lost):\n")
gp <- prey[is.na(prey$mass) & is.na(prey$fill_mass), ]
agg <- aggregate(n_rec ~ clade, gp, sum)
agg <- agg[order(-agg$n_rec), ]
print(agg, row.names = FALSE)
cat(sprintf("\nWrote data/mass_overrides.csv (%d fills) and data/mass_gaps.csv (%d gaps)\n",
            nrow(ovr), nrow(gaps)))
