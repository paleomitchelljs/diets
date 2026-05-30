###################################################################
# x00_reformatData.R
# Convert the wide diet matrix (data/raw_data.csv) into long format.
#
# For diet:
#   per-prey-item ratio = log(prey mass) - log(predator mass)   [ "ppreymass" ]
# Goal: characterise the shape of that distribution and estimate it
# at every level (study, species, genus, subclade, clade, all predators),
# then ask where extinct taxa (gut contents / coprolites) fall.
#
# Input layout of data/raw_data.csv (read with header=FALSE):
#   row 1            : prey genus           (cols >= StartCol)
#   row 2            : prey species         (cols >= StartCol)
#   row 3            : field names (cols 1..StartCol-1) AND prey mass (cols >= StartCol)
#   rows 4..N        : one predator diet record each
#   cols 1..StartCol-1 : predator metadata (RefNo, Journal, ... ObsType, Class, Clade,
#                        Genus, Species, Mass)
#   cols >= StartCol   : integer counts of each prey taxon in that record
#
# Column semantics carried forward (kept consistent with x01):
#   clade    <- Class  (Aves / Mammalia / Squamata / Crocodylia ...)
#   subclade <- Clade  (order, e.g. Feliformes / Accipitriformes ...)
###################################################################

## --- locate repo root so the script runs from anywhere ----------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
	ROOT <- normalizePath(file.path(script_dir, ".."))
} else {
	ROOT <- normalizePath(getwd())          # fallback: assume launched at repo root
}
DATA_IN  <- file.path(ROOT, "data", "raw_data.csv")
DATA_OUT <- file.path(ROOT, "data", "longform.csv")

####################################################################
# Expand a vector of prey counts into one entry per individual prey item.
# preycount[i] copies of preymasses[i]. Prey with NA mass, NA count, or
# count <= 0 are dropped.
####################################################################
genDistro <- function(preycount, preymasses) {
	preycount  <- suppressWarnings(as.numeric(preycount))
	preymasses <- suppressWarnings(as.numeric(preymasses))
	keep <- !is.na(preycount) & !is.na(preymasses) & preycount > 0
	if (!any(keep)) return(numeric(0))
	rep(preymasses[keep], times = preycount[keep])
}

####################################################################
Table <- read.csv(DATA_IN, header = FALSE, stringsAsFactors = FALSE,
                  check.names = FALSE)
StartCol <- 31

hdr <- as.character(unlist(Table[3, ]))   # field names (left) + prey masses (right)
col <- function(name) {
	w <- which(hdr == name)
	if (length(w) != 1) stop("expected exactly one '", name, "' column; found ", length(w))
	w
}

dataRows <- 4:nrow(Table)
Nobs    <- Table[dataRows, col("Nobservations")]
ObsType <- Table[dataRows, col("ObsType")]
Class   <- Table[dataRows, col("Class")]
Order   <- Table[dataRows, col("Clade")]
Genus   <- Table[dataRows, col("Genus")]
Species_epithet <- apply(Table[dataRows, c(col("Genus"), col("Species"))], 1,
                         paste, collapse = "_")
PredMass <- suppressWarnings(as.numeric(Table[dataRows, col("Mass")]))
RefN     <- Table[dataRows, 1]

PreyMass    <- suppressWarnings(as.numeric(hdr[StartCol:ncol(Table)]))
PreyGenus   <- as.character(Table[1, StartCol:ncol(Table)])
PreySpecies <- as.character(Table[2, StartCol:ncol(Table)])
PreySp      <- paste(PreyGenus, PreySpecies, sep = "_")

# prey-count block as a numeric matrix (one row per predator record)
Counts <- suppressWarnings(matrix(as.numeric(as.matrix(Table[dataRows, StartCol:ncol(Table)])),
                                  nrow = length(dataRows)))

## --- optional: apply curated/derived mass fills (from x03) -------
# data/mass_overrides.csv is a reviewable side table (key, mass_g, ...).
# It only ever FILLS missing masses; existing values are never overwritten.
# Delete the file to revert to raw masses. Keys are "Genus species-field".
OVR <- file.path(ROOT, "data", "mass_overrides.csv")
if (file.exists(OVR)) {
	ov <- read.csv(OVR, stringsAsFactors = FALSE)
	lut <- setNames(suppressWarnings(as.numeric(ov$mass_g)), ov$key)
	predKey <- paste(Genus, Table[dataRows, col("Species")])
	preyKey <- paste(PreyGenus, PreySpecies)
	nP <- sum(is.na(PredMass) & predKey %in% names(lut))
	nQ <- sum(is.na(PreyMass) & preyKey %in% names(lut))
	hitP <- is.na(PredMass) & predKey %in% names(lut)
	hitQ <- is.na(PreyMass) & preyKey %in% names(lut)
	PredMass[hitP] <- lut[predKey[hitP]]
	PreyMass[hitQ] <- lut[preyKey[hitQ]]
	message(sprintf("Applied mass_overrides.csv: filled %d predator + %d prey masses", nP, nQ))
}

## --- data-quality report ----------------------------------------
badMass <- which(is.na(PredMass))
if (length(badMass)) {
	message(sprintf("WARNING: %d records dropped for non-numeric predator Mass (e.g. 'GET' placeholders):",
	                length(badMass)))
	message(paste0("  ", RefN[badMass], " ", Species_epithet[badMass],
	               " [Mass=", Table[dataRows, col("Mass")][badMass], "]", collapse = "\n"))
}
message(sprintf("Prey taxa with no mass (counts ignored): %d of %d",
                sum(is.na(PreyMass)), length(PreyMass)))

####################################################################
# Build long-format data, one row per individual prey item.
####################################################################
pieces <- vector("list", length(dataRows))
Means  <- rep(NA_real_, length(dataRows))
SDs    <- rep(NA_real_, length(dataRows))

for (j in seq_along(dataRows)) {
	if (is.na(PredMass[j])) next
	preyMassList <- genDistro(Counts[j, ], PreyMass)
	if (!length(preyMassList)) next

	logPrey <- log(preyMassList)
	logPred <- log(PredMass[j])
	pPrey   <- logPrey - logPred

	Means[j] <- mean(pPrey)
	SDs[j]   <- sd(pPrey)

	pieces[[j]] <- data.frame(
		preyMass   = logPrey,
		ppreymass  = pPrey,
		predmass   = logPred,
		species    = Species_epithet[j],
		genus      = Genus[j],
		subclade   = Order[j],
		clade      = Class[j],
		obstype    = ObsType[j],
		study      = factor(RefN[j]),
		stringsAsFactors = FALSE
	)
}

outMat <- do.call(rbind, pieces)

####################################################################
write.csv(outMat, DATA_OUT, quote = FALSE, row.names = FALSE)
message(sprintf("Wrote %d prey-item rows from %d usable records -> %s",
                nrow(outMat), sum(!vapply(pieces, is.null, logical(1))), DATA_OUT))
