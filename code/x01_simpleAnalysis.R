###################################################################
# x01_simpleAnalysis.R
# Model the per-item predator-prey mass ratio (ppreymass) as a function
# of predator mass, with nested taxonomic and methodological grouping.
#
# Run x00_reformatData.R first to produce data/longform.csv.
###################################################################

## --- locate repo root so the script runs from anywhere ----------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
	script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
	ROOT <- normalizePath(file.path(script_dir, ".."))
} else {
	ROOT <- normalizePath(getwd())
}
LONGFORM <- file.path(ROOT, "data", "longform.csv")

outMat <- read.csv(LONGFORM, stringsAsFactors = FALSE)
outMat$study <- factor(outMat$study)

# center predmass: raw log-grams put the intercept at a 1 g predator (pure
# extrapolation) and induce a strong slope/intercept correlation that drove
# the treedepth/funnel pathology in the first fits. Centering fixes both.
outMat$predmass_c <- outMat$predmass - mean(outMat$predmass)

########## NON-BAYESIAN (lme4) ####################################
suppressPackageStartupMessages(library(lme4))
sMat <- outMat[order(as.character(outMat$genus)), ]
Model <- lmer(
	ppreymass ~ predmass_c + (1 | clade/subclade/species) + (1 | obstype) + (1 | study),
	data    = sMat,
	control = lmerControl(optimizer = "nloptwrap")
)
cat("\n================ lme4 summary ================\n")
print(summary(Model))

########## SIMPLE BAYESIAN (brms) #################################
# Compiles a Stan model; can take several minutes the first time.
# Set RUN_BRMS=TRUE in the environment to enable.
if (isTRUE(as.logical(Sys.getenv("RUN_BRMS", "FALSE")))) {
	suppressPackageStartupMessages(library(brms))
	# Student-t response: PPMR has heavy tails (min ~ -15) and is mixture-like
	# across observation modes; gaussian over-weights the tails.
	bform <- bf(ppreymass ~ predmass_c + (1 | clade) + (1 | study) + (1 | obstype))
	Prior <- c(
		prior(student_t(3, 0, 5), class = "Intercept"),
		prior(student_t(3, 0, 5), class = "b"),
		prior(student_t(3, 0, 2.5), class = "sd")   # tighter prior; cauchy(0,5) caused warmup funnel on few-level groups
	)
	# cores: parallel chains FORK, which hangs in the Claude sandbox (chains
	# freeze at warmup iter 1). Single-threaded sampling works fine. So default
	# to 4 cores (your own machine) but allow STAN_CORES=1 in the sandbox.
	NCORES <- as.integer(Sys.getenv("STAN_CORES", "4"))
	fit1 <- brm(
		bform, data = outMat, family = student(),
		chains = 4, warmup = 1000, iter = 3000,   # 2000 post-warmup draws/chain
		control = list(max_treedepth = 14, adapt_delta = 0.95),
		prior = Prior, cores = NCORES
	)
	cat("\n================ brms summary ================\n")
	print(summary(fit1))
	saveRDS(fit1, file.path(ROOT, "data", "fit_simple.rds"))
}
