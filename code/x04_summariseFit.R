###################################################################
# x04_summariseFit.R
# Summarise a fitted brms model: population effects, group-level SDs with
# convergence diagnostics (Rhat / Bulk_ESS), residual sigma, and -- when a
# phylogenetic effect is present -- the phylogenetic signal
# (phylo variance / (phylo + non-phylo species variance), a lambda-like ratio).
#
# Usage:
#   Rscript code/x04_summariseFit.R data/fit_phylo.rds
#   Rscript code/x04_summariseFit.R data/fit_simple.rds
###################################################################

suppressWarnings(suppressPackageStartupMessages(library(brms)))

a <- commandArgs(trailingOnly = TRUE)
fit_path <- if (length(a)) a[1] else "data/fit_phylo.rds"
if (!file.exists(fit_path)) stop("fit not found: ", fit_path)

f <- readRDS(fit_path)
s <- summary(f)

cat("=== fit:", fit_path, "===\n")
cat(sprintf("chains: %d | post-warmup draws/chain: %d | total: %d\n\n",
            s$chains, s$iter - s$warmup, s$chains * (s$iter - s$warmup)))

cols <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Rhat", "Bulk_ESS")
keep <- function(m) round(m[, intersect(cols, colnames(m)), drop = FALSE], 3)

cat("--- Population (fixed) effects ---\n"); print(keep(s$fixed))
cat("\n--- Group-level SDs ---\n")
for (g in names(s$random)) { cat(g, ":\n"); print(keep(s$random[[g]])); cat("\n") }
cat("--- Residual sigma ---\n"); print(keep(s$spec_pars))

## phylogenetic signal, if a phylo group SD was estimated
sd_of <- function(grp) if (!is.null(s$random[[grp]])) s$random[[grp]]["sd(Intercept)", "Estimate"] else NA_real_
v_phylo <- sd_of("phylo")^2
v_sp    <- sd_of("species")^2
if (is.finite(v_phylo) && is.finite(v_sp)) {
	cat(sprintf("\n--- Phylogenetic signal ---\n  lambda-like = var(phylo)/(var(phylo)+var(species)) = %.3f\n",
	            v_phylo / (v_phylo + v_sp)))
}

## flag any non-converged parameters
rh <- rhat(f); bad <- rh[is.finite(rh) & rh > 1.05]
if (length(bad)) {
	cat(sprintf("\n!! %d parameters with Rhat > 1.05 (worst %.2f): %s\n",
	            length(bad), max(bad), paste(head(names(sort(bad, decreasing = TRUE)), 6), collapse = ", ")))
} else {
	cat("\nAll Rhat <= 1.05.\n")
}
