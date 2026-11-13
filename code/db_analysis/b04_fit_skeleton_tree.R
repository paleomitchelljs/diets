# b04: as b02, but minor-clade effects are correlated along the skeleton tree from b03
# (lambda in [0,1] per effect: 0 = exchangeable clades as in b02, 1 = Brownian motion).
# Input: results/6_work/analysis_cells.csv (from b01). Output: results/6_work/fit_skeleton/
# Env: ITER (default 2000, half warm-up), CHAINS (4), PAR_CHAINS (4; set 1 if chains hang).
suppressPackageStartupMessages({ library(data.table); library(cmdstanr); library(posterior) })
ITER   <- as.integer(Sys.getenv("ITER", "2000"))
CHAINS <- as.integer(Sys.getenv("CHAINS", "4"))
PAR    <- as.integer(Sys.getenv("PAR_CHAINS", "4"))
out <- Sys.getenv("OUT_DIR", "results/6_work/fit_skeleton")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

cells <- fread("results/6_work/analysis_cells.csv")
obs_levels <- c("stomach_content", setdiff(sort(unique(cells$obs)), "stomach_content"))
minor_levels <- cells[, .N, by = .(major_clade, minor_clade)][order(major_clade, minor_clade), minor_clade]
sp_levels  <- sort(unique(cells$pred_taxon_id))
src_levels <- sort(unique(cells$source_id))

sdat <- list(
  C = nrow(cells), K = length(minor_levels), J = length(sp_levels),
  R = length(src_levels), O = length(obs_levels),
  n = cells$n, ybar = cells$ybar, SS = cells$SS, m = cells$m,
  kk = match(cells$minor_clade, minor_levels), jj = match(cells$pred_taxon_id, sp_levels),
  rr = match(cells$source_id, src_levels), oo = match(cells$obs, obs_levels),
  prior_only = 0L
)
Cm <- as.matrix(fread("results/6_work/skeleton_corr.csv"), rownames = "minor_clade")
stopifnot(setequal(rownames(Cm), minor_levels))
sdat$Ctree <- Cm[minor_levels, minor_levels]
saveRDS(list(sdat = sdat, obs_levels = obs_levels, minor_levels = minor_levels,
             sp_levels = sp_levels, src_levels = src_levels), file.path(out, "stan_data.rds"))

mod <- cmdstan_model("code/db_analysis/b04_skeleton_tree.stan", dir = "results/6_work")
fit <- mod$sample(data = sdat, chains = CHAINS, parallel_chains = PAR,
                  iter_warmup = ITER %/% 2, iter_sampling = ITER %/% 2,
                  seed = 20261112, adapt_delta = 0.9, max_treedepth = 12,
                  refresh = 100, output_dir = out)
fit$save_object(file.path(out, "fit.rds"))

pars <- c("alpha0", "beta0", "gamma0", "gamma1", "delta", "zeta", "tau_a", "tau_b", "tau_g",
          "tau_u", "tau_w", "lambda_a", "lambda_b", "lambda_g", "clade_intercept", "clade_slope", "clade_sigma")
sm <- as.data.table(fit$summary(pars))
sm[, label := variable]
for (v in c("clade_intercept", "clade_slope", "clade_sigma"))
  sm[grepl(paste0("^", v, "\\["), variable),
     label := paste0(v, ":", minor_levels[as.integer(gsub("\\D", "", variable))])]
for (v in c("delta", "zeta"))
  sm[grepl(paste0("^", v, "\\["), variable),
     label := paste0(v, ":", obs_levels[as.integer(gsub("\\D", "", variable))])]
fwrite(sm, file.path(out, "summary.csv"))

all_sm <- fit$summary(NULL, "rhat", "ess_bulk", "ess_tail")
diag <- fit$diagnostic_summary()
cat("\n== diagnostics ==\n")
cat(sprintf("max rhat %.4f | min ess_bulk %.0f | min ess_tail %.0f\n",
            max(all_sm$rhat, na.rm = TRUE), min(all_sm$ess_bulk, na.rm = TRUE),
            min(all_sm$ess_tail, na.rm = TRUE)))
cat("divergences per chain:", diag$num_divergent, "| max treedepth hits:", diag$num_max_treedepth, "\n")
cat("total time (s):", round(fit$time()$total), "\n")
writeLines(capture.output(print(diag), print(fit$time())), file.path(out, "diagnostics.txt"))
print(sm[, .(label, median, q5, q95, rhat, ess_bulk)], nrows = 200)
cat("DONE\n")
