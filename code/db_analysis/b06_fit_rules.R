# b06: fit the skeleton-tree model under one pooling rule (see b06_rules.stan).
# Usage: RULE=free|lambda0|lambda1|unpooled|pooled Rscript code/db_analysis/b06_fit_rules.R
# Env: WORK (default results/6_work/terr; must hold analysis_cells.csv and skeleton_corr.csv
#      from b05 and b03), ITER (2000, half warm-up), CHAINS (4), PAR_CHAINS (4).
# Output: <WORK>/fit_<RULE>/ with fit.rds, stan_data.rds, summary.csv, diagnostics.txt
suppressPackageStartupMessages({ library(data.table); library(cmdstanr); library(posterior) })
RULE   <- Sys.getenv("RULE", "free")
WORK   <- Sys.getenv("WORK", "results/6_work/terr")
ITER   <- as.integer(Sys.getenv("ITER", "2000"))
CHAINS <- as.integer(Sys.getenv("CHAINS", "4"))
PAR    <- as.integer(Sys.getenv("PAR_CHAINS", "4"))
out <- file.path(WORK, paste0("fit_", RULE))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

rules <- list(
  free     = list(est_tau = 1L, tau_fixed = c(1, 1, 1), est_lambda = 1L, lambda_fixed = c(0, 0, 0), est_global = 1L),
  lambda0  = list(est_tau = 1L, tau_fixed = c(1, 1, 1), est_lambda = 0L, lambda_fixed = c(0, 0, 0), est_global = 1L),
  lambda1  = list(est_tau = 1L, tau_fixed = c(1, 1, 1), est_lambda = 0L, lambda_fixed = c(1, 1, 1), est_global = 1L),
  unpooled = list(est_tau = 0L, tau_fixed = c(2, 0.5, 1), est_lambda = 0L, lambda_fixed = c(0, 0, 0), est_global = 0L),
  pooled   = list(est_tau = 0L, tau_fixed = c(0, 0, 0), est_lambda = 0L, lambda_fixed = c(0, 0, 0), est_global = 1L)
)
stopifnot(RULE %in% names(rules))

cells <- fread(file.path(WORK, "analysis_cells.csv"))
obs_levels <- c("stomach_content", setdiff(sort(unique(cells$obs)), "stomach_content"))
minor_levels <- cells[, .N, by = .(major_clade, minor_clade)][order(major_clade, minor_clade), minor_clade]
major_of <- cells[, .N, by = .(major_clade, minor_clade)][order(major_clade, minor_clade), major_clade]
sp_levels  <- sort(unique(cells$pred_taxon_id))
src_levels <- sort(unique(cells$source_id))
Cm <- as.matrix(fread(file.path(WORK, "skeleton_corr.csv")), rownames = "minor_clade")
stopifnot(setequal(rownames(Cm), minor_levels))

sdat <- c(list(
  C = nrow(cells), K = length(minor_levels), J = length(sp_levels),
  R = length(src_levels), O = length(obs_levels),
  n = cells$n, ybar = cells$ybar, SS = cells$SS, m = cells$m,
  kk = match(cells$minor_clade, minor_levels), jj = match(cells$pred_taxon_id, sp_levels),
  rr = match(cells$source_id, src_levels), oo = match(cells$obs, obs_levels),
  Ctree = Cm[minor_levels, minor_levels],
  global_fixed = c(log(50), 1, 0), prior_only = 0L), rules[[RULE]])
saveRDS(list(sdat = sdat, rule = RULE, obs_levels = obs_levels, minor_levels = minor_levels,
             major_of = major_of, sp_levels = sp_levels, src_levels = src_levels),
        file.path(out, "stan_data.rds"))

mod <- cmdstan_model("code/db_analysis/b06_rules.stan", dir = "results/6_work")
fit <- mod$sample(data = sdat, chains = CHAINS, parallel_chains = PAR,
                  iter_warmup = ITER %/% 2, iter_sampling = ITER %/% 2,
                  seed = 20261112, adapt_delta = 0.9, max_treedepth = 12,
                  refresh = 200, output_dir = out)
fit$save_object(file.path(out, "fit.rds"))

pars <- c("alpha0", "beta0", "gamma0", "gamma1", "delta", "zeta", "tau_a", "tau_b", "tau_g",
          "tau_u", "tau_w", "lambda_a", "lambda_b", "lambda_g",
          "clade_intercept", "clade_slope", "clade_sigma")
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
line <- sprintf("rule %s | max rhat %.4f | min ess_bulk %.0f | min ess_tail %.0f | divergences %s | treedepth hits %s | time %.0f s",
                RULE, max(all_sm$rhat, na.rm = TRUE), min(all_sm$ess_bulk, na.rm = TRUE),
                min(all_sm$ess_tail, na.rm = TRUE), paste(diag$num_divergent, collapse = ","),
                paste(diag$num_max_treedepth, collapse = ","), fit$time()$total)
cat("\n", line, "\n")
writeLines(c(line, capture.output(print(diag), print(fit$time()))), file.path(out, "diagnostics.txt"))
cat("DONE\n")
