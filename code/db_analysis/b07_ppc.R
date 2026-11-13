# b07: posterior predictive check for a b02/b04/b06 fit.
# For every analysis row, the probability integral transform (PIT) of the observed
# ln prey mass under the posterior predictive, averaged over posterior draws, in two forms:
#   cond : the row's own fitted species and source effects (in-sample fit of the line)
#   new  : a new species and a new source (u, w integrated out). This is the form the
#          fossil test uses, so it is the check that matters for fossil placement.
# Targets: median PIT 0.5; share of rows inside the central 80% interval 0.8;
# share below the 10th and above the 90th percentile 0.1 each.
# Also reported species-weighted (each species counts once), because Strigiformes
# pellets and Accipitriformes rows would otherwise dominate.
# Env: FIT_DIR (folder with stan_data.rds and the CmdStan CSVs), ROWS (analysis_rows.csv
#      matching the fit), NDRAW (200).
# Output: <FIT_DIR>/ppc_by_minor.csv, ppc_by_major_obs.csv, ppc_rows.csv.gz
suppressPackageStartupMessages({ library(data.table); library(cmdstanr); library(posterior) })
FIT_DIR <- Sys.getenv("FIT_DIR", "results/6_work/fit_skeleton")
ROWS    <- Sys.getenv("ROWS", "results/6_work/analysis_rows.csv")
NDRAW   <- as.integer(Sys.getenv("NDRAW", "200"))

sd_info <- readRDS(file.path(FIT_DIR, "stan_data.rds"))
csvs <- list.files(FIT_DIR, pattern = "-[0-9]+-[0-9a-f]+\\.csv$", full.names = TRUE)
vars <- c("alpha0", "beta0", "gamma0", "gamma1", "delta", "zeta", "a", "b", "g",
          "tau_u", "tau_w", "z_u", "z_w")
dr <- read_cmdstan_csv(csvs, variables = vars, format = "draws_matrix")$post_warmup_draws
set.seed(7)
dr <- dr[sort(sample(nrow(dr), min(NDRAW, nrow(dr)))), ]
dr <- matrix(as.numeric(dr), nrow(dr), dimnames = list(NULL, colnames(dr)))
col <- function(v) dr[, grep(paste0("^", v, "(\\[|$)"), colnames(dr)), drop = FALSE]

rows <- fread(ROWS)
rows <- rows[minor_clade %in% sd_info$minor_levels]
k <- match(rows$minor_clade, sd_info$minor_levels)
j <- match(rows$pred_taxon_id, sd_info$sp_levels)
r <- match(rows$source_id, sd_info$src_levels)
o <- match(rows$obs, sd_info$obs_levels)
stopifnot(!anyNA(k), !anyNA(j), !anyNA(r), !anyNA(o))

A <- col("a"); B <- col("b"); G <- col("g"); D <- col("delta"); Z <- col("zeta")
U <- col("z_u"); W <- col("z_w")
pit_c <- pit_n <- numeric(nrow(rows))
for (d in seq_len(nrow(dr))) {
  mu0 <- dr[d, "alpha0"] + A[d, k] + (dr[d, "beta0"] + B[d, k]) * rows$m + D[d, o]
  s   <- exp(dr[d, "gamma0"] + G[d, k] + dr[d, "gamma1"] * rows$m + Z[d, o])
  muc <- mu0 + dr[d, "tau_u"] * U[d, j] + dr[d, "tau_w"] * W[d, r]
  pit_c <- pit_c + pnorm(rows$y, muc, s)
  pit_n <- pit_n + pnorm(rows$y, mu0, sqrt(s^2 + dr[d, "tau_u"]^2 + dr[d, "tau_w"]^2))
}
rows[, `:=`(pit_cond = pit_c / nrow(dr), pit_new = pit_n / nrow(dr))]

summ <- function(x) {
  f <- function(p) list(median_pit = median(p), in80 = mean(p > 0.1 & p < 0.9),
                        below10 = mean(p <= 0.1), above90 = mean(p >= 0.9))
  sp <- x[, .(pc = median(pit_cond), pn = median(pit_new),
              cc = mean(pit_cond > 0.1 & pit_cond < 0.9), cn = mean(pit_new > 0.1 & pit_new < 0.9)),
          by = pred_taxon_id]
  c(list(rows = nrow(x), species = uniqueN(x$pred_taxon_id), sources = uniqueN(x$source_id)),
    setNames(f(x$pit_cond), paste0("cond_", names(f(0.5)))),
    setNames(f(x$pit_new),  paste0("new_",  names(f(0.5)))),
    list(spw_new_median_pit = median(sp$pn), spw_new_in80 = mean(sp$cn)))
}
by_minor <- rows[, summ(.SD), by = .(major_clade, minor_clade)][order(major_clade, minor_clade)]
by_mo    <- rows[, summ(.SD), by = .(major_clade, obs)][order(major_clade, obs)]
overall  <- rows[, summ(.SD)]
fwrite(by_minor, file.path(FIT_DIR, "ppc_by_minor.csv"))
fwrite(by_mo,    file.path(FIT_DIR, "ppc_by_major_obs.csv"))
fwrite(rows[, .(pair_id, pred_taxon_id, source_id, minor_clade, obs, pit_cond, pit_new)],
       file.path(FIT_DIR, "ppc_rows.csv.gz"))

options(width = 200)
cat("FIT:", FIT_DIR, "| draws:", nrow(dr), "\n")
cat("overall:\n"); print(as.data.table(overall), digits = 3)
num <- function(x) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)]
cat("\nby minor clade:\n"); print(num(by_minor))
cat("\nby major clade x observation type:\n"); print(num(by_mo))
