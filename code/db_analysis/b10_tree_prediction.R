# b10: what the tree predicts for a fossil tip before any fossil is looked at (no refit).
# For each tree-placed tip in code/db_analysis/fossil_tree_tips.csv (primary and sensitivity
# ages), plus reference candidates (each major clade at its crown, "none", and the minor
# clades named in REF_MINOR), draw the clade line from the fitted model and report, on a grid
# of predator mass, at observation type stomach_content:
#   line_*  : the clade mean line, ln prey = alpha0 + A + (beta0 + B) m (median, 10%, 90%)
#   pred_*  : prey mass for a new species from a new source (10/50/90% predictive quantiles)
# Also the borrowing weights: for each tip, the coefficient on each fitted minor clade's
# effect in the conditional mean (intercept, slope, spread), posterior median.
# Env: FIT_DIR, WORK, NDRAW (400), NINNER (100), REF_MINOR (comma list).
# Output: <FIT_DIR>/tree_prediction/{lines.csv, params.csv, weights.csv}
source("code/db_analysis/b08_fossil_lib.R")
FIT_DIR <- Sys.getenv("FIT_DIR", "results/6_work/terr/fit_free")
WORK    <- Sys.getenv("WORK", "results/6_work/terr")
NDRAW   <- as.integer(Sys.getenv("NDRAW", "400"))
NINNER  <- as.integer(Sys.getenv("NINNER", "100"))
REF_MINOR <- strsplit(Sys.getenv("REF_MINOR", "Accipitriformes,Strigiformes,Falconiformes,Crocodylia,Varanidae,Feliformia,Caniformia"), ",")[[1]]
out <- file.path(FIT_DIR, "tree_prediction"); dir.create(out, recursive = TRUE, showWarnings = FALSE)

F <- load_fit(FIT_DIR, WORK, NDRAW)
cands <- build_candidates(F, fread("code/db_analysis/fossil_tree_tips.csv"))
keep <- Filter(function(x) x$type %in% c("tree", "tree_sens", "major", "none") || (x$type == "minor" && x$id %in% REF_MINOR), cands)
grid_g <- exp(seq(log(10), log(1e7), length.out = 61)); mg <- log(grid_g) - log(1000)
dr <- F$dr; ND <- nrow(dr); M <- NINNER
qs <- function(x, p) apply(x, 2, quantile, p)

lines <- list(); params <- list(); weights <- list()
for (cn in keep) {
  set.seed(10)
  LM <- matrix(NA, ND * M, length(mg)); PY <- LM; PAR <- matrix(NA, ND * M, 3)
  for (d in seq_len(ND)) {
    idx <- (d - 1) * M + seq_len(M)
    if (cn$type == "minor") {
      Ae <- rep(F$A[d, cn$k], M); Be <- rep(F$B[d, cn$k], M); Ge <- rep(F$G[d, cn$k], M)
    } else {
      tc <- tip_conditional(F, d, cn$cvec)
      Ae <- tc[1, 1] + tc[2, 1] * rnorm(M); Be <- tc[1, 2] + tc[2, 2] * rnorm(M); Ge <- tc[1, 3] + tc[2, 3] * rnorm(M)
    }
    lm <- outer(dr[d, "alpha0"] + Ae, rep(1, length(mg))) + outer(dr[d, "beta0"] + Be, mg)
    s  <- exp(outer(dr[d, "gamma0"] + Ge, rep(1, length(mg))) + dr[d, "gamma1"] * matrix(mg, M, length(mg), byrow = TRUE))
    sd_new <- sqrt(s^2 + dr[d, "tau_u"]^2 + dr[d, "tau_w"]^2)
    LM[idx, ] <- lm
    PY[idx, ] <- lm + sd_new * matrix(rnorm(M * length(mg)), M)
    PAR[idx, ] <- cbind(dr[d, "alpha0"] + Ae, dr[d, "beta0"] + Be, exp(dr[d, "gamma0"] + Ge))
  }
  lines[[cn$id]] <- data.table(candidate = cn$id, cand_type = cn$type, pred_g = grid_g,
    line_med = exp(qs(LM, 0.5)), line_lo = exp(qs(LM, 0.1)), line_hi = exp(qs(LM, 0.9)),
    pred_q10 = exp(qs(PY, 0.1)), pred_q50 = exp(qs(PY, 0.5)), pred_q90 = exp(qs(PY, 0.9)))
  params[[cn$id]] <- data.table(candidate = cn$id, cand_type = cn$type,
    par = c("ln_prey_at_1kg", "slope", "sigma_at_1kg"),
    median = apply(PAR, 2, median), q10 = apply(PAR, 2, quantile, 0.1), q90 = apply(PAR, 2, quantile, 0.9))
  if (cn$type %in% c("tree", "tree_sens")) {
    W <- array(NA, c(ND, F$K, 3))
    for (d in seq_len(ND)) for (e in 1:3) {
      lam <- dr[d, c("lambda_a", "lambda_b", "lambda_g")[e]]
      W[d, , e] <- solve(lam * F$Cm + (1 - lam) * diag(F$K), lam * cn$cvec)
    }
    weights[[cn$id]] <- data.table(candidate = cn$id, minor_clade = F$info$minor_levels,
      major_clade = F$info$major_of, shared_history = cn$cvec,
      w_intercept = apply(W[, , 1], 2, median), w_slope = apply(W[, , 2], 2, median), w_spread = apply(W[, , 3], 2, median))
  }
}
lines <- rbindlist(lines); params <- rbindlist(params); weights <- rbindlist(weights)
fwrite(lines, file.path(out, "lines.csv")); fwrite(params, file.path(out, "params.csv")); fwrite(weights, file.path(out, "weights.csv"))

options(width = 160)
cat("FIT:", FIT_DIR, "\n")
print(dcast(params, candidate + cand_type ~ par, value.var = "median")[order(cand_type)], digits = 3)
cat("\nintervals (10-90%):\n")
print(params[, .(candidate, par, txt = sprintf("%.2f [%.2f, %.2f]", median, q10, q90))][, dcast(.SD, candidate ~ par, value.var = "txt")])
cat("\nborrowing weights, primary theropod tip:\n")
print(weights[candidate == "tree:Theropoda@165", .(minor_clade, shared_history = round(shared_history, 3),
      w_intercept = round(w_intercept, 3), w_slope = round(w_slope, 3), w_spread = round(w_spread, 3))])
cat("\nlambda posterior median (a, b, g):", round(apply(dr[, c("lambda_a", "lambda_b", "lambda_g")], 2, median), 3), "\n")
cat("DONE\n")
