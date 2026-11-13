# b09: calibration of the fossil similarity test (no refit).
# (a) Design simulation: for each candidate in turn as the truth, simulate data at the real
#     design of a fossil unit (same taxa, records per taxon, specimens, predator-mass
#     distributions, observation types, record classes and interval widths), score the
#     simulated unit against every candidate, and tabulate how often each candidate ranks
#     first, and the mean relative support for the truth.
#     Simulation: draw a posterior draw, the truth's clade effects, new species and source
#     effects, the true predator mass of each specimen from its stated distribution, then
#     the true ln prey. A point is observed with its stated se; a bracket keeps its width
#     and is placed so the truth falls uniformly inside it.
# (b) Thinned extant samples: for each extant minor clade with at least 15 species, draw
#     samples of NSP species with 1-4 records each (stomach contents where the clade has at
#     least NSP species with them), give predator and prey masses the median fossil
#     uncertainty, and score them against all candidates with the full-data posterior.
#     Optimistic: those species helped fit their own clade.
# Env: FIT_DIR, WORK, UNIT (Theropoda), NREP (50), NDRAW (100), NINNER (100), CORES (4),
#      PART (a | b | ab), NSP (11), NTHIN (50)
# Output: <FIT_DIR>/calibration/{design_<UNIT>.csv, design_<UNIT>_summary.csv, thinned.csv, thinned_summary.csv}
source("code/db_analysis/b08_fossil_lib.R")
library(parallel)
FIT_DIR <- Sys.getenv("FIT_DIR", "results/6_work/terr/fit_free")
WORK    <- Sys.getenv("WORK", "results/6_work/terr")
UNIT    <- Sys.getenv("UNIT", "Theropoda")
NREP    <- as.integer(Sys.getenv("NREP", "50"))
NDRAW   <- as.integer(Sys.getenv("NDRAW", "100"))
NINNER  <- as.integer(Sys.getenv("NINNER", "100"))
CORES   <- as.integer(Sys.getenv("CORES", "4"))
PART    <- Sys.getenv("PART", "ab")
NSP     <- as.integer(Sys.getenv("NSP", "11"))
NTHIN   <- as.integer(Sys.getenv("NTHIN", "50"))
out <- file.path(FIT_DIR, "calibration"); dir.create(out, recursive = TRUE, showWarnings = FALSE)

F <- load_fit(FIT_DIR, WORK, NDRAW)
tt <- fread("code/db_analysis/fossil_tree_tips.csv")
cands_all <- build_candidates(F, tt)
cands <- Filter(function(x) x$type %in% c("minor", "major", "none") || (x$type == "tree" && x$group == UNIT), cands_all)
tipcs <- precompute_tips(F, cands)
cid <- sapply(cands, `[[`, "id"); ctype <- sapply(cands, `[[`, "type"); cmaj <- sapply(cands, function(x) x$major)

score_ids <- function(u, seed) {
  sc <- score_unit(F, u, cands, tipcs, M = NINNER, seed = seed)
  apply(sc$L, 2, logmeanexp)
}
rank_summary <- function(lpd, truth_minor, truth_major) {
  mi <- which(ctype == "minor"); mj <- which(ctype %in% c("major", "none"))
  pm <- softmax(lpd[mi]); pj <- softmax(lpd[mj])
  list(top_minor = cid[mi][which.max(lpd[mi])], top_major = cid[mj][which.max(lpd[mj])],
       p_true_minor = if (is.na(truth_minor)) NA_real_ else pm[cid[mi] == truth_minor],
       p_true_major = if (is.na(truth_major)) NA_real_ else pj[cid[mj] == truth_major],
       p_none = pj[cid[mj] == "none"])
}

## ---- (a) design simulation ----------------------------------------------------------
if (grepl("a", PART)) {
  recs <- fossil_table()[!startsWith(record_class, "excluded") & group == UNIT]
  u0 <- pack_unit(recs, F$info$obs_levels)
  simulate_unit <- function(truth, seed) {
    set.seed(seed)
    d <- sample(nrow(F$dr), 1); u <- u0; dr <- F$dr
    if (truth$type == "minor") { ef <- c(F$A[d, truth$k], F$B[d, truth$k], F$G[d, truth$k]) }
    else { tc <- tip_conditional(F, d, truth$cvec); ef <- tc[1, ] + tc[2, ] * rnorm(3) }
    m_true <- draw_masses(u, matrix(rnorm(u$P), 1))[1, u$spec]
    uu <- dr[d, "tau_u"] * rnorm(u$T)[u$tax]; ww <- dr[d, "tau_w"] * rnorm(u$S)[u$src]
    mu <- dr[d, "alpha0"] + ef[1] + uu + ww + (dr[d, "beta0"] + ef[2]) * m_true + F$D[d, u$o]
    s  <- exp(dr[d, "gamma0"] + ef[3] + dr[d, "gamma1"] * m_true + F$Z[d, u$o])
    y  <- rnorm(u$n, mu, s)
    width <- u$y_hi - u$y_lo
    for (i in seq_len(u$n)) switch(u$cls[i],
      point   = { u$y[i] <- y[i] + rnorm(1, 0, u$y_se[i]) },
      bracket = { u$y_lo[i] <- y[i] - runif(1) * width[i]; u$y_hi[i] <- u$y_lo[i] + width[i] },
      upper   = { u$y_hi[i] <- y[i] + abs(rnorm(1)) })
    u
  }
  truths <- cands
  jobs <- expand.grid(t = seq_along(truths), r = seq_len(NREP))
  t0 <- Sys.time()
  res <- mclapply(seq_len(nrow(jobs)), function(ji) {
    tr <- truths[[jobs$t[ji]]]; seed <- 7e5 + ji
    u <- simulate_unit(tr, seed)
    lpd <- score_ids(u, seed)
    tmin <- if (tr$type == "minor") tr$id else NA_character_
    tmaj <- if (tr$type == "minor") paste0("major:", tr$major) else if (tr$type %in% c("major", "none")) tr$id else NA_character_
    c(list(truth = tr$id, truth_type = tr$type, rep = jobs$r[ji]), rank_summary(lpd, tmin, tmaj),
      as.list(setNames(lpd, cid)))
  }, mc.cores = CORES, mc.preschedule = FALSE)
  bad <- sapply(res, inherits, "try-error"); if (any(bad)) { print(res[[which(bad)[1]]]); stop("worker errors") }
  cat(sprintf("(a) %d simulations in %.1f min\n", length(res), as.numeric(Sys.time() - t0, units = "mins")))
  A <- rbindlist(res, fill = TRUE)
  fwrite(A, file.path(out, sprintf("design_%s.csv", UNIT)))
  S <- A[, .(n = .N,
             top_minor_correct = if (truth_type[1] == "minor") mean(top_minor == truth) else NA_real_,
             top_major_correct = mean(top_major == fifelse(truth_type == "minor", paste0("major:", sapply(truth, function(x) cands[[match(x, cid)]]$major)), truth)),
             mean_p_true_minor = mean(p_true_minor), mean_p_true_major = mean(p_true_major),
             none_wins = mean(top_major == "none"), mean_p_none = mean(p_none)),
         by = .(truth, truth_type)]
  fwrite(S, file.path(out, sprintf("design_%s_summary.csv", UNIT)))
  options(width = 160); cat("\n(a) design simulation at the", UNIT, "design:\n"); print(S, digits = 3)
  cat("\nconfusion (rows truth major-or-none, columns top major-or-none):\n")
  A[, truth_major := fifelse(truth_type == "minor", paste0("major:", sapply(truth, function(x) cands[[match(x, cid)]]$major)), truth)]
  print(dcast(A[truth_type != "tree"], truth_major ~ top_major, fun.aggregate = length))
}

## ---- (b) thinned extant samples -----------------------------------------------------
if (grepl("b", PART)) {
  rows <- fread(file.path(WORK, "analysis_rows.csv"))
  fos <- fossil_table()[!startsWith(record_class, "excluded") & group == "Theropoda"]
  pred_sd <- median(fos$pred_sd, na.rm = TRUE); y_se <- median(fos[record_class == "point_interval", y_se])
  big <- rows[, .(nsp = uniqueN(pred_taxon_id)), by = minor_clade][nsp >= 15, minor_clade]
  t0 <- Sys.time()
  res <- mclapply(seq_len(length(big) * NTHIN), function(ji) {
    k <- big[(ji - 1) %/% NTHIN + 1]; set.seed(9e5 + ji)
    x <- rows[minor_clade == k]
    st <- x[obs == "stomach_content"]
    if (uniqueN(st$pred_taxon_id) >= NSP) x <- st
    sp <- sample(unique(x$pred_taxon_id), min(NSP, uniqueN(x$pred_taxon_id)))
    pick <- rbindlist(lapply(sp, function(s) { z <- x[pred_taxon_id == s]; z[sample(.N, min(.N, sample(1:4, 1)))] }))
    recs <- pick[, .(pair_id, pred_taxon_id, source_id, specimen_key = paste(pred_taxon_id, source_id),
                     obs, record_class = "point_interval", pred_type = "normal",
                     pred_mu = log(pred_g) + rnorm(.N, 0, pred_sd), pred_sd = pred_sd, pred_lo = NA_real_, pred_hi = NA_real_,
                     y = y + rnorm(.N, 0, y_se), y_se = y_se, y_lo = NA_real_, y_hi = NA_real_)]
    recs[, pred_mu := pred_mu[1], by = specimen_key]
    u <- pack_unit(recs, F$info$obs_levels)
    lpd <- score_ids(u, 9e5 + ji)
    maj <- F$info$major_of[match(k, F$info$minor_levels)]
    c(list(truth = k, rep = ji, obs_used = paste(unique(recs$obs), collapse = ";"), n_records = nrow(recs)),
      rank_summary(lpd, k, paste0("major:", maj)))
  }, mc.cores = CORES, mc.preschedule = FALSE)
  bad <- sapply(res, inherits, "try-error"); if (any(bad)) { print(res[[which(bad)[1]]]); stop("worker errors") }
  cat(sprintf("(b) %d thinned samples in %.1f min\n", length(res), as.numeric(Sys.time() - t0, units = "mins")))
  B <- rbindlist(res, fill = TRUE)
  fwrite(B, file.path(out, "thinned.csv"))
  SB <- B[, .(n = .N, obs_used = obs_used[1], top_minor_correct = mean(top_minor == truth),
              top_major_correct = mean(top_major == paste0("major:", F$info$major_of[match(truth[1], F$info$minor_levels)])),
              mean_p_true_minor = mean(p_true_minor), mean_p_true_major = mean(p_true_major), none_wins = mean(top_major == "none")),
          by = truth]
  fwrite(SB, file.path(out, "thinned_summary.csv"))
  options(width = 160); cat("\n(b) thinned extant samples:\n"); print(SB, digits = 3)
}
cat("DONE\n")
