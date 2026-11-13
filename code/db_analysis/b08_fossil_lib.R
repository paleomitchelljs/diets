# b08_fossil_lib: functions for the fossil similarity test (sourced by b08, b09).
# Model: any b02/b04/b06 fit. Clade effects e in {a, b, g} have covariance
# tau_e^2 * (lambda_e * Ctree + (1 - lambda_e) * I) on the minor-clade skeleton tree.
# A candidate supplies the clade-level (A, B, G) for a fossil unit:
#   minor k   : the unit's taxa are new species of minor clade k (A, B, G = a_k, b_k, g_k)
#   major c   : the unit is a new minor clade attached at the crown node of major clade c,
#               drawn from the conditional normal given the fitted clade effects
#   none      : a new minor clade attached at the root (independent of every fitted clade)
#   tree tip  : a new minor clade attached on a named stem at a stated age (fossil_tree_tips.csv)
# For every candidate: a new species effect per taxon, a new source effect per fossil
# source, and predator masses drawn per specimen, all shared across that unit's records.
suppressPackageStartupMessages({ library(data.table); library(cmdstanr); library(posterior); library(ape) })

logmeanexp <- function(x) { mx <- max(x); mx + log(mean(exp(x - mx))) }
lbetween <- function(a, b) {             # log(Phi(b) - Phi(a)), a < b, stable in both tails
  lb <- pnorm(b, log.p = TRUE); la <- pnorm(a, log.p = TRUE)
  out <- lb + log1p(-exp(pmin(la - lb, 0)))
  pos <- a > 0
  if (any(pos)) {
    ua <- pnorm(-a[pos], log.p = TRUE); ub <- pnorm(-b[pos], log.p = TRUE)
    out[pos] <- ua + log1p(-exp(pmin(ub - ua, 0)))
  }
  out
}

## ---- fit and skeleton ------------------------------------------------------
load_fit <- function(fit_dir, work, ndraw = 400, seed = 8) {
  info <- readRDS(file.path(fit_dir, "stan_data.rds"))
  cells <- fread(file.path(work, "analysis_cells.csv"))
  mm <- unique(cells[, .(major_clade, minor_clade)])
  info$major_of <- mm$major_clade[match(info$minor_levels, mm$minor_clade)]
  csvs <- list.files(fit_dir, pattern = "-[0-9]+-[0-9a-f]+\\.csv$", full.names = TRUE)
  hdr <- readLines(csvs[1], n = 200); hdr <- strsplit(hdr[!startsWith(hdr, "#")][1], ",")[[1]]
  base <- unique(sub("\\..*$", "", hdr))
  want <- c("alpha0", "beta0", "gamma0", "gamma1", "delta", "zeta", "a", "b", "g",
            "tau_a", "tau_b", "tau_g", "tau_u", "tau_w", "lambda_a", "lambda_b", "lambda_g")
  dr <- read_cmdstan_csv(csvs, variables = intersect(want, base), format = "draws_matrix")$post_warmup_draws
  set.seed(seed)
  dr <- dr[sort(sample(nrow(dr), min(ndraw, nrow(dr)))), ]
  dr <- matrix(as.numeric(dr), nrow(dr), dimnames = list(NULL, colnames(dr)))
  for (l in c("lambda_a", "lambda_b", "lambda_g")) if (!l %in% colnames(dr)) {
    dr <- cbind(dr, 0); colnames(dr)[ncol(dr)] <- l      # b02 fits: clades exchangeable
  }
  col <- function(v) dr[, grep(paste0("^", v, "\\["), colnames(dr)), drop = FALSE]
  K <- length(info$minor_levels)
  Cm <- as.matrix(fread(file.path(work, "skeleton_corr.csv")), rownames = "minor_clade")[info$minor_levels, info$minor_levels]
  sk <- read.tree(file.path(work, "skeleton_tree.tre"))
  root_age <- max(node.depth.edgelength(sk))
  tips <- fread(file.path(work, "skeleton_tips.csv"))
  design <- fread(file.path(work, "design_counts.csv"))
  list(info = info, dr = dr, A = col("a"), B = col("b"), G = col("g"), D = col("delta"), Z = col("zeta"),
       K = K, Cm = Cm, root_age = root_age, tips = tips, design = design)
}

# Shared-history vector (fraction of root age) between a new tip and each fitted minor clade.
# attach_to: a set of minor clades (or one major clade name) forming a clade on the skeleton;
# attach_age: age of the attachment point on the stem of that set (or at its crown).
tip_vector <- function(F, attach_to, attach_age) {
  ml <- F$info$minor_levels
  members <- if (all(attach_to %in% ml)) attach_to else ml[F$info$major_of %in% attach_to]
  stopifnot(length(members) > 0)
  shared_in <- 1 - attach_age / F$root_age
  ref <- members[1]
  cvec <- F$Cm[ref, ]                              # outside the set: same as any member
  cvec[members] <- shared_in
  # the attachment point must sit between the set's crown and its parent node
  outside <- setdiff(ml, members)
  parent_age <- if (length(outside)) F$root_age * (1 - max(F$Cm[ref, outside])) else F$root_age
  stopifnot(attach_age <= parent_age + 1e-6)
  cvec
}
crown_age <- function(F, major) {
  ml <- F$info$minor_levels[F$info$major_of == major]
  if (length(ml) > 1) F$root_age * (1 - min(F$Cm[ml, ml][upper.tri(F$Cm[ml, ml])]))
  else F$tips[minor_clade == ml, crown_age_ma]
}

build_candidates <- function(F, tree_tips = NULL) {
  ml <- F$info$minor_levels
  cands <- c(lapply(seq_along(ml), function(k) list(id = ml[k], type = "minor", major = F$info$major_of[k], k = k)),
             lapply(unique(F$info$major_of), function(c)
               list(id = paste0("major:", c), type = "major", major = c,
                    cvec = tip_vector(F, c, crown_age(F, c)))),
             list(list(id = "none", type = "none", major = NA_character_, cvec = rep(0, F$K))))
  if (!is.null(tree_tips)) for (i in seq_len(nrow(tree_tips))) {
    tt <- tree_tips[i]
    ages <- c(tt$attach_age_ma, as.numeric(strsplit(tt$sens_ages_ma, ";")[[1]]))
    for (j in seq_along(ages)) cands[[length(cands) + 1]] <- list(
      id = sprintf("tree:%s@%g", tt$group, ages[j]), type = if (j == 1) "tree" else "tree_sens",
      group = tt$group, major = if (tt$attach_to %in% F$info$major_of) tt$attach_to else F$info$major_of[match(tt$attach_to, ml)],
      cvec = tip_vector(F, tt$attach_to, ages[j]))
  }
  cands
}

# Per posterior draw and effect: conditional mean weights and sd for a tip candidate.
tip_conditional <- function(F, d, cvec) {
  out <- matrix(NA_real_, 2, 3)
  for (e in 1:3) {
    lam <- F$dr[d, c("lambda_a", "lambda_b", "lambda_g")[e]]
    tau <- F$dr[d, c("tau_a", "tau_b", "tau_g")[e]]
    x <- list(F$A, F$B, F$G)[[e]][d, ]
    if (tau <= 0) { out[, e] <- c(0, 0); next }
    R <- lam * F$Cm + (1 - lam) * diag(F$K); r <- lam * cvec
    w <- solve(R, r)
    out[, e] <- c(sum(w * x), tau * sqrt(max(1 - sum(w * r), 0)))
  }
  out                                                # row 1 mean, row 2 sd, for a, b, g
}

## ---- fossil records ----------------------------------------------------------
OBS_MAP <- c(stomach_content = "stomach_content", pellet = "pellet_scat", combat_association = "direct_observation")

fossil_table <- function() {
  pairs <- fread("db/pairs.csv", colClasses = "character", na.strings = NULL)
  taxa  <- fread("db/taxa.csv", colClasses = "character", na.strings = NULL)
  num <- function(x) suppressWarnings(as.numeric(x))
  f <- pairs[is_fossil == "TRUE"]
  f <- merge(f, taxa[, .(pred_taxon_id = taxon_id, pred_clade = analysis_clade)], by = "pred_taxon_id", all.x = TRUE)
  f <- merge(f, taxa[, .(prey_taxon_id = taxon_id, prey_vert = is_vertebrate)], by = "prey_taxon_id", all.x = TRUE)
  for (v in c("pred_mass_g", "pred_mass_lo_g", "pred_mass_hi_g", "prey_mass_g", "prey_mass_lo_g", "prey_mass_hi_g"))
    set(f, j = v, value = num(f[[v]]))
  f[, `:=`(pp = !is.na(pred_mass_g), plh = !is.na(pred_mass_lo_g) & !is.na(pred_mass_hi_g),
           yp = !is.na(prey_mass_g), ylh = !is.na(prey_mass_lo_g) & !is.na(prey_mass_hi_g),
           yh = !is.na(prey_mass_hi_g))]
  f[, record_class := fcase(
    observation_type == "bite_trace", "excluded: bite trace",
    prey_vert != "TRUE", "excluded: invertebrate prey",
    !pp & !plh, "excluded: no predator mass",
    !(observation_type %in% names(OBS_MAP)), "excluded: observation type has no extant analogue",
    yp & ylh, "point_interval",
    yp, "point",
    ylh, "bracket",
    yh, "upper",
    default = "excluded: no prey mass")]
  f[, obs := OBS_MAP[observation_type]]
  f[, group := fcase(pred_clade == "Theropoda", "Theropoda", pred_clade == "Avialae", "Avialae",
                     pred_clade == "Crocodyliformes", "Crocodyliformes",
                     pred_taxon_id == "Repenomamus_robustus", "Repenomamus",
                     pred_taxon_id == "Sanajeh_indicus", "Sanajeh", default = pred_clade)]
  f[, specimen_key := fifelse(pred_specimen != "", paste(pred_taxon_id, pred_specimen), pair_id)]
  # Mass rule (written in results/6_talk.md): an interval is read as +/- 2 sd on the log scale
  # around the point; with no point, the true ln mass is uniform between the bounds.
  f[, `:=`(pred_type = fifelse(pp, "normal", "uniform"),
           pred_mu = fifelse(pp, log(pred_mass_g), NA_real_),
           pred_sd = fifelse(pp & plh, (log(pred_mass_hi_g) - log(pred_mass_lo_g)) / 4, fifelse(pp, 0.1, NA_real_)),
           pred_lo = log(pred_mass_lo_g), pred_hi = log(pred_mass_hi_g),
           y = log(prey_mass_g),
           y_se = fifelse(ylh, (log(prey_mass_hi_g) - log(prey_mass_lo_g)) / 4, 0),
           y_lo = log(prey_mass_lo_g), y_hi = log(prey_mass_hi_g))]
  f[order(group, pred_taxon_id, pair_id)]
}

# Pack a unit's records into index vectors.
pack_unit <- function(recs, obs_levels) {
  list(n = nrow(recs), pair_id = recs$pair_id, taxa = unique(recs$pred_taxon_id),
       tax = match(recs$pred_taxon_id, unique(recs$pred_taxon_id)), T = uniqueN(recs$pred_taxon_id),
       src = match(recs$source_id, unique(recs$source_id)), S = uniqueN(recs$source_id),
       spec = match(recs$specimen_key, unique(recs$specimen_key)),
       P = uniqueN(recs$specimen_key),
       sp = unique(recs[, .(specimen_key, pred_type, pred_mu, pred_sd, pred_lo, pred_hi)]),
       o = match(recs$obs, obs_levels), obs = recs$obs,
       cls = sub("point_interval", "point", recs$record_class),
       y = recs$y, y_se = recs$y_se, y_lo = recs$y_lo, y_hi = recs$y_hi)
}

draw_masses <- function(u, Zm) {        # Zm: M x P standard normals -> ln predator mass - ln 1000
  M <- nrow(Zm); nrm <- u$sp$pred_type == "normal"
  out <- matrix(0, M, u$P)
  if (any(nrm)) out[, nrm] <- sweep(sweep(Zm[, nrm, drop = FALSE], 2, u$sp$pred_sd[nrm], "*"), 2, u$sp$pred_mu[nrm], "+")
  if (any(!nrm)) out[, !nrm] <- sweep(sweep(pnorm(Zm[, !nrm, drop = FALSE]), 2, u$sp$pred_hi[!nrm] - u$sp$pred_lo[!nrm], "*"), 2, u$sp$pred_lo[!nrm], "+")
  out - log(1000)
}

# Joint log likelihood of a unit's records for one posterior draw d and one candidate,
# averaged over M inner draws (common random numbers Zs shared across candidates).
# Returns list(ll = log mean joint likelihood, pct = per-record predictive percentiles).
unit_ll <- function(F, d, cand, u, Zs, tipc = NULL, want_pct = FALSE) {
  dr <- F$dr; M <- nrow(Zs$e)
  if (cand$type == "minor") {
    Ae <- rep(F$A[d, cand$k], M); Be <- rep(F$B[d, cand$k], M); Ge <- rep(F$G[d, cand$k], M)
  } else if (cand$type == "tree_multi") {           # one tip per taxon, drawn jointly
    stopifnot(identical(cand$taxa, u$taxa))
    eff <- lapply(1:3, function(e) (matrix(tipc$mean[, e], M, u$T, byrow = TRUE) +
                                     Zs$et[, , e, drop = FALSE][, , 1] %*% t(tipc$L[[e]]))[, u$tax, drop = FALSE])
    Ae <- eff[[1]]; Be <- eff[[2]]; Ge <- eff[[3]]
  } else {
    Ae <- tipc[1, 1] + tipc[2, 1] * Zs$e[, 1]
    Be <- tipc[1, 2] + tipc[2, 2] * Zs$e[, 2]
    Ge <- tipc[1, 3] + tipc[2, 3] * Zs$e[, 3]
  }
  mm <- draw_masses(u, Zs$m)[, u$spec, drop = FALSE]
  uu <- dr[d, "tau_u"] * Zs$u[, u$tax, drop = FALSE]
  ww <- dr[d, "tau_w"] * Zs$w[, u$src, drop = FALSE]
  od <- matrix(F$D[d, u$o], M, u$n, byrow = TRUE)
  oz <- matrix(F$Z[d, u$o], M, u$n, byrow = TRUE)
  mu <- dr[d, "alpha0"] + Ae + uu + ww + (dr[d, "beta0"] + Be) * mm + od
  s  <- exp(dr[d, "gamma0"] + Ge + dr[d, "gamma1"] * mm + oz)
  ll <- matrix(0, M, u$n)
  ip <- which(u$cls == "point"); ib <- which(u$cls == "bracket"); iu <- which(u$cls == "upper")
  if (length(ip)) {
    sdp <- sqrt(s[, ip, drop = FALSE]^2 + matrix(u$y_se[ip]^2, M, length(ip), byrow = TRUE))
    ll[, ip] <- dnorm(matrix(u$y[ip], M, length(ip), byrow = TRUE), mu[, ip, drop = FALSE], sdp, log = TRUE)
  }
  if (length(ib)) {
    sb <- s[, ib, drop = FALSE]; mb <- mu[, ib, drop = FALSE]
    ll[, ib] <- lbetween((matrix(u$y_lo[ib], M, length(ib), byrow = TRUE) - mb) / sb,
                         (matrix(u$y_hi[ib], M, length(ib), byrow = TRUE) - mb) / sb)
  }
  if (length(iu))
    ll[, iu] <- pnorm((matrix(u$y_hi[iu], M, length(iu), byrow = TRUE) - mu[, iu, drop = FALSE]) / s[, iu, drop = FALSE], log.p = TRUE)
  res <- list(ll = logmeanexp(rowSums(ll)))
  if (want_pct) {
    pt <- ifelse(u$cls == "point", u$y, NA); lo <- ifelse(u$cls == "bracket", u$y_lo, NA)
    hi <- ifelse(u$cls %in% c("bracket", "upper"), u$y_hi, NA)
    pc <- function(v) colMeans(pnorm((matrix(v, M, u$n, byrow = TRUE) - mu) / s))
    res$pct <- cbind(pt = pc(pt), lo = pc(lo), hi = pc(hi))
  }
  res
}

# Score one unit against all candidates. Returns per-draw ll matrix (ND x n_cand) and percentiles.
score_unit <- function(F, u, cands, tipcs, M = 200, seed = 1, want_pct = FALSE) {
  ND <- nrow(F$dr); nc <- length(cands)
  L <- matrix(NA_real_, ND, nc, dimnames = list(NULL, sapply(cands, `[[`, "id")))
  pct <- if (want_pct) array(0, c(u$n, 3, nc)) else NULL
  for (d in seq_len(ND)) {
    set.seed((seed * 1000 + d) %% 2147483647)
    Zs <- list(e = matrix(rnorm(M * 3), M), m = matrix(rnorm(M * u$P), M),
               u = matrix(rnorm(M * u$T), M), w = matrix(rnorm(M * u$S), M))
    Zs$et <- array(rnorm(M * u$T * 3), c(M, u$T, 3))     # drawn last: other candidates' streams unchanged
    for (ci in seq_len(nc)) {
      r <- unit_ll(F, d, cands[[ci]], u, Zs, tipcs[[ci]][[d]], want_pct)
      L[d, ci] <- r$ll
      if (want_pct) pct[, , ci] <- pct[, , ci] + r$pct
    }
  }
  if (want_pct) pct <- pct / ND
  list(L = L, pct = pct)
}

precompute_tips <- function(F, cands) lapply(cands, function(cn)
  if (cn$type == "minor") vector("list", nrow(F$dr))
  else if (cn$type == "tree_multi") lapply(seq_len(nrow(F$dr)), function(d) tip_conditional_multi(F, d, cn))
  else lapply(seq_len(nrow(F$dr)), function(d) tip_conditional(F, d, cn$cvec)))

## ---- dated composite tree (code/db_analysis/a02_theropod_tree.R) ---------------------
# Each fossil taxon is its own tip attached to the crown-bird line at its dated split; fossil
# pairs share history to their dated MRCA. Shared history is a fraction of the skeleton root
# age, as for the extant clades; the diagonal is 1 (as for the single tree tip).
dated_candidate <- function(F, taxa, dir = "db/tree/theropod", id = "tree:dated") {
  sm <- fread(file.path(dir, "summary.csv"))
  mr <- readRDS(file.path(dir, "mrca_ages.rds"))
  med <- apply(simplify2array(mr), c(1, 2), median)
  stopifnot(all(taxa %in% sm$taxon_id))
  sa <- sm$split_med[match(taxa, sm$taxon_id)]
  Cfo <- t(sapply(sa, function(a) tip_vector(F, "Aves", a)))
  Cff <- 1 - med[taxa, taxa, drop = FALSE] / F$root_age; diag(Cff) <- 1
  list(id = id, type = "tree_multi", major = "Aves", taxa = taxa, split = sa, Cfo = Cfo, Cff = Cff)
}
tip_conditional_multi <- function(F, d, cn) {
  T <- length(cn$taxa); mean <- matrix(0, T, 3); L <- vector("list", 3)
  for (e in 1:3) {
    lam <- F$dr[d, c("lambda_a", "lambda_b", "lambda_g")[e]]
    tau <- F$dr[d, c("tau_a", "tau_b", "tau_g")[e]]
    x <- list(F$A, F$B, F$G)[[e]][d, ]
    if (tau <= 0) { L[[e]] <- matrix(0, T, T); next }
    R <- lam * F$Cm + (1 - lam) * diag(F$K)
    Rfo <- lam * cn$Cfo; Rff <- lam * cn$Cff + (1 - lam) * diag(T)
    W <- t(solve(R, t(Rfo)))
    mean[, e] <- W %*% x
    S <- tau^2 * (Rff - W %*% t(Rfo)); S <- (S + t(S)) / 2
    L[[e]] <- t(chol(S + diag(1e-10, T)))
  }
  list(mean = mean, L = L)
}

lpd_summary <- function(L, nbatch = 20) {
  ND <- nrow(L); b <- rep(seq_len(nbatch), length.out = ND)
  t(apply(L, 2, function(x) {
    lpd <- logmeanexp(x)
    lb <- tapply(x, b, logmeanexp)
    c(lpd = lpd, mcse = sd(exp(lb - lpd)) / sqrt(nbatch))
  }))
}
softmax <- function(x) { e <- exp(x - max(x)); e / sum(e) }
