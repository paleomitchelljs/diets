# e03_comparative.R — Phase 5 Step C: signal, BM / OU / EB with measurement error,
# reconstruction, and the fossil test. Methods: docs/EVOLUTION_METHODS.md sections 5-6.
#
# Env vars:
#   EVO_TAG       Step-A output folder in results/evolution/ (default "primary")
#   EVO_C_TAG     output name (default = EVO_TAG)
#   EVO_C_ROBUST  "1" = species with >= 3 records from >= 2 studies only
#   EVO_C_ADDE    "1" = add tier E species-means (species_trait_tierE.csv)
#   EVO_C_DROP    comma-separated clades to drop at this step
#   EVO_NTREE     number of trees (default 100)
#   EVO_RECON     "1" = reconstruction + signal checks on tree 1 (default 1 for primary)
suppressPackageStartupMessages({ library(ape); library(data.table); library(phytools) })
args <- commandArgs(trailingOnly = FALSE)
ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grep("^--file=", args)])), "..", ".."))
setwd(ROOT)
TAG   <- Sys.getenv("EVO_TAG", "primary"); CTAG <- Sys.getenv("EVO_C_TAG", TAG)
ROB   <- Sys.getenv("EVO_C_ROBUST", "0") == "1"; ADDE <- Sys.getenv("EVO_C_ADDE", "0") == "1"
DROP  <- setdiff(strsplit(Sys.getenv("EVO_C_DROP", ""), ",")[[1]], "")
NT    <- as.integer(Sys.getenv("EVO_NTREE", "100"))
RECON <- Sys.getenv("EVO_RECON", ifelse(CTAG == "primary", "1", "0")) == "1"
IN  <- file.path("results", "evolution", TAG); OUT <- file.path("results", "evolution", "comparative", CTAG)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
set.seed(20261112)

## ---- species data --------------------------------------------------------------------
st <- fread(file.path(IN, "species_trait.csv"))
if (ADDE && file.exists(file.path(IN, "species_trait_tierE.csv"))) {
  e <- fread(file.path(IN, "species_trait_tierE.csv")); e[, robust := TRUE]
  st <- rbind(st, e[!sp %in% st$sp], fill = TRUE)
}
if (ROB) st <- st[robust == TRUE | source == "tier E species mean"]
if (length(DROP)) st <- st[!clade %in% DROP]
tx <- fread("db/taxa.csv", colClasses = "character", na.strings = NULL)
st <- merge(st, tx[, .(sp = taxon_id, tree_label)], by = "sp", all.x = TRUE)
unmatched <- st[is.na(tree_label) | tree_label == "", sp]
st <- st[!(is.na(tree_label) | tree_label == "")]
# several taxon ids on one tip (synonyms, or splits the tree does not recognise): precision-weighted mean
st[, w := 1 / theta_sd^2]
dat <- st[, .(theta = sum(w * theta_mean) / sum(w), se = sqrt(1 / sum(w)), lnM = sum(w * lnM) / sum(w),
              clade = clade[1], n_records = sum(n_records), n_taxon_ids = .N, taxon_ids = paste(sp, collapse = ";")),
          by = .(tip = tree_label)]
cat(sprintf("[%s] %d tips (%d taxon ids; %d merged onto shared tips; %d unmatched dropped: %s)\n", CTAG, nrow(dat),
            nrow(st), sum(dat$n_taxon_ids > 1), length(unmatched), paste(unmatched, collapse = ",")))
fwrite(dat, file.path(OUT, "tip_data.csv"))

trees <- read.tree("db/tree/combined_100.tre")[seq_len(NT)]
ages  <- fread("db/tree/group_root_ages.csv")[seq_len(NT)]
tt    <- fread("db/tree/raw/timetree5/pairwise_queries.csv"); tage <- setNames(tt$precomputed_age, tt$node)

## ---- covariance functions on shared-path matrix S and root-to-tip depths h ----------------
covf <- function(model, s2, q, S, hi, hj = hi) {
  switch(model,
    BM  = s2 * S,
    OU  = s2 / (2 * q) * exp(-q * (outer(hi, hj, "+") - 2 * S)) * (1 - exp(-2 * q * S)),
    EB  = s2 * (exp(q * S) - 1) / q,
    LAM = { V <- s2 * S * q; if (identical(hi, hj) && nrow(S) == ncol(S)) diag(V) <- s2 * diag(S); V })
}
gls_ll <- function(V, y, X, se) {
  Vt <- V + diag(se^2, length(y)); R <- tryCatch(chol(Vt), error = function(e) NULL)
  if (is.null(R)) return(list(ll = -1e100))
  Ri <- backsolve(R, diag(length(y))); Vi <- Ri %*% t(Ri)
  XtVi <- crossprod(X, Vi); A <- XtVi %*% X; b <- solve(A, XtVi %*% y); r <- y - X %*% b
  list(ll = -0.5 * (length(y) * log(2 * pi) + 2 * sum(log(diag(R))) + drop(crossprod(r, Vi %*% r))),
       b = drop(b), Vi = Vi, Ainv = solve(A))
}
fit_model <- function(model, S, h, y, X, se, Tmax) {
  qb <- switch(model, BM = NULL, OU = c(1e-6, 10 * log(2) / Tmax), EB = c(-0.2, -1e-6), LAM = c(0, 1))
  nll <- function(par) { s2 <- exp(par[1]); q <- if (length(par) > 1) par[2] else NA
    -gls_ll(covf(model, s2, q, S, h), y, X, se)$ll }
  s0 <- log(max(var(y) / max(diag(S)), 1e-6))
  if (is.null(qb)) { o <- optimize(function(p) nll(p), c(s0 - 12, s0 + 6)); par <- o$minimum; val <- o$objective
  } else {
    best <- NULL
    for (q0 in seq(qb[1], qb[2], length.out = 5)[2:4]) {
      o <- optim(c(s0, q0), nll, method = "L-BFGS-B", lower = c(s0 - 12, qb[1]), upper = c(s0 + 6, qb[2]))
      if (is.null(best) || o$value < best$value) best <- o }
    par <- best$par; val <- best$value }
  k <- ncol(X) + length(par); n <- length(y)
  g <- gls_ll(covf(model, exp(par[1]), if (length(par) > 1) par[2] else NA, S, h), y, X, se)
  list(model = model, s2 = exp(par[1]), q = if (length(par) > 1) par[2] else NA, ll = -val,
       aicc = 2 * val + 2 * k + 2 * k * (k + 1) / (n - k - 1), b = g$b, k = k)
}

## ---- fossil records -----------------------------------------------------------------------
p <- fread("db/pairs.csv", colClasses = "character", na.strings = c("", "NA"))
rules <- fread("code/evolution/fossil_placement_rules.csv")
num <- function(x) suppressWarnings(as.numeric(x))
fo <- p[is_fossil == "TRUE" & pred_taxon_id %in% rules$pred_taxon_id]
fo <- merge(fo, tx[, .(prey_taxon_id = taxon_id, prey_vert = is_vertebrate)], by = "prey_taxon_id", all.x = TRUE)
mid <- function(pt, lo, hi) ifelse(!is.na(pt), pt, ifelse(!is.na(lo) & !is.na(hi), sqrt(lo * hi), NA))
fo[, `:=`(pm = mid(num(pred_mass_g), num(pred_mass_lo_g), num(pred_mass_hi_g)),
          qm = mid(num(prey_mass_g), num(prey_mass_lo_g), num(prey_mass_hi_g)))]
fo <- fo[!is.na(pm) & !is.na(qm) & prey_vert == "TRUE"]
isd <- function(lo, hi) ifelse(!is.na(lo) & !is.na(hi) & lo > 0, (log(hi) - log(lo)) / 3.92, NA)
fo[, `:=`(obs = log(qm) - log(pm), lnM = log(pm), age = num(age_ma),
          sd_pred = isd(num(pred_mass_lo_g), num(pred_mass_hi_g)), sd_prey = isd(num(prey_mass_lo_g), num(prey_mass_hi_g)))]
fo[, se_obs := sqrt(fifelse(is.na(sd_pred), 0, sd_pred)^2 + fifelse(is.na(sd_prey), 0, sd_prey)^2)]
fo[, interval_note := paste0(ifelse(is.na(sd_pred), "pred no interval", "pred interval"), "; ",
                             ifelse(is.na(sd_prey), "prey no interval", "prey interval"))]
fo <- merge(fo, rules[, .(pred_taxon_id, stem_of)], by = "pred_taxon_id")
sig <- fread(file.path(IN, "trait_model_sigma.csv"))
rec_var <- mean(sig$sigma^2) + sig$tau_study[1]^2          # one record about its species' theta
fwrite(fo[, .(pair_id, pred_taxon_id, prey_taxon_id, observation_type, pm, qm, obs, se_obs, interval_note, age, stem_of)],
       file.path(OUT, "fossil_records.csv"))

## ---- main loop over trees ---------------------------------------------------------------------
models <- c("BM", "OU", "EB")
res <- list(); fos <- list(); lam <- list()
group_tips <- list(`crown Aves` = dat[clade == "Aves", tip], `crown Crocodylia` = dat[clade == "Crocodylia", tip],
                   `crown Theria` = dat[clade == "Mammalia", tip])
for (j in seq_len(NT)) {
  tr <- keep.tip(trees[[j]], dat$tip); d <- dat[match(tr$tip.label, tip)]
  S <- vcv(tr); h <- diag(S); Tmax <- max(h)
  for (form in c("intercept", "size")) {
    X <- if (form == "intercept") matrix(1, nrow(d)) else cbind(1, d$lnM)
    fits <- lapply(models, function(m) fit_model(m, S, h, d$theta, X, d$se, Tmax))
    lf <- fit_model("LAM", S, h, d$theta, X, d$se, Tmax)
    lam[[length(lam) + 1]] <- data.table(tree = j, form = form, lambda = lf$q, ll_lambda = lf$ll)
    # lambda = 0 (white noise with measurement error) for the LRT
    g0 <- gls_ll(covf("LAM", lf$s2, 0, S, h), d$theta, X, d$se)
    o0 <- optimize(function(ls2) -gls_ll(covf("LAM", exp(ls2), 0, S, h), d$theta, X, d$se)$ll, c(-10, 5))
    lam[[length(lam)]][, `:=`(ll_lambda0 = -o0$objective, LR_p = pchisq(2 * (lf$ll + o0$objective), 1, lower.tail = FALSE))]
    aw <- exp(-0.5 * (sapply(fits, `[[`, "aicc") - min(sapply(fits, `[[`, "aicc")))); aw <- aw / sum(aw)
    for (m in seq_along(fits)) {
      f <- fits[[m]]
      res[[length(res) + 1]] <- data.table(tree = j, form = form, model = f$model, ll = f$ll, aicc = f$aicc, aicc_w = aw[m],
        sigma2 = f$s2, param = f$q, halflife = if (f$model == "OU") log(2) / f$q else NA_real_,
        b0 = f$b[1], b_lnM = if (length(f$b) > 1) f$b[2] else NA_real_, n = nrow(d))
      # fossil predictions under this model
      Vtt <- covf(f$model, f$s2, f$q, S, h) + diag(d$se^2); Vi <- solve(Vtt)
      Xt <- X; A_inv <- solve(t(Xt) %*% Vi %*% Xt)
      for (i in seq_len(nrow(fo))) {
        g  <- fo$stem_of[i]; G <- intersect(group_tips[[g]], tr$tip.label)
        crown <- switch(g, `crown Aves` = ages$crown_aves[j], `crown Crocodylia` = tage[["Crocodylia_crown"]],
                        `crown Theria` = ages$crown_theria[j])
        oldA  <- switch(g, `crown Aves` = tage[["Archosauria"]], `crown Crocodylia` = tage[["Archosauria"]],
                        `crown Theria` = ages$crown_mammalia[j])
        mrca_h <- if (length(G) > 1) S[G[1], G[2]] else h[G[1]]          # depth of the group MRCA in the pruned tree
        stem_parent_h <- max(S[G[1], setdiff(tr$tip.label, G)])           # depth of the node below the stem
        for (bound in c("young", "old")) {
          A  <- if (bound == "young") max(fo$age[i], crown) else oldA     # attachment age, Ma
          ha <- Tmax - A                                                  # attachment depth from root
          ha <- min(max(ha, stem_parent_h), mrca_h)                       # on the stem edge
          L  <- max(0, (Tmax - fo$age[i]) - ha)                           # terminal branch to the fossil's age
          sf <- ifelse(tr$tip.label %in% G, ha, S[G[1], ])                 # shared path with each extant tip
          hf <- ha + L
          cv <- drop(covf(f$model, f$s2, f$q, matrix(sf, 1), hf, h))
          vff <- drop(covf(f$model, f$s2, f$q, matrix(hf, 1, 1), hf, hf))
          xf <- if (form == "intercept") 1 else c(1, fo$lnM[i])
          w  <- drop(Vi %*% cv); mu <- sum(xf * f$b) + sum(w * (d$theta - X %*% f$b))
          u  <- xf - drop(t(X) %*% w)
          v  <- vff - sum(cv * w) + drop(t(u) %*% A_inv %*% u) + rec_var + fo$se_obs[i]^2
          fos[[length(fos) + 1]] <- data.table(tree = j, form = form, model = f$model, pair_id = fo$pair_id[i],
            bound = bound, attach_age = Tmax - ha, terminal_L = L, pred_mean = mu, pred_sd = sqrt(v),
            obs = fo$obs[i], pct = pnorm((fo$obs[i] - mu) / sqrt(v)))
        }
      }
    }
  }
  if (j %% 10 == 0) cat("tree", j, "\n")
}
res <- rbindlist(res); fos <- rbindlist(fos); lam <- rbindlist(lam)
fwrite(res, file.path(OUT, "model_fits_by_tree.csv")); fwrite(fos, file.path(OUT, "fossil_pred_by_tree.csv"))
fwrite(lam, file.path(OUT, "lambda_by_tree.csv"))

q3 <- function(x) if (all(is.na(x))) NA_character_ else
  sprintf("%.3g [%.3g, %.3g]", median(x, na.rm = TRUE), quantile(x, .025, na.rm = TRUE), quantile(x, .975, na.rm = TRUE))
res[, `:=`(dAICc = aicc - min(aicc), best = aicc == min(aicc)), by = .(tree, form)]
sm <- res[, .(n = n[1], dAICc_vs_best = q3(dAICc), aicc_w = q3(aicc_w), best_in_pct_trees = round(100 * mean(best)),
              sigma2 = q3(sigma2), param = q3(param), halflife_Myr = q3(halflife), b0 = q3(b0), b_lnM = q3(b_lnM)),
          by = .(form, model)]
fwrite(sm, file.path(OUT, "model_summary.csv")); print(sm)
ls <- lam[, .(lambda = q3(lambda), LR_p_median = median(LR_p), pct_trees_p_lt_0.05 = round(100 * mean(LR_p < .05))), by = form]
fwrite(ls, file.path(OUT, "lambda_summary.csv")); print(ls)
fsum <- fos[, .(pred_mean = median(pred_mean), pred_sd = median(pred_sd), obs = obs[1], pct = median(pct),
                pct_lo = quantile(pct, .025), pct_hi = quantile(pct, .975), inside95_pct_trees = round(100 * mean(pct > .025 & pct < .975)),
                attach_age = median(attach_age), terminal_L = median(terminal_L)), by = .(pair_id, form, model, bound)]
fsum <- merge(fsum, fo[, .(pair_id, pred_taxon_id, prey_taxon_id, observation_type, pm, qm, se_obs, interval_note)], by = "pair_id")
# predator-size support: fossil lnM vs extant range of the group it sits on
rng <- dat[, .(lnM_min = min(lnM), lnM_max = max(lnM)), by = clade]
fsum[, grp := fcase(grepl("Aves", fo$stem_of[match(pair_id, fo$pair_id)]), "Aves",
                    grepl("Croc", fo$stem_of[match(pair_id, fo$pair_id)]), "Crocodylia", default = "Mammalia")]
fsum <- merge(fsum, rng, by.x = "grp", by.y = "clade", all.x = TRUE)
fsum[, size_supported := log(pm) >= lnM_min & log(pm) <= lnM_max]
fsum[, obs_type_matched := observation_type == "stomach_content"]
fwrite(fsum, file.path(OUT, "fossil_test_summary.csv"))

## ---- tree 1: checks, signal (K), reconstruction --------------------------------------------------
if (RECON) {
  suppressPackageStartupMessages(library(geiger))
  tr <- keep.tip(trees[[1]], dat$tip); d <- dat[match(tr$tip.label, tip)]
  y <- setNames(d$theta, d$tip); se <- setNames(d$se, d$tip)
  chk <- rbindlist(lapply(c("BM", "OU", "EB"), function(m) {
    gc <- tryCatch(fitContinuous(tr, y, SE = se, model = m, ncores = 1)$opt$lnL, error = function(e) NA)
    data.table(model = m, own_lnL = res[tree == 1 & form == "intercept" & model == m, ll], geiger_lnL = gc) }))
  fwrite(chk, file.path(OUT, "check_vs_geiger.csv")); print(chk)
  K  <- phylosig(tr, y, method = "K", se = se, test = TRUE, nsim = 200)
  L1 <- phylosig(tr, y, method = "lambda", se = se, test = TRUE)
  fwrite(data.table(stat = c("K", "lambda_phytools"), value = c(K$K, L1$lambda), p = c(K$P, L1$P)),
         file.path(OUT, "signal_tree1.csv"))
  # reconstruction: conditional mean/var of nodes, intercept-only, under BM and the AICc-best model on tree 1
  Sf <- vcvPhylo(tr, anc.nodes = TRUE); hf <- diag(Sf)
  tips <- tr$tip.label; nodes <- setdiff(rownames(Sf), tips)
  rec <- list()
  best <- res[tree == 1 & form == "intercept"][which.min(aicc), model]
  for (m in unique(c("BM", best))) {
    f <- res[tree == 1 & form == "intercept" & model == m]
    Vall <- covf(m, f$sigma2, f$param, Sf, hf)
    Vtt <- Vall[tips, tips] + diag(se[tips]^2); Vnt <- Vall[nodes, tips, drop = FALSE]
    w <- Vnt %*% solve(Vtt); mu <- f$b0 + drop(w %*% (y[tips] - f$b0))
    v <- diag(Vall[nodes, nodes]) - rowSums(w * Vnt)
    rec[[m]] <- data.table(model = m, node = as.integer(nodes), est = mu, sd = sqrt(pmax(v, 0)))
  }
  rec <- rbindlist(rec); fwrite(rec, file.path(OUT, "reconstruction_tree1.csv"))
  write.tree(tr, file.path(OUT, "tree1_pruned.tre"))
  key <- list(Root_Amniota = tips, Sauria = d[clade != "Mammalia", tip], Archosauria = d[clade %in% c("Aves", "Crocodylia"), tip],
              Aves = d[clade == "Aves", tip], Mammalia = d[clade == "Mammalia", tip], Squamata = d[clade %in% c("Lacertilia", "Serpentes"), tip],
              Serpentes = d[clade == "Serpentes", tip], Crocodylia = d[clade == "Crocodylia", tip])
  kn <- rbindlist(lapply(names(key), function(k) { nd <- getMRCA(tr, key[[k]])
    rec[node == nd, .(clade_node = k, model, node, est, sd, age_Ma = max(hf) - hf[as.character(nd)])] }))
  fwrite(kn, file.path(OUT, "reconstruction_key_nodes.csv")); print(kn)
}
cat("done", CTAG, "\n")
