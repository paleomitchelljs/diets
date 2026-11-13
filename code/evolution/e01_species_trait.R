# e01_species_trait.R — Phase 5 Step A: species-level predator–prey mass-ratio trait.
# Definition and model: docs/EVOLUTION_METHODS.md sections 1-3.
#
# Env vars (sensitivity runs; defaults = primary analysis):
#   EVO_TIERS   letters of tiers to use, default "ABCD" (e.g. "ABC")
#   EVO_OBS     "all" (default) | "stomach" (stomach rows only, no offsets) |
#               "noUnk" (drop unknown + dna_detection) | "byclade" (offsets vary by clade)
#   EVO_DROP    comma-separated analysis clades to drop, default "" (e.g. "Aves,Mammalia")
#   EVO_TAG     output tag, default "primary"
#   STAN_CORES  default 1 (sandbox: parallel chains can hang at warm-up)
#   EVO_ITER    default 2000
suppressPackageStartupMessages({ library(data.table); library(brms) })
args <- commandArgs(trailingOnly = FALSE)
ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grep("^--file=", args)])), "..", ".."))
setwd(ROOT)

TIERS <- strsplit(Sys.getenv("EVO_TIERS", "ABCD"), "")[[1]]
OBS   <- Sys.getenv("EVO_OBS", "all")
DROP  <- setdiff(strsplit(Sys.getenv("EVO_DROP", ""), ",")[[1]], "")
TAG   <- Sys.getenv("EVO_TAG", "primary")
CORES <- as.integer(Sys.getenv("STAN_CORES", "1"))
ITER  <- as.integer(Sys.getenv("EVO_ITER", "2000"))
OUT   <- file.path("results", "evolution", TAG); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
CLADES <- c("Aves", "Mammalia", "Lacertilia", "Serpentes", "Crocodylia")

## ---- analysis table ---------------------------------------------------------
p  <- fread("db/pairs.csv", colClasses = "character", na.strings = c("", "NA"))
tx <- fread("db/taxa.csv",  colClasses = "character", na.strings = c("", "NA"))
num <- function(x) suppressWarnings(as.numeric(x))
mid <- function(pt, lo, hi) ifelse(!is.na(pt), pt, ifelse(!is.na(lo) & !is.na(hi), sqrt(lo * hi), NA))
p[, pm := mid(num(pred_mass_g), num(pred_mass_lo_g), num(pred_mass_hi_g))]
p[, qm := mid(num(prey_mass_g), num(prey_mass_lo_g), num(prey_mass_hi_g))]
p <- merge(p, tx[, .(pred_taxon_id = taxon_id, pred_rank = rank, clade = analysis_clade,
                     pred_name = accepted_name)], by = "pred_taxon_id", all.x = TRUE)
p <- merge(p, tx[, .(prey_taxon_id = taxon_id, prey_vert = is_vertebrate)], by = "prey_taxon_id", all.x = TRUE)

d <- p[is_fossil == "FALSE" & pred_rank == "species" & clade %in% setdiff(CLADES, DROP) &
       prey_vert == "TRUE" & !is.na(pm) & !is.na(qm) & pm > 0 & qm > 0 &
       substr(pair_resolution, 1, 1) %in% TIERS]
if (OBS == "stomach") d <- d[observation_type == "stomach_content"]
if (OBS == "noUnk")   d <- d[!observation_type %in% c("unknown", "dna_detection")]

measured <- c("measured", "regressed_from_length", "regressed_from_bone", "reconstructed_from_remains")
d[, pred_measured := pred_mass_method %in% measured]

# reference mass M_s: median taxon-average predator mass if any, else geometric mean of measured
ref <- d[, .(lnM = if (any(!pred_measured)) log(median(pm[!pred_measured])) else mean(log(pm)),
             ref_mass_basis = if (any(!pred_measured)) "taxon_average_median" else "measured_sample_mean"),
         by = pred_taxon_id]
d <- merge(d, ref, by = "pred_taxon_id")
d[, `:=`(y = log(qm), dev = ifelse(pred_measured, log(pm) - lnM, 0))]
cc <- mean(ref$lnM)
d[, lnMc := lnM - cc]
d[, obs := relevel(factor(observation_type), ref = "stomach_content")]
d[, clade := factor(clade)]
d[, sp := pred_taxon_id]; d[, study := source_id]
cat(sprintf("[%s] rows %d | species %d | studies %d | tiers %s | obs %s | drop %s\n", TAG, nrow(d),
            uniqueN(d$sp), uniqueN(d$study), paste(TIERS, collapse = ""), OBS, paste(DROP, collapse = ",")))
print(d[, .N, by = .(clade, obs)][order(clade, -N)])
fwrite(d[, .(pair_id, sp, clade, study, obs, pair_resolution, pm, qm, y, lnM, dev, pred_measured)],
       file.path(OUT, "trait_input_rows.csv"))

## ---- model ------------------------------------------------------------------
multi_obs <- nlevels(droplevels(d$obs)) > 1 && OBS != "stomach"
has_dev   <- sum(d$dev != 0) > 20
rhs <- c("lnMc", if (has_dev) "dev",
         if (multi_obs && OBS != "byclade") "obs",
         if (multi_obs && OBS == "byclade") "(1 | obs:clade) + obs",
         "(1 | sp)", "(1 | study)")
f <- bf(as.formula(paste("y ~", paste(rhs, collapse = " + "))),
        if (nlevels(droplevels(d$clade)) > 1) sigma ~ 0 + clade else sigma ~ 1)
d <- droplevels(d)
pri <- c(prior_string(sprintf("normal(%.2f, 5)", mean(d$y)), class = "Intercept"),
         prior(normal(0.8, 0.5), class = "b", coef = "lnMc"),
         prior(student_t(3, 0, 2.5), class = "sd"))
if (has_dev) pri <- c(pri, prior(normal(0.8, 0.5), class = "b", coef = "dev"))
if (multi_obs) for (lv in levels(d$obs)[-1])
  pri <- c(pri, prior_string("normal(0, 1)", class = "b", coef = paste0("obs", make.names(lv))))
if (nlevels(d$clade) > 1) pri <- c(pri, prior(normal(0, 2), dpar = "sigma", class = "b"))
fitfile <- file.path(OUT, "fit_trait")
fit <- brm(f, data = d, prior = pri, chains = 4, cores = CORES, iter = ITER, warmup = ITER / 2,
           seed = 20261112, backend = "rstan", file = fitfile, file_refit = "on_change",
           control = list(adapt_delta = 0.95), refresh = 200)

## ---- diagnostics --------------------------------------------------------------
sm <- summary(fit)
rh <- brms::rhat(fit); ess <- brms::neff_ratio(fit)
np <- nuts_params(fit); ndiv <- sum(np$Value[np$Parameter == "divergent__"])
fx <- as.data.table(sm$fixed, keep.rownames = "term")
rn <- as.data.table(do.call(rbind, lapply(names(sm$random), function(g) cbind(group = g, as.data.frame(sm$random[[g]])))), keep.rownames = "term")
diag <- data.table(tag = TAG, n_rows = nrow(d), n_species = uniqueN(d$sp), n_studies = uniqueN(d$study),
                   max_rhat = max(rh, na.rm = TRUE), min_neff_ratio = min(ess, na.rm = TRUE),
                   divergences = ndiv)
fwrite(fx, file.path(OUT, "trait_model_fixed.csv")); fwrite(rn, file.path(OUT, "trait_model_random.csv"))
fwrite(diag, file.path(OUT, "trait_model_diagnostics.csv"))
print(diag); print(fx[, 1:5]); print(rn[, 1:5])

## ---- species trait theta_s ----------------------------------------------------
dr <- as_draws_df(fit)
spl <- unique(d[, .(sp, clade, lnM, lnMc, ref_mass_basis)])
re  <- ranef(fit, summary = FALSE)$sp[, , "Intercept"]          # draws x species
a   <- dr$b_Intercept; bb <- dr$b_lnMc
th  <- sapply(spl$sp, function(s) a + bb * spl[sp == s, lnMc] + re[, s] - spl[sp == s, lnM])
# observation offset by clade: when offsets vary by clade the stomach level carries a clade deviation
if (OBS == "byclade") {
  rc <- ranef(fit, summary = FALSE)$`obs:clade`[, , "Intercept"]
  for (j in seq_along(spl$sp)) {
    key <- paste0("stomach_content_", spl$clade[j])
    if (key %in% colnames(rc)) th[, j] <- th[, j] + rc[, key]
  }
}
cnt <- d[, .(n_records = .N, n_studies = uniqueN(study), n_measured_pred = sum(pred_measured),
             n_stomach = sum(obs == "stomach_content"),
             obs_types = paste(sort(unique(as.character(obs))), collapse = ";"),
             tiers = paste(sort(unique(substr(pair_resolution, 1, 1))), collapse = "")), by = sp]
st <- data.table(sp = spl$sp, clade = spl$clade, lnM = spl$lnM, M_g = exp(spl$lnM),
                 ref_mass_basis = spl$ref_mass_basis,
                 theta_mean = colMeans(th), theta_sd = apply(th, 2, sd),
                 theta_q025 = apply(th, 2, quantile, .025), theta_q975 = apply(th, 2, quantile, .975))
st <- merge(st, cnt, by = "sp")
st[, `:=`(single_record = n_records == 1, no_stomach_records = n_stomach == 0, source = "A-D model")]
st <- merge(st, tx[, .(sp = taxon_id, accepted_name)], by = "sp", all.x = TRUE)

## residual SDs by clade (posterior means), used for tier E and fossil predictive variance
sig <- if (nlevels(d$clade) > 1) {
  cols <- grep("^b_sigma_clade", names(dr), value = TRUE)
  data.table(clade = sub("b_sigma_clade", "", cols), sigma = sapply(cols, function(cl) mean(exp(dr[[cl]]))))
} else data.table(clade = levels(d$clade), sigma = mean(exp(dr$b_sigma_Intercept)))
sig[, tau_study := mean(dr$sd_study__Intercept)]; sig[, tau_sp := mean(dr$sd_sp__Intercept)]
fwrite(sig, file.path(OUT, "trait_model_sigma.csv"))

## ---- tier E species-means (Glaudas 2019) for species without an A-D estimate -----
if ("E" %in% strsplit(Sys.getenv("EVO_ADD_E", "1"), "")[[1]] && "Serpentes" %in% sig$clade) {
  e <- p[source_id == "Glaudas2019" & pair_resolution == "E_coarse_or_aggregate" & !is.na(pm) & !is.na(qm)]
  e <- e[!pred_taxon_id %in% st$sp & clade == "Serpentes"]
  if (nrow(e)) {
    # RPM mean = prey/pred from the row; n = number of prey ("RPM mean ... n = k prey" in basis)
    e[, n_prey := as.integer(sub(".*n = ([0-9]+) prey.*", "\\1", prey_mass_basis))]
    e[is.na(n_prey), n_prey := as.integer(n_items)]
    s_sn <- sig[clade == "Serpentes", sigma]
    eE <- e[, .(theta_mean = log(qm[1] / pm[1]) - s_sn^2 / 2, theta_sd = s_sn / sqrt(max(1, n_prey[1])),
                lnM = log(pm[1]), n_records = n_prey[1]), by = .(sp = pred_taxon_id)]
    eE[, `:=`(clade = "Serpentes", M_g = exp(lnM), ref_mass_basis = "Glaudas2019 mass at mean SVL",
              theta_q025 = theta_mean - 1.96 * theta_sd, theta_q975 = theta_mean + 1.96 * theta_sd,
              n_studies = 1L, n_measured_pred = 0L, n_stomach = n_records, obs_types = "stomach_content (museum)",
              tiers = "E", single_record = FALSE, no_stomach_records = FALSE, source = "tier E species mean")]
    eE <- merge(eE, tx[, .(sp = taxon_id, accepted_name)], by = "sp", all.x = TRUE)
    fwrite(eE, file.path(OUT, "species_trait_tierE.csv"))
    cat("tier E species-means written:", nrow(eE), "\n")
  }
}
st[, robust := n_records >= 3 & n_studies >= 2]
fwrite(st[order(clade, theta_mean)], file.path(OUT, "species_trait.csv"))
saveRDS(list(theta_draws = th, species = spl$sp, center = cc, sigma = sig), file.path(OUT, "theta_draws.rds"))
cat(sprintf("[%s] species trait written: %d species (%d single-record)\n", TAG, nrow(st), sum(st$single_record)))
