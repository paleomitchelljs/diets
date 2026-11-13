# b08: fossil similarity test from the posterior draws of one extant fit (no refit).
# Units: the fossil groups (Theropoda, Avialae, Crocodyliformes, Repenomamus, Sanajeh)
# and every fossil taxon in them on its own. Candidates: every minor clade, every major
# clade, "none of the above", and the group's tree-placed tip (code/db_analysis/fossil_tree_tips.csv,
# primary age plus two sensitivity ages). Score = joint log predictive density of the
# unit's records (log of the likelihood averaged over inner draws of the new effects and
# masses, then over posterior draws). Relative support assumes equal prior weight.
# Env: FIT_DIR, WORK, NDRAW (400), NINNER (200), SENS (none | no_combat | no_juv_pred |
#      no_bracket | stomach_only).
# Output in <FIT_DIR>/fossil[_<SENS>]/: records.csv, lpd.csv, support.csv, percentiles.csv
source("code/db_analysis/b08_fossil_lib.R")
FIT_DIR <- Sys.getenv("FIT_DIR", "results/6_work/terr/fit_free")
WORK    <- Sys.getenv("WORK", "results/6_work/terr")
NDRAW   <- as.integer(Sys.getenv("NDRAW", "400"))
NINNER  <- as.integer(Sys.getenv("NINNER", "200"))
SENS    <- Sys.getenv("SENS", "none")
out <- file.path(FIT_DIR, if (SENS == "none") "fossil" else paste0("fossil_", SENS))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

F <- load_fit(FIT_DIR, WORK, NDRAW)
f <- fossil_table()
fwrite(f[, .(pair_id, source_id, group, pred_taxon_id, pred_specimen, prey_taxon_id, observation_type, obs,
             pred_stage, record_class, pred_mass_g, pred_mass_lo_g, pred_mass_hi_g,
             prey_mass_g, prey_mass_lo_g, prey_mass_hi_g, age_ma)], file.path(out, "records.csv"))
use <- f[!startsWith(record_class, "excluded") & group %in% c("Theropoda", "Avialae", "Crocodyliformes", "Repenomamus", "Sanajeh")]
use <- switch(SENS,
  none = use,
  no_combat = use[observation_type != "combat_association"],
  no_juv_pred = use[!pred_stage %in% c("juvenile", "hatchling", "subadult")],
  no_bracket = use[record_class %in% c("point_interval", "point")],
  stomach_only = use[observation_type == "stomach_content"],
  stop("unknown SENS"))
cat(sprintf("fit %s | sens %s | %d usable fossil records, %d taxa\n", FIT_DIR, SENS, nrow(use), uniqueN(use$pred_taxon_id)))
print(use[, .N, by = .(group, record_class)][order(group)])

tt <- fread("code/db_analysis/fossil_tree_tips.csv")
cands <- build_candidates(F, tt)
tipcs <- precompute_tips(F, cands)
cid <- sapply(cands, `[[`, "id"); ctype <- sapply(cands, `[[`, "type")
cmaj <- sapply(cands, function(x) x$major); cgrp <- sapply(cands, function(x) if (is.null(x$group)) NA else x$group)

# observation types present in the extant data, per minor and per major clade
dz <- F$design
has_obs <- function(cn, o) switch(cn$type,
  minor = o %in% dz[minor_clade == cn$id, obs],
  none  = TRUE,
  o %in% dz[major_clade == cn$major, obs])

units <- c(lapply(unique(use$group), function(g) list(id = g, type = "group", group = g, recs = use[group == g])),
           lapply(unique(use$pred_taxon_id), function(t) list(id = t, type = "taxon",
             group = use[pred_taxon_id == t, group][1], recs = use[pred_taxon_id == t])))
units <- Filter(function(x) uniqueN(x$recs$pred_taxon_id) > 1 || x$type == "taxon", units)

lpd_rows <- list(); sup_rows <- list(); pct_rows <- list()
for (ui in seq_along(units)) {
  un <- units[[ui]]
  keep <- which(ctype %in% c("minor", "major", "none") | cgrp %in% un$group)
  u <- pack_unit(un$recs, F$info$obs_levels)
  t0 <- Sys.time()
  sc <- score_unit(F, u, cands[keep], tipcs[keep], M = NINNER, seed = ui, want_pct = TRUE)
  ls <- lpd_summary(sc$L)
  n_ext <- sapply(cands[keep], function(cn) sum(!sapply(u$obs, function(o) has_obs(cn, o))))
  lt <- data.table(unit = un$id, unit_type = un$type, group = un$group, n_records = u$n,
                   n_taxa = u$T, candidate = cid[keep], cand_type = ctype[keep], cand_major = cmaj[keep],
                   lpd = ls[, "lpd"], mcse = ls[, "mcse"], n_extrapolated = n_ext)
  lpd_rows[[ui]] <- lt
  # comparisons (equal prior weight within each)
  cmp <- function(name, ids) if (length(ids) > 1) {
    x <- lt[candidate %in% ids]; x[, `:=`(comparison = name, rel_support = softmax(lpd))]; x }
  majors <- lt[cand_type %in% c("major", "none"), candidate]
  best_major <- sub("^major:", "", lt[cand_type == "major"][which.max(lpd), candidate])
  sup_rows[[ui]] <- rbind(
    cmp("major_vs_none", majors),
    cmp("all_minor", lt[cand_type == "minor", candidate]),
    cmp(paste0("minor_within_", best_major), lt[cand_type == "minor" & cand_major == best_major, candidate]),
    cmp("tree_vs_major_vs_none", c(lt[cand_type == "tree", candidate], majors)))
  # per-record percentiles against minor and major candidates
  sel <- which(ctype[keep] %in% c("minor", "major", "tree"))
  pct_rows[[ui]] <- rbindlist(lapply(sel, function(ci) data.table(
    unit = un$id, unit_type = un$type, pair_id = u$pair_id, candidate = cid[keep][ci],
    pct_point = sc$pct[, 1, ci], pct_lo = sc$pct[, 2, ci], pct_hi = sc$pct[, 3, ci],
    extrapolated_obs = !sapply(u$obs, function(o) has_obs(cands[keep][[ci]], o)))))
  cat(sprintf("  %-28s %2d rec  %5.1f s\n", un$id, u$n, as.numeric(Sys.time() - t0, units = "secs")))
}
lpd <- rbindlist(lpd_rows); sup <- rbindlist(sup_rows); pct <- rbindlist(pct_rows)
pct <- pct[unit_type == "taxon"]                      # one row per record and candidate
fwrite(lpd, file.path(out, "lpd.csv")); fwrite(sup, file.path(out, "support.csv")); fwrite(pct, file.path(out, "percentiles.csv"))

options(width = 180)
for (g in unique(sup[unit_type == "group", unit])) {
  cat("\n====", g, "====\n")
  x <- sup[unit == g][order(comparison, -rel_support)]
  print(x[rel_support > 0.01 | comparison %in% c("major_vs_none", "tree_vs_major_vs_none"),
          .(comparison, candidate, lpd = round(lpd, 2), mcse = round(mcse, 3), rel = round(rel_support, 3), n_extrapolated)])
}
cat("\n==== taxa: top minor clade and top major-or-none ====\n")
print(sup[unit_type == "taxon" & comparison %in% c("all_minor", "major_vs_none"),
          .SD[which.max(rel_support)], by = .(unit, comparison)][, .(unit, comparison, candidate, rel = round(rel_support, 3))])
cat("DONE\n")
