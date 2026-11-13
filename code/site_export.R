# site_export: build static/data/data.js (and data.json) for the talk site.
# The site reads only this file. DRAFT: until the terrestrial fits and the fossil test
# finish, model numbers come from whatever fit folder is named below; every block carries
# its source so the site can mark draft values.
# Current talk build:
#   FIT=results/6_work/terr/fit_free LAM0=results/6_work/terr/fit_lambda0 LAM1=results/6_work/terr/fit_lambda1 \
#   UNPOOLED=results/6_work/terr/fit_unpooled Rscript code/site_export.R
# Env: FIT (results/6_work/fit_skeleton), ROWS (results/6_work/terr/analysis_rows.csv),
#      LAM0 (results/6_work/fit_primary), UNPOOLED (empty -> per-clade OLS placeholder),
#      FOSSIL (<FIT>/fossil), TREE (<FIT>/tree_prediction)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
FIT    <- Sys.getenv("FIT", "results/6_work/fit_skeleton")
ROWS   <- Sys.getenv("ROWS", "results/6_work/terr/analysis_rows.csv")
LAM0   <- Sys.getenv("LAM0", "results/6_work/fit_primary")
LAM1   <- Sys.getenv("LAM1", "")
UNPOOL <- Sys.getenv("UNPOOLED", "")
FOSSIL <- Sys.getenv("FOSSIL", file.path(FIT, "fossil"))
TREE   <- Sys.getenv("TREE", file.path(FIT, "tree_prediction"))
HAWK   <- Sys.getenv("HAWK", "Buteo_jamaicensis")
dir.create("static/data", recursive = TRUE, showWarnings = FALSE)
L10 <- log(10)

clade_table <- function(dir) {
  if (dir == "" || !file.exists(file.path(dir, "summary.csv"))) return(NULL)
  s <- fread(file.path(dir, "summary.csv"))
  get <- function(v) s[startsWith(label, paste0(v, ":")), .(minor = sub("^[^:]+:", "", label), median, q5, q95)]
  i <- get("clade_intercept"); b <- get("clade_slope"); g <- get("clade_sigma")
  out <- merge(merge(i[, .(minor, int = median, int_lo = q5, int_hi = q95)],
                     b[, .(minor, slope = median, slope_lo = q5, slope_hi = q95)], by = "minor"),
               g[, .(minor, sigma = median)], by = "minor")
  glob <- s[label %in% c("alpha0", "beta0", "gamma0", "gamma1", "tau_u", "tau_w", "lambda_a", "lambda_b", "lambda_g"),
            setNames(as.list(median), label)]
  list(source = dir, clades = out, global = glob)
}

rows <- fread(ROWS)
## clades: counts + lines from each fit available
cnt <- rows[, .(rows = .N, species = uniqueN(pred_taxon_id), m_lo = min(m), m_hi = max(m)), by = .(major = major_clade, minor = minor_clade)]
# no-pooling placeholder until the unpooled fit exists: OLS of ln prey on m per minor clade
ols <- rows[, { f <- lm(y ~ m); ci <- suppressWarnings(confint(f));
                .(int = coef(f)[1], int_lo = ci[1, 1], int_hi = ci[1, 2], slope = coef(f)[2],
                  slope_lo = ci[2, 1], slope_hi = ci[2, 2], sigma = summary(f)$sigma) }, by = .(minor = minor_clade)]
fits <- list(primary = clade_table(FIT), lambda0 = clade_table(LAM0), lambda1 = clade_table(LAM1),
             unpooled = if (UNPOOL != "") clade_table(UNPOOL) else list(source = "OLS per minor clade (placeholder)", clades = ols, global = NULL))
fits <- Filter(Negate(is.null), fits)
for (k in names(fits)) fits[[k]]$clades <- fits[[k]]$clades[minor %in% cnt$minor]

## points: stratified sample for display (all rows for small clades)
set.seed(1)
pts <- rows[, .SD[sample(.N, min(.N, 350))], by = minor_clade]
pts <- pts[, .(x = round(log10(pred_g), 3), y = round(log10(prey_g), 3), k = minor_clade, o = obs, sp = pred_taxon_id)]

## spindles: every record of a few living predators with very different diets
SPIN <- fread(text = Sys.getenv("SPINDLES", "taxon,label
Lanius_excubitor,Lanius
Strix_aluco,Strix
Salvator_merianae,Salvator
Vulpes_vulpes,Vulpes
Morelia_spilota,Morelia
Alligator_mississippiensis,Alligator"))
spindles <- lapply(seq_len(nrow(SPIN)), function(i) {
  x <- rows[pred_taxon_id == SPIN$taxon[i]]
  stopifnot(nrow(x) > 0)
  # fit of the Compsognathus meal (Conrad 2017: 9.1 g prey, 1.57 kg predator) as a ratio:
  # kernel density (bw 0.15 log10) of this species' log10(prey/predator) at Compsognathus's ratio
  cr <- log10(9.095 / 1568)
  list(species = gsub("_", " ", SPIN$taxon[i]), label = SPIN$label[i], major = x$major_clade[1], minor = x$minor_clade[1],
       pred_g = median(x$pred_g), n = nrow(x), sources = uniqueN(x$source_id), y = round(sort(log10(x$prey_g)), 3),
       comp_fit = signif(mean(dnorm(cr, log10(x$prey_g / x$pred_g), 0.15)), 4))
})

## every well-sampled species as a density spindle (log10 prey on a fixed grid), coloured by
## minor clade; minor clades with fewer than 3 such species fold into "other <major>"
MINREC <- as.integer(Sys.getenv("SPIN_MIN", "20"))
grid <- seq(-3.5, 6.5, by = 0.1)
sp_all <- rows[, .(n = .N, src = uniqueN(source_id), pred_g = median(pred_g), y = list(log10(prey_g))),
               by = .(major_clade, minor_clade, pred_taxon_id)][n >= MINREC]
small <- sp_all[, .N, by = minor_clade][N < 3, minor_clade]
sp_all[, group := fifelse(minor_clade %in% small, paste("other", major_clade), minor_clade)]
sp_all[group == "other Crocodylia", group := "Crocodylia"]
dens <- lapply(seq_len(nrow(sp_all)), function(i) {
  y <- sp_all$y[[i]]; d <- sapply(grid, function(g) sum(dnorm(g, y, 0.15)))
  d <- d / max(d); keep <- which(d > 0.03)
  list(sp = sp_all$pred_taxon_id[i], major = sp_all$major_clade[i], group = sp_all$group[i], n = sp_all$n[i],
       x = round(log10(sp_all$pred_g[i]), 3), y0 = grid[min(keep)], d = round(d[min(keep):max(keep)], 2))
})
spin_groups <- sp_all[, .(species = .N), by = .(major = major_clade, group)][order(major, -species)]

## fits along the trees: each extant minor clade's predicted meal size (as % of predator mass)
## at its median predator mass, new species from a new source, stomach contents (posterior
## medians of the clade line, spread and variance components). A minor clade "differs" when it
## has >= 20 species and the 90% interval of its slope excludes its major clade's line slope
## (major:<clade> in tree_prediction/params.csv).
tf <- local({
  sm <- fread(file.path(FIT, "summary.csv")); gl <- sm[, setNames(as.list(median), label)]
  get <- function(v) sm[startsWith(label, paste0(v, ":")), .(minor = sub("^[^:]+:", "", label), median, q5, q95)]
  ci <- get("clade_intercept"); cs <- get("clade_slope"); cg <- get("clade_sigma")
  maj <- fread(file.path(TREE, "params.csv"))[cand_type == "major" & par == "slope", .(major = sub("major:", "", candidate), major_slope = median)]
  m <- rows[, .(m = median(m), species = uniqueN(pred_taxon_id), major = major_clade[1]), by = .(minor = minor_clade)]
  x <- Reduce(function(a, b) merge(a, b, by = "minor"), list(m, ci[, .(minor, int = median)], cs[, .(minor, slope = median, slope_lo = q5, slope_hi = q95)], cg[, .(minor, sigma = median)]))
  x <- merge(x, maj, by = "major", all.x = TRUE)
  x[, `:=`(mu = int + slope * m, sd = sqrt((sigma * exp(gl$gamma1 * m))^2 + gl$tau_u^2 + gl$tau_w^2))]
  x[, lnpred := m + log(1000)]
  x[, `:=`(r50 = (mu - lnpred) / log(10) + 2, r10 = (mu - 1.2816 * sd - lnpred) / log(10) + 2, r90 = (mu + 1.2816 * sd - lnpred) / log(10) + 2,
           pred_g = exp(lnpred))]
  x[, differs := fifelse(species >= 20 & !is.na(major_slope) & (major_slope < slope_lo | major_slope > slope_hi),
                         fifelse(slope > major_slope, "steeper", "shallower"), "")]
  # posterior intervals from the joint draws, at each clade's median predator mass:
  #   c5/c95  90% interval of the clade's median meal as % of predator (the line, not new meals)
  #   dev     clade line minus the global line (alpha0 + beta0 m), log10 units, with 90% interval
  #   ds      clade slope minus the global slope, 90% interval
  # dev_differs / slope_differs: that interval excludes zero.
  ci <- local({
    f <- readRDS(file.path(FIT, "fit.rds"))
    lev <- readRDS(file.path(FIT, "stan_data.rds"))$minor_levels
    d <- posterior::as_draws_matrix(f$draws(c("alpha0", "beta0", "clade_intercept", "clade_slope")))
    rbindlist(lapply(seq_along(lev), function(k) {
      mk <- x[minor == lev[k], m]; if (!length(mk)) return(NULL)
      mu <- d[, sprintf("clade_intercept[%d]", k)] + d[, sprintf("clade_slope[%d]", k)] * mk
      r <- (mu - (mk + log(1000))) / log(10) + 2
      dv <- (mu - d[, "alpha0"] - d[, "beta0"] * mk) / log(10)
      ds <- d[, sprintf("clade_slope[%d]", k)] - d[, "beta0"]
      q <- function(v) as.numeric(quantile(v, c(0.05, 0.5, 0.95)))
      data.table(minor = lev[k], c5 = q(r)[1], c95 = q(r)[3], dev = q(dv)[2], dev_lo = q(dv)[1], dev_hi = q(dv)[3],
                 ds = q(ds)[2], ds_lo = q(ds)[1], ds_hi = q(ds)[3])
    }))
  })
  x <- merge(x, ci, by = "minor")
  x <- merge(x, rows[, .(rows = .N), by = .(minor = minor_clade)], by = "minor")
  x[, `:=`(dev_differs = fifelse(dev_lo > 0, "above", fifelse(dev_hi < 0, "below", "")),
           slope_differs = fifelse(ds_lo > 0, "steeper", fifelse(ds_hi < 0, "shallower", "")))]
  x[, .(minor, major, species, rows, pred_g = signif(pred_g, 3), slope = round(slope, 2), major_slope = round(major_slope, 2), differs,
        r10 = round(r10, 3), r50 = round(r50, 3), r90 = round(r90, 3), c5 = round(c5, 3), c95 = round(c95, 3),
        dev = round(dev, 3), dev_lo = round(dev_lo, 3), dev_hi = round(dev_hi, 3),
        ds = round(ds, 3), ds_lo = round(ds_lo, 3), ds_hi = round(ds_hi, 3), dev_differs, slope_differs)]
})
## observation type: the fit's shift in ln prey mass for each kind of record, against stomach contents
obs <- local({
  sm <- fread(file.path(FIT, "summary.csv"))[startsWith(label, "delta:"), .(obs = sub("delta:", "", label), d = median, d_lo = q5, d_hi = q95)]
  merge(sm, rows[, .(rows = .N, species = uniqueN(pred_taxon_id), sources = uniqueN(source_id)), by = obs], by = "obs")
})
## major-clade lines (tree_prediction/params.csv): int = ln prey at 1 kg (m = 0), slope
majors <- dcast(fread(file.path(TREE, "params.csv"))[cand_type == "major"], candidate ~ par, value.var = "median")[
  , .(major = sub("major:", "", candidate), int = ln_prey_at_1kg, slope)]
## theropod tree: median dated MRCA ages among the theropod taxa with records -> UPGMA on 2 x age
thero_tree <- local({
  mr <- readRDS("db/tree/theropod/mrca_ages.rds"); med <- apply(simplify2array(mr), c(1, 2), median)
  tx <- intersect(rownames(med), fread(file.path(FOSSIL, "records.csv"))[!startsWith(record_class, "excluded") & group == "Theropoda", unique(pred_taxon_id)])
  A <- med[tx, tx]; diag(A) <- 0
  tr <- ape::as.phylo(hclust(as.dist(2 * A), method = "average"))
  list(newick = ape::write.tree(tr), split = fread("db/tree/theropod/summary.csv")[taxon_id %in% tx, .(taxon_id, split_med)])
})

## fossils
fr <- fread(file.path(FOSSIL, "records.csv"))
fr <- fr[!startsWith(record_class, "excluded")]
fos <- fr[, .(pair_id, group, taxon = gsub("_", " ", pred_taxon_id), prey = gsub("_", " ", prey_taxon_id),
              obs = observation_type, cls = record_class, specimen = pred_specimen,
              pred = pred_mass_g, pred_lo = pred_mass_lo_g, pred_hi = pred_mass_hi_g,
              prey_g = prey_mass_g, prey_lo = prey_mass_lo_g, prey_hi = prey_mass_hi_g, age = age_ma)]

## tree prediction lines and fossil support
tl <- if (file.exists(file.path(TREE, "lines.csv"))) fread(file.path(TREE, "lines.csv")) else NULL
thero_tip <- if (!is.null(tl)) tl[cand_type == "tree" & startsWith(candidate, "tree:Theropoda@"), candidate[1]] else NA
tree <- if (!is.null(tl)) list(source = TREE, theropod_tip = thero_tip, lines = tl[candidate %in% c(thero_tip, "none", "major:Aves", "Crocodylia",
                                                                       "Accipitriformes", "Strigiformes")]) else NULL
sp <- if (file.exists(file.path(FOSSIL, "support.csv"))) fread(file.path(FOSSIL, "support.csv")) else NULL
support <- if (!is.null(sp)) list(source = FOSSIL,
  rows = sp[(unit %in% c("Theropoda", "Avialae", "Crocodyliformes", "Repenomamus_robustus", "Sanajeh_indicus",
                        "Aplestosuchus_sordidus", "Confractosuchus_sauroktonos") | startsWith(unit, "Theropoda:")) &
              comparison %in% c("all_minor", "major_vs_none", "tree_vs_major_vs_none", "dated_tree_vs_major_vs_none"),
            .(unit, comparison, candidate, lpd = round(lpd, 3), mcse = round(mcse, 3), rel = round(rel_support, 4))]) else NULL

## skeleton tree (terrestrial) and its shared-history matrix
SKW <- Sys.getenv("SKEL", "results/6_work/terr")
skel <- list(newick = readLines(file.path(SKW, "skeleton_tree.tre"))[1],
             corr = fread(file.path(SKW, "skeleton_corr.csv")))

D <- list(built = format(Sys.time(), "%Y-%m-%d %H:%M"), draft = TRUE,
          counts = cnt, fits = fits, skeleton = skel, points = pts, spindles = spindles, treefits = list(extant = tf, theropod_tree = thero_tree), majors = majors, obs = obs, allspin = list(step = 0.1, min_records = MINREC, groups = spin_groups, species = dens), fossils = fos, tree = tree, support = support)
js <- toJSON(D, dataframe = "rows", auto_unbox = TRUE, digits = 5, na = "null")
writeLines(js, "static/data/data.json")
writeLines(paste0("window.DATA = ", js, ";"), "static/data/data.js")
cat(sprintf("wrote static/data/data.js (%.0f kB): %d points, %d fossil records, fits: %s\n",
            file.size("static/data/data.js") / 1024, nrow(pts), nrow(fos), paste(names(fits), collapse = ", ")))
