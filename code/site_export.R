# site_export: build static/data/data.js (and data.json) for the talk site.
# The site reads only this file. DRAFT: until the terrestrial fits and the fossil test
# finish, model numbers come from whatever fit folder is named below; every block carries
# its source so the site can mark draft values.
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

## one hawk species: every record
hk <- rows[pred_taxon_id == HAWK]
hawk <- list(species = gsub("_", " ", HAWK), pred_g = hk$pred_g[1], n = nrow(hk), sources = uniqueN(hk$source_id),
             y = round(sort(log10(hk$prey_g)), 3))

## fossils
fr <- fread(file.path(FOSSIL, "records.csv"))
fr <- fr[!startsWith(record_class, "excluded")]
fos <- fr[, .(pair_id, group, taxon = gsub("_", " ", pred_taxon_id), prey = gsub("_", " ", prey_taxon_id),
              obs = observation_type, cls = record_class, specimen = pred_specimen,
              pred = pred_mass_g, pred_lo = pred_mass_lo_g, pred_hi = pred_mass_hi_g,
              prey_g = prey_mass_g, prey_lo = prey_mass_lo_g, prey_hi = prey_mass_hi_g, age = age_ma)]

## tree prediction lines and fossil support
tl <- if (file.exists(file.path(TREE, "lines.csv"))) fread(file.path(TREE, "lines.csv")) else NULL
tree <- if (!is.null(tl)) list(source = TREE, lines = tl[candidate %in% c("tree:Theropoda@165", "none", "major:Aves", "Crocodylia",
                                                                       "Accipitriformes", "Strigiformes")]) else NULL
sp <- if (file.exists(file.path(FOSSIL, "support.csv"))) fread(file.path(FOSSIL, "support.csv")) else NULL
support <- if (!is.null(sp)) list(source = FOSSIL,
  rows = sp[unit %in% c("Theropoda", "Avialae", "Crocodyliformes", "Repenomamus_robustus", "Sanajeh_indicus",
                        "Aplestosuchus_sordidus", "Confractosuchus_sauroktonos") &
              comparison %in% c("all_minor", "major_vs_none", "tree_vs_major_vs_none"),
            .(unit, comparison, candidate, lpd = round(lpd, 3), mcse = round(mcse, 3), rel = round(rel_support, 4))]) else NULL

## skeleton tree (terrestrial) and its shared-history matrix
SKW <- Sys.getenv("SKEL", "results/6_work/terr")
skel <- list(newick = readLines(file.path(SKW, "skeleton_tree.tre"))[1],
             corr = fread(file.path(SKW, "skeleton_corr.csv")))

D <- list(built = format(Sys.time(), "%Y-%m-%d %H:%M"), draft = TRUE,
          counts = cnt, fits = fits, skeleton = skel, points = pts, hawk = hawk, fossils = fos, tree = tree, support = support)
js <- toJSON(D, dataframe = "rows", auto_unbox = TRUE, digits = 5, na = "null")
writeLines(js, "static/data/data.json")
writeLines(paste0("window.DATA = ", js, ";"), "static/data/data.js")
cat(sprintf("wrote static/data/data.js (%.0f kB): %d points, %d fossil records, fits: %s\n",
            file.size("static/data/data.js") / 1024, nrow(pts), nrow(fos), paste(names(fits), collapse = ", ")))
