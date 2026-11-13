# b01: minor-clade map and analysis cells for the Phase 6 first-pass fit.
# Reads db/pairs.csv and db/taxa.csv only. Writes:
#   db/clade_map.csv                      one row per extant predator taxon with data
#   results/6_work/analysis_rows.csv      extant, vertebrate prey, both masses
#   results/6_work/analysis_cells.csv     sufficient statistics per cell
#   results/6_work/design_counts.csv      rows/species/sources by minor clade x obs type
# First pass (JM 2026-11-12): minor clades are the focal unit, pooled toward one
# global relationship; scat and pellet are one observation type.
suppressPackageStartupMessages(library(data.table))
dir.create("results/6_work", recursive = TRUE, showWarnings = FALSE)

pairs <- fread("db/pairs.csv", colClasses = "character", na.strings = NULL)
taxa  <- fread("db/taxa.csv",  colClasses = "character", na.strings = NULL)
num <- function(x) suppressWarnings(as.numeric(x))

d <- pairs[is_fossil != "TRUE"]
d[, `:=`(pred_g = num(pred_mass_g), prey_g = num(prey_mass_g))]
d <- merge(d, taxa[, .(prey_taxon_id = taxon_id, prey_vert = is_vertebrate, prey_class = class)],
           by = "prey_taxon_id", all.x = TRUE)
d <- d[!is.na(pred_g) & !is.na(prey_g) & pred_g > 0 & prey_g > 0 & prey_vert == "TRUE"]

## ---- clade map -------------------------------------------------------------
tx <- taxa[taxon_id %in% unique(d$pred_taxon_id),
           .(taxon_id, class, order, family, analysis_clade)]
tx[, genus := sub("_.*$", "", taxon_id)]
tx[family == "Acciptridae", family := "Accipitridae"]

# Explicit genus tables for taxa whose order or family is blank in db/taxa.csv.
# Whole genus names only; basis = standard taxonomy, entered by hand 2026-11-12.
genus_minor <- c(
  # Aves
  Aphrodroma = "Procellariiformes", Daption = "Procellariiformes", Halobaena = "Procellariiformes",
  Macronectes = "Procellariiformes", Pachyptila = "Procellariiformes", Pagodroma = "Procellariiformes",
  Thalassoica = "Procellariiformes",
  Aptenodytes = "Sphenisciformes", Pygoscelis = "Sphenisciformes",
  Ardea = "Pelecaniformes", Egretta = "Pelecaniformes",
  Phalacrocorax = "Suliformes",
  Larus = "Charadriiformes", Sterna = "Charadriiformes", Limnodromus = "Charadriiformes",
  Falco = "Falconiformes",
  Cathartes = "Cathartiformes", Mycteria = "Ciconiiformes", Ramphastos = "Piciformes",
  # Mammalia
  Arctocephalus = "Pinnipedia", Halichoerus = "Pinnipedia", Hydrurga = "Pinnipedia",
  Leptonychotes = "Pinnipedia", Lobodon = "Pinnipedia", Mirounga = "Pinnipedia",
  Ommatophoca = "Pinnipedia",
  Balaenoptera = "Cetacea", Lagenorhynchus = "Cetacea", Orcinus = "Cetacea",
  Physeter = "Cetacea", Tursiops = "Cetacea",
  Canis = "Caniformia", Lycaeon = "Caniformia", Ursus = "Caniformia", Procyon = "Caniformia",
  Mustela = "Caniformia", Hydrictis = "Caniformia",
  Herpestes = "Feliformia", Puma = "Feliformia", Uncia = "Feliformia",
  # Squamata
  Basiliscus = "Iguania"
)
caniform <- c("Canidae", "Mustelidae", "Mephitidae", "Procyonidae", "Ursidae")
pinniped <- c("Phocidae", "Otariidae", "Odobenidae")
feliform <- c("Felidae", "Hyaenidae", "Viverridae", "Herpestidae", "Eupleridae")
iguania  <- c("Corytophanidae", "Crotaphytidae", "Leiosauridae", "Liolaemidae", "Tropiduridae",
              "Iguanidae", "Agamidae", "Chamaeleonidae", "Phrynosomatidae", "Dactyloidae")

tx[, major_clade := fcase(analysis_clade == "Aves", "Aves",
                          analysis_clade == "Mammalia", "Mammalia",
                          analysis_clade %in% c("Lacertilia", "Serpentes"), "Squamata",
                          analysis_clade == "Crocodylia", "Crocodylia",
                          default = NA_character_)]
tx[, `:=`(minor_raw = NA_character_, basis = NA_character_)]
tx[analysis_clade == "Serpentes",  `:=`(minor_raw = "Serpentes",  basis = "taxa.csv analysis_clade")]
tx[analysis_clade == "Crocodylia", `:=`(minor_raw = "Crocodylia", basis = "taxa.csv analysis_clade")]
tx[major_clade == "Aves" & order != "", `:=`(minor_raw = order, basis = "taxa.csv order")]
tx[minor_raw == "Ardeiformes", `:=`(minor_raw = "Pelecaniformes", basis = "Ardeiformes merged into Pelecaniformes")]
tx[major_clade == "Aves" & family == "Cathartidae", `:=`(minor_raw = "Cathartiformes", basis = "taxa.csv family")]
tx[major_clade == "Aves" & family == "Ciconiidae",  `:=`(minor_raw = "Ciconiiformes",  basis = "taxa.csv family")]
tx[major_clade == "Aves" & family == "Ramphastidae",`:=`(minor_raw = "Piciformes",     basis = "taxa.csv family")]
tx[major_clade == "Mammalia" & family %in% caniform, `:=`(minor_raw = "Caniformia", basis = "taxa.csv family")]
tx[major_clade == "Mammalia" & family %in% pinniped, `:=`(minor_raw = "Pinnipedia", basis = "taxa.csv family")]
tx[major_clade == "Mammalia" & family %in% feliform, `:=`(minor_raw = "Feliformia", basis = "taxa.csv family")]
tx[major_clade == "Mammalia" & family == "Dasyuridae",  `:=`(minor_raw = "Dasyuromorphia",  basis = "taxa.csv family (order field inconsistent)")]
tx[major_clade == "Mammalia" & family == "Didelphidae", `:=`(minor_raw = "Didelphimorphia", basis = "taxa.csv family")]
tx[major_clade == "Mammalia" & is.na(minor_raw) & order != "" & order != "Carnivora",
   `:=`(minor_raw = order, basis = "taxa.csv order")]
tx[analysis_clade == "Lacertilia" & family == "Varanidae", `:=`(minor_raw = "Varanidae", basis = "taxa.csv family")]
tx[analysis_clade == "Lacertilia" & family %in% c("Teiidae", "Gymnophthalmidae"),
   `:=`(minor_raw = "Teiioidea", basis = "taxa.csv family")]
tx[analysis_clade == "Lacertilia" & family %in% iguania, `:=`(minor_raw = "Iguania", basis = "taxa.csv family")]
tx[analysis_clade == "Lacertilia" & is.na(minor_raw) & family != "",
   `:=`(minor_raw = "other Squamata", basis = "taxa.csv family; not Varanidae, Teiioidea or Iguania")]
tx[is.na(minor_raw) & genus %in% names(genus_minor),
   `:=`(minor_raw = genus_minor[genus], basis = "genus table in b01 (order/family blank in taxa.csv)")]

unplaced <- tx[is.na(minor_raw) | is.na(major_clade)]
if (nrow(unplaced)) {
  cat("UNPLACED predator taxa (dropped):\n"); print(unplaced[, .(taxon_id, class, order, family)])
}
tx <- tx[!is.na(minor_raw) & !is.na(major_clade)]

# Merge minor clades with fewer than 5 predator species into "other <major>".
nsp <- tx[, .N, by = .(major_clade, minor_raw)]
small <- nsp[N < 5 & minor_raw != major_clade]
tx[, minor_clade := minor_raw]
tx[small, on = .(major_clade, minor_raw), minor_clade := paste("other", major_clade)]
fwrite(tx[order(major_clade, minor_clade, taxon_id),
          .(taxon_id, major_clade, minor_clade, minor_before_merge = minor_raw, family_clean = family, basis)],
       "db/clade_map.csv")

## ---- analysis rows ---------------------------------------------------------
d <- merge(d, tx[, .(pred_taxon_id = taxon_id, major_clade, minor_clade)], by = "pred_taxon_id")
d[, obs := fifelse(observation_type %in% c("pellet", "scat"), "pellet_scat", observation_type)]
d[, `:=`(y = log(prey_g), m = log(pred_g) - log(1000))]
fwrite(d[, .(pair_id, source_id, pred_taxon_id, prey_taxon_id, major_clade, minor_clade,
             observation_type, obs, pair_resolution, individual_pair, prey_class,
             pred_g, prey_g, y, m)], "results/6_work/analysis_rows.csv")

cells <- d[, .(n = .N, ybar = mean(y), SS = sum((y - mean(y))^2)),
           by = .(pred_taxon_id, source_id, obs, m, major_clade, minor_clade)]
fwrite(cells, "results/6_work/analysis_cells.csv")

cnt <- d[, .(rows = .N, species = uniqueN(pred_taxon_id), sources = uniqueN(source_id)),
         by = .(major_clade, minor_clade, obs)][order(major_clade, minor_clade, obs)]
fwrite(cnt, "results/6_work/design_counts.csv")

## ---- check: cell likelihood equals row likelihood ---------------------------
set.seed(1)
cid <- d[, .GRP, by = .(pred_taxon_id, source_id, obs, m)]
mu <- rnorm(nrow(cells), 4, 1); s <- exp(rnorm(nrow(cells), 0, 0.3))
dd <- merge(d, cbind(cells[, .(pred_taxon_id, source_id, obs, m)], mu = mu, s = s),
            by = c("pred_taxon_id", "source_id", "obs", "m"))
row_ll  <- sum(dnorm(dd$y, dd$mu, dd$s, log = TRUE))
cell_ll <- sum(-cells$n * log(s) - (cells$SS + cells$n * (cells$ybar - mu)^2) / (2 * s^2)) -
           nrow(d) * 0.5 * log(2 * pi)
stopifnot(abs(row_ll - cell_ll) < 1e-6 * abs(row_ll))

cat(sprintf("rows %d | cells %d | species %d | sources %d | minor clades %d\n",
            nrow(d), nrow(cells), uniqueN(d$pred_taxon_id), uniqueN(d$source_id), uniqueN(d$minor_clade)))
print(d[, .(rows = .N, species = uniqueN(pred_taxon_id)), by = .(major_clade, minor_clade)][order(major_clade, -species)])
print(dcast(d, minor_clade ~ obs, value.var = "y", fun.aggregate = length))
