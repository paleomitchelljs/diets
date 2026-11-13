###################################################################
# build_duplicate_evidence.R -- which records of a merged publication are duplicates?
#
# evidence/duplicate_studies.csv (hand-checked) lists publications that reached us through more
# than one upstream table. Within each, a block of records is a duplicate when another copy has
# the same predator species (synonyms below) and the same kind of evidence: "gut" (stomach,
# regurgitate) or "other" (pellets, scats, prey remains, observations). Different evidence kinds
# from one paper are different data and are all kept (e.g. Heintzelman 1964 stomachs in AvianDietDB,
# pellets in raw_data). The copy kept is chosen by preference
#     aviandietdb > raw_data > os_prey
# AvianDietDB first: full citations, documented AVONET/PanTHERIA masses, and the copy x20 used for
# bird stomachs. raw_data next: the copy x20 used for pellets, scats and observations, with masses
# for all prey classes. OS-Prey last.
# Writes evidence/duplicate_records.csv: one row per merged publication x upstream x predator x kind.
###################################################################
args_all <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args_all, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa[1]))), "../../..")) else normalizePath(getwd())
EVID <- file.path(ROOT, "code", "db_migrate", "evidence")
source(file.path(ROOT, "code", "db_migrate", "_ids.R"))
DS <- read.csv(file.path(EVID, "duplicate_studies.csv"), colClasses = "character", na.strings = character(0))
PREF <- c(aviandietdb = 1, raw_data = 2, os_prey = 3)
SYN <- c(Bubo_scandiaca = "Bubo_scandiacus", Circus_cyaneus = "Circus_hudsonius", Beteo_swainsoni = "Buteo_swainsoni")
syn <- function(v) ifelse(v %in% names(SYN), SYN[v], v)

## raw_data records
T <- read.csv(file.path(ROOT, "data", "raw_data.csv"), header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
h <- as.character(unlist(T[3, ])); d <- T[4:nrow(T), ]; f <- function(n) which(h == n)
rd <- data.frame(key = trimws(d[, 1]), pred = taxon_id_of(paste(trimws(d[, f("Genus")]), trimws(d[, f("Species")]), sep = "_")),
                 kind = ifelse(trimws(d[, f("ObsType")]) == "Stomach", "gut", "other"), stringsAsFactors = FALSE)
## AvianDietDB gut rows: citation from the full export
g <- read.csv(file.path(ROOT, "data", "structured", "aviandiet_gut_ppmr.csv"), colClasses = "character")
db <- read.csv(file.path(ROOT, "data", "structured", "aviandietdb_dietdb.csv"), colClasses = "character")
cits <- unique(unlist(strsplit(DS$adb_citation[nzchar(DS$adb_citation)], " \\|\\| ")))
ad <- unique(db[db$Source %in% cits & db$Scientific_Name %in% g$bird_species, c("Source", "Scientific_Name")])
ad <- data.frame(key = ad$Source, pred = taxon_id_of(ad$Scientific_Name), kind = "gut", stringsAsFactors = FALSE)
## OS-Prey
r <- read.csv(file.path(ROOT, "data", "structured", "raptor_diets_long.csv"), colClasses = "character")
os <- data.frame(key = r$study, pred = taxon_id_of(r$pred_species), kind = ifelse(r$obstype == "Stomach", "gut", "other"), stringsAsFactors = FALSE)

rows <- list()
for (i in seq_len(nrow(DS))) {
  m <- DS[i, ]; cp <- list()
  if (nzchar(m$raw_data_refno)) cp$raw_data <- rd[rd$key %in% strsplit(m$raw_data_refno, ";")[[1]], ]
  if (nzchar(m$adb_citation)) cp$aviandietdb <- ad[ad$key %in% strsplit(m$adb_citation, " \\|\\| ")[[1]], ]
  if (nzchar(m$osprey_study)) { x <- os[os$key %in% strsplit(m$osprey_study, ";")[[1]], ]
    if (nzchar(m$osprey_predators)) x <- x[x$pred %in% strsplit(m$osprey_predators, ";")[[1]], ]
    cp$os_prey <- x }
  if (length(cp) < 2) next                                  # source-only merges (Auffenberg, Weavers)
  blocks <- do.call(rbind, lapply(names(cp), function(u) { x <- cp[[u]]; if (!nrow(x)) return(NULL)
    a <- aggregate(list(n_records = rep(1L, nrow(x))), list(key = x$key, pred = x$pred, kind = x$kind), sum); cbind(upstream = u, a) }))
  blocks$match <- syn(blocks$pred)
  best <- tapply(PREF[blocks$upstream], paste(blocks$match, blocks$kind), min)
  blocks$kept_upstream <- names(PREF)[best[paste(blocks$match, blocks$kind)]]
  blocks$action <- ifelse(blocks$upstream == blocks$kept_upstream, "keep", "drop")
  rows[[length(rows) + 1]] <- cbind(merged_source_id = m$merged_source_id, blocks)
}
R <- do.call(rbind, rows); R <- R[order(R$merged_source_id, R$match, R$kind, PREF[R$upstream]), ]
names(R)[names(R) == "n_records"] <- "n_upstream_rows"
write.csv(R[, c("merged_source_id", "upstream", "key", "pred", "match", "kind", "n_upstream_rows", "action", "kept_upstream")], file.path(EVID, "duplicate_records.csv"), row.names = FALSE)
cat(sprintf("duplicate_records.csv: %d blocks, %d dropped (%d upstream rows)\n", nrow(R), sum(R$action == "drop"), sum(R$n_upstream_rows[R$action == "drop"])))
print(aggregate(n_upstream_rows ~ upstream + action, R, sum))
