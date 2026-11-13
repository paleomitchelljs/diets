###################################################################
# 90_prey_mass_fill.R -- Phase 4b Part 2: prey masses for extant pairs that have a predator mass and no prey mass.
# POST-PROCESSING SCRIPT: it owns no source and writes no new pair. It fills empty prey-mass fields of pairs owned by other
# scripts, so run it after all of them (run_all.R does: the name sorts last). Idempotent: every row it fills carries the tag
# "[Phase 4b prey fill; tier before: X]" in notes; each run first undoes its own earlier fills (clearing prey mass, interval, method,
# basis, restoring the tier and removing the tag) and then fills again from the empty fields.
# Rules (Prompt 4b, Part 2; never invents a mass):
#  1. Prey identified to species in a bird, mammal, reptile or amphibian: the species mean (AVONET, PanTHERIA, Amniote, AmphiBIO;
#     genus mean if the species name is not found, as 04_raptordiets.R does) -> species_average_database. The tier is kept or
#     made coarser by tier_of(). Prey whose stage is not adult (juvenile, neonate, hatchling, egg, larva) are NOT filled: an adult mean
#     would misstate them.
#  2. Prey identified to genus, family or order in those four classes: the median of the species means of that taxon in the same
#     database, interval = min to max of those species means -> assigned_approximate, tier E. Basis names the database, the
#     rank, the number of species and how the interval is defined. (Assigned values are typical values, not measurements.)
#  3. Invertebrates and fishes (no species trait database on disk): the median of GATEWAy 1.0 resource masses (res.mass.mean.g)
#     for the most specific taxon name that GATEWAy holds among the prey's own name, family, order and class; interval = min to max of
#     those rows -> assigned_approximate, tier E; GATEWAy is the source, its own size citations are in the GATEWAy metadata.
#     Published length-mass equations were not used: no invertebrate prey row in db/ has a source-given body length (checked in
#     measurements.csv), and no invertebrate equation is registered in methodology_index.csv.
#  4. Everything else stays without a prey mass: class-level or coarser prey, pooled placeholders, non-adult prey stages, taxa
#     absent from the databases. They are listed in db/migration/prey_mass_remaining.csv with the reason.
# Outputs: db/migration/prey_mass_fill.csv (every fill), db/migration/prey_mass_remaining.csv.
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "90_prey_mass_fill"; TAG <- "[Phase 4b prey fill; tier before: "
P <- read_db("pairs"); X <- read_db("taxa")
## ---- undo earlier fills ----
filled <- grepl(TAG, P$notes, fixed = TRUE)
if (any(filled)) {
  tb <- sub(paste0("^.*\\Q", TAG, "\\E([A-Z_a-z]+)\\].*$"), "\\1", P$notes[filled], perl = TRUE)
  P$pair_resolution[filled] <- tb
  for (cl in c("prey_mass_g", "prey_mass_lo_g", "prey_mass_hi_g", "prey_mass_method", "prey_mass_method_id", "prey_mass_basis")) P[[cl]][filled] <- ""
  P$notes[filled] <- sub(paste0(" ?\\Q", TAG, "\\E[A-Z_a-z]+\\]"), "", P$notes[filled], perl = TRUE); P$notes[filled] <- trimws(sub("^; ", "", P$notes[filled]))
}
## ---- reference tables ----
M <- .MASSDB()
av <- read.csv(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), check.names = FALSE, stringsAsFactors = FALSE)
avt <- data.frame(order = normn(av$Order1), family = normn(av$Family1), genus = sub(" .*$", "", normn(av$Species1)), m = num(av$Mass))
pan <- read.delim(file.path(ROOT, "data", "pantheria"), header = FALSE, skip = 1, quote = "", colClasses = "character")
pmm <- num(pan[[5]]); pmm[pmm == -999] <- NA
pat <- data.frame(order = normn(pan[[1]]), family = normn(pan[[2]]), genus = normn(pan[[3]]), m = pmm)
amt <- data.frame(order = normn(M$am_tab$order), family = normn(M$am_tab$family), genus = normn(M$am_tab$genus), m = M$am_tab$adult_body_mass_g)
pht <- data.frame(order = normn(M$ph_tab$Order), family = normn(M$ph_tab$Family), genus = normn(M$ph_tab$Genus), m = M$ph_tab$Body_mass_g)
REF <- list(Aves = list(t = avt, db = "AVONET1_BirdLife (Mass)"), Mammalia = list(t = pat, db = "PanTHERIA (column 5)"),
            Reptilia = list(t = amt, db = "Amniote database (adult_body_mass_g)"), Amphibia = list(t = pht, db = "AmphiBIO (Body_mass_g)"))
## GATEWAy resource masses (invertebrates and ectotherm vertebrates)
ZIP <- "data/structured/raw_downloads/gateway/GATEWAy_v1.0_iDiv283_v3.zip"; td <- tempfile("gw"); dir.create(td)
inner <- grep("FoodWebDataBase.*\\.csv$", unzip(file.path(ROOT, ZIP), list = TRUE)$Name, value = TRUE)
suppressWarnings(unzip(file.path(ROOT, ZIP), files = inner, exdir = td, junkpaths = TRUE))
gw <- read.csv(list.files(td, pattern = "csv$", full.names = TRUE)[1], colClasses = "character", na.strings = "NA", check.names = FALSE)
gw <- gw[gw$res.metabolic.type %in% c("invertebrate", "ectotherm vertebrate"), c("res.taxonomy", "res.mass.mean.g.", "res.lifestage")]
gw$m <- num(gw$res.mass.mean.g.); gw <- gw[is.finite(gw$m) & gw$m > 0, ]
gw$nm <- normn(sub("\\s+(sp|spp)\\.?$", "", sub("\\s*\\(.*\\)\\s*$", "", gw$res.taxonomy)))
gwl <- split(gw$m, gw$nm)
gw_stat <- function(nm) { v <- gwl[[nm]]; if (is.null(v)) return(NULL); list(med = median(v), lo = min(v), hi = max(v), n = length(v)) }
## ---- candidate rows ----
ex <- P$is_fossil == "FALSE" & P$pred_mass_g != "" & P$prey_mass_g == ""
ti <- match(P$prey_taxon_id, X$taxon_id)
tx <- data.frame(row = which(ex), name = normn(X$accepted_name[ti[ex]]), rank = X$rank[ti[ex]], cls = X$class[ti[ex]], ord = normn(X$order[ti[ex]]), fam = normn(X$family[ti[ex]]),
                 vert = X$is_vertebrate[ti[ex]], id = P$prey_taxon_id[ex], stage = P$prey_stage[ex], stringsAsFactors = FALSE)
nonadult <- grepl("juv|neonat|hatchl|egg|larv|nymph|pup|chick|young|immature|pupa", tolower(tx$stage))
POOLED <- grepl("pooled|indet|_or_|^animalia$|^vertebrata$", tolower(tx$id)) | tx$rank %in% c("unranked", "unranked_alternatives", "unranked_clade", "")
## unique prey taxa: resolve once
cls_of_name <- function(nm, rk) {            # which of the four trait tables holds this name at this rank
  for (cl in names(REF)) { t <- REF[[cl]]$t; col <- switch(rk, genus = t$genus, family = t$family, order = t$order, species = NULL); if (!is.null(col) && nm %in% col) return(cl) }
  NA_character_ }
sp_table <- function(cl, nm) {
  two <- sub("^(\\S+ \\S+).*$", "\\1", nm); sp <- switch(cl, Aves = M$av_sp, Mammalia = M$pan_sp, Reptilia = M$am_sp, Amphibia = M$ph_sp)
  gn <- switch(cl, Aves = M$av_gen, Mammalia = M$pan_gen, Reptilia = M$am_gen, Amphibia = M$ph_gen); g <- sub(" .*$", "", nm)
  if (two %in% names(sp)) return(list(v = sp[[two]], b = paste0(REF[[cl]]$db, " species mean for ", tools::toTitleCase(two)), lvl = "species"))
  if (g %in% names(gn)) return(list(v = gn[[g]], b = paste0(REF[[cl]]$db, " genus mean for ", tools::toTitleCase(g), " (species not found under this name)"), lvl = "genus_mean"))
  NULL }
typ_table <- function(cl, nm, rk) {
  t <- REF[[cl]]$t; col <- switch(rk, genus = t$genus, family = t$family, order = t$order); v <- t$m[col == nm & is.finite(t$m) & t$m > 0]
  if (!length(v)) return(NULL)
  list(v = median(v), lo = min(v), hi = max(v), b = sprintf("median of %d species means in %s for %s %s; interval = min to max of those species means; typical value, not a measurement", length(v), REF[[cl]]$db, rk, tools::toTitleCase(nm)), lvl = rk) }
uq <- unique(tx[, c("name", "rank", "cls", "ord", "fam", "vert")]); uq$key <- paste(uq$name, uq$rank, uq$cls, uq$ord, uq$fam, uq$vert)
res <- lapply(seq_len(nrow(uq)), function(i) {
  nm <- uq$name[i]; rk <- uq$rank[i]; cl <- uq$cls[i]
  tc <- if (cl %in% names(REF)) cl else NA_character_
  if (is.na(tc) && isTRUE(uq$vert[i] == "TRUE") && rk %in% c("genus", "family", "order")) tc <- cls_of_name(nm, rk)
  if (is.na(tc) && isTRUE(uq$vert[i] == "TRUE") && rk == "species") for (c2 in names(REF)) if (!is.null(sp_table(c2, nm)) && grepl(" ", nm)) { tc <- c2; break }
  if (!is.na(tc) && rk == "species" && grepl(" ", nm)) { s <- sp_table(tc, nm); if (!is.null(s)) return(list(kind = "species", s = s)) }
  if (!is.na(tc) && rk %in% c("genus", "family", "order")) { s <- typ_table(tc, nm, rk); if (!is.null(s)) return(list(kind = "typical", s = s)) }
  if (!is.na(tc) && rk == "species") {                       # species absent from its class table: typical mass of its family or order
    for (lv in c("family", "order")) { nn <- if (lv == "family") uq$fam[i] else uq$ord[i]; if (nn != "") { s <- typ_table(tc, nn, lv); if (!is.null(s)) { s$b <- paste0(s$b, " (species ", tools::toTitleCase(nm), " not in the database; typical mass of its ", lv, ")"); return(list(kind = "typical", s = s)) } } }
  }
  ## invertebrates, fishes, anything not in the four trait tables: GATEWAy, most specific name first
  if (is.na(tc) && !(isTRUE(uq$vert[i] == "TRUE") && cl %in% c("Aves", "Mammalia", "Reptilia", "Amphibia"))) {
    for (cand in list(c(nm, rk), c(uq$fam[i], "family"), c(uq$ord[i], "order"), c(normn(cl), "class"))) {
      if (cand[1] != "" && !is.na(cand[1])) { s <- gw_stat(cand[1]); if (!is.null(s)) return(list(kind = "gateway", s = list(v = s$med, lo = s$lo, hi = s$hi, b = sprintf("median of %d GATEWAy 1.0 resource mass records (res.mass.mean.g) for %s %s; interval = min to max of those records; typical value, not a measurement; no length available for a published length-mass equation", s$n, cand[2], tools::toTitleCase(cand[1])), lvl = cand[2]))) }
    }
  }
  NULL })
names(res) <- uq$key
tx$key <- paste(tx$name, tx$rank, tx$cls, tx$ord, tx$fam, tx$vert); r_of <- res[tx$key]
order_t <- c("A_both_measured", "B_pred_measured_prey_average", "C_prey_measured_pred_average", "D_both_average", "E_coarse_or_aggregate")
n <- nrow(tx); why <- rep("", n); kind <- character(n); v <- lo <- hi <- rep(NA_real_, n); bas <- lvl <- character(n)
for (i in seq_len(n)) {
  r <- r_of[[i]]
  why[i] <- if (POOLED[i]) "prey is a pooled or class-level placeholder" else if (nonadult[i]) paste0("prey stage '", tx$stage[i], "' is not adult; an adult mean would misstate it") else if (is.null(r)) "taxon not found in the species databases or GATEWAy at its rank, family, order or class" else ""
  if (why[i] == "") { s <- r$s; kind[i] <- r$kind; v[i] <- s$v; bas[i] <- s$b; lvl[i] <- s$lvl; if (r$kind != "species") { lo[i] <- s$lo; hi[i] <- s$hi } }
}
f <- which(why == ""); j <- tx$row[f]
meth <- ifelse(kind[f] == "species", "species_average_database", "assigned_approximate")
old <- P$pair_resolution[j]; new_t <- tier_of(P$pred_mass_method[j], meth)
tier <- order_t[pmax(match(old, order_t), match(new_t, order_t))]
okint <- is.finite(lo[f]) & is.finite(hi[f]) & lo[f] <= v[f] & hi[f] >= v[f]
P$prey_mass_g[j] <- fnum(v[f]); P$prey_mass_method[j] <- meth
P$prey_mass_basis[j] <- ifelse(kind[f] == "species", paste0(bas[f], "; adult mean (filled Phase 4b)"), paste0(bas[f], " (filled Phase 4b)"))
P$prey_mass_lo_g[j] <- ifelse(okint, fnum(lo[f]), ""); P$prey_mass_hi_g[j] <- ifelse(okint, fnum(hi[f]), "")
P$pair_resolution[j] <- tier
P$notes[j] <- paste0(ifelse(P$notes[j] != "", paste0(P$notes[j], " "), ""), TAG, old, "]")
log_fill <- data.frame(pair_id = P$pair_id[j], source_id = P$source_id[j], prey_taxon_id = P$prey_taxon_id[j], prey_rank = tx$rank[f], method = meth, level = lvl[f], prey_mass_g = P$prey_mass_g[j],
  lo_g = P$prey_mass_lo_g[j], hi_g = P$prey_mass_hi_g[j], tier_before = old, tier_after = tier)
rm_i <- which(why != ""); remain <- data.frame(pair_id = P$pair_id[tx$row[rm_i]], source_id = P$source_id[tx$row[rm_i]], prey_taxon_id = P$prey_taxon_id[tx$row[rm_i]], prey_rank = tx$rank[rm_i], reason = why[rm_i])
write.csv(log_fill[order(log_fill$pair_id), ], file.path(MIG, "prey_mass_fill.csv"), row.names = FALSE)
write.csv(remain[order(remain$pair_id), ], file.path(MIG, "prey_mass_remaining.csv"), row.names = FALSE)
P <- P[order(P$pair_id), ]; write_db("pairs", P)
cat(sprintf("%s: candidates %d, filled %d, remain without a prey mass %d\n", OWNER, nrow(tx), nrow(log_fill), nrow(remain)))
