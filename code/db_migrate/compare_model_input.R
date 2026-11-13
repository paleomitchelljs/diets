###################################################################
# compare_model_input.R -- does the migrated tier A-D vertebrate-prey subset reproduce the
# 15,260-row input of the current model (x20_gutSlopesStan.R, run logs/vertonly_20260603_113941.log)?
#
# Step 1  Rebuild x20's assembly from the OLD files exactly as x20 does it (x00 expansion of
#         raw_data.csv + mass_overrides.csv; aviandiet_gut_ppmr.csv; extant_new_observations.csv),
#         but keep a row id for every item so each can be traced to a db pair. Check the rebuild
#         against data/longform.csv and against the June log (144,152 -> 15,260).
# Step 2  Build the same thing from db/: pairs from the same three sources, tier A-D, prey taxon
#         is_vertebrate TRUE, both masses present, predator analysis_clade in x20's five clades,
#         observation type mapped to x20's four levels, raw_data pairs expanded by n_items.
# Step 3  Join the two item by item and give every difference a reason.
# Step 4  Apply x20's subsampling (all Stomach kept; other cells <= 150 per study x species by
#         predator-mass spread, then <= 1500 per cell) to both and compare cell counts.
# Tolerance: an item reproduces when both ln masses agree to 1e-6 (db stores 8 significant digits).
# Writes db/migration/compare_*.csv; prints a summary.
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
TOL <- 1e-6
CL5 <- c("Aves", "Crocodylia", "Lacertilia", "Mammalia", "Serpentes")

## ================= Step 1: x20's assembly, with provenance =================
T <- read.csv(file.path(ROOT, "data", "raw_data.csv"), header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
SC <- 31; pc <- SC:ncol(T); d <- T[4:nrow(T), ]; lines <- 4:nrow(T)
hdr <- as.character(unlist(T[3, ])); f <- function(n) which(hdr == n)
PreyMass <- num(hdr[pc]); PreyGenus <- as.character(T[1, pc]); PreyCls <- as.character(T[2, pc])
PredMass <- num(d[, f("Mass")]); Genus <- d[, f("Genus")]
ov <- read.csv(file.path(ROOT, "data", "mass_overrides.csv"), stringsAsFactors = FALSE); lut <- setNames(num(ov$mass_g), ov$key)
pk <- paste(Genus, d[, f("Species")]); qk <- paste(PreyGenus, PreyCls)
hp <- is.na(PredMass) & pk %in% names(lut); PredMass[hp] <- lut[pk[hp]]
hq <- is.na(PreyMass) & qk %in% names(lut); PreyMass[hq] <- lut[qk[hq]]
Cn <- suppressWarnings(matrix(num(as.matrix(d[, pc])), nrow = nrow(d)))
w <- which(!is.na(Cn) & Cn > 0, arr.ind = TRUE); w <- w[order(w[, 1], w[, 2]), , drop = FALSE]
ok <- is.finite(PredMass[w[, 1]]) & is.finite(PreyMass[w[, 2]]); w <- w[ok, , drop = FALSE]
reps <- as.integer(Cn[w])                                    # rep() truncates fractional counts, as in x00
cells <- data.frame(line = lines[w[, 1]], col = SC + w[, 2] - 1, reps = reps, ref = trimws(d[w[, 1], 1]),
  species = paste(Genus[w[, 1]], d[w[, 1], f("Species")], sep = "_"), clade0 = d[w[, 1], f("Class")], sub0 = d[w[, 1], f("Clade")],
  obstype = d[w[, 1], f("ObsType")], prey_type = prey_type(PreyCls[w[, 2]]), predmass = log(PredMass[w[, 1]]), preyMass = log(PreyMass[w[, 2]]), stringsAsFactors = FALSE)
cells <- cells[cells$reps > 0, ]
lf_rebuilt <- cells[rep(seq_len(nrow(cells)), cells$reps), ]
lf_old <- read.csv(file.path(ROOT, "data", "longform.csv"))
chk_lf <- c(rebuilt = nrow(lf_rebuilt), longform_csv = nrow(lf_old),
            same_multiset = isTRUE(all.equal(sort(round(lf_rebuilt$preyMass, 6)), sort(round(lf_old$preyMass, 6)))) &&
                            isTRUE(all.equal(sort(round(lf_rebuilt$predmass, 6)), sort(round(lf_old$predmass, 6)))))
## x20 section 1a
lf <- lf_rebuilt
lf$clade <- ifelse(lf$clade0 != "Squamata", lf$clade0, ifelse(lf$sub0 == "Serpentes", "Serpentes", "Lacertilia"))
lf$obs2 <- ifelse(lf$clade == "Aves" & lf$obstype %in% c("Scat", "Larder"), "Pellet", lf$obstype)
lf <- lf[lf$clade %in% CL5 & lf$obs2 %in% c("Stomach", "Scat", "Follow", "Pellet") & !(lf$clade == "Aves" & lf$obs2 == "Stomach"), ]
X_lf <- data.frame(src = "raw_data", key = paste0("L", lf$line, "C", lf$col), preyMass = lf$preyMass, predmass = lf$predmass, clade = lf$clade,
                   prey_type = lf$prey_type, obstype = lf$obs2, species = lf$species, study = lf$ref, stringsAsFactors = FALSE)
## 1b ADB
adb <- read.csv(file.path(ROOT, "data", "structured", "aviandiet_gut_ppmr.csv")); adb$k <- seq_len(nrow(adb))
adb <- adb[is.finite(adb$pred_mass_g) & is.finite(adb$prey_mass_g) & adb$pred_mass_g > 0 & adb$prey_mass_g > 0, ]
X_adb <- data.frame(src = "aviandietdb", key = paste0("ADB_r", adb$k), preyMass = log(adb$prey_mass_g), predmass = log(adb$pred_mass_g), clade = "Aves",
                    prey_type = prey_type(adb$prey_class), obstype = "Stomach", species = adb$bird_species, study = adb$source_file, stringsAsFactors = FALSE)
## 1c new observations
OBMAP <- c(Stomach = "Stomach", Stomach_DNA_metabarcoding = "Stomach", Gizzard = "Stomach", Regurgitation = "Stomach", prey_remains_at_nest = "Follow",
           Pellet = "Pellet", Scat = "Scat", Fecal_DNA_metabarcoding = "Scat", Observation = "Follow", Telemetry_radio_tracking = "Follow")
en <- read.csv(file.path(ROOT, "data", "extant_new_observations.csv"), stringsAsFactors = FALSE)
en$pm <- num(en$pred_mass_g); en$qm <- num(en$prey_mass_g)
en <- en[en$obs_type %in% names(OBMAP) & is.finite(en$pm) & is.finite(en$qm) & en$pm > 0 & en$qm > 0, ]
snake_gen <- "Elaphe|Acrochordus|Lampropeltis|Pituophis|Hemorrhois|Ophiophagus|Coluber|Crotalus|Sistrurus|Morelia|Python|Naja|Vipera|Natrix|Boa|Thamnophis|Nerodia|Masticophis|Bothrops|Philodryas"
en_clade <- ifelse(en$pred_class == "Aves", "Aves", ifelse(en$pred_class == "Mammalia", "Mammalia", ifelse(en$pred_class == "Reptilia" & grepl("Crocodyl", en$pred_order), "Crocodylia",
            ifelse(en$pred_class == "Reptilia" & grepl(snake_gen, en$pred_genus), "Serpentes", ifelse(en$pred_class == "Reptilia", "Lacertilia", NA)))))
X_en <- data.frame(src = "extant_new", key = paste0("ref", en$ref_id), preyMass = log(en$qm), predmass = log(en$pm), clade = en_clade, prey_type = prey_type(en$prey_class),
                   obstype = unname(OBMAP[en$obs_type]), species = paste0(en$pred_genus, "_", en$pred_species), study = en$source, ref_id = en$ref_id, stringsAsFactors = FALSE)
X_en <- X_en[!is.na(X_en$clade), ]
X <- rbind(X_lf, X_adb, X_en[, names(X_adb)])
X <- X[is.finite(X$preyMass) & is.finite(X$predmass) & X$prey_type %in% c("invert", "vert"), ]
n_animal <- nrow(X)
Xv <- X[X$prey_type == "vert", ]
X_en_v <- X_en[X_en$prey_type %in% "vert", ]
## rows that existed for the 2026-06-03 run: extant_new_observations ref_id <= 205. The log's "newobs=142" is reproduced
## exactly by that cut (ref 206-229 arrived with the 2026-06-04 herp intake, ref 230-251 on 2026-11-11); git holds no June copy.
june <- !(Xv$src == "extant_new" & as.integer(sub("ref", "", ifelse(Xv$src == "extant_new", Xv$key, "ref0"))) > 205)
n_newobs_june <- sum(X_en$src == "extant_new" & as.integer(X_en$ref_id) <= 205 & is.finite(X_en$preyMass))

## ================= x20 section 2: subsample =================
subsample <- function(D, seed = 20260601) {
  set.seed(seed)
  bal <- function(ix) {
    if (D$obstype[ix[1]] == "Stomach") return(ix)
    k <- unlist(lapply(split(ix, interaction(D$study[ix], D$species[ix], drop = TRUE)), function(g) {
      if (length(g) <= 150) return(g); o <- g[order(D$predmass[g])]; o[unique(round(seq(1, length(o), length.out = 150)))] }))
    if (length(k) > 1500) k <- sample(k, 1500); k }
  keep <- unlist(lapply(split(seq_len(nrow(D)), interaction(D$clade, D$obstype, D$prey_type, drop = TRUE)), bal))
  D[sort(keep), ]
}
cellt <- function(D) { t <- as.data.frame(table(clade = D$clade, obstype = D$obstype), stringsAsFactors = FALSE); t[t$Freq > 0, ] }
S_june <- subsample(Xv[june, ]); S_now <- subsample(Xv)

## ================= Step 2: the same from db =================
P <- read_db("pairs"); S <- read_db("sources"); TX <- read_db("taxa")
## upstream of each pair from its pair_id tag (pairs of merged publications cite a source owned by 00_merged_sources.R)
P$src <- ifelse(grepl("_L[0-9]+C[0-9]+$", P$pair_id), "raw_data", ifelse(grepl("_r[0-9]+$", P$pair_id), "aviandietdb", ifelse(grepl("_ref[0-9]+$", P$pair_id), "extant_new", "other")))
P <- P[P$src %in% c("raw_data", "aviandietdb", "extant_new"), ]
P$clade <- TX$analysis_clade[match(P$pred_taxon_id, TX$taxon_id)]
P$prey_vert <- TX$is_vertebrate[match(P$prey_taxon_id, TX$taxon_id)]
P$pm <- num(P$pred_mass_g); P$qm <- num(P$prey_mass_g)
OB_DB <- c(stomach_content = "Stomach", regurgitate = "Stomach", pellet = "Pellet", scat = "Scat", direct_observation = "Follow", prey_remains = "Follow")
P$obstype <- unname(OB_DB[P$observation_type])
## the key that links a db pair to an x20 row
P$key <- ifelse(P$src == "raw_data", sub("^.*_(L[0-9]+C[0-9]+)$", "\\1", P$pair_id), ifelse(P$src == "aviandietdb", sub("^.*_r([0-9]+)$", "ADB_r\\1", P$pair_id), sub("^.*_(ref[0-9]+)$", "\\1", P$pair_id)))
P$tierAD <- P$pair_resolution %in% c("A_both_measured", "B_pred_measured_prey_average", "C_prey_measured_pred_average", "D_both_average")
P$nrep <- ifelse(P$src == "raw_data", ifelse(blank(P$n_items), 0L, as.integer(P$n_items)), 1L)
## x20 drops Aves stomach rows of raw_data (they were replaced by AvianDietDB); apply the same model-input rule
P$aves_stomach_rd <- P$src == "raw_data" & P$clade == "Aves" & P$obstype %in% "Stomach"
sel <- P$tierAD & P$prey_vert %in% "TRUE" & is.finite(P$pm) & is.finite(P$qm) & P$pm > 0 & P$qm > 0 & P$clade %in% CL5 & !is.na(P$obstype) & !P$aves_stomach_rd
Q <- P[sel, ]
Q <- Q[rep(seq_len(nrow(Q)), Q$nrep), ]
Dv <- data.frame(src = Q$src, key = Q$key, preyMass = log(Q$qm), predmass = log(Q$pm), clade = Q$clade, prey_type = "vert", obstype = Q$obstype,
                 species = Q$pred_taxon_id, study = ifelse(Q$src == "aviandietdb", "aviandietdb", Q$source_id), stringsAsFactors = FALSE)
S_db <- subsample(Dv)

## ================= Step 3: item-level reconciliation =================
Xv$key2 <- Xv$key; Pm <- P[match(Xv$key, P$key), ]
why <- with(Pm, ifelse(is.na(pair_id), "no db pair (dropped at migration)",
  ifelse(!tierAD, paste0("tier E in db: pred ", ifelse(blank(pred_mass_method), "none", pred_mass_method), ", prey ", ifelse(blank(prey_mass_method), "none", prey_mass_method)),
  ifelse(!is.finite(qm), "prey mass removed (stomach-remains weight moved to measurements.csv)",
  ifelse(!is.finite(pm), "predator mass removed",
  ifelse(is.na(obstype), paste0("observation type ", observation_type, " is outside x20's four levels"),
  ifelse(!prey_vert %in% "TRUE", "prey taxon not flagged vertebrate in taxa.csv",
  ifelse(!clade %in% CL5, paste0("predator analysis_clade '", clade, "'"),
  ifelse(aves_stomach_rd, "Aves stomach row from raw_data (x20 excludes these too)", "in db subset")))))))))
## among rows kept, did the clade, observation level or masses change?
kept <- why == "in db subset"
chg <- ifelse(!kept, "", ifelse(Pm$clade != Xv$clade, paste0("clade ", Xv$clade, " -> ", Pm$clade),
       ifelse(Pm$obstype != Xv$obstype, paste0("observation level ", Xv$obstype, " -> ", Pm$obstype, " (", Pm$observation_type, ")"),
       ifelse(abs(log(Pm$qm) - Xv$preyMass) > TOL | abs(log(Pm$pm) - Xv$predmass) > TOL, "mass changed (re-read or recomputed)", "identical"))))
rec <- data.frame(src = Xv$src, june = june, clade_x20 = Xv$clade, obs_x20 = Xv$obstype, outcome = ifelse(kept, chg, why), stringsAsFactors = FALSE)
recon <- aggregate(list(items = rep(1L, nrow(rec))), rec[, c("src", "june", "outcome")], sum); recon <- recon[order(recon$src, -recon$items), ]
recon_cell <- aggregate(list(items = rep(1L, sum(rec$june))), rec[rec$june, c("clade_x20", "obs_x20", "outcome")], sum)   # June rows only
## items in the db subset that x20 did not have
extra <- Dv[!Dv$key %in% Xv$key, ]
extra_tab <- if (nrow(extra)) aggregate(list(items = rep(1L, nrow(extra))), extra[, c("src", "clade", "obstype")], sum) else data.frame()
extra_why <- P[match(unique(extra$key), P$key), c("key", "pair_id", "pred_taxon_id", "prey_taxon_id", "observation_type", "pair_resolution", "notes")]

## ================= outputs =================
cmp <- merge(merge(setNames(cellt(S_june), c("clade", "obstype", "x20_june")), setNames(cellt(S_now), c("clade", "obstype", "x20_now")), all = TRUE),
             setNames(cellt(S_db), c("clade", "obstype", "db_AD")), all = TRUE)
cmp[is.na(cmp)] <- 0; cmp$diff_db_minus_june <- cmp$db_AD - cmp$x20_june
pre <- merge(merge(setNames(cellt(Xv[june, ]), c("clade", "obstype", "x20_june")), setNames(cellt(Xv), c("clade", "obstype", "x20_now")), all = TRUE),
             setNames(cellt(Dv), c("clade", "obstype", "db_AD")), all = TRUE); pre[is.na(pre)] <- 0
write.csv(cmp, file.path(MIG, "compare_cells_subsampled.csv"), row.names = FALSE)
write.csv(pre, file.path(MIG, "compare_cells_presubsample.csv"), row.names = FALSE)
write.csv(recon, file.path(MIG, "compare_reconciliation.csv"), row.names = FALSE)
write.csv(recon_cell, file.path(MIG, "compare_reconciliation_by_cell.csv"), row.names = FALSE)
write.csv(extra_why, file.path(MIG, "compare_db_only_rows.csv"), row.names = FALSE)
summ <- data.frame(quantity = c("x20 newobs rows assembled, June state (log: newobs=142)", "longform rebuilt (items)", "data/longform.csv (items)", "rebuilt = longform.csv (multiset of ln masses)",
    "x20 animal-prey rows, current files", "x20 vertebrate-prey rows, June state (log: 144152)", "x20 subsample, June state (log: 15260)",
    "x20 vertebrate-prey rows, current files", "x20 subsample, current files", "db A-D vertebrate-prey rows (same 3 sources)", "db A-D subsample",
    "x20 June rows reproduced identically in db A-D", "x20 June rows in db A-D with a changed mass, clade or level", "x20 June rows not in db A-D", "db A-D rows not in x20"),
  value = c(n_newobs_june, chk_lf[["rebuilt"]], chk_lf[["longform_csv"]], as.logical(chk_lf[["same_multiset"]]), n_animal, sum(june), nrow(S_june), nrow(Xv), nrow(S_now), nrow(Dv), nrow(S_db),
    sum(rec$june & rec$outcome == "identical"), sum(rec$june & kept & rec$outcome != "identical"), sum(rec$june & !kept), nrow(extra)))
write.csv(summ, file.path(MIG, "compare_summary.csv"), row.names = FALSE)
options(width = 200); print(summ, row.names = FALSE); cat("\n"); print(recon, row.names = FALSE); cat("\n"); print(cmp, row.names = FALSE)
cat("\nextra db rows by cell:\n"); print(extra_tab, row.names = FALSE)
## June rows not identical, by source study and cell (for the report's per-cell explanations)
Pm$study_id <- Pm$source_id
bys <- aggregate(list(items = rep(1L, sum(rec$june & rec$outcome != "identical"))),
  list(clade = Xv$clade[rec$june & rec$outcome != "identical"], level = Xv$obstype[rec$june & rec$outcome != "identical"],
       source_id = ifelse(is.na(Pm$study_id), Xv$study, Pm$study_id)[rec$june & rec$outcome != "identical"],
       outcome = rec$outcome[rec$june & rec$outcome != "identical"]), sum)
bys <- bys[order(bys$clade, bys$level, -bys$items), ]
write.csv(bys, file.path(MIG, "compare_reconciliation_by_source.csv"), row.names = FALSE)
