# e02_build_tree.R — Phase 5 Step B: dated tree for the predator species.
# Sources and rules: docs/EVOLUTION_METHODS.md section 4; provenance in db/tree/README.md.
# Writes: db/tree/combined_100.tre, db/tree/combined_tree001.tre, db/tree/tip_map.csv,
#         db/tree/match_report.csv, db/tree/unmatched_predators.csv; fills tree_label in db/taxa.csv.
suppressPackageStartupMessages({ library(ape); library(data.table) })
args <- commandArgs(trailingOnly = FALSE)
ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grep("^--file=", args)])), "..", ".."))
setwd(ROOT)
TR <- "db/tree"; RAW <- file.path(TR, "raw"); NT <- 100

## ---- backbone and crocodylians (TimeTree 5 pairwise medians, raw/timetree5) -----
tt <- fread(file.path(RAW, "timetree5", "pairwise_queries.csv"))
age <- setNames(tt$precomputed_age, tt$node)
croc_nwk <- sprintf("((Caiman_crocodilus:%.4f,Paleosuchus_palpebrosus:%.4f):%.4f,Alligator_mississippiensis:%.4f);",
                    age["Caimaninae"], age["Caimaninae"], age["Alligatoridae"] - age["Caimaninae"], age["Alligatoridae"])
croc <- read.tree(text = croc_nwk)

## ---- group trees -------------------------------------------------------------------
cat("reading trees...\n")
bird <- read.tree(text = readLines(file.path(RAW, "jetz2012", "HackettStage2_trees0001-0100.tre"), n = NT))
mam  <- read.nexus(file.path(RAW, "upham2019", "MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_NDexp_v2_sample100_nexus.trees"))
squ  <- read.tree(text = readLines(file.path(RAW, "tonini2016", "squam_shl_new_Posterior_9755_trees0001-0100.tre"), n = NT))
strip_upham <- function(phy) {         # "Genus_species_FAMILY_ORDER" -> "Genus_species"; drop the Anolis outgroup
  phy <- drop.tip(phy, grep("^_", phy$tip.label))
  phy$tip.label <- sub("^([^_]+_[^_]+)_.*$", "\\1", phy$tip.label); phy }
mam <- lapply(mam, strip_upham)
labs <- list(Jetz2012 = bird[[1]]$tip.label, Upham2019 = mam[[1]]$tip.label,
             Tonini2016 = squ[[1]]$tip.label, TimeTree5 = croc$tip.label)

## ---- name matching for every taxon in db/taxa.csv -------------------------------------
tx <- fread("db/taxa.csv", colClasses = "character", na.strings = NULL)
tx[, grp := fcase(class == "Aves", "Jetz2012", class == "Mammalia", "Upham2019",
                  class == "Reptilia" & order %in% c("Squamata", "Rhynchocephalia"), "Tonini2016",
                  class == "Reptilia" & order == "Crocodylia", "TimeTree5", default = NA_character_)]
tx[analysis_clade %in% c("Lacertilia", "Serpentes") & is.na(grp), grp := "Tonini2016"]
tx[analysis_clade == "Crocodylia" & is.na(grp), grp := "TimeTree5"]
cw <- fread("data/AVONET/PhylogeneticData/BirdLife-BirdTree crosswalk.csv")
cw <- cw[, .(Species3 = Species3[1], n = .N), by = Species1]           # BirdLife -> BirdTree
man <- fread(file.path(TR, "name_matches_manual.csv"))
tx[, `:=`(match_label = NA_character_, match_method = NA_character_)]
for (i in which(!is.na(tx$grp) & tx$rank %in% c("species", "subspecies"))) {
  g <- tx$grp[i]; nm <- tx$accepted_name[i]; lab <- gsub(" ", "_", nm)
  if (lab %in% labs[[g]]) { set(tx, i, c("match_label", "match_method"), list(lab, "exact")); next }
  m <- man[accepted_name == nm & tree == g]
  if (nrow(m)) { set(tx, i, c("match_label", "match_method"), list(m$tree_label[1], "manual")); next }
  if (g == "Jetz2012") {
    c1 <- cw[Species1 == nm]
    if (nrow(c1) && gsub(" ", "_", c1$Species3) %in% labs[[g]]) {
      set(tx, i, c("match_label", "match_method"), list(gsub(" ", "_", c1$Species3), "AVONET_crosswalk")); next }
  }
  if (tx$rank[i] == "subspecies") {                                  # trinomial -> its species tip
    sp <- paste(strsplit(lab, "_")[[1]][1:2], collapse = "_")
    if (sp %in% labs[[g]]) set(tx, i, c("match_label", "match_method"), list(sp, "species_of_subspecies"))
  }
}
stopifnot(all(man$tree_label %in% unlist(labs)))

## ---- which taxa are predators in the analysis ---------------------------------------
p <- fread("db/pairs.csv", colClasses = "character", na.strings = c("", "NA"),
           select = c("pred_taxon_id", "is_fossil"))
pred_ids <- unique(p[is_fossil == "FALSE", pred_taxon_id])
tx[, is_pred := taxon_id %in% pred_ids & analysis_clade %in% c("Aves", "Mammalia", "Lacertilia", "Serpentes", "Crocodylia")]

## ---- fill tree_label (only that column changes) --------------------------------------
tx0 <- fread("db/taxa.csv", colClasses = "character", na.strings = NULL)
stopifnot(identical(tx0$taxon_id, tx$taxon_id))
tx0[, tree_label := ifelse(is.na(tx$match_label), "", tx$match_label)]
fwrite(tx0, "db/taxa.csv", quote = TRUE, na = "")

## ---- match report ------------------------------------------------------------------
vert <- tx[is_vertebrate == "TRUE" & rank == "species"]
rep <- rbind(
  tx[is_pred == TRUE & rank == "species", .(set = "extant predator species", n = .N, matched = sum(!is.na(match_label)),
         exact = sum(match_method == "exact", na.rm = TRUE), crosswalk = sum(match_method == "AVONET_crosswalk", na.rm = TRUE),
         manual = sum(match_method == "manual", na.rm = TRUE)), by = .(group = analysis_clade)],
  vert[, .(set = "all vertebrate species in taxa.csv", n = .N, matched = sum(!is.na(match_label)),
           exact = sum(match_method == "exact", na.rm = TRUE), crosswalk = sum(match_method == "AVONET_crosswalk", na.rm = TRUE),
           manual = sum(match_method == "manual", na.rm = TRUE)), by = .(group = fifelse(is.na(grp), paste0("no tree: ", class), grp))])
rep[, rate := round(matched / n, 3)]
fwrite(rep, file.path(TR, "match_report.csv")); print(rep)
fwrite(tx[is_pred == TRUE & is.na(match_label), .(taxon_id, accepted_name, rank, analysis_clade)],
       file.path(TR, "unmatched_predators.csv"))
tm <- tx[!is.na(match_label), .(taxon_id, accepted_name, tree_source = grp, tree_label = match_label, match_method, is_pred)]
fwrite(tm, file.path(TR, "tip_map.csv"))

## ---- assemble 100 combined trees of the matched predators ----------------------------
pl <- tx[is_pred == TRUE & !is.na(match_label), .(lab = unique(match_label)), by = grp]
keep <- function(phy, l) { k <- intersect(phy$tip.label, l); drop.tip(phy, setdiff(phy$tip.label, k)) }
height <- function(phy) max(node.depth.edgelength(phy))
sub_nwk <- function(phy, parent_age) {               # subtree with stem edge to the parent node
  h <- height(phy); stopifnot(h < parent_age)
  phy$root.edge <- parent_age - h; sub(";$", "", write.tree(phy)) }
comb <- vector("list", NT); hts <- data.table()
for (j in seq_len(NT)) {
  B <- keep(bird[[j]], pl[grp == "Jetz2012", lab]); M <- keep(mam[[j]], pl[grp == "Upham2019", lab])
  S <- keep(squ[[j]], pl[grp == "Tonini2016", lab]); C <- keep(croc, pl[grp == "TimeTree5", lab])
  arch <- sprintf("(%s,%s)", sub_nwk(B, age["Archosauria"]), sub_nwk(C, age["Archosauria"]))
  saur <- sprintf("(%s,%s:%.4f)", sub_nwk(S, age["Sauria"]), arch, age["Sauria"] - age["Archosauria"])
  amni <- sprintf("(%s,%s:%.4f);", sub_nwk(M, age["Amniota"]), saur, age["Amniota"] - age["Sauria"])
  comb[[j]] <- read.tree(text = amni)
  mj <- mam[[j]]; hj <- height(mj); nd <- node.depth.edgelength(mj)
  crown_age <- function(a, b) hj - nd[getMRCA(mj, c(a, b))]
  hts <- rbind(hts, data.table(tree = j, aves_mrca = height(B), mammal_mrca = height(M), squamate_mrca = height(S),
                                croc_mrca = height(C), ntip = Ntip(comb[[j]]),
                                crown_aves = height(bird[[j]]),                       # Jetz trees are rooted at crown Aves
                                crown_theria = crown_age("Didelphis_virginiana", "Homo_sapiens"),
                                crown_mammalia = crown_age("Ornithorhynchus_anatinus", "Homo_sapiens")))
}
stopifnot(all(sapply(comb, is.ultrametric, tol = 1e-6)), all(sapply(comb, is.binary)))
class(comb) <- "multiPhylo"
write.tree(comb, file.path(TR, "combined_100.tre")); write.tree(comb[[1]], file.path(TR, "combined_tree001.tre"))
fwrite(hts, file.path(TR, "group_root_ages.csv"))
cat(sprintf("combined trees: %d, %d tips each; root %.1f Ma\n", NT, Ntip(comb[[1]]), height(comb[[1]])))
print(summary(hts))
