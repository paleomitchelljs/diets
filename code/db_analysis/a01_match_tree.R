# a01_match_tree.R -- Phase 6 Step A (part 1): match the CURRENT predator species in db/pairs.csv to the raw
# group trees in db/tree/raw. The Phase 5 tip_map was built before the 4b expansion, so it covers only 46% of
# current predators; this re-matches with the same rules as e02 (exact, manual table, AVONET crosswalk,
# species-of-subspecies). db/ is not modified.
suppressPackageStartupMessages({ library(ape); library(data.table) })
setwd("/Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject")
RAW <- "db/tree/raw"; OUT <- "results/6_work"
p <- fread("db/pairs.csv", select=c("pred_taxon_id","is_fossil","pred_mass_g","prey_mass_g","prey_mass_lo_g"), na.strings=c("","NA"))
tx <- fread("db/taxa.csv", colClasses="character", na.strings=NULL)
ok <- p[is_fossil==FALSE & !is.na(pred_mass_g) & (!is.na(prey_mass_g)|!is.na(prey_mass_lo_g)), .N, by=pred_taxon_id]
pr <- merge(ok, tx[,.(taxon_id,accepted_name,rank,analysis_clade,class,order)], by.x="pred_taxon_id", by.y="taxon_id")
pr <- pr[analysis_clade %in% c("Aves","Mammalia","Lacertilia","Serpentes","Crocodylia") & rank %in% c("species","subspecies")]
labs <- list(
  Jetz2012 = read.tree(text=readLines(file.path(RAW,"jetz2012","HackettStage2_trees0001-0100.tre"), n=1))$tip.label,
  Upham2019 = { m <- read.nexus(file.path(RAW,"upham2019","MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_NDexp_MCC_v2_target.tre")); l <- m$tip.label; sub("^([^_]+_[^_]+)_.*$","\\1",l[!grepl("^_",l)]) },
  Tonini2016 = read.tree(text=readLines(file.path(RAW,"tonini2016","squam_shl_new_Posterior_9755_trees0001-0100.tre"), n=1))$tip.label)
cw <- fread("data/AVONET/PhylogeneticData/BirdLife-BirdTree crosswalk.csv"); cw <- cw[, .(Species3=Species3[1]), by=Species1]
man <- fread("db/tree/name_matches_manual.csv")
pr[, grp := fcase(analysis_clade=="Aves","Jetz2012", analysis_clade=="Mammalia","Upham2019",
                  analysis_clade %in% c("Lacertilia","Serpentes"),"Tonini2016", default=NA_character_)]
pr[, `:=`(label=NA_character_, how=NA_character_)]
for (i in seq_len(nrow(pr))) {
  g <- pr$grp[i]; if (is.na(g)) next
  nm <- pr$accepted_name[i]; lab <- gsub(" ","_",nm); L <- labs[[g]]
  if (pr$rank[i]=="subspecies") lab <- paste(strsplit(lab,"_")[[1]][1:2], collapse="_")
  if (lab %in% L) { set(pr,i,c("label","how"),list(lab,"exact")); next }
  m <- man[accepted_name==nm & tree==g]; if (nrow(m)) { set(pr,i,c("label","how"),list(m$tree_label[1],"manual")); next }
  if (g=="Jetz2012") { c1 <- cw[Species1==nm]; if (nrow(c1) && gsub(" ","_",c1$Species3) %in% L) set(pr,i,c("label","how"),list(gsub(" ","_",c1$Species3),"crosswalk")) }
}
# crocodylians: TimeTree5 only dates 3 species; the other 6 are placed by topology in a02 (flagged)
pr[analysis_clade=="Crocodylia", `:=`(label=gsub(" ","_",accepted_name), how="croc_topology")]
# several db taxa can map to one tip (subspecies, synonyms): rows pool at the tip
pr[, matched := !is.na(label)]
print(pr[, .(species=.N, matched=sum(matched), rate=round(mean(matched),3), rows=sum(N), rows_matched=sum(N[matched])), by=analysis_clade])
cat(sprintf("\nOVERALL species matched: %d / %d = %.3f ; row-weighted %.3f\n", sum(pr$matched), nrow(pr), mean(pr$matched), sum(pr$N[pr$matched])/sum(pr$N)))
fwrite(pr, file.path(OUT,"pred_tree_match.csv"))
print(head(pr[matched==FALSE][order(-N)],25))
