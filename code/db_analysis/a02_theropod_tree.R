# a02: dated composite tree for the fossil predators on the bird stem (non-avian
# theropods and stem birds), grafted onto the extant minor-clade skeleton.
# Topology: Lloyd et al. 2016 dinosaur metatrees (100 trees, db/tree/raw/lloyd2016/).
# Dating: minimum branch length. A tip sits at the midpoint of its stratigraphic range
# (TIP=fad: at its first appearance); a node is MBL (0.1 Myr) older than its oldest
# child. Node ages are minimum ages set by the oldest fossil below them. Polytomies kept.
# A 1 Myr MBL accumulates along the long nested chains of a 960-tip tree (paravian node
# 182 Ma against an oldest paravian tip of 168 Ma), hence 0.1 Myr. Range FADs are bin
# bottoms and wide bins inflate nodes (Shenqiornis 164 Ma), hence the midpoint.
# Crown-bird line: the split of a fossil from crown birds is the age of its MRCA with
# Vegavis iaai (the crown bird in the metatrees).
# Taxa not in the metatrees are attached by rule (code/db_analysis/theropod_tree_rules.csv).
# Output (db/tree/theropod/):
#   name_map.csv         db taxon -> metatree tip or rule
#   split_ages.csv       per tree and taxon: age of the split from the crown-bird line
#   mrca_ages.rds        per tree: matrix of MRCA ages among the fossil taxa
#   summary.csv          per taxon: split age median, 10%, 90%
#   composite_example.tre  skeleton + fossil taxa from the tree with the median theropod split
suppressPackageStartupMessages({ library(ape); library(data.table) })
RAW <- "db/tree/raw/lloyd2016"
MBL  <- as.numeric(Sys.getenv("MBL", "0.1"))           # Myr added above the oldest child
TIP  <- Sys.getenv("TIP", "mid")                         # tip age: mid (range midpoint) or fad
DROP <- strsplit(Sys.getenv("DROP", "Eshanosaurus_deguchiianus"), ",")[[1]]   # tips removed before dating
# Primary drops Eshanosaurus: one Early Jurassic specimen placed in Therizinosauria that alone
# moves every coelurosaur split from the crown-bird line from about 163-168 Ma to 201 Ma.
# Its placement and age have been debated. Dropped in the primary (JM, 2026-11-12). DROP= keeps it (sensitivity).
OUT  <- Sys.getenv("OUT", "db/tree/theropod")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
CROWN <- "Vegavis_iaai"

trees <- read.tree(file.path(RAW, "graemeTrees.tre"))
occ <- fread(file.path(RAW, "occurrence_times.txt"), header = FALSE, col.names = c("taxon", "fad", "lad"))

## db fossil predators -> metatree tips (exact species where present; genus-level noted)
nm <- fread(text = "
taxon_id,tip,basis
Anchiornis,Anchiornis_huxleyi,genus in db (cf. A. huxleyi per Zheng et al. 2018); tip A. huxleyi
Compsognathus_longipes,Compsognathus_longipes,exact
Daspletosaurus,Daspletosaurus_torosus,genus in db (OTM 200 Daspletosaurus sp.); tip D. torosus
Gorgosaurus_libratus,Gorgosaurus_libratus,exact
Huaxiagnathus_orientalis,Huaxiagnathus_orientalis,exact
Microraptor_gui,Microraptor_gui,exact
Microraptor_zhaoianus,Microraptor_zhaoianus,exact
Scipionyx_samniticus,Scipionyx_samniticus,exact
Sinocalliopteryx_gigas,Sinocalliopteryx_gigas,exact
Sinosauropteryx_prima,Sinosauropteryx_prima,exact
Velociraptor_mongoliensis,Velociraptor_mongoliensis,exact
Coelophysis_bauri,Coelophysis_bauri,exact
Baryonyx_walkeri,Baryonyx_walkeri,exact
Tyrannosaurus_rex,Tyrannosaurus_rex,exact
Confuciusornis_sanctus,Confuciusornis_sanctus,exact
Yanornis,Yanornis_martini,genus in db (Yanornis sp. STM9); tip Y. martini
Yanornis_martini,Yanornis_martini,exact
Jianchangornis_microdonta,Jianchangornis_microdonta,exact
")
rules <- fread("code/db_analysis/theropod_tree_rules.csv")    # taxon_id, anchor_tips (;), basis
fwrite(rbind(nm[, .(taxon_id, tip, rule = "", basis)],
             rules[, .(taxon_id, tip = "", rule = paste("MRCA of", anchor_tips), basis)]),
       file.path(OUT, "name_map.csv"))
taxa <- c(nm$taxon_id, rules$taxon_id)

date_mbl <- function(t) {
  t <- drop.tip(t, union(setdiff(t$tip.label, occ$taxon), DROP))
  n <- Ntip(t); age <- numeric(n + t$Nnode)
  m <- match(t$tip.label, occ$taxon)
  age[1:n] <- if (TIP == "fad") occ$fad[m] else (occ$fad[m] + occ$lad[m]) / 2
  ord <- postorder(t)
  for (i in ord) { p <- t$edge[i, 1]; ch <- t$edge[i, 2]; age[p] <- max(age[p], age[ch] + MBL) }
  t$edge.length <- age[t$edge[, 1]] - age[t$edge[, 2]]
  t$root.time <- age[n + 1]
  list(tree = t, age = age)
}
node_of <- function(t, tips) if (length(tips) == 1) which(t$tip.label == tips) else getMRCA(t, tips)
anchor_of <- function(tx) {
  if (tx %in% nm$taxon_id) return(nm[taxon_id == tx, tip])
  strsplit(rules[taxon_id == tx, anchor_tips], ";")[[1]]
}

split_rows <- list(); mrca <- list()
for (k in seq_along(trees)) {
  D <- date_mbl(trees[[k]]); t <- D$tree; age <- D$age
  stopifnot(CROWN %in% t$tip.label)
  anc <- lapply(taxa, anchor_of); names(anc) <- taxa
  ok <- sapply(anc, function(a) all(a %in% t$tip.label))
  if (!all(ok)) stop("tree ", k, ": missing ", paste(names(anc)[!ok], collapse = ", "))
  split_rows[[k]] <- data.table(tree = k, taxon_id = taxa,
    split_age = sapply(anc, function(a) age[getMRCA(t, c(a, CROWN))]),
    anchor_age = sapply(anc, function(a) if (length(a) > 1) age[getMRCA(t, a)] else NA_real_))
  M <- matrix(NA_real_, length(taxa), length(taxa), dimnames = list(taxa, taxa))
  for (i in seq_along(taxa)) for (j in seq_along(taxa))
    M[i, j] <- if (i == j) NA_real_ else age[node_of(t, unique(c(anc[[i]], anc[[j]])))]   # same tip: the tip's own age
  mrca[[k]] <- M
}
sp <- rbindlist(split_rows)
fwrite(sp, file.path(OUT, "split_ages.csv"))
saveRDS(mrca, file.path(OUT, "mrca_ages.rds"))
sm <- sp[, .(split_med = median(split_age), split_q10 = quantile(split_age, 0.1), split_q90 = quantile(split_age, 0.9)), by = taxon_id]
fwrite(sm, file.path(OUT, "summary.csv"))

## example composite for figures: tree whose theropod median split is the median across trees
thero <- c("Anchiornis", "Compsognathus_longipes", "Daspletosaurus", "Gorgosaurus_libratus", "Huaxiagnathus_orientalis",
           "Microraptor_gui", "Microraptor_zhaoianus", "Scipionyx_samniticus", "Sinocalliopteryx_gigas",
           "Sinosauropteryx_prima", "Velociraptor_mongoliensis", "Huadanosaurus_sinensis")
mk <- sp[taxon_id %in% thero, .(m = median(split_age)), by = tree]
kx <- mk[which.min(abs(m - median(m))), tree]
saveRDS(list(tree_index = kx), file.path(OUT, "composite_example_index.rds"))

options(width = 160)
cat("dated", length(trees), "metatrees | MBL", MBL, "Myr | tip", TIP, "| dropped", paste(DROP, collapse = ","), "\n")
print(sm[order(-split_med)], digits = 4)
cat("example tree:", kx, "\n")
