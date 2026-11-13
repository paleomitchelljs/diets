# b03: skeleton tree whose tips are the minor clades of db/clade_map.csv.
# Source: db/tree/combined_tree001.tre (Phase 5 dated tree) and db/tree/tip_map.csv.
# Rule: the split age between minor clades i and j is the age of the MRCA of all
# tree tips belonging to i or j. This gives an ultrametric matrix whether or not a
# minor clade is monophyletic: a paraphyletic or leftover group ("other Aves",
# Caniformia without Pinnipedia) attaches at the node that contains all its members.
# A minor clade with no species in the tree is attached at the crown node of its
# major clade.
# Env WORK (default results/6_work) sets where analysis_cells.csv is read and outputs go.
# Writes <WORK>/skeleton_tree.tre, skeleton_corr.csv (shared-history
# correlation matrix), skeleton_tips.csv (tips used, monophyly, crown age).
suppressPackageStartupMessages({ library(ape); library(data.table) })
TREE <- Sys.getenv("TREE_FILE", "db/tree/combined_tree001.tre")
tr <- read.tree(TREE)
stopifnot(is.ultrametric(tr, tol = 1e-3))
cm <- fread("db/clade_map.csv")
tm <- fread("db/tree/tip_map.csv")[, .(taxon_id, tree_label)]
x <- merge(cm, tm, by = "taxon_id")[tree_label %in% tr$tip.label]

WORK <- Sys.getenv("WORK", "results/6_work")
cells <- fread(file.path(WORK, "analysis_cells.csv"))
x <- x[taxon_id %in% cells$pred_taxon_id]   # tips: only predators in this analysis table
minors <- cells[, .N, by = .(major_clade, minor_clade)][order(major_clade, minor_clade)]
tips_of <- lapply(minors$minor_clade, function(k) unique(x[minor_clade == k, tree_label]))
names(tips_of) <- minors$minor_clade
major_tips <- lapply(split(x$tree_label, x$major_clade), unique)
fallback <- lengths(tips_of) == 0
for (k in names(tips_of)[fallback]) tips_of[[k]] <- major_tips[[minors[minor_clade == k, major_clade]]]

depth <- node.depth.edgelength(tr); H <- max(depth)
age_of <- function(tips) if (length(tips) == 1) 0 else H - depth[getMRCA(tr, tips)]

K <- nrow(minors)
A <- matrix(0, K, K, dimnames = list(minors$minor_clade, minors$minor_clade))
for (i in 1:(K - 1)) for (j in (i + 1):K)
  A[i, j] <- A[j, i] <- age_of(union(tips_of[[i]], tips_of[[j]]))

sk <- as.phylo(hclust(as.dist(2 * A), method = "average"))   # exact for an ultrametric matrix
stopifnot(max(abs(cophenetic(sk)[rownames(A), colnames(A)] - 2 * A)) < 1e-6)
write.tree(sk, file.path(WORK, "skeleton_tree.tre"))

root_age <- max(A)
Cmat <- 1 - A / root_age            # shared history as a fraction of root age; diagonal 1
fwrite(as.data.table(Cmat, keep.rownames = "minor_clade"), file.path(WORK, "skeleton_corr.csv"))

info <- data.table(minors[, .(major_clade, minor_clade)],
  n_species_in_tree = ifelse(fallback, 0L, lengths(tips_of)),
  monophyletic = mapply(function(tp, fb) if (fb) NA else if (length(tp) == 1) TRUE else is.monophyletic(tr, tp),
                        tips_of, fallback),
  crown_age_ma = mapply(function(tp, fb) if (fb) NA_real_ else age_of(tp), tips_of, fallback),
  attached_by = ifelse(fallback, "no species in tree: major-clade crown node", "MRCA of its species"))
fwrite(info, file.path(WORK, "skeleton_tips.csv"))

print(info)
cat("root age (Ma):", round(root_age, 1), "| tree:", TREE, "\n")
cat(write.tree(sk), "\n")
maj <- minors$major_clade
cat("mean correlation within major clade:", round(mean(Cmat[outer(maj, maj, "==") & upper.tri(Cmat)]), 3),
    "| between:", round(mean(Cmat[outer(maj, maj, "!=") & upper.tri(Cmat)]), 3), "\n")
