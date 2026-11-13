# b05: terrestrial-only analysis table for the talk fits (JM 2026-11-12: drop the
# marine clades). Removes predators whose pre-merge minor clade in db/clade_map.csv is
# Procellariiformes, Suliformes, Sphenisciformes (penguins, inside "other Aves"),
# Cetacea or Pinnipedia. Charadriiformes and Pelecaniformes are kept (shorebirds,
# gulls, herons: not marine as clades).
# Input: results/6_work/analysis_rows.csv (b01), db/clade_map.csv.
# Output: results/6_work/terr/{analysis_rows,analysis_cells,design_counts}.csv
suppressPackageStartupMessages(library(data.table))
MARINE <- c("Procellariiformes", "Suliformes", "Sphenisciformes", "Cetacea", "Pinnipedia")
out <- "results/6_work/terr"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

rows <- fread("results/6_work/analysis_rows.csv")
cm <- fread("db/clade_map.csv")
drop_taxa <- cm[minor_before_merge %in% MARINE, taxon_id]
d <- rows[!pred_taxon_id %in% drop_taxa]
cat(sprintf("dropped %d rows, %d species (%s)\n", nrow(rows) - nrow(d),
            uniqueN(rows[pred_taxon_id %in% drop_taxa, pred_taxon_id]), paste(MARINE, collapse = ", ")))
fwrite(d, file.path(out, "analysis_rows.csv"))

cells <- d[, .(n = .N, ybar = mean(y), SS = sum((y - mean(y))^2)),
           by = .(pred_taxon_id, source_id, obs, m, major_clade, minor_clade)]
fwrite(cells, file.path(out, "analysis_cells.csv"))
cnt <- d[, .(rows = .N, species = uniqueN(pred_taxon_id), sources = uniqueN(source_id)),
         by = .(major_clade, minor_clade, obs)][order(major_clade, minor_clade, obs)]
fwrite(cnt, file.path(out, "design_counts.csv"))
cat(sprintf("rows %d | cells %d | species %d | sources %d | minor clades %d\n",
            nrow(d), nrow(cells), uniqueN(d$pred_taxon_id), uniqueN(d$source_id), uniqueN(d$minor_clade)))
print(d[, .(rows = .N, species = uniqueN(pred_taxon_id)), by = .(major_clade, minor_clade)][order(major_clade, -species)])
