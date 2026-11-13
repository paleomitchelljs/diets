###################################################################
# expansion_report.R -- writes db/migration/expansion_tables.md (every number in db/EXPANSION_REPORT.md comes from here).
# Run after run_all.R. Reads db/*.csv, db/migration/*.csv and evidence/expansion/file_ledger.csv only.
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
P <- read_db("pairs"); S <- read_db("sources"); X <- read_db("taxa")
md <- function(df) { df <- as.data.frame(df, stringsAsFactors = FALSE); h <- paste0("| ", paste(names(df), collapse = " | "), " |"); sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  b <- apply(df, 1, function(r) paste0("| ", paste(gsub("\\|", "/", as.character(r)), collapse = " | "), " |")); paste(c(h, sep, b), collapse = "\n") }
TAG <- "[Phase 4b prey fill; tier before: "
NEW <- "^(Gronlund1970_|Nikolov2004_|Fearn2001_|Pianka_(OZ|NA)_|SqB[0-9a-z]+_[0-9a-f]{6}_r|.*_CD_cd[0-9]+$|.*_GW_gw[0-9]+$)"
ex <- P[P$is_fossil == "FALSE", ]; ex$new <- grepl(NEW, ex$pair_id)
ex$filled <- grepl(TAG, ex$notes, fixed = TRUE)
ex$tier_before <- ex$pair_resolution; ex$tier_before[ex$filled] <- sub(paste0("^.*\\Q", TAG, "\\E([A-Za-z_]+)\\].*$"), "\\1", ex$notes[ex$filled], perl = TRUE)
ex$prey_mass_before <- ifelse(ex$filled, "", ex$prey_mass_g)
tx <- X[match(ex$pred_taxon_id, X$taxon_id), ]; ex$clade <- tx$analysis_clade; ex$sp <- ex$pred_taxon_id
ex$family_group <- ifelse(ex$new, "new", "old")
grp <- function(id) ifelse(grepl("^Gronlund1970_", id), "20 Gronlund1970", ifelse(grepl("^Nikolov2004_", id), "21 Nikolov2004", ifelse(grepl("^Fearn2001_", id), "22 Fearn2001", ifelse(grepl("^Pianka_", id), "33 Pianka", ifelse(grepl("^SqB", id), "30 SquamataBase", ifelse(grepl("_CD_cd", id), "31 CarniDIET", ifelse(grepl("_GW_gw", id), "32 GATEWAy", "old")))))))
ex$script <- grp(ex$pair_id)
out <- character()
add <- function(...) out <<- c(out, ...)
## --- rows per source
nn <- ex[ex$new, ]
dr <- function(f) { x <- file.path(MIG, f); if (file.exists(x)) read.csv(x, stringsAsFactors = FALSE) else NULL }
dd <- function(f) { d <- dr(f); if (is.null(d)) 0L else nrow(d) }
t1 <- do.call(rbind, lapply(sort(unique(nn$script)), function(s) { z <- nn[nn$script == s, ]
  data.frame(script = s, pairs = nrow(z), sources = length(unique(z$source_id[z$source_id %in% S$source_id])), `predator species` = length(unique(z$sp)), `with both masses` = sum(z$pred_mass_g != "" & z$prey_mass_g != ""),
             `individual pairs` = sum(z$individual_pair == "TRUE"), `dropped rows` = dd(paste0("dropped_", sub("^([0-9]+) .*$", "\\1", s), "_", switch(sub("^[0-9]+ ", "", s), Gronlund1970 = "gronlund1970", Nikolov2004 = "nikolov2004", Fearn2001 = "fearn2001", SquamataBase = "squamatabase", CarniDIET = "carnidiet", GATEWAy = "gateway", Pianka = "pianka"), ".csv")), check.names = FALSE) }))
t1[nrow(t1) + 1, ] <- c("All new", sum(t1$pairs), NA, length(unique(nn$sp)), sum(t1$`with both masses`), sum(t1$`individual pairs`), sum(t1$`dropped rows`))
add("## Rows added per source", "", md(t1), "")
## --- fills
fl <- dr("prey_mass_fill.csv"); rm <- dr("prey_mass_remaining.csv")
if (!is.null(fl)) {
  add("## Part 2: prey masses filled", "", sprintf("%d pairs had a predator mass and no prey mass. %d now have a prey mass; %d remain without one.", nrow(fl) + nrow(rm), nrow(fl), nrow(rm)), "")
  add(md(as.data.frame(table(method = fl$method, level = fl$level), stringsAsFactors = FALSE)[as.data.frame(table(method = fl$method, level = fl$level))$Freq > 0, ]), "")
  add("Tier change of the filled rows (before → after):", "", md(as.data.frame(table(before = fl$tier_before, after = fl$tier_after))[as.data.frame(table(before = fl$tier_before, after = fl$tier_after))$Freq > 0, ]), "")
  add("Filled rows by source (top 15):", "", md(head(as.data.frame(sort(table(fl$source_id), decreasing = TRUE), stringsAsFactors = FALSE), 15) |> setNames(c("source", "rows filled"))), "")
  add("Rows that remain without a prey mass, by reason:", "", md(as.data.frame(sort(table(rm$reason), decreasing = TRUE), stringsAsFactors = FALSE) |> setNames(c("reason", "rows"))), "")
  tt <- table(rm$prey_rank[rm$reason != "" & grepl("not found", rm$reason)]); add("Of the rows with a taxon not found, by prey rank:", "", md(as.data.frame(tt, stringsAsFactors = FALSE) |> setNames(c("rank", "rows"))), "")
  ts <- table(rm$prey_taxon_id[grepl("not found", rm$reason)]); add("Taxa not found, most frequent (top 25 by rows):", "", md(head(as.data.frame(sort(ts, decreasing = TRUE), stringsAsFactors = FALSE), 25) |> setNames(c("prey taxon", "rows"))), "")
}
## --- species per clade
sp_tab <- function(d) tapply(d$sp, d$clade, function(v) length(unique(v)))
before_sp <- sp_tab(ex[!ex$new, ]); after_sp <- sp_tab(ex); cl <- sort(unique(c(names(before_sp), names(after_sp))))
t2 <- data.frame(clade = cl, before = as.integer(before_sp[cl]), after = as.integer(after_sp[cl])); t2[is.na(t2)] <- 0L; t2$added <- t2$after - t2$before
add("## Predator species added, per analysis clade", "", "Counts of distinct predator taxa with at least one extant pair.", "", md(t2), "")
## --- clade x obs type x tier
tiers <- c("A_both_measured", "B_pred_measured_prey_average", "C_prey_measured_pred_average", "D_both_average", "E_coarse_or_aggregate")
cellf <- function(d, tier_col) { d$tier <- d[[tier_col]]; r <- aggregate(sp ~ clade + observation_type + tier, d, function(v) length(unique(v))); r }
mk_table <- function(sel_after, sel_before, title) {
  a <- cellf(ex[sel_after, ], "pair_resolution"); b <- cellf(ex[sel_before, ], "tier_before")
  keys <- unique(rbind(a[, c("clade", "observation_type")], b[, c("clade", "observation_type")])); keys <- keys[order(keys$clade, keys$observation_type), ]
  rows <- lapply(seq_len(nrow(keys)), function(i) { r <- c(keys$clade[i], keys$observation_type[i])
    for (t in tiers) { na <- a$sp[a$clade == keys$clade[i] & a$observation_type == keys$observation_type[i] & a$tier == t]; nb <- b$sp[b$clade == keys$clade[i] & b$observation_type == keys$observation_type[i] & b$tier == t]
      na <- if (length(na)) na else 0L; nb <- if (length(nb)) nb else 0L; r <- c(r, if (na == 0 && nb == 0) "" else if (na == nb) as.character(na) else sprintf("%d (%+d)", na, na - nb)) }
    r })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(df) <- c("clade", "observation type", "A", "B", "C", "D", "E")
  add(title, "", "Cells are species counts after the expansion; the change from before is in brackets. A species can occupy several cells.", "", md(df), "") }
vert <- X$is_vertebrate[match(ex$prey_taxon_id, X$taxon_id)] == "TRUE"
both_b <- ex$pred_mass_g != "" & ex$prey_mass_before != ""; both_a <- ex$pred_mass_g != "" & ex$prey_mass_g != ""
mk_table(rep(TRUE, nrow(ex)), !ex$new, "## Clade by observation type by tier: species counts, all extant pairs")
mk_table(vert & both_a, !ex$new & vert & both_b, "## Clade by observation type by tier: species counts, vertebrate prey with both masses")
## --- duplicates
dups <- do.call(rbind, lapply(c("30_squamatabase", "31_carnidiet", "32_gateway"), function(s) { d <- dr(paste0("dropped_", s, ".csv")); if (is.null(d)) return(NULL); d <- d[grepl("duplicate", d$reason), ]; if (!nrow(d)) return(NULL); cbind(script = s, d) }))
if (!is.null(dups)) {
  add("## Duplicates dropped", "", sprintf("%d records in all. Each block is listed once with the number of rows and the db copy that was kept (full row list in `db/migration/dropped_<script>.csv`).", nrow(dups)), "")
  key <- sub("^.*(\\[[^]]*\\]); kept copy: ", "\\1 | kept ", dups$detail); key <- ifelse(grepl("kept copy:", dups$detail) & key == dups$detail, sub("^.*kept copy: ", "kept ", dups$detail), key)
  bl <- as.data.frame(table(script = dups$script, block = key), stringsAsFactors = FALSE); bl <- bl[bl$Freq > 0, ]; names(bl)[3] <- "rows"
  bl$block <- ifelse(bl$script == "30_squamatabase", sub("^.*kept copy: ", "kept ", bl$block), bl$block)
  add(md(bl[order(bl$script, -bl$rows), ]), "")
}
## --- file ledger
lg <- read.csv(file.path(EVID, "expansion", "file_ledger.csv"), stringsAsFactors = FALSE)
add("## Part 1 ledger: outcome by file", "", md(as.data.frame(table(outcome = lg$outcome), stringsAsFactors = FALSE) |> setNames(c("outcome", "files"))), "")
writeLines(out, file.path(MIG, "expansion_tables.md"))
cat("wrote db/migration/expansion_tables.md\n")
