###################################################################
# migration_counts.R -- tables for db/MIGRATION_REPORT.md.
# Writes db/migration/{counts_by_source,tier_by_source,method_pred,method_prey,obs_by_source,
#   dropped_rows,unknown_methods,unknown_observation_type}.csv and db/migration/report_tables.md
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
P <- read_db("pairs"); S <- read_db("sources"); M <- read_db("measurements"); TX <- read_db("taxa")
SRCS <- c("00_merged_sources" = "merged:", "01_rawdata" = "data/raw_data.csv", "02_aviandietdb" = "data/structured/aviandiet_gut_ppmr.csv", "03_extant_new_observations" = "data/extant_new_observations.csv",
          "04_raptordiets" = "data/structured/raptor_diets_long.csv", "05_tucker2016" = "data/structured/tucker2016_mammal_ppmr.csv", "06_jekyll" = "data/structured/gator_jekyll_stomach.csv",
          "07_glaudas2019" = "data/structured/glaudas2019_snake_rpm.csv", "08_tegu" = "data/structured/tegu_diet_", "09_varanus" = "data/structured/varanus_")
own <- rep(NA_character_, nrow(S)); for (k in names(SRCS)) own[startsWith(S$extracted_from, SRCS[[k]])] <- k
TAGS <- c("01_rawdata" = "_L[0-9]+C[0-9]+$", "02_aviandietdb" = "_r[0-9]+$", "03_extant_new_observations" = "_ref[0-9]+$", "04_raptordiets" = "_osp[0-9]+$",
  "05_tucker2016" = "^Tucker2016_", "06_jekyll" = "^JekyllIsland_Dryad2023_", "07_glaudas2019" = "^Glaudas2019_", "08_tegu" = "^Harman2025_tegu_",
  "09_varanus" = "^(Mazzotti2020_t2r[0-9]+|Weavers1989_occ_.+)$")
tag_of <- function(id) { out <- rep(NA_character_, length(id)); for (k in names(TAGS)) out[is.na(out) & grepl(TAGS[[k]], id)] <- k; out }
P$script <- tag_of(P$pair_id)
M$script <- ifelse(M$pair_id != "", tag_of(M$pair_id), own[match(M$source_id, S$source_id)])
P$vert <- TX$is_vertebrate[match(P$prey_taxon_id, TX$taxon_id)]
P$both <- !blank(P$pred_mass_g) & !blank(P$prey_mass_g)
rc <- do.call(rbind, lapply(names(SRCS), function(k) { f <- file.path(MIG, paste0("rowcount_", k, ".csv")); if (file.exists(f)) read.csv(f, colClasses = "character") else NULL }))
dr <- do.call(rbind, lapply(names(SRCS), function(k) { f <- file.path(MIG, paste0("dropped_", k, ".csv")); if (!file.exists(f)) return(NULL); x <- read.csv(f, colClasses = "character"); if (nrow(x)) cbind(script = k, x) else NULL }))
write.csv(dr, file.path(MIG, "dropped_rows.csv"), row.names = FALSE)
cs <- data.frame(script = names(SRCS), input = SRCS, stringsAsFactors = FALSE)
cs$rows_in <- rc$input_rows[match(cs$script, rc$script)]; cs$unit_in <- rc$input_unit[match(cs$script, rc$script)]; cs$note_in <- rc$note[match(cs$script, rc$script)]
cs$dropped <- vapply(cs$script, function(k) sum(dr$script == k), 0L)
cs$sources_out <- vapply(cs$script, function(k) sum(own == k, na.rm = TRUE), 0L)
cs$pairs_out <- vapply(cs$script, function(k) sum(P$script == k, na.rm = TRUE), 0L)
cs$measurements_out <- vapply(cs$script, function(k) sum(M$script == k, na.rm = TRUE), 0L)
cs$pairs_both_masses <- vapply(cs$script, function(k) sum(P$script == k & P$both, na.rm = TRUE), 0L)
cs$individual_pairs <- vapply(cs$script, function(k) sum(P$script == k & P$individual_pair == "TRUE", na.rm = TRUE), 0L)
write.csv(cs, file.path(MIG, "counts_by_source.csv"), row.names = FALSE)
xt <- function(a, b) { t <- as.data.frame.matrix(table(a, b)); t <- cbind(rownames(t), t); rownames(t) <- NULL; t }
tier <- xt(P$script, P$pair_resolution); names(tier)[1] <- "script"
tier_v <- xt(P$script[P$vert %in% "TRUE" & P$both], P$pair_resolution[P$vert %in% "TRUE" & P$both]); names(tier_v)[1] <- "script"
mp <- xt(ifelse(blank(P$pred_mass_method), "(no mass)", P$pred_mass_method), P$script); names(mp)[1] <- "pred_mass_method"
mq <- xt(ifelse(blank(P$prey_mass_method), "(no mass)", P$prey_mass_method), P$script); names(mq)[1] <- "prey_mass_method"
ob <- xt(P$observation_type, P$script); names(ob)[1] <- "observation_type"
write.csv(tier, file.path(MIG, "tier_by_source.csv"), row.names = FALSE); write.csv(mp, file.path(MIG, "method_pred.csv"), row.names = FALSE)
write.csv(mq, file.path(MIG, "method_prey.csv"), row.names = FALSE); write.csv(ob, file.path(MIG, "obs_by_source.csv"), row.names = FALSE)
unk <- P[P$pred_mass_method == "unknown" | P$prey_mass_method == "unknown", c("pair_id", "source_id", "pred_taxon_id", "prey_taxon_id", "pred_mass_g", "pred_mass_method", "pred_mass_basis", "prey_mass_g", "prey_mass_method", "prey_mass_basis")]
write.csv(unk, file.path(MIG, "unknown_methods.csv"), row.names = FALSE)
uo <- P[P$observation_type == "unknown", c("pair_id", "source_id", "pred_taxon_id", "prey_taxon_id", "pair_resolution", "notes")]
write.csv(uo, file.path(MIG, "unknown_observation_type.csv"), row.names = FALSE)
## unknown-method rows grouped by source for the report
ug <- aggregate(list(pairs = rep(1L, nrow(unk))), list(source_id = unk$source_id, side = ifelse(unk$pred_mass_method == "unknown" & unk$prey_mass_method == "unknown", "both",
                ifelse(unk$pred_mass_method == "unknown", "predator", "prey"))), sum)
ug$basis <- vapply(seq_len(nrow(ug)), function(i) { b <- unk[unk$source_id == ug$source_id[i], ]; b <- if (ug$side[i] == "predator") b$pred_mass_basis else b$prey_mass_basis; names(sort(table(b), decreasing = TRUE))[1] }, "")
ug <- ug[order(-ug$pairs), ]; write.csv(ug, file.path(MIG, "unknown_methods_by_source.csv"), row.names = FALSE)
## dropped rows grouped
dg <- aggregate(list(rows = rep(1L, nrow(dr))), dr[, c("script", "source_file", "reason")], sum)
write.csv(dg, file.path(MIG, "dropped_by_reason.csv"), row.names = FALSE)
## markdown fragments
md <- function(df) { df[] <- lapply(df, function(v) gsub("\\|", "/", as.character(v))); c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
  apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))) }
out <- c("## counts_by_source", md(cs), "", "## tier_by_source (all pairs)", md(tier), "", "## tier_by_source (vertebrate prey, both masses)", md(tier_v), "",
         "## pred method", md(mp), "", "## prey method", md(mq), "", "## observation type", md(ob), "", "## dropped by reason", md(dg), "", "## unknown by source", md(ug))
writeLines(out, file.path(MIG, "report_tables.md"))
cat(sprintf("pairs %d, sources %d, measurements %d, taxa %d; dropped %d; unknown-method pairs %d; unknown observation type %d\n", nrow(P), nrow(S), nrow(M), nrow(TX), nrow(dr), nrow(unk), nrow(uo)))
## possible duplicate studies: the same AuthorYear under two upstream tables (not merged; see report)
k <- sub("^([A-Za-z]+[0-9]{4}).*$", "\\1", S$source_id)
ns <- ifelse(grepl("_DM[0-9]+$", S$source_id), "raw_data", ifelse(grepl("_ADB[a-z]?$", S$source_id), "aviandietdb", ifelse(grepl("_OSP", S$source_id), "os_prey", "direct")))
tt <- table(k, ns); dd <- tt[rowSums(tt > 0) >= 2 & grepl("[0-9]{4}$", rownames(tt)), , drop = FALSE]
write.csv(data.frame(author_year = rownames(dd), unclass(dd)), file.path(MIG, "possible_duplicate_sources.csv"), row.names = FALSE)
## dropped rows, grouped for the report appendix
g <- split(dr, paste(dr$script, dr$reason, sep = " || "))
app <- unlist(lapply(names(g), function(n) { x <- g[[n]]; c(sprintf("**%s** (%s): %s. %d row%s.", sub(" \\|\\|.*", "", n), x$source_file[1], sub(".*\\|\\| ", "", n), nrow(x), ifelse(nrow(x) == 1, "", "s")), "",
  if (nrow(x) <= 12) paste0("- ", x$source_row, ": ", x$detail) else {
    rr <- if (grepl("tegu", x$source_file[1])) tapply(x$source_row, sub("^tegu [^:]+: ", "", x$detail), function(v) sprintf("%d tegus", length(v)))
          else if (grepl("raptor_diets", x$source_file[1]))   # one line per OS-Prey study x dataset x predator: "L<line> <prey>"
            tapply(paste0(sub("^line ", "L", x$source_row), " ", sub("^[^:]*: .*? x ([^;]*);.*$", "\\1", x$detail, perl = TRUE)),
                   sub(" x [^;]*;", ";", x$detail), paste, collapse = ", ")
          else tapply(paste0(sub("^line ([0-9]+), column ([0-9]+)$", "L\\1C\\2", x$source_row), " ", sub("^.*prey '([^']*)'.*$", "\\1", x$detail)), sub(";.*$", "", x$detail), paste, collapse = ", ")
    paste0("- ", names(rr), ": ", rr) }, "") }))
writeLines(app, file.path(MIG, "dropped_appendix.md"))
