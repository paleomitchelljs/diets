###################################################################
# parse_oliveira2025.R -- Table S2 of Oliveira et al. 2025 (Sci Rep, saurophagy in South American
# lizards) from the supplement .docx into evidence/oliveira2025_tableS2.csv, so that the 68 staged
# records in data/extant_new_observations.csv (ref_id 133-200) can be linked to the original
# reference, predator and prey family, prey age and the two sizes in mm.
# Reads word/document.xml with unzip and splits it into table rows (w:tr) and cells (w:tc), keeping empty cells.
###################################################################
args_all <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args_all, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa[1]))), "../../..")) else normalizePath(getwd())
f <- file.path(ROOT, "pdfs", "extant_to_ingest", "ai_found", "41598_2025_89810_MOESM1_ESM.docx")
x <- paste(system2("unzip", c("-p", shQuote(f), "word/document.xml"), stdout = TRUE), collapse = "")
rows <- strsplit(x, "</w:tr>", fixed = TRUE)[[1]]
cell_text <- function(c) { t <- regmatches(c, gregexpr("<w:t[^>]*>[^<]*</w:t>", c))[[1]]; trimws(gsub("&amp;", "&", paste(gsub("<[^>]+>", "", t), collapse = ""))) }
tab <- lapply(rows, function(r) { cs <- strsplit(r, "</w:tc>", fixed = TRUE)[[1]]; cs <- cs[grepl("<w:tc[ >]", cs)]; cs <- sub("^.*<w:tc[ >]", "", cs); vapply(cs, cell_text, "", USE.NAMES = FALSE) })
tab <- tab[lengths(tab) == 23]                         # Table S2 rows have 23 cells; the header row is the first
H <- tab[[1]]; if (H[1] != "Predator" || H[23] != "Reference") stop("unexpected Table S2 header: ", paste(H, collapse = " | "))
D <- as.data.frame(do.call(rbind, tab[-1]), stringsAsFactors = FALSE); names(D) <- H
D[D == "" | D == "NA"] <- NA
D$table_row <- seq_len(nrow(D))
write.csv(D, file.path(ROOT, "code", "db_migrate", "evidence", "oliveira2025_tableS2.csv"), row.names = FALSE)
cat("parsed", nrow(D), "Table S2 records\n")
