###################################################################
# x08_intake.R
# Source-material intake registry. Scans the pdfs/ tree and the
# structured data drops, and maintains data/intake_manifest.csv: one
# row per source file, tracking what it is and whether we've processed it.
#
# Design goals
#   * Single source of truth for "what have we ingested / what's left".
#   * IDEMPOTENT: re-running never resets a file you've already marked
#     done. Status/extraction columns are matched by FILENAME and carried
#     forward (so a file can move to pdfs/_processed/ without losing state).
#   * Provenance: every extracted observation row should carry a
#     `source_file` = the manifest `file`, so data points trace back to a PDF.
#
# Workflow
#   1. Rscript code/x08_intake.R           # refresh manifest (new files -> 'queued')
#   2. extract a paper -> append rows to its target table with source_file set
#   3. edit the manifest row: status='done', n_obs, ref_ids, date_processed, notes
#      (and move the file to pdfs/_processed/<category>/ to clear the queue)
#   4. Rscript code/x08_intake.R           # status preserved; queue shrinks
#
# Statuses: queued | processing | done | irrelevant | superseded | structured
#   structured = a ready-made dataset (CSV/XLSX) harmonised by its own script,
#                not a paper to read by hand.
###################################################################

args <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
MAN <- file.path(ROOT, "data", "intake_manifest.csv")
TODAY <- format(Sys.Date())

## --- scan source materials --------------------------------------
roots <- c(file.path(ROOT, "pdfs"))
files <- list.files(roots, recursive = TRUE, full.names = TRUE)
files <- files[!grepl("/\\.|\\.DS_Store$|xx_todo", files)]            # skip hidden/junk
rel   <- sub(paste0("^", ROOT, "/"), "", files)

guess_category <- function(p) {
	# files moved to pdfs/_processed/<cat>/ keep their category from that subfolder
	moved <- sub(".*_processed/([^/]+)/.*", "\\1", p)
	ifelse(grepl("_processed/(fossil_pdf|extant_pdf|extant_data|reference)/", p), moved,
	ifelse(grepl("fossil_cases", p), "fossil_pdf",
	ifelse(grepl("reference", p),    "reference",
	ifelse(grepl("\\.(csv|xlsx|xls)$", p, ignore.case = TRUE), "extant_data",
	       "extant_pdf"))))
}
guess_obstype <- function(f) {
	l <- tolower(f)
	ifelse(grepl("pellet|owl|tyto|strigi", l),                "pellet",
	ifelse(grepl("stomach|gullet|gut|gastric", l),            "gut",
	ifelse(grepl("scat|fecal|faec|dropping|dna|metabarcod", l), "scat",
	ifelse(grepl("observ|follow|kill|foraging", l),           "observation",
	       "unknown"))))
}
fmt <- tolower(sub(".*\\.", "", rel))

scan <- data.frame(
	file = basename(rel), path = rel, category = guess_category(rel),
	format = fmt, size_kb = round(file.size(files)/1024),
	obstype_hint = ifelse(guess_category(rel) %in% c("fossil_pdf","reference"), "",
	                      guess_obstype(basename(rel))),
	# files already carrying a '_done' tag (e.g. fossil cases already in the
	# fossil tables) seed as done; everything else starts queued.
	status = ifelse(grepl("_done", basename(rel), ignore.case = TRUE), "done", "queued"),
	target_table = "", n_obs = NA_integer_,
	ref_ids = "", date_added = TODAY, date_processed = "", notes = "",
	stringsAsFactors = FALSE)
scan <- scan[order(scan$category, scan$file), ]

## --- merge with existing manifest (preserve human-set state) -----
state_cols <- c("category","status","target_table","n_obs","ref_ids","date_added","date_processed","notes","obstype_hint")
if (file.exists(MAN)) {
	old <- read.csv(MAN, stringsAsFactors = FALSE, colClasses = "character")
	m <- match(scan$file, old$file)                  # match by FILENAME (survives moves)
	for (cc in state_cols) {
		keep <- !is.na(m) & cc %in% names(old)
		val <- old[[cc]][m]
		# don't overwrite a refined obstype_hint or any human edit with the guess
		scan[[cc]][keep] <- ifelse(is.na(val[keep]) | val[keep] == "", scan[[cc]][keep], val[keep])
	}
	gone <- setdiff(old$file, scan$file)
	if (length(gone)) message(sprintf("NOTE: %d files in manifest no longer on disk: %s",
	                                   length(gone), paste(head(gone, 5), collapse = ", ")))
}
scan$n_obs <- suppressWarnings(as.integer(scan$n_obs))

write.csv(scan, MAN, row.names = FALSE, quote = TRUE)

## --- report ------------------------------------------------------
cat("=== intake manifest:", nrow(scan), "source files ->", sub(paste0(ROOT,"/"),"",MAN), "===\n\n")
cat("by category x status (file counts):\n")
print(addmargins(table(scan$category, scan$status)))
cat("\nextant-PDF queue by obstype hint:\n")
print(table(scan$obstype_hint[scan$category == "extant_pdf" & scan$status == "queued"]))
cat(sprintf("\nDONE so far: %d files, %s observations extracted.\n",
            sum(scan$status == "done"), sum(scan$n_obs, na.rm = TRUE)))
