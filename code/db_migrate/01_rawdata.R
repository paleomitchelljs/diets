###################################################################
# 01_rawdata.R -- migrate the 2019 diet matrix (data/raw_data.csv; longform.csv is derived from it
# by x00) into db/. One pairs row per predator record x prey column with a count > 0;
# n_items = the count. Masses: predator col "Mass", prey header row 3, plus the x03 fills in
# data/mass_overrides.csv (NA-only), exactly as x00 builds longform.csv.
# How each mass was produced is read from the evidence tables written by
# evidence/build_rawdata_evidence.R (see that script for the rules).
# Owns: sources with extracted_from starting "data/raw_data.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "01_rawdata"; OWNS <- "data/raw_data.csv"

T <- read.csv(file.path(ROOT, "data", "raw_data.csv"), header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
SC <- 31; pc <- SC:ncol(T); d <- T[4:nrow(T), ]; lines <- 4:nrow(T)
hdr <- as.character(unlist(T[3, ])); f <- function(n) which(hdr == n)
C <- suppressWarnings(matrix(num(as.matrix(d[, pc])), nrow = nrow(d))); C[is.na(C)] <- 0
ref <- trimws(d[, 1]); obs_raw <- trimws(d[, f("ObsType")]); cls_raw <- trimws(d[, f("Class")])
refs <- read.csv(file.path(ROOT, "data", "longform_refs.csv"), stringsAsFactors = FALSE)
ST <- read.csv(file.path(EVID, "rawdata_studies.csv"), colClasses = "character")
PC <- read.csv(file.path(EVID, "rawdata_prey_columns.csv"), colClasses = "character")
PR <- read.csv(file.path(EVID, "rawdata_pred_records.csv"), colClasses = "character")
DEC <- read.csv(file.path(EVID, "rawdata_study_decisions.csv"), colClasses = "character")

## ---------------- sources ----------------
surname <- function(a) { a <- trimws(a); a[a == ""] <- "Anon"; make_id(gsub("[ -]", "", tools::toTitleCase(a))) }
st <- ST[order(num(ST$refno)), ]
st$source_id <- paste0(surname(st$first_author), ifelse(blank(st$year), "", st$year), "_DM", st$refno)
desc <- refs$descriptor[match(num(st$refno), refs$refno)]
src <- mk("sources", source_id = st$source_id,
  citation = sprintf("%s (%s) %s. Title not recorded in raw_data.csv; raw_data RefNo %s%s.", ifelse(blank(st$first_author), "Author not recorded", st$first_author),
                     ifelse(blank(st$year), "year not recorded", st$year), ifelse(blank(st$journal), "Journal not recorded", st$journal), st$refno,
                     ifelse(is.na(desc), "", paste0("; descriptor '", desc, "'"))),
  file_name = st$pdf, data_type = ifelse(grepl("thesis|PhD", st$journal, ignore.case = TRUE), "thesis", "journal_article"),
  extracted_from = paste0("data/raw_data.csv RefNo ", st$refno, " (prey matrix; masses in row 3 and column 'Mass')"),
  compiled_in = "DietMatrix2019",
  notes = ifelse(blank(st$pdf), "no PDF found in archive_old_pipeline/compiled/Compiled/",
                 paste0("PDF in archive_old_pipeline/compiled/Compiled/", ifelse(st$pdf_text %in% "TRUE", "", " (no usable text layer)"))))
src <- rbind(mk("sources", source_id = "DietMatrix2019", citation = "Mitchell JS (compiler). Predator diet matrix: predator records x prey taxa with counts, prey masses (row 3) and predator masses. data/raw_data.csv, byte-identical to archive_old_pipeline/data/dryad/data_files/raw_diet_data.csv (2019). Unpublished compilation.",
  file_name = "raw_data.csv", data_type = "compilation_database", extracted_from = "data/raw_data.csv",
  notes = "x00_reformatData.R expands it to data/longform.csv (one row per prey item); x03 fills missing masses via data/mass_overrides.csv"), src)
sid_of <- setNames(st$source_id, st$refno)
## publications also found in another upstream table cite the merged source written by 00_merged_sources.R
DS <- MERGED(); mref <- strsplit(DS$raw_data_refno, ";")
merged_of <- setNames(rep(DS$merged_source_id, lengths(mref)), unlist(mref))
src <- src[!src$source_id %in% st$source_id[st$refno %in% names(merged_of)], ]
sid_of[names(merged_of)] <- merged_of
## a RefNo whose records duplicate another RefNo keeps its source row, with a note; its records are dropped below
dupof <- setNames(DEC$duplicate_of, DEC$refno); dupof <- dupof[!blank(dupof)]
i_d <- match(sid_of[names(dupof)], src$source_id)
src$notes[i_d] <- paste0(src$notes[i_d], "; duplicate entry of raw_data RefNo ", dupof, " (identical prey counts): its records are dropped, see db/migration/dropped_rows.csv")
i_k <- match(sid_of[unname(dupof)], src$source_id); pdf_d <- st$pdf[match(names(dupof), st$refno)]
usepdf <- blank(src$file_name[i_k]) & !blank(pdf_d)                 # the kept entry has no PDF of its own: point at the other entry's
src$file_name[i_k][usepdf] <- pdf_d[usepdf]
src$notes[i_k][usepdf] <- paste0("PDF in archive_old_pipeline/compiled/Compiled/", pdf_d[usepdf], " (filed under raw_data RefNo ", names(dupof)[usepdf], ")")
src$notes[i_k] <- paste0(src$notes[i_k], "; the same records are also entered as raw_data RefNo ", names(dupof), ", which is dropped (evidence/rawdata_study_decisions.csv)")

## ---------------- taxa: predators ----------------
sq <- SQUAMATES()
pg <- trimws(d[, f("Genus")]); ps <- trimws(d[, f("Species")]); pclade_raw <- trimws(d[, f("Clade")])
pred_id <- make_id(paste(pg, ps, sep = "_"))
ord_of <- function(cl, cd) ifelse(cl == "Mammalia" & cd %in% c("Feliformes", "Caniformes", "Caniformia", "Feliformia"), "Carnivora",
                           ifelse(cl %in% c("Aves", "Mammalia"), cd, ifelse(cl == "Crocodylia", "Crocodylia", ifelse(cl == "Squamata", "Squamata", ""))))
pfam <- ifelse(cls_raw == "Squamata", sq$family[match(pg, sq$genus)], ifelse(grepl("idae$", pclade_raw), pclade_raw, ""))
tx_pred <- taxon_row(pred_id, paste(pg, ps, sep = "_"), "species", cls = cls_raw, ord = ord_of(cls_raw, pclade_raw), fam = pfam,
                     clade = pred_clade(cls_raw, pg, sq), vert = TRUE, notes = paste0("raw_data Class '", cls_raw, "', Clade '", pclade_raw, "'"))

## ---------------- pairs ----------------
w <- which(C > 0, arr.ind = TRUE)                      # record x column cells with a count
w <- w[order(w[, 1], w[, 2]), , drop = FALSE]
ii <- w[, 1]; jj <- w[, 2]; colno <- SC + jj - 1
pcx <- PC[match(paste(ref[ii], colno), paste(PC$refno, PC$col)), ]
prx <- PR[match(lines[ii], num(PR$line)), ]
stx <- ST[match(ref[ii], ST$refno), ]
dcx <- DEC[match(ref[ii], DEC$refno), ]
ptype <- prey_type(pcx$class_string)
count <- C[cbind(ii, jj)]

## drops: non-animal "prey" (plants, fungi, placeholders) are not predation (same rule as x20)
drop <- is.na(ptype)
dropped <- data.frame(source_file = "data/raw_data.csv", source_row = paste0("line ", lines[ii[drop]], ", column ", colno[drop]),
  reason = "non-animal food item (prey class string is plant, fungus, placeholder or unclassified); not a predator-prey link",
  detail = sprintf("RefNo %s; %s %s; prey '%s' [%s]; count %s", ref[ii[drop]], pg[ii[drop]], ps[ii[drop]], pcx$label[drop], pcx$class_string[drop], count[drop]))
norec <- setdiff(seq_len(nrow(d)), unique(ii))
dropped <- rbind(dropped, data.frame(source_file = "data/raw_data.csv", source_row = paste0("line ", lines[norec]),
  reason = "record has no prey count > 0 in any prey column", detail = sprintf("RefNo %s; %s %s; ObsType %s", ref[norec], pg[norec], ps[norec], obs_raw[norec])))
## duplicate records: the same publication, predator and evidence kind kept from AvianDietDB (evidence/duplicate_records.csv)
DR <- DUPRECS(); DR <- DR[DR$upstream == "raw_data" & DR$action == "drop", ]
kind <- ifelse(obs_raw[ii] == "Stomach", "gut", "other")
dup <- paste(ref[ii], taxon_id_of(paste(pg[ii], ps[ii], sep = "_")), kind) %in% paste(DR$key, DR$pred, DR$kind) & !drop
dropped <- rbind(dropped, data.frame(source_file = "data/raw_data.csv", source_row = paste0("line ", lines[ii[dup]], ", column ", colno[dup]),
  reason = "duplicate record: the same publication, predator species and evidence kind is kept from AvianDietDB (evidence/duplicate_records.csv)",
  detail = sprintf("RefNo %s (merged source %s); %s %s; prey '%s'; count %s", ref[ii[dup]], merged_of[ref[ii[dup]]], pg[ii[dup]], ps[ii[dup]], pcx$label[dup], count[dup])))
drop <- drop | dup
## duplicate entries within raw_data: the same records under two RefNos (rawdata_study_decisions.csv `duplicate_of`)
wdup <- !blank(dcx$duplicate_of) & !drop
dropped <- rbind(dropped, data.frame(source_file = "data/raw_data.csv", source_row = paste0("line ", lines[ii[wdup]], ", column ", colno[wdup]),
  reason = "duplicate entry within raw_data.csv: the same records are entered under another RefNo, which is kept (evidence/rawdata_study_decisions.csv)",
  detail = sprintf("RefNo %s duplicates RefNo %s; %s %s; prey '%s'; count %s", ref[ii[wdup]], dcx$duplicate_of[wdup], pg[ii[wdup]], ps[ii[wdup]], pcx$label[wdup], count[wdup])))
drop <- drop | wdup
keep <- !drop; ii <- ii[keep]; jj <- jj[keep]; colno <- colno[keep]; pcx <- pcx[keep, ]; prx <- prx[keep, ]; stx <- stx[keep, ]; dcx <- dcx[keep, ]; count <- count[keep]; ptype <- ptype[keep]

## observation type
ob <- obs_raw[ii]; cl <- cls_raw[ii]
otype <- ifelse(ob == "Stomach", "stomach_content", ifelse(ob == "Larder", "prey_remains",
         ifelse(ob == "Scat" & cl == "Aves", stx$obs_aves_scat, ifelse(ob == "Scat", "scat", ifelse(ob == "Follow", stx$obs_follow, NA)))))
onote <- ifelse(ob == "Larder", "shrike larder (impaled cached prey); raw_data ObsType 'Larder'",
         ifelse(ob == "Scat" & cl == "Aves", stx$obs_aves_scat_note, ifelse(ob == "Follow", stx$obs_follow_note, paste0("raw_data ObsType '", ob, "'"))))
ovr <- !blank(dcx$observation_override); otype[ovr] <- dcx$observation_override[ovr]; onote[ovr] <- paste0("set from source text: ", dcx$evidence_quote[ovr])

## cell decisions (evidence/rawdata_cell_decisions.csv): one record or one prey column of a study, where the
## study-level rules above do not fit (Maritz 2011: two weighed prey items inside a pooled record).
## Blank line = every record of the study; blank col = every prey column of the record.
CD <- read.csv(file.path(EVID, "rawdata_cell_decisions.csv"), colClasses = "character", na.strings = character(0))
## guard against shifted rows (read.csv pads a short row silently): every row has all fields, and methods are vocabulary values
nf <- count.fields(file.path(EVID, "rawdata_cell_decisions.csv"), sep = ",", quote = "\"")
if (any(nf != ncol(CD))) stop("rawdata_cell_decisions.csv: rows with the wrong number of fields: ", paste(which(nf != ncol(CD)), collapse = ", "))
voc <- readLines(file.path(ROOT, "db", "vocab.yml")); voc <- trimws(sub("#.*$", "", voc))
mm <- sub("^- *", "", voc[seq(which(voc == "mass_method:") + 1, length(voc))]); mm <- mm[grepl("^[a-z_]+$", mm)]
for (cc in c("pred_method", "prey_method")) { bad <- !blank(CD[[cc]]) & !CD[[cc]] %in% c(mm, "remains")
  if (any(bad)) stop("rawdata_cell_decisions.csv: ", cc, " not a mass_method: ", paste(substr(CD[[cc]][bad], 1, 40), collapse = " | ")) }
pcx$pred_method_cell <- ""; pcx$pred_basis_cell <- ""; pcx$pred_mass_cell <- ""; pcx$pred_mass_note <- ""
for (k in seq_len(nrow(CD))) {
  cdk <- CD[k, ]
  hit <- ref[ii] == cdk$refno & (blank(cdk$line) | lines[ii] == num(cdk$line)) & (blank(cdk$col) | colno == num(cdk$col))
  if (!any(hit)) stop("rawdata_cell_decisions.csv row ", k, " matches no raw_data cell")
  if (!blank(cdk$label) && any(pcx$label[hit] != cdk$label)) stop("rawdata_cell_decisions.csv row ", k, ": label does not match column ", cdk$col)
  if (!blank(cdk$observation_type)) { otype[hit] <- cdk$observation_type; onote[hit] <- paste0("set from source text (cell decision): ", cdk$evidence_quote) }
  if (!blank(cdk$prey_method)) { pcx$prey_method[hit] <- cdk$prey_method; pcx$prey_basis[hit] <- cdk$prey_basis }
  if (!blank(cdk$pred_method)) { pcx$pred_method_cell[hit] <- cdk$pred_method; pcx$pred_basis_cell[hit] <- cdk$pred_basis }
  ## a corrected predator mass (method unchanged; pred_basis says what was corrected and on whose authority)
  if (!blank(cdk$pred_mass_g)) { if (blank(cdk$pred_basis)) stop("rawdata_cell_decisions.csv row ", k, ": a corrected pred_mass_g needs pred_basis")
    pcx$pred_mass_cell[hit] <- cdk$pred_mass_g; if (blank(cdk$pred_method)) pcx$pred_mass_note[hit] <- cdk$pred_basis }
}
if (anyNA(otype)) stop("unmapped ObsType: ", paste(unique(ob[is.na(otype)]), collapse = ", "))

## individual pairs
nobs <- trimws(d[ii, f("Nobservations")])
indiv <- (dcx$individual_rule %in% "all") | (dcx$individual_rule %in% "nobs1" & nobs == "1")
spec <- ifelse(indiv, sprintf("%s %s individual, raw_data line %d (%s%s, %s g)", trimws(d[ii, f("First Author")]), trimws(d[ii, f("Year")]), lines[ii],
               trimws(d[ii, f("LocationA")]), ifelse(blank(d[ii, f("LocationB")]), "", paste0(", ", trimws(d[ii, f("LocationB")]))), trimws(d[ii, f("Mass")])), "")

## masses
pmass <- num(prx$mass_g)
cellp <- !blank(pcx$pred_method_cell); prx$pred_method[cellp] <- pcx$pred_method_cell[cellp]; prx$pred_basis[cellp] <- pcx$pred_basis_cell[cellp]
cellm <- !blank(pcx$pred_mass_cell); pmass[cellm] <- num(pcx$pred_mass_cell[cellm])
prx$pred_basis[cellm] <- ifelse(blank(pcx$pred_mass_note[cellm]), prx$pred_basis[cellm], paste0(prx$pred_basis[cellm], "; ", pcx$pred_mass_note[cellm]))
pmeth <- ifelse(is.finite(pmass), prx$pred_method, "")
qmass <- num(pcx$mass_g)
ovr_fill <- pcx$category == "override_db"              # NA in raw_data, filled by x03 via mass_overrides.csv
if (any(ovr_fill)) { ov <- read.csv(file.path(ROOT, "data", "mass_overrides.csv"), stringsAsFactors = FALSE)
  qmass[ovr_fill] <- ov$mass_g[match(paste(pcx$label[ovr_fill], pcx$class_string[ovr_fill]), ov$key)] }
qmeth <- ifelse(is.finite(qmass), pcx$prey_method, "")
remains <- qmeth == "remains"
remains_val <- ifelse(remains, qmass, NA)
remains_el  <- ifelse(otype == "scat", "scat remains", "stomach remains")   # where the weighed remains came from
remains_dim <- ifelse(blank(dcx$remains_dimension), "wet mass per item", dcx$remains_dimension)
qmass[remains] <- NA; qmeth[remains] <- ""
tier <- tier_of(pmeth, qmeth)

## counts -> n_items (x00 expands with rep(), which truncates fractional counts)
n_items <- ifelse(count >= 1, floor(count), NA)
cnote <- ifelse(count != floor(count), sprintf("source count %s is fractional; n_items = floor (as x00 expands it)%s", fnum(count), ifelse(count < 1, ", here none", "")), "")

source_id <- sid_of[ref[ii]]
pair_id <- paste0(source_id, "_L", lines[ii], "C", colno)
prey_basis <- ifelse(qmeth == "", "", pcx$prey_basis)
notes <- trimws(gsub("; ;|^; |; $", "", paste(
  onote, ifelse(blank(pcx$label_note), "", pcx$label_note),
  sprintf("prey label '%s', class string '%s'", pcx$label, pcx$class_string),
  ifelse(remains, sprintf("prey mass in raw_data (%s g) is a %s weight per item, not a body mass; moved to measurements.csv. %s", fnum(remains_val), sub(" ", "-", remains_el), pcx$prey_basis), ""),
  ifelse(qmeth == "unknown", "prey mass method unknown (see db/migration/unknown_methods.csv)", ""),
  ifelse(pmeth == "unknown", "predator mass method unknown", ""),
  ifelse(indiv, "individual record (one animal)", ""), cnote, sep = "; ")))
notes <- gsub("(; )+", "; ", notes); notes <- gsub("^; |; $", "", notes)
pairs <- mk("pairs", pair_id = pair_id, source_id = source_id, pred_taxon_id = pred_id[ii], prey_taxon_id = pcx$taxon_id,
  pred_specimen = spec, observation_type = otype, pair_resolution = tier, individual_pair = ifelse(indiv, "TRUE", "FALSE"),
  pred_mass_g = fnum(pmass), pred_mass_method = pmeth, pred_mass_basis = ifelse(pmeth == "", "", prx$pred_basis),
  prey_mass_g = fnum(qmass), prey_mass_method = qmeth, prey_mass_basis = prey_basis,
  prey_stage = pcx$prey_stage, n_items = ifelse(is.na(n_items), "", as.character(n_items)), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("data/raw_data.csv line %d, column %d (prey '%s', count %s); RefNo %s", lines[ii], colno, pcx$label, fnum(count), ref[ii]),
  notes = notes)

## measurements: remains weights
rm_i <- which(remains)
meas <- mk("measurements", measurement_id = paste0(pair_id[rm_i], "_rem"), source_id = source_id[rm_i], pair_id = pair_id[rm_i],
  taxon_id = pcx$taxon_id[rm_i], role = "prey", element = remains_el[rm_i], dimension = remains_dim[rm_i], value = fnum(remains_val[rm_i]),
  unit = "g", value_type = "derived", locator = sprintf("data/raw_data.csv line %d, column %d (prey mass row 3 of that column)", lines[ii][rm_i], colno[rm_i]),
  notes = pcx$prey_basis[rm_i])

## ---------------- taxa: prey ----------------
pt <- unique(data.frame(id = pcx$taxon_id, name = pcx$taxon_name, cls = pcx$tax_class, ord = pcx$tax_order, fam = pcx$tax_family, vt = ptype == "vert", stringsAsFactors = FALSE))
pt <- pt[!duplicated(pt$id), ]
tx_prey <- taxon_row(pt$id, pt$name, guess_rank(pt$name), cls = pt$cls, ord = pt$ord, fam = pt$fam, vert = pt$vt,
                     notes = "from a raw_data.csv prey label; spelling as in the source")
taxa <- rbind(tx_pred, tx_prey); taxa <- taxa[!duplicated(taxa$taxon_id), ]

## ---------------- side logs ----------------
log_dropped(OWNER, dropped)
unk <- pairs[pairs$pred_mass_method == "unknown" | pairs$prey_mass_method == "unknown", c("pair_id", "source_id", "pred_taxon_id", "prey_taxon_id", "pred_mass_method", "prey_mass_method", "pred_mass_basis", "prey_mass_basis")]
write.csv(unk, file.path(MIG, paste0("unknown_", OWNER, ".csv")), row.names = FALSE)
log_input(OWNER, nrow(d), "predator records (rows 4+ of raw_data.csv)", sprintf("%d record x prey cells with count > 0; longform.csv has %d item rows", nrow(w), nrow(read.csv(file.path(ROOT, "data", "longform.csv"), colClasses = c("numeric", rep("NULL", 9))))))
commit(OWNER, OWNS, src, pairs, meas, taxa, pair_tag = "_L[0-9]+C[0-9]+$")
