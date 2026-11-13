###################################################################
# build_rawdata_evidence.R -- how was each mass in data/raw_data.csv produced?
#
# raw_data.csv (the 2019 diet matrix) records one prey mass per prey COLUMN and one predator
# mass per RECORD, with no field saying where either came from. This script gathers the
# evidence that can be checked mechanically and classifies each value; 01_rawdata.R reads
# the three tables it writes, so the migration itself does not need the PDFs.
#
# Evidence used, in order:
#   1. mass_overrides.csv (x03 fills: PanTHERIA/AVONET binomial or genus mean)
#   2. an exact (<1%) match to the PanTHERIA (col 5, by position) or AVONET species/genus mean
#   3. reuse: the same label with the same mass in >= 2 studies => a taxon value, not a study value
#   4. the study's own PDF (archive_old_pipeline/compiled/Compiled/<RefNo>_*.pdf): is the value,
#      or value x count, printed in it? A study counts as "reporting" its values only when its hit
#      rate beats a control of random masses from other studies by >= 0.2 and is >= 0.5.
#   5. hand decisions with quotes: evidence/rawdata_study_decisions.csv (stomach-content papers)
# Observation type: raw_data ObsType, refined for bird "Scat" and for "Follow" by keyword counts
# in the PDF (pellet vs prey remains; kill site/GPS cluster vs observation).
#
# Needs: pdftotext (poppler). Writes evidence/rawdata_{studies,prey_columns,pred_records}.csv
###################################################################
args_all <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args_all, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa[1]))), "../../..")) else normalizePath(getwd())
EVID <- file.path(ROOT, "code", "db_migrate", "evidence")
PDFDIR <- file.path(ROOT, "archive_old_pipeline", "compiled", "Compiled")
num <- function(x) suppressWarnings(as.numeric(x))

## ---------------- raw_data ----------------
T <- read.csv(file.path(ROOT, "data", "raw_data.csv"), header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
SC <- 31; pc <- SC:ncol(T); d <- T[4:nrow(T), ]; ref <- trimws(d[, 1]); line <- 4:nrow(T)
hdr <- as.character(unlist(T[3, ])); f <- function(n) which(hdr == n)
lab <- trimws(as.character(unlist(T[1, pc]))); cls <- trimws(as.character(unlist(T[2, pc]))); pmr <- trimws(hdr[pc]); pm <- num(pmr)
C <- suppressWarnings(matrix(num(as.matrix(d[, pc])), nrow = nrow(d))); C[is.na(C)] <- 0
pred_sp <- paste(trimws(d[, f("Genus")]), trimws(d[, f("Species")])); pred_m <- num(d[, f("Mass")])

## ---------------- reference masses ----------------
ov <- read.csv(file.path(ROOT, "data", "mass_overrides.csv"), stringsAsFactors = FALSE)
pan <- read.delim(file.path(ROOT, "data", "pantheria"), header = FALSE, skip = 1, quote = "", colClasses = "character")
pmass <- num(pan[[5]]); pmass[pmass == -999] <- NA
pan_b <- tapply(pmass, paste(pan[[3]], pan[[4]]), function(z) mean(z, na.rm = TRUE)); pan_b <- pan_b[is.finite(pan_b)]
pan_g <- tapply(pmass, pan[[3]], function(z) mean(z, na.rm = TRUE)); pan_g <- pan_g[is.finite(pan_g)]
av <- read.csv(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), stringsAsFactors = FALSE, check.names = FALSE)
avm <- num(av$Mass); k <- is.finite(avm) & avm > 0; avs <- gsub("[ _]+", " ", trimws(av$Species1[k])); avm <- avm[k]
av_b <- tapply(avm, avs, mean); av_g <- tapply(avm, sub(" .*", "", avs), mean)
dbmatch <- function(name, m) {
  if (!is.finite(m)) return(NA_character_)
  nm <- gsub("[ _]+", " ", name); gen <- sub(" .*", "", nm); ok <- function(x) is.finite(x) && abs(x - m) / m < 0.01
  if (nm %in% names(pan_b) && ok(pan_b[[nm]])) return(paste0("PanTHERIA species mean for ", nm))
  if (nm %in% names(av_b)  && ok(av_b[[nm]]))  return(paste0("AVONET species mean for ", nm))
  if (gen %in% names(pan_g) && ok(pan_g[[gen]])) return(paste0("PanTHERIA genus mean for ", gen))
  if (gen %in% names(av_g)  && ok(av_g[[gen]]))  return(paste0("AVONET genus mean for ", gen))
  NA_character_ }

## ---------------- prey labels -> taxon, stage ----------------
GENERIC <- c("indet","unidentified","unknown","other","fish","fishes","pisces","bird","birds","small bird","large passerine","medium passerine",
  "small passerine","v small passerine","passerine","frog","frogs","toad","snake","turtle","turtles","rodents","mice","mouse","rats","shrew","large",
  "small","adult","lagartos no identificados","geckos","civets","sloth","arboreal","bird egg","egg aves","eggs","feathers","fowl","poultry",
  "waterfowl","duck","quail","lark","sunbird","egret","cormorant","anatids","bufonid","insects","insect","beetles","crab","crabs","snail","snails",
  "prawn","crayfish","crayfish indet","centipede","locust","locust indet","maggots","mussel","dragonfly","larva","larvae","pupae","mollusc",
  "crustacean","arthropod","insects and spiders","13.6","19.2","red squirrel","scincidae1","scincidae2","fruit","plant","carrion","sceloporine")
STAGE <- c(nestling = "nestling", nestlings = "nestling", chick = "chick", chicks = "chick", young = "young", baby = "young", juv = "juvenile",
  juvenile = "juvenile", subadult = "subadult", lamb = "juvenile", joey = "juvenile", tadpole = "tadpole", larva = "larva", larvae = "larva",
  egg = "egg", eggs = "egg", adult = "adult", pupae = "pupa")
NOTE_TOK <- c("conifer", "hisp", "cape", "nz", "size", "average", "large", "small")
clean_cls <- function(s) { t <- unlist(strsplit(s, "_")); t <- t[!toupper(t) %in% c("INVERT", "INDET", "")]; t }
parse_label <- function(l, s) {
  x <- gsub("[ ]+", "_", trimws(l)); x <- gsub("mad/sis", "madagascariensis", x, fixed = TRUE)
  note <- ""; stage <- ""
  m <- regmatches(x, regexpr("_(mass ?of|massof|MassOf)[A-Za-z_]*$", x, ignore.case = TRUE))
  if (length(m)) { note <- paste0("compiler note '", sub("^_", "", m), "' (mass taken from a related species)"); x <- sub("_(mass ?of|massof|MassOf)[A-Za-z_]*$", "", x, ignore.case = TRUE) }
  x <- sub("_Indet\\(Drawf\\)$", "", x)
  toks <- unlist(strsplit(x, "_"))
  if (length(toks) > 1) {
    lt <- tolower(toks[length(toks)])
    if (lt %in% names(STAGE)) { stage <- STAGE[[lt]]; toks <- toks[-length(toks)] }
    else if (lt %in% NOTE_TOK) { note <- paste0("label suffix '", toks[length(toks)], "' kept in notes"); toks <- toks[-length(toks)] }
  }
  base <- paste(toks, collapse = "_")
  if (tolower(base) %in% c("bird_egg", "egg_aves")) stage <- "egg"
  generic <- tolower(gsub("_", " ", base)) %in% GENERIC | grepl("^(indet|Indet)(_|$)", base) | !grepl("[A-Za-z]", base)
  ct <- clean_cls(s)
  if (generic) {
    deep <- if (length(ct)) ct[length(ct)] else "Animalia_indet"
    id <- make_id(paste0(deep, if (grepl("idae$|inae$|formes$|^[A-Z][a-z]+$", deep) && length(ct) <= 1) "" else ""))
    name <- deep; note <- trimws(paste(note, sprintf("raw_data label '%s' mapped to %s", l, deep)))
  } else { id <- make_id(base); name <- base }
  cl <- if (length(ct)) ct[1] else ""
  ord <- if (length(ct) >= 2 && !grepl("idae$|inae$", ct[2])) ct[2] else ""
  fam <- if (length(ct) >= 2 && grepl("idae$", ct[length(ct)])) ct[length(ct)] else ""
  data.frame(taxon_id = id, name = name, stage = stage, label_note = note, cls = cl, ord = ord, fam = fam, generic = generic, stringsAsFactors = FALSE)
}
make_id <- function(x) { x <- gsub("/", "_or_", x, fixed = TRUE); x <- gsub("[^A-Za-z0-9_.:-]+", "_", x); x <- gsub("_+", "_", x); gsub("^[_.:-]+|[_.:-]+$", "", x) }
resolved_rank <- function(name) grepl("^[A-Z][a-z]+(_[a-z]+){0,2}$", name) & !grepl("_(sp|spp|indet)$", name)   # genus, species or subspecies

## ---------------- PDF text ----------------
pdf_of <- function(r) { fs <- list.files(PDFDIR, pattern = paste0("^", r, "[_.]"), full.names = TRUE); fs }
txt_of <- function(r) {
  fs <- pdf_of(r); if (!length(fs)) return(NA_character_)
  t <- paste(vapply(fs, function(p) paste(system2("pdftotext", c("-q", shQuote(p), "-"), stdout = TRUE), collapse = " "), ""), collapse = " ")
  t <- gsub("(\\d)[, ](\\d{3})(?!\\d)", "\\1\\2", t, perl = TRUE)
  if (nchar(gsub("\\s", "", t)) < 500) NA_character_ else t }
forms <- function(v) { if (!is.finite(v) || v <= 0) return(character(0)); out <- c()
  for (dd in 0:2) { s <- formatC(v, format = "f", digits = dd); if (abs(num(s) - v) / v < 0.005) out <- c(out, s) }
  kg <- v / 1000; for (dd in 1:2) { s <- formatC(kg, format = "f", digits = dd); if (kg >= 1 && abs(num(s) - kg) / kg < 0.005) out <- c(out, s) }
  out <- unique(out); out[nchar(gsub("[^0-9]", "", out)) >= 2] }
found <- function(t, v) { fo <- forms(v); length(fo) > 0 && any(vapply(fo, function(s) grepl(paste0("(?<![0-9.])", gsub(".", "\\.", s, fixed = TRUE), "(?![0-9])"), t, perl = TRUE), TRUE)) }
kw <- function(t, p) lengths(regmatches(t, gregexpr(p, t, ignore.case = TRUE, perl = TRUE)))

## ---------------- per column: category ----------------
used <- colSums(C > 0) > 0
lm_key <- paste(lab, signif(pm, 6))
refs_by_lm <- tapply(seq_along(pc), lm_key, function(js) length(unique(ref[rowSums(C[, js, drop = FALSE] > 0) > 0])))
nref_lm <- as.integer(refs_by_lm[lm_key])
okey <- paste(lab, cls)
ovr_src <- ov$source[match(okey, ov$key)]
dbm <- mapply(dbmatch, lab, pm)
PL <- do.call(rbind, Map(parse_label, lab, cls))

## ---------------- per study ----------------
dec <- read.csv(file.path(EVID, "rawdata_study_decisions.csv"), colClasses = "character", check.names = FALSE)
set.seed(20261112); ctl_pool <- pm[used & is.finite(pm)]
studies <- list(); preyrows <- list(); predrows <- list()
for (r in unique(ref)) {
  i <- which(ref == r); sub <- C[i, , drop = FALSE]; cols <- which(colSums(sub > 0) > 0)
  t <- txt_of(r); has <- !is.na(t); D <- dec[dec$refno == r, , drop = FALSE]
  obs_raw <- unique(trimws(d[i, f("ObsType")])); cl_raw <- unique(trimws(d[i, f("Class")]))
  cnt <- if (has) c(pellet = kw(t, "pellet"), remains = kw(t, "prey remains|plucking|remains at|nest remains|remains found|food remains"),
                    killsite = kw(t, "kill site|kill-site|carcass|cluster|GPS"), observ = kw(t, "observ"), weighed = kw(t, "weigh(ed|ing)")) else
         c(pellet = NA, remains = NA, killsite = NA, observ = NA, weighed = NA)
  ## column categories
  cat0 <- ifelse(!is.na(ovr_src[cols]) & !is.finite(pm[cols]), "override_db", ifelse(!is.finite(pm[cols]), "no_mass",
          ifelse(!is.na(dbm[cols]), "db_match", ifelse(nref_lm[cols] >= 2, "shared_constant", "study_specific"))))
  ss <- cols[cat0 == "study_specific"]
  hit <- if (has) vapply(ss, function(j) found(t, pm[j]) || found(t, pm[j] * sum(sub[, j])) || found(t, round(pm[j] * sum(sub[, j]))), TRUE) else rep(NA, length(ss))
  ctl <- if (has) mean(vapply(sample(ctl_pool, 40), function(v) found(t, v), TRUE)) else NA
  rate <- if (length(ss) && has) mean(hit) else NA
  reports <- has && length(ss) >= 1 && isTRUE(rate >= 0.5 && rate >= ctl + 0.2)
  studies[[r]] <- data.frame(refno = r, first_author = d[i[1], f("First Author")], year = d[i[1], f("Year")], journal = d[i[1], f("Journal")],
    pdf = paste(basename(pdf_of(r)), collapse = "; "), pdf_text = has, obs_types = paste(obs_raw, collapse = "/"), classes = paste(cl_raw, collapse = "/"),
    n_ss_cols = length(ss), ss_hit_rate = round(rate, 2), control_rate = round(ctl, 2), reports_prey_values = reports,
    pellet = cnt[["pellet"]], remains = cnt[["remains"]], killsite = cnt[["killsite"]], observ = cnt[["observ"]], weighed = cnt[["weighed"]], stringsAsFactors = FALSE)
  ## prey columns of this study
  for (jj in seq_along(cols)) {
    j <- cols[jj]; cg <- cat0[jj]; P <- PL[j, ]; meth <- ""; basis <- ""; fnd <- NA
    if (cg == "override_db") { meth <- "species_average_database"; basis <- sprintf("filled by x03 from data/mass_overrides.csv (%s): %s", ovr_src[j], okey[j]) }
    else if (cg == "db_match") { meth <- "species_average_database"; basis <- paste0(dbm[j], " (value matches to <1%)") }
    else if (cg == "shared_constant") {
      if (resolved_rank(P$name) && !P$generic) { meth <- "species_average_literature"; basis <- sprintf("taxon value reused for this label in %d raw_data studies; reference not recorded in raw_data.csv", nref_lm[j]) }
      else { meth <- "assigned_approximate"; basis <- sprintf("value for a coarse label ('%s') reused in %d raw_data studies; no reference recorded", lab[j], nref_lm[j]) } }
    else if (cg == "study_specific") {
      fnd <- if (has) hit[match(j, ss)] else NA
      if (nrow(D) && nzchar(D$prey_ss_override)) { meth <- D$prey_ss_override; basis <- D$prey_ss_basis }
      else if (reports && isTRUE(fnd)) {
        if (all(obs_raw == "Stomach")) { meth <- "unknown"; basis <- "value printed in the source paper; how the authors obtained stomach prey masses was not checked" }
        else { meth <- "species_average_literature"; basis <- "value printed in the source paper; prey from pellets, scats, remains or observed kills are not weighed, so this is the authors' reference weight" } }
      else {
        same_lab <- cols[lab[cols] == lab[j] & cat0 == "study_specific"]
        if (resolved_rank(P$name) && !P$generic && length(unique(pm[same_lab])) == 1) { meth <- "species_average_literature"; basis <- sprintf("compiler's value for this taxon; %s; reference not recorded", if (has) "not found in the source text" else "source PDF not on disk or image-only") }
        else { meth <- "unknown"; basis <- sprintf("study-specific value for '%s'%s; %s", lab[j], if (length(unique(pm[same_lab])) > 1) " that differs between records of this study" else " (coarse label)", if (has) "not found in the source text" else "source PDF not on disk or image-only") } } }
    preyrows[[length(preyrows) + 1]] <- data.frame(refno = r, col = SC + j - 1, label = lab[j], class_string = cls[j], mass_raw = pmr[j], mass_g = pm[j],
      category = cg, found_in_pdf = fnd, prey_method = meth, prey_basis = basis, taxon_id = P$taxon_id, taxon_name = P$name, prey_stage = P$stage,
      label_note = P$label_note, tax_class = P$cls, tax_order = P$ord, tax_family = P$fam, generic_label = P$generic, stringsAsFactors = FALSE)
  }
  ## predator records of this study
  for (ii in i) {
    m <- pred_m[ii]; sp <- pred_sp[ii]; meth <- ""; basis <- ""; fnd <- NA
    if (is.finite(m)) {
      reuse <- length(unique(ref[pred_sp == sp & is.finite(pred_m) & abs(pred_m - m) < 1e-9]))
      nval  <- length(unique(pred_m[i][pred_sp[i] == sp & is.finite(pred_m[i])]))
      dm <- dbmatch(sp, m); fnd <- if (has) found(t, m) else NA
      if (nrow(D) && nzchar(D$pred_override)) { meth <- D$pred_override; basis <- D$pred_basis }
      else if (!is.na(dm)) { meth <- "species_average_database"; basis <- paste0(dm, " (value matches to <1%)") }
      else if (reuse >= 2) { meth <- "species_average_literature"; basis <- sprintf("species value reused in %d raw_data studies; reference not recorded", reuse) }
      else if (isTRUE(fnd)) { meth <- "species_average_literature"; basis <- "value printed in the source paper (taken as the authors' species or sex mean)" }
      else if (nval <= 2) { meth <- "species_average_literature"; basis <- sprintf("compiler's species%s value; %s; reference not recorded", if (nval == 2) "-by-sex or site" else "", if (has) "not found in the source text" else "source PDF not on disk or image-only") }
      else { meth <- "unknown"; basis <- sprintf("one of %d different masses for %s within this study; not found in the source text", nval, sp) }
    }
    predrows[[length(predrows) + 1]] <- data.frame(refno = r, line = line[ii], species = sp, mass_raw = d[ii, f("Mass")], mass_g = m, found_in_pdf = fnd,
      pred_method = meth, pred_basis = basis, nobs = trimws(d[ii, f("Nobservations")]), obstype_raw = trimws(d[ii, f("ObsType")]), stringsAsFactors = FALSE)
  }
}
S <- do.call(rbind, studies); S <- S[order(num(S$refno)), ]
## observation type per study x ObsType (applied per record in 01_rawdata.R)
S$obs_aves_scat <- with(S, ifelse(!pdf_text, "pellet", ifelse(remains > pellet & pellet < 3, "prey_remains", "pellet")))
S$obs_aves_scat_note <- with(S, ifelse(!pdf_text, "bird 'Scat' record: source PDF not on disk or image-only; coded pellet as in x20 (not verified)",
  ifelse(remains > pellet & pellet < 3, sprintf("bird 'Scat' record coded prey_remains: PDF mentions prey remains/plucking %d times, pellets %d times", remains, pellet),
  ifelse(pellet == 0 & remains == 0, "bird 'Scat' record coded pellet as in x20: PDF mentions neither pellets nor prey remains (not verified)",
  ifelse(remains >= 3, sprintf("bird 'Scat' record coded pellet: PDF mentions pellets %d times and prey remains %d times (sources pooled)", pellet, remains),
  sprintf("bird 'Scat' record coded pellet: PDF mentions pellets %d times", pellet))))))
S$obs_follow <- with(S, ifelse(pdf_text & killsite > observ, "prey_remains", "direct_observation"))
S$obs_follow_note <- with(S, ifelse(!pdf_text, "'Follow' record: source PDF not on disk; coded direct_observation (not verified)",
  ifelse(killsite > observ, sprintf("'Follow' record coded prey_remains: kill site/carcass/GPS cluster mentioned %d times vs observation %d", killsite, observ),
  sprintf("'Follow' record coded direct_observation: observation mentioned %d times, kill site/carcass/GPS %d", observ, killsite))))
write.csv(S, file.path(EVID, "rawdata_studies.csv"), row.names = FALSE)
write.csv(do.call(rbind, preyrows), file.path(EVID, "rawdata_prey_columns.csv"), row.names = FALSE)
write.csv(do.call(rbind, predrows), file.path(EVID, "rawdata_pred_records.csv"), row.names = FALSE)
cat("wrote rawdata_studies.csv (", nrow(S), "), rawdata_prey_columns.csv, rawdata_pred_records.csv\n")
