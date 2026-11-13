###################################################################
# _expand.R -- helpers for the Phase 4b expansion scripts (2x_*.R). Source after _common.R.
# hand_pairs(): turns a hand-transcribed evidence table (code/db_migrate/evidence/expansion/<name>.csv)
# into pairs + taxa. Masses: a value given in the table keeps its stated method; the keyword
# "lookup" in *_mass_method asks for the AVONET (birds) / PanTHERIA (mammals) species mean, else genus
# mean, as 04_raptordiets.R does. Anything else stays empty; 90_prey_mass_fill.R fills prey masses later.
###################################################################
normn <- function(x) tolower(gsub("[ _]+", " ", trimws(x)))
.MASSDB <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    av <- read.csv(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), check.names = FALSE, stringsAsFactors = FALSE)
    avm <- num(av$Mass); ok <- is.finite(avm) & avm > 0; avs <- normn(av$Species1[ok]); avm <- avm[ok]
    pan <- read.delim(file.path(ROOT, "data", "pantheria"), header = FALSE, skip = 1, quote = "", colClasses = "character")
    pmm <- num(pan[[5]]); pmm[pmm == -999] <- NA
    ps <- tapply(pmm, normn(paste(pan[[3]], pan[[4]])), function(z) mean(z, na.rm = TRUE)); pg <- tapply(pmm, normn(pan[[3]]), function(z) mean(z, na.rm = TRUE))
    ## Amniote (Myhrvold et al. 2015): reptile adult body mass, missing = -999
    am <- read.csv(file.path(ROOT, "data", "structured", "raw_downloads", "amniote", "Amniote_Database_Aug_2015.csv"), stringsAsFactors = FALSE)
    am <- am[am$class == "Reptilia" & am$adult_body_mass_g > 0, ]; ams <- normn(paste(am$genus, am$species))
    ## AmphiBIO (Oliveira et al. 2017): Body_mass_g
    ph <- read.csv(file.path(ROOT, "data", "structured", "raw_downloads", "amphibio", "AmphiBIO_v1.csv"), check.names = FALSE, stringsAsFactors = FALSE)
    ph <- ph[is.finite(ph$Body_mass_g) & ph$Body_mass_g > 0, ]; phs <- normn(ph$Species)
    cache <<- list(av_sp = tapply(avm, avs, mean), av_gen = tapply(avm, sub(" .*$", "", avs), mean),
                   pan_sp = ps[is.finite(ps)], pan_gen = pg[is.finite(pg)],
                   am_sp = tapply(am$adult_body_mass_g, ams, mean), am_gen = tapply(am$adult_body_mass_g, normn(am$genus), mean),
                   ph_sp = tapply(ph$Body_mass_g, phs, mean), ph_gen = tapply(ph$Body_mass_g, normn(ph$Genus), mean),
                   am_tab = am, ph_tab = ph)
    cache }
})
## class: Aves -> AVONET, Mammalia -> PanTHERIA, Reptilia -> Amniote, Amphibia -> AmphiBIO. Returns list(v, b)
mass_lookup <- function(name, cls) {
  D <- .MASSDB(); n <- length(name); v <- rep(NA_real_, n); b <- rep("", n)
  for (i in seq_len(n)) {
    nm <- normn(name[i]); two <- sub("^(\\S+ \\S+).*$", "\\1", nm); gen <- sub(" .*$", "", nm); bin <- grepl(" ", nm) && !grepl(" (sp|spp|indet)\\.?$", nm)
    if (cls[i] == "Aves") { sp <- D$av_sp; gn <- D$av_gen; db <- "AVONET1_BirdLife (Mass)" }
    else if (cls[i] == "Mammalia") { sp <- D$pan_sp; gn <- D$pan_gen; db <- "PanTHERIA (data/pantheria column 5)" }
    else if (cls[i] == "Reptilia") { sp <- D$am_sp; gn <- D$am_gen; db <- "Amniote database (adult_body_mass_g)" }
    else if (cls[i] == "Amphibia") { sp <- D$ph_sp; gn <- D$ph_gen; db <- "AmphiBIO (Body_mass_g)" }
    else next
    if (bin && two %in% names(sp)) { v[i] <- sp[[two]]; b[i] <- paste0(db, " species mean for ", tools::toTitleCase(two)) }
    else if (gen %in% names(gn)) { v[i] <- gn[[gen]]; b[i] <- paste0(db, " genus mean for ", tools::toTitleCase(gen), if (bin) " (species not found under this name)" else " (prey identified to genus)") }
  }
  list(v = v, b = b)
}
EXP_EVID <- function(name) { x <- read.csv(file.path(EVID, "expansion", paste0(name, ".csv")), colClasses = "character", na.strings = character(0), check.names = FALSE); x }

## ev: evidence data frame. Required columns: rec, pred_name, pred_clade, pred_class, prey_name, prey_class, observation_type,
## quote. Optional: any other pairs column name (as listed in the header comment) and prey_rank, pred_family, prey_vertebrate.
hand_pairs <- function(ev, source_id, tag) {
  need <- c("rec", "pred_name", "pred_clade", "pred_class", "prey_name", "prey_class", "observation_type", "quote")
  if (length(setdiff(need, names(ev)))) stop("evidence table lacks: ", paste(setdiff(need, names(ev)), collapse = ", "))
  g <- function(cl, d = "") if (cl %in% names(ev)) ev[[cl]] else rep(d, nrow(ev))
  pm <- g("pred_mass_method"); ym <- g("prey_mass_method")
  pln <- ifelse(g("pred_lookup_name") != "", g("pred_lookup_name"), ev$pred_name); qln <- ifelse(g("prey_lookup_name") != "", g("prey_lookup_name"), ev$prey_name)
  pl <- mass_lookup(pln, ifelse(pm == "lookup", ev$pred_class, "")); ql <- mass_lookup(qln, ifelse(ym == "lookup", ev$prey_class, ""))
  pmass <- g("pred_mass_g"); pbas <- g("pred_mass_basis"); ymass <- g("prey_mass_g"); ybas <- g("prey_mass_basis")
  i <- pm == "lookup"; pmass[i] <- fnum(pl$v[i]); pbas[i] <- pl$b[i]; pm[i & !is.finite(pl$v)] <- ""; pm[i & is.finite(pl$v)] <- "species_average_database"
  i <- ym == "lookup"; ymass[i] <- fnum(ql$v[i]); ybas[i] <- paste0(ql$b[i], ifelse(is.finite(ql$v[i]), "; adult mean", ""))
  ym[i & !is.finite(ql$v)] <- ""; ym[i & is.finite(ql$v)] <- "species_average_database"
  pm[pmass == "" & g("pred_mass_lo_g") == ""] <- ""; ym[ymass == "" & g("prey_mass_lo_g") == ""] <- ""; pbas[pm == "" & pbas == ""] <- ""; ybas[ym == ""] <- ""
  pid <- taxon_id_of(ev$pred_name); yid <- taxon_id_of(ev$prey_name)
  tier <- tier_of(pm, ym, pred_none_from_animal = g("pred_len_recorded", "FALSE") == "TRUE", prey_none_from_animal = g("prey_len_recorded", "FALSE") == "TRUE")
  ip <- g("individual_pair", "FALSE")
  pairs <- mk("pairs", pair_id = paste0(source_id, "_", ev$rec), source_id = source_id, pred_taxon_id = pid, prey_taxon_id = yid,
    pred_specimen = g("pred_specimen"), observation_type = ev$observation_type, pair_resolution = tier, individual_pair = ip,
    pred_mass_g = pmass, pred_mass_lo_g = g("pred_mass_lo_g"), pred_mass_hi_g = g("pred_mass_hi_g"), pred_mass_method = pm, pred_mass_method_id = g("pred_mass_method_id"), pred_mass_basis = pbas,
    prey_mass_g = ymass, prey_mass_lo_g = g("prey_mass_lo_g"), prey_mass_hi_g = g("prey_mass_hi_g"), prey_mass_method = ym, prey_mass_method_id = g("prey_mass_method_id"), prey_mass_basis = ybas,
    pred_stage = g("pred_stage"), prey_stage = g("prey_stage"), n_items = g("n_items"), interaction_outcome = ifelse(g("interaction_outcome") == "", "consumed", g("interaction_outcome")),
    is_fossil = "FALSE", quote_or_locator = ev$quote, notes = g("notes"))
  pv <- g("prey_vertebrate"); pt <- prey_type(ev$prey_class); pvv <- ifelse(pv != "", pv == "TRUE", ifelse(is.na(pt), NA, pt == "vert"))
  taxa <- rbind(taxon_row(pid, ev$pred_name, "species", cls = ev$pred_class, ord = g("pred_order"), fam = g("pred_family"), clade = ev$pred_clade, vert = TRUE),
                taxon_row(yid, ev$prey_name, ifelse(g("prey_rank") != "", g("prey_rank"), guess_rank(ev$prey_name)), cls = ev$prey_class, ord = g("prey_order"), fam = g("prey_family"), vert = pvv))
  list(pairs = pairs, taxa = taxa[!duplicated(taxa$taxon_id), ])
}

## apply script-level defaults to an evidence table (columns already present win)
with_defaults <- function(ev, ...) { d <- list(...); for (k in names(d)) if (!k %in% names(ev)) ev[[k]] <- d[[k]]; ev }
