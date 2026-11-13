###################################################################
# 32_gateway.R -- GATEWAy 1.0 (Brose et al. 2019, Nat Ecol Evol 3:919-927; iDiv 10.25829/iDiv.283-3-756, CC BY 4.0), the
# tetrapod-consumer links (Phase 4b, Part 3). Input: the zip in data/structured/raw_downloads/gateway/ (data CSV, 222,151 links
# from 23 food-web compilations; each row has consumer and resource taxonomy, mean masses and a methodology string).
# Admitted: consumer is a bird, mammal, reptile or amphibian (the file has no class column, so the class comes from the genus
# in AVONET, PanTHERIA, the Amniote database or AmphiBIO; consumers above genus, or in none of those lists, e.g. fish, are
# dropped); interaction.type predacious (not herbivory, parasitism or parasitoidism); resource is an animal (invertebrate or
# vertebrate by res.metabolic.type) identified to species, genus, family or order; and at least one mass is given.
# Masses: GATEWAy's con/res.mass.mean.g (compiled species means, some regression-derived) -> species_average_literature, with
# min and max as the interval when given; the size citation and method go in the basis. A missing consumer mass is looked up in
# the mass databases as elsewhere (species_average_database); a missing resource mass is left to 90_prey_mass_fill.R.
# Observation type: `direct_observation` where link.methodology is exactly "direct observation"; everything else (published
# account, expert knowledge, gut content extrapolated from similar taxa, mixed methods) is `unknown`, with the methodology in
# notes, so these rows enter only the least comparable analysis stage. Tier D (E if a mass method forces it).
# Duplicates (publication level, as in MIGRATION_REPORT.md): a GATEWAy food-web citation whose first-author surname and year match
# a db source with the same consumer species is dropped for that species (evidence kind: other). Link-level repeats of a
# predator-prey pair already in db/ from other publications are kept, and counted in the log note.
# Owns: sources whose extracted_from starts with the zip path; pairs "*_GW_gw<autoID>".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
source(file.path(ROOT, "code", "db_migrate", "_expand.R"))
OWNER <- "32_gateway"; ZIP <- "data/structured/raw_downloads/gateway/GATEWAy_v1.0_iDiv283_v3.zip"; OWNS <- ZIP
td <- tempfile("gw"); dir.create(td)
inner <- grep("FoodWebDataBase.*\\.csv$", unzip(file.path(ROOT, ZIP), list = TRUE)$Name, value = TRUE)
suppressWarnings(unzip(file.path(ROOT, ZIP), files = inner, exdir = td, junkpaths = TRUE))
d <- read.csv(list.files(td, pattern = "csv$", full.names = TRUE)[1], colClasses = "character", na.strings = "NA", check.names = FALSE)
for (cl in names(d)) { v <- d[[cl]]; v[is.na(v)] <- ""; bad <- !validUTF8(v); if (any(bad)) v[bad] <- iconv(v[bad], "latin1", "UTF-8"); d[[cl]] <- v }
n_in <- nrow(d)
drop <- data.frame(source_file = character(), source_row = character(), reason = character(), detail = character())
keep <- rep(TRUE, n_in)
mark <- function(i, reason) { i <- i & keep; if (any(i)) { drop <<- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("autoID ", d$autoID[i][seq_len(min(sum(i), 50))]), reason = paste0(reason, " (", sum(i), " links; first 50 listed)"),
  detail = sprintf("%s x %s", d$con.taxonomy[i], d$res.taxonomy[i])[seq_len(min(sum(i), 50))])); keep[i] <<- FALSE } }
## consumer class from genus
M <- .MASSDB()
gen_class <- c(setNames(rep("Aves", length(M$av_gen)), names(M$av_gen)), setNames(rep("Mammalia", length(M$pan_gen)), names(M$pan_gen)),
               setNames(rep("Reptilia", length(M$am_gen)), names(M$am_gen)), setNames(rep("Amphibia", length(M$ph_gen)), names(M$ph_gen)))
cname <- trimws(sub("\\s*\\(.*\\)\\s*$", "", d$con.taxonomy)); cname <- sub("\\s+(sp|spp)\\.?$", "", cname)
cgen <- sub(" .*$", "", normn(cname)); ccls <- unname(gen_class[cgen])
mark(!d$con.metabolic.type %in% c("endotherm vertebrate", "ectotherm vertebrate"), "consumer is not a vertebrate")
mark(is.na(ccls) | cname == "", "consumer not identified as a bird, mammal, reptile or amphibian (fish, or above genus level, or not in the trait lists)")
mark(!d$interaction.type %in% c("predacious", ""), "interaction is herbivory, detritivory, parasitism or other")
mark(!d$res.metabolic.type %in% c("invertebrate", "ectotherm vertebrate", "endotherm vertebrate"), "resource is not an animal (plant, detritus, fungus, bacterium, other)")
rname <- trimws(sub("\\s*\\(.*\\)\\s*$", "", d$res.taxonomy)); rname <- sub("\\s+(sp|spp)\\.?$", "", rname)
mark(rname == "" | grepl("^(animal|invertebrate|vertebrate|other)s?$", tolower(rname)), "resource not identified to a named taxon")
cm <- num(d$con.mass.mean.g.); cm[cm <= 0] <- NA; rm <- num(d$res.mass.mean.g.); rm[rm <= 0] <- NA
## note: -999 -> negative -> NA
mark(is.na(cm) & is.na(rm) & !(cgen %in% names(gen_class)), "no mass on either side")
## ---- duplicates by publication ----
norm_s <- function(x) gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
P0 <- read_db("pairs"); S0 <- read_db("sources"); P0 <- P0[P0$is_fossil == "FALSE" & !grepl("_GW_gw[0-9]+$", P0$pair_id), ]
S0$yr <- sub("^[^0-9]*((1[89]|20)[0-9]{2}).*$", "\\1", S0$source_id); S0$yr[!grepl("^(1[89]|20)[0-9]{2}$", S0$yr)] <- ""
S0$sur <- norm_s(sub("^([^0-9_]*)[0-9].*$", "\\1", S0$source_id))
sp_of <- function(x) sub("^(\\S+ \\S+).*$", "\\1", gsub("_", " ", x))
P0$key <- paste(S0$sur[match(P0$source_id, S0$source_id)], S0$yr[match(P0$source_id, S0$source_id)], sp_of(P0$pred_taxon_id))
cit_sur <- norm_s(sub("^([^ ,(]+).*$", "\\1", d$link.citation)); cit_yr <- sub("^.*((19|20)[0-9]{2}).*$", "\\1", d$link.citation); cit_yr[!grepl("^(19|20)[0-9]{2}$", cit_yr)] <- ""
dk <- paste(cit_sur, cit_yr, sp_of(cname)); dh <- keep & dk %in% P0$key
if (any(dh)) { drop <- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("autoID ", d$autoID[dh]), reason = "duplicate record: same first author, year and consumer species already in db/",
  detail = sprintf("%s x %s [%s]; kept copy: %s", cname[dh], rname[dh], d$link.citation[dh], vapply(dk[dh], function(k) paste(unique(P0$source_id[P0$key == k]), collapse = "+"), "")))); keep[dh] <- FALSE }
sel <- which(keep); d <- d[sel, , drop = FALSE]; cname <- cname[sel]; rname <- rname[sel]; ccls <- ccls[sel]; cm <- cm[sel]; rm <- rm[sel]
## ---- ids and sources ----
cit <- d$link.citation; au <- make_id(gsub("[ '.-]", "", tools::toTitleCase(sub("^([^ ,(]+).*$", "\\1", cit)))); yr <- ifelse(grepl("(19|20)[0-9]{2}", cit), sub("^.*((19|20)[0-9]{2}).*$", "\\1", cit), "nd")
ucit <- unique(cit); ub <- paste0(make_id(gsub("[ '.-]", "", tools::toTitleCase(sub("^([^ ,(]+).*$", "\\1", ucit)))), ifelse(grepl("(19|20)[0-9]{2}", ucit), sub("^.*((19|20)[0-9]{2}).*$", "\\1", ucit), "nd"))
sfx <- ave(ub, ub, FUN = function(v) if (length(v) == 1) "" else letters[seq_along(v)]); sidmap <- setNames(paste0(ub, sfx, "_GW"), ucit); sid <- unname(sidmap[cit])
## ---- masses ----
pl <- mass_lookup(cname, ifelse(is.finite(cm), "", ccls))
pmass <- ifelse(is.finite(cm), cm, pl$v); pmeth <- ifelse(is.finite(cm), "species_average_literature", ifelse(is.finite(pl$v), "species_average_database", ""))
con_lo <- num(d$con.mass.min.g.); con_hi <- num(d$con.mass.max.g.); ok_i <- is.finite(cm) & is.finite(con_lo) & is.finite(con_hi) & con_lo > 0 & con_hi >= cm & con_lo <= cm
res_lo <- num(d$res.mass.min.g.); res_hi <- num(d$res.mass.max.g.); ok_r <- is.finite(rm) & is.finite(res_lo) & is.finite(res_hi) & res_lo > 0 & res_hi >= rm & res_lo <= rm
pbas <- ifelse(is.finite(cm), sprintf("GATEWAy con.mass.mean.g; size citation: %s; size method: %s", ifelse(d$con.size.citation == "", "not given", d$con.size.citation), ifelse(d$con.size.method == "", "not given", gsub("\\s+", " ", d$con.size.method))), ifelse(is.finite(pl$v), pl$b, ""))
ymeth <- ifelse(is.finite(rm), "species_average_literature", "")
ybas <- ifelse(is.finite(rm), sprintf("GATEWAy res.mass.mean.g; size citation: %s; size method: %s%s", ifelse(d$res.size.citation == "", "not given", d$res.size.citation), ifelse(d$res.size.method == "", "not given", gsub("\\s+", " ", d$res.size.method)), ifelse(d$res.lifestage != "", paste0("; resource life stage: ", d$res.lifestage), "")), "")
## ---- taxa ----
rlvl <- d$res.taxonomy.level; rlvl[!rlvl %in% c("species", "genus", "family", "order", "class")] <- ""
rrank <- ifelse(rlvl != "", rlvl, guess_rank(rname)); rid <- taxon_id_of(rname); pid <- taxon_id_of(cname)
rvert <- d$res.metabolic.type != "invertebrate"
taxa <- rbind(taxon_row(pid, cname, "species", cls = ccls, clade = ifelse(ccls == "Reptilia", "Lacertilia", ccls), vert = TRUE),
              taxon_row(rid, rname, rrank, cls = unname(gen_class[sub(" .*$", "", normn(rname))]) , vert = rvert))
taxa$class[is.na(taxa$class)] <- ""; taxa <- taxa[!duplicated(taxa$taxon_id), ]
## reptile predators need a curated clade (no genus regex): use the curated genus table; others stay blank and are dropped below
sq <- SQUAMATES(); tx_gen <- sub("_.*$", "", taxa$taxon_id)
rep_i <- taxa$analysis_clade == "Lacertilia" & taxa$class == "Reptilia"
taxa$analysis_clade[rep_i] <- sq$analysis_clade[match(tx_gen[rep_i], sq$genus)]; taxa$analysis_clade[is.na(taxa$analysis_clade)] <- ""
bad_clade <- taxa$taxon_id[rep_i & taxa$analysis_clade == ""]
amph <- taxa$taxon_id[taxa$class == "Amphibia" & taxa$analysis_clade == "Amphibia"]; bad_clade <- c(bad_clade, amph)
isb <- pid %in% bad_clade
if (any(isb)) { drop <- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("autoID ", d$autoID[isb][seq_len(min(sum(isb), 50))]),
  reason = paste0("consumer is a reptile genus absent from the curated squamate/crocodylian table, or an amphibian: no analysis clade (", sum(isb), " links; first 50 listed)"), detail = sprintf("%s x %s", cname[isb], rname[isb])[seq_len(min(sum(isb), 50))])) }
f <- !isb
d <- d[f, , drop = FALSE]; cname <- cname[f]; rname <- rname[f]; ccls <- ccls[f]; pmass <- pmass[f]; pmeth <- pmeth[f]; pbas <- pbas[f]; rm <- rm[f]; ymeth <- ymeth[f]; ybas <- ybas[f]
ok_i <- ok_i[f]; ok_r <- ok_r[f]; con_lo <- con_lo[f]; con_hi <- con_hi[f]; res_lo <- res_lo[f]; res_hi <- res_hi[f]; sid <- sid[f]; pid <- pid[f]; rid <- rid[f]; cm <- cm[f]
taxa <- taxa[taxa$taxon_id %in% c(pid, rid), ]
## second mass-presence filter: at least one mass after lookup
nom <- !is.finite(pmass) & !is.finite(rm)
if (any(nom)) { drop <- rbind(drop, data.frame(source_file = ZIP, source_row = paste0("autoID ", d$autoID[nom][seq_len(min(sum(nom), 50))]), reason = paste0("no mass for either side after lookup (", sum(nom), " links; first 50 listed)"), detail = sprintf("%s x %s", cname[nom], rname[nom])[seq_len(min(sum(nom), 50))])) }
g <- !nom; d <- d[g, , drop = FALSE]; cname <- cname[g]; rname <- rname[g]; pmass <- pmass[g]; pmeth <- pmeth[g]; pbas <- pbas[g]; rm <- rm[g]; ymeth <- ymeth[g]; ybas <- ybas[g]
ok_i <- ok_i[g]; ok_r <- ok_r[g]; con_lo <- con_lo[g]; con_hi <- con_hi[g]; res_lo <- res_lo[g]; res_hi <- res_hi[g]; sid <- sid[g]; pid <- pid[g]; rid <- rid[g]
## ---- pairs ----
meth <- gsub("\\s+", " ", trimws(d$link.methodology)); otype <- ifelse(tolower(meth) == "direct observation", "direct_observation", "unknown")
tier <- tier_of(pmeth, ymeth)
loc <- trimws(paste(d$geographic.location, d$study.site, sep = " / ")); loc <- gsub("^ / | / $|^/ | /$", "", loc)
pairs <- mk("pairs", pair_id = paste0(sid, "_gw", d$autoID), source_id = sid, pred_taxon_id = pid, prey_taxon_id = rid, observation_type = otype, pair_resolution = tier, individual_pair = "FALSE",
  pred_mass_g = fnum(pmass), pred_mass_lo_g = ifelse(ok_i & pmeth == "species_average_literature", fnum(con_lo), ""), pred_mass_hi_g = ifelse(ok_i & pmeth == "species_average_literature", fnum(con_hi), ""),
  pred_mass_method = pmeth, pred_mass_basis = pbas,
  prey_mass_g = fnum(rm), prey_mass_lo_g = ifelse(ok_r, fnum(res_lo), ""), prey_mass_hi_g = ifelse(ok_r, fnum(res_hi), ""), prey_mass_method = ymeth, prey_mass_basis = ybas,
  prey_stage = ifelse(d$res.lifestage %in% c("adult", "juvenile", "larvae", "egg", "immature", "neonate"), d$res.lifestage, ""),
  pred_stage = ifelse(d$con.lifestage %in% c("adult", "juvenile", "larvae", "egg", "immature", "neonate"), d$con.lifestage, ""),
  interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("GATEWAy 1.0 autoID %s: %s; web: %s; %s", d$autoID, d$link.citation, gsub("\\s+", " ", substr(d$foodweb.name, 1, 90)), ifelse(loc == "", "no locality", loc)),
  notes = paste0("food-web link, not an individual pair; link methodology: ", ifelse(meth == "", "not given", meth), "; interaction classification ", ifelse(d$interaction.classification == "", "not given", d$interaction.classification),
                 ifelse(grepl("extrapolated", meth), "; link extrapolated from similar taxa, not recorded for this species", ""), "; resource as given: ", d$res.taxonomy))
## prey_mass_lo/hi without a point are not used; point masses only
src <- rbind(
  mk("sources", source_id = "GATEWAy_1.0", citation = "Brose U, Archambault P, Barnes AD, et al. (2019) Predator traits determine food-web architecture across ecosystems. Nature Ecology & Evolution 3:919-927 (data: GATEWAy v1.0, iDiv Data Repository 10.25829/iDiv.283-3-756, CC BY 4.0)",
     doi = "10.1038/s41559-019-0899-x", file_name = "GATEWAy_v1.0_iDiv283_v3.zip; Brose_etal_2019_NatEcolEvol_predator_traits_food_webs.pdf", data_type = "compilation_database",
     extracted_from = paste0(ZIP, " (all webs)"), notes = "Phase 4b. Links with tetrapod consumers only."),
  mk("sources", source_id = unique(sid), citation = paste0(ucit[match(unique(sid), unname(sidmap[ucit]))], " (food-web study as cited in GATEWAy link.citation; full reference in the GATEWAy metadata)"), data_type = "journal_article",
     extracted_from = paste0(ZIP, " (link.citation)"), compiled_in = "GATEWAy_1.0"))
log_dropped(OWNER, drop); log_input(OWNER, n_in, "links in GATEWAy 1.0")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "_GW_gw[0-9]+$")
