###################################################################
# 03_extant_new_observations.R -- migrate data/extant_new_observations.csv (251 hand-extracted
# rows) into db/. Every coding decision is in evidence/extant_new_obs_decisions.csv (ref_id
# ranges, applied in order, later lines override earlier ones field by field), each with the
# source wording it rests on. Masses:
#   staged   = keep the staged value, method as decided
#   avonet   = re-read the AVONET1_BirdLife species mean the staged note cites (staged values were rounded)
#   svl:x / tl:x / oliveira / poulin = recompute from the recorded length with a registered
#              equation (Meiri 2010 lizards, Feldman & Meiri 2013 snakes), replacing the staged
#              uncited "1.5e-5 * SVL^3" values, as SCHEMA.md asks for the Poulin example
#   classmid = Chabreck & Joanen 1979 on the size-class midpoint TL (refs 230-251)
#   none     = no mass (lengths go to measurements.csv)
# Owns: sources with extracted_from starting "data/extant_new_observations.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "03_extant_new_observations"; OWNS <- "data/extant_new_observations.csv"

en <- read.csv(file.path(ROOT, "data", "extant_new_observations.csv"), colClasses = "character", na.strings = character(0))
DEC <- read.csv(file.path(EVID, "extant_new_obs_decisions.csv"), colClasses = "character", na.strings = character(0))
S2  <- read.csv(file.path(EVID, "oliveira2025_tableS2.csv"), colClasses = "character", check.names = FALSE)
sq  <- SQUAMATES(); ref <- num(en$ref_id); n <- nrow(en)

## ---------------- apply decisions ----------------
F <- setdiff(names(DEC), c("ref_from", "ref_to", "evidence"))
D <- as.data.frame(matrix("", n, length(F), dimnames = list(NULL, F)), stringsAsFactors = FALSE); D$evidence <- ""
for (k in seq_len(nrow(DEC))) {
  hit <- ref >= num(DEC$ref_from[k]) & ref <= num(DEC$ref_to[k])
  for (cl in F) if (!blank(DEC[[cl]][k])) D[[cl]][hit] <- DEC[[cl]][k]
  if (!blank(DEC$evidence[k])) D$evidence[hit] <- DEC$evidence[k]
}
D$source_id[ref %in% 207:218] <- "Delany1999"; D$source_id[ref %in% 219:226] <- "Gabrey2010"
if (any(blank(D$observation_type))) stop("rows without an observation type: ", paste(en$ref_id[blank(D$observation_type)], collapse = ","))

## ---------------- Oliveira 2025: link each staged row to its Table S2 record ----------------
oli <- ref %in% 133:200
psvl <- num(sub(".*predSVL ([0-9.]+).*", "\\1", en$notes)); qsvl <- num(sub(".*preySVL ([0-9.]+).*", "\\1", en$notes))
k_en <- paste(en$pred_genus, en$pred_species, en$prey_genus, en$prey_species, psvl, qsvl)
k_s2 <- paste(S2$Predator, S2$Prey, num(S2[["Predator size(mm)"]]), num(S2[["Prey size(mm)"]]))
j <- ifelse(oli, match(k_en, k_s2), NA)
if (any(oli & is.na(j))) stop("Oliveira rows not found in Table S2: ", paste(en$ref_id[oli & is.na(j)], collapse = ","))
s2 <- S2[j, ]
ref_au <- make_id(gsub("[ '-]", "", sub(",.*$", "", s2$Reference))); ref_yr <- ifelse(grepl("(19|20)[0-9]{2}", s2$Reference), sub("^.*?((19|20)[0-9]{2}).*$", "\\1", s2$Reference, perl = TRUE), "nd")
orig <- unique(data.frame(cit = s2$Reference[oli], base = make_id(paste0(ref_au[oli], ref_yr[oli])), stringsAsFactors = FALSE))
orig$sid <- ave(orig$base, orig$base, FUN = function(v) if (length(v) == 1) v else paste0(v, letters[seq_along(v)]))
D$source_id[oli] <- orig$sid[match(s2$Reference[oli], orig$cit)]
D$prey_stage[oli] <- tolower(s2[["Prey age"]][oli])
D$pred_specimen[oli] <- sprintf("%s SVL %s mm eating %s SVL %s mm (%s, Oliveira et al. 2025 Table S2 row %s)", s2$Predator[oli], s2[["Predator size(mm)"]][oli],
                                s2$Prey[oli], s2[["Prey size(mm)"]][oli], tolower(s2$Interaction[oli]), s2$table_row[oli])

## ---------------- masses ----------------
av <- read.csv(file.path(ROOT, "data", "AVONET", "TraitData", "AVONET1_BirdLife.csv"), check.names = FALSE)
avm <- function(sp) av$Mass[match(trimws(sp), av$Species1)]
pbin <- trimws(paste(en$pred_genus, sub("_.*", "", en$pred_species))); qbin <- trimws(paste(en$prey_genus, en$prey_species))
eq_of <- function(genus) sq$mass_eq[match(genus, sq$genus)]
PM <- num(en$pred_mass_g); QM <- num(en$prey_mass_g); PMI <- PQI <- rep("", n); QLO <- QHI <- rep(NA_real_, n)
PB <- D$pred_basis; QB <- D$prey_basis; pmeth <- D$pred_method; qmeth <- D$prey_method
meas <- list(); addm <- function(i, role, tax, el, dim, val, unit, vt, loc) meas[[length(meas) + 1]] <<- mk("measurements",
  measurement_id = paste0(D$source_id[i], "_ref", en$ref_id[i], "_", role, "_", make_id(dim)), source_id = D$source_id[i], pair_id = paste0(D$source_id[i], "_ref", en$ref_id[i]),
  specimen = D$pred_specimen[i], taxon_id = tax, role = role, element = el, dimension = dim, value = fnum(val), unit = unit, value_type = vt, locator = loc)
pred_id <- make_id(pbin); pred_id[ref %in% 15] <- "Canis_lupus_familiaris"
prey_id <- make_id(ifelse(blank(en$prey_genus) | tolower(en$prey_genus) == "indet", en$prey_class, ifelse(blank(en$prey_species) | en$prey_species %in% c("indet", "sp", "spp"), en$prey_genus, qbin)))
prey_id <- sub("_(sp|spp|indet|sp_juv|egg|nestling|juvenile)$", "", prey_id)
prey_id[!blank(D$prey_taxon)] <- D$prey_taxon[!blank(D$prey_taxon)]
pred_from_len <- rep(FALSE, n); prey_from_len <- rep(FALSE, n)
for (i in seq_len(n)) {
  pr <- D$pred_rule[i]; yr <- D$prey_rule[i]
  ## predator
  if (pr == "none") { PM[i] <- NA; pmeth[i] <- "" }
  else if (pr == "avonet") { v <- avm(pbin[i]); if (is.finite(v)) PM[i] <- v else PB[i] <- paste(PB[i], "(name not in AVONET1_BirdLife; staged value kept)") }
  else if (startsWith(pr, "svl:")) { L <- num(sub("svl:", "", pr)); PMI[i] <- eq_of(en$pred_genus[i]); PM[i] <- eq_mass(PMI[i], L); pred_from_len[i] <- TRUE
    addm(i, "predator", pred_id[i], "body", "SVL", L, "mm", "measured", "staged pred_meas_note / notes") }
  else if (pr == "oliveira") { L <- num(s2[["Predator size(mm)"]][i]); e <- eq_of(en$pred_genus[i]); pred_from_len[i] <- TRUE
    addm(i, "predator", pred_id[i], "body", "SVL", L, "mm", "measured", sprintf("Oliveira et al. 2025 Table S2 row %s, 'Predator size(mm)'", s2$table_row[i]))
    if (blank(e)) { PM[i] <- NA; pmeth[i] <- "" } else { PMI[i] <- e; PM[i] <- eq_mass(e, L); PB[i] <- sprintf("SVL %s mm of this lizard (Table S2)", fnum(L)) } }
  else if (pr == "classmid") { L <- num(sub(".*midpoint ([0-9.]+) cm.*", "\\1", en$pred_meas_note[i])); PMI[i] <- "ChabreckJoanen1979_Fig2_classmid"; PM[i] <- eq_mass(PMI[i], L * 10)
    PB[i] <- sprintf("size class %s; midpoint TL %s cm converted with Chabreck & Joanen 1979%s", sub("^([a-z]+ size class, TL [0-9-]+ cm).*", "\\1", en$pred_meas_note[i]), fnum(L), if (L < 100) "; below the 1 m TL the authors give as the reliable range" else "") }
  ## prey
  if (yr == "none") { QM[i] <- NA; qmeth[i] <- "" }
  else if (yr == "avonet") { v <- avm(qbin[i]); if (is.finite(v)) QM[i] <- v else QB[i] <- paste(QB[i], "(name not in AVONET1_BirdLife; staged value kept)") }
  else if (yr == "avonet_third") { v <- avm(qbin[i]); QM[i] <- v / 3 }
  else if (startsWith(yr, "svl:")) { L <- num(sub("svl:", "", yr)); PQI[i] <- eq_of(en$prey_genus[i]); QM[i] <- eq_mass(PQI[i], L); prey_from_len[i] <- TRUE
    addm(i, "prey", prey_id[i], "body", "SVL", L, "mm", "measured", "staged notes") }
  else if (startsWith(yr, "tl:")) { L <- num(sub("tl:", "", yr)); PQI[i] <- "FeldmanMeiri2013_T1_Colubridae_TL"; QM[i] <- eq_mass(PQI[i], L); prey_from_len[i] <- TRUE
    addm(i, "prey", prey_id[i], "body", "total length", L, "mm", "measured", "staged notes (48 cm TL)") }
  else if (yr == "interval") { QLO[i] <- 700; QHI[i] <- 1100 }
  else if (yr == "oliveira") { L <- num(s2[["Prey size(mm)"]][i]); e <- eq_of(en$prey_genus[i]); prey_from_len[i] <- TRUE
    addm(i, "prey", prey_id[i], "body", "SVL", L, "mm", "measured", sprintf("Oliveira et al. 2025 Table S2 row %s, 'Prey size(mm)'", s2$table_row[i]))
    if (blank(e)) { QM[i] <- NA; qmeth[i] <- "" } else { PQI[i] <- e; QM[i] <- eq_mass(e, L); QB[i] <- sprintf("SVL %s mm of this prey (Table S2)", fnum(L)) } }
  else if (yr == "poulin") {
    if (en$prey_genus[i] == "Anolis") { PQI[i] <- "Meiri2010_T3_Polychrotidae"; QM[i] <- eq_mass(PQI[i], 33.5); qmeth[i] <- "regressed_from_length"
      QB[i] <- "mean SVL 33.5 mm of Anolis in diet samples, pooled across all bird species (Poulin et al. 2001: 'averaged 33.5 mm SVL, ranging from 19 to 44 mm (n = 31)'); not specific to this predator, so the tier treats it as an average"
      addm(i, "prey", prey_id[i], "body", "SVL", 33.5, "mm", "measured", "Poulin et al. 2001: Anolis in diet samples averaged 33.5 mm SVL (n = 31), pooled over predators") }
    else { qmeth[i] <- "unknown"; QB[i] <- "staged 0.3 g from mean SVL 14.5 mm by an unrecorded equation; no frog length-mass equation is registered"
      addm(i, "prey", prey_id[i], "body", "SVL", 14.5, "mm", "measured", "Poulin et al. 2001: Eleutherodactylus in diet samples averaged 14.5 mm SVL, pooled over predators") } }
}
## lengths recorded without a mass
for (i in which(ref %in% 1:2)) addm(i, "predator", pred_id[i], "body", "total length", 2850, "mm", "approximate", "staged pred_meas_note 'total_length_~9-10_ft_=_2700-3000mm' (midpoint)")
for (i in which(ref == 96)) addm(i, "prey", prey_id[i], "stomach contents (2 prey)", "volume", 8.5, "cm3", "measured", "staged notes: 2 juvenile Sceloporus, 8500 mm3 total")
for (i in which(ref == 206)) { addm(i, "predator", pred_id[i], "body", "SVL", 500, "mm", "measured", "Dutra-Araujo et al. 2017: 50 cm snout-vent length"); addm(i, "predator", pred_id[i], "body", "total length", 950, "mm", "measured", "95 cm total length") }
for (i in which(ref %in% 227:228)) addm(i, "predator", pred_id[i], "body", "total length", num(sub("TL ~([0-9]+)mm.*", "\\1", en$pred_meas_note[i])), "mm", "approximate", paste("staged pred_meas_note", en$pred_meas_note[i]))
pmeth[!is.finite(PM)] <- ""; qmeth[!is.finite(QM) & !is.finite(QLO)] <- ""
PB[pmeth == ""] <- ""; QB[qmeth == ""] <- ""

## ---------------- tier, individual, specimen ----------------
agg <- D$aggregate == "TRUE"
pm_t <- ifelse(D$aggregate == "pred_avg" & pmeth %in% FROM_ANIMAL, "species_average_literature", pmeth)
qm_t <- ifelse(en$prey_genus == "Anolis" & ref %in% 102:132, "species_average_literature", qmeth)
tier <- tier_of(pm_t, qm_t, pred_none_from_animal = pred_from_len | ref %in% c(1, 2), prey_none_from_animal = prey_from_len, aggregate = agg)
indiv <- D$individual_pair == "TRUE"
auto <- indiv & blank(D$pred_specimen)
D$pred_specimen[auto] <- sprintf("%s %s %s eating %s (%s, staged ref_id %s)", en$pred_genus[auto], en$pred_species[auto],
  ifelse(blank(en$pred_mass_g[auto]), "", paste0(en$pred_mass_g[auto], " g")), qbin[auto], D$source_id[auto], en$ref_id[auto])
D$pred_specimen[!indiv] <- ""

## ---------------- drops ----------------
drop <- !blank(D$drop)
dropped <- data.frame(source_file = "data/extant_new_observations.csv", source_row = paste0("ref_id ", en$ref_id[drop]), reason = D$drop[drop],
                      detail = sprintf("%s; %s %s; obs %s", en$source[drop], en$pred_genus[drop], en$pred_species[drop], en$obs_type[drop]))
keep <- !drop

## ---------------- n_items ----------------
cnt <- num(en$count)
nit <- ifelse(D$n_items == "blank", NA, ifelse(is.finite(cnt) & cnt >= 1, floor(cnt), NA))

## ---------------- pairs ----------------
pid <- paste0(D$source_id, "_ref", en$ref_id)
notes <- paste0("staged obs_type '", en$obs_type, "'; staged certainty ", en$certainty, "; staged notes: ", one_line(en$notes),
  ifelse(blank(en$pred_meas_note), "", paste0("; pred_meas_note: ", one_line(en$pred_meas_note))))
pairs <- mk("pairs", pair_id = pid, source_id = D$source_id, pred_taxon_id = pred_id, prey_taxon_id = prey_id, pred_specimen = D$pred_specimen,
  observation_type = D$observation_type, pair_resolution = tier, individual_pair = ifelse(indiv, "TRUE", "FALSE"),
  pred_mass_g = fnum(PM), pred_mass_method = pmeth, pred_mass_method_id = ifelse(pmeth == "", "", PMI), pred_mass_basis = PB,
  prey_mass_g = fnum(QM), prey_mass_lo_g = fnum(QLO), prey_mass_hi_g = fnum(QHI), prey_mass_method = qmeth,
  prey_mass_method_id = ifelse(qmeth == "", "", PQI), prey_mass_basis = ifelse(is.finite(QLO), paste0(QB, "; interval = assigned adult range"), QB),
  prey_stage = ifelse(blank(D$prey_stage), ifelse(blank(en$prey_stage), "", en$prey_stage), D$prey_stage),
  n_items = ifelse(is.na(nit), "", as.character(nit)), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = ifelse(blank(D$evidence), paste0("data/extant_new_observations.csv ref_id ", en$ref_id), paste0(D$evidence, " [data/extant_new_observations.csv ref_id ", en$ref_id, "]")),
  notes = notes)[keep, ]
M <- if (length(meas)) do.call(rbind, meas) else NULL
M <- M[M$pair_id %in% pairs$pair_id, ]
M$measurement_id <- make.unique(M$measurement_id, sep = "_")

## ---------------- sources ----------------
SRC <- read.csv(text = '
source_id,citation,file_name,data_type
Dolia2018,"Dolia J (2018) King Cobra (Ophiophagus hannah) diet note. Herpetology Notes 11:217 (staged as Dolia_2018_HerpetolNotes_11_217; full title not recorded); also reports Bhaisare et al. 2010 (radio-tracked King Cobra, Agumbe)",,journal_article
Lu2023,"Lu Q et al. (2023) Current Biology 33:647 (staged as Lu_et_al_2023_CurrBiol_33_647; carnivore faecal DNA metabarcoding, Qinghai-Tibet Plateau)",,journal_article
CapizziLuiselli1997,"Capizzi D, Luiselli L (1997) The diet of the four-lined snake (Elaphe quatuorlineata) in Mediterranean central Italy. Herpetological Journal 7:1-5","new_HJ07-1_pp1-5 The diet of the four-lined snake (Elaphe quatuorlineata) in Mediterranean central Italy.pdf",journal_article
Capula1997,"Capula M et al. (1997) Notes on the food habits of Coluber hippocrepis nigrescens from Pantelleria Island: a snake that feeds on both carrion and living prey. Herpetological Journal 7:67-70","new_HJ07-2_pp67-70 Notes on the food habits of Coluber hippocrepis nigrescens from Pantelleria Island a snake that feeds on both carrion and living prey.pdf",journal_article
Platt2013,"Platt SG et al. (2013) Journal of Herpetology 47:1 (Crocodylus acutus stomach contents; staged as Platt_et_al_2013_JHerpetol_47_1)",new_platt2013.pdf,journal_article
RodriguezRobles2002,"Rodriguez-Robles JA (2002) Biological Journal of the Linnean Society 77:165 (gopher snake Pituophis catenifer diet review)",new_rodriguez-robles_2002.pdf,journal_article
Wiseman2019,"Wiseman KD et al. (2019) Herpetological Conservation and Biology 14:1 (Lampropeltis californiae feeding ecology)",new_KingsnakeFeedingEcologyWiseman_etal_2019.pdf,journal_article
HoustonShine1993,"Houston D, Shine R (1993) Journal of Animal Ecology 62:737 (Acrochordus arafurae)",new_113sexualdimorphism.pdf,journal_article
GonzalezSolorzano2024,"Gonzalez-Solorzano et al. (2024) Animals 14:2610 (hatchling Crocodylus moreletii diet)",new_animals-14-02610.pdf,journal_article
WallaceLeslie2008,"Wallace KM, Leslie AJ (2008) Journal of Herpetology 42:361 (Crocodylus niloticus diet, Okavango)",new_Diet_2008.pdf,journal_article
Bontemps2016,"Bontemps DR et al. (2016) Management of Biological Invasions 7:287 (Caiman crocodilus, Puerto Rico)",new_MBI_2016_Bontemps_etal.pdf,journal_article
Boyle1917,"Boyle (1917), seriema gizzard record; full reference not recorded in the staged table",,unpublished
Elmore2024,"Elmore et al. (2024) Journal of Heredity 115:432 (barn owl diet from DNA, Hawaii)",esad068.pdf,journal_article
Engler2022,"Engler M et al. (2022) Bird Study (goshawk prey in urban and rural Germany)",Engleretal.2022.pdf,journal_article
Greer1966,"Greer JK, Bullock DS (1966) Notes on stomach contents and weights of some Chilean birds of prey. Auk 83:308-309",greer1966.pdf,journal_article
Weavers1989,"Weavers BW (1989) Australian Zoologist 25:83 (diet of the lace monitor Varanus varius)",az_1989_007.pdf,journal_article
Auffenberg1981,"Auffenberg W (1981) The Behavioral Ecology of the Komodo Monitor. University Presses of Florida (book; not on disk)",,book_or_chapter
Jessop2010,"Jessop T et al. (2010) Biawak 4(2):59 (lace monitor predation on ringtail possums)",Vol4_No2_Jessop_et_al.pdf,journal_article
vanWinkel2012,"van Winkel D, Ji W (2012) New Zealand Journal of Zoology 39:201 (kingfisher pellets, Oligosoma)",vanWinkel_Ji_2012_NZkingfisher.pdf,journal_article
PaulissenMontgomery2024,"Paulissen MA, Montgomery CE, Southwestern Naturalist (collared lizard saurophagy, Colorado; staged as Paulissen_Montgomery_SWNat_collared_Colorado_2024)",Paulissen_Montgomery_collared_Colorado.pdf,journal_article
Passos2016,"Passos DC et al. (2016) Biota Neotropica 16:e20150062 (Tropidurus saurophagy)",Passos_2016_Tropidurus_saurophagy.pdf,journal_article
BestPfaffenberger1987,"Best TL, Pfaffenberger GS (1987) Southwestern Naturalist 32:415 (collared lizard diet)",Best_Pfaffenberger_1987_collared.pdf,journal_article
Poulin2001,"Poulin B et al. (2001) Avian predation upon lizards and frogs in a neotropical forest understorey. Journal of Tropical Ecology 17:21",Poulin_etal_2001_JTropEcol_avian_predation_herps.pdf,journal_article
Oliveira2025,"Oliveira PMA, Souza UF, de Sousa JD, de Mello AVA, da Silva NVN, Lima JHA (2025) Unraveling patterns and drivers of saurophagy in South American lizards. Scientific Reports, Supplementary Table S2",41598_2025_89810_MOESM1_ESM.docx,compilation_database
MartinLopez1996,"Martin J, Lopez P (1996), avian predation on Lacerta lepida (compiled raptor diet percentages)",Martin_Lopez_1996_avian_predation_Lacerta_lepida.pdf,journal_article
DutraAraujo2017,"Dutra-Araujo D et al. (2017) Herpetology Notes 10:169 (Paleosuchus palpebrosus eating Atractus torquatus)",27206.pdf,journal_article
Delany1999,"Delany MF et al. (1999) alligator diet in four Florida lakes, Proceedings of the Annual Conference of the Southeastern Association of Fish and Wildlife Agencies, pp. 375-389",DELANY-375-389.pdf,journal_article
Gabrey2010,"Gabrey SW (2010) Demographic and geographic variation in food habits of American alligators in Louisiana. Herpetological Conservation and Biology 5:241",Demographic_and_geographic_variation_in_food_habit.pdf,journal_article
Kasperoviczus2015,"Kasperoviczus KN et al. (2015) Herpetology Notes 8:21 (tegu predation on snakes)",10605-34203-1-SM.pdf,journal_article
Nifong2016,"Nifong JC (2016) Bulletin of the Florida Museum of Natural History 54(2) (alligator diet, Guana)",Vol54No2_archival.pdf,journal_article
', colClasses = "character")
SRC$doi <- ifelse(SRC$source_id == "Oliveira2025", "10.1038/s41598-025-89810-7", "")
used_sid <- unique(pairs$source_id)
src <- mk("sources", source_id = SRC$source_id, citation = SRC$citation, doi = SRC$doi, file_name = SRC$file_name, data_type = SRC$data_type,
          extracted_from = paste0("data/extant_new_observations.csv (rows with source ", SRC$source_id, ")"), notes = "citation assembled from the staged source string and file; not checked against a reference manager")
src <- src[src$source_id %in% c(used_sid, "Oliveira2025") & !src$source_id %in% MERGED()$extant_source_id, ]   # merged publications are written by 00
osrc <- mk("sources", source_id = orig$sid, citation = orig$cit, data_type = "journal_article",
           extracted_from = "data/extant_new_observations.csv (ref_id 133-200, via Oliveira et al. 2025 Table S2)", compiled_in = "Oliveira2025",
           notes = "reference string as printed in Oliveira et al. 2025 Table S2; original not on disk")
osrc <- osrc[osrc$source_id %in% pairs$source_id & !osrc$source_id %in% src$source_id, ]   # a Table S2 reference already extracted directly keeps its direct row
src <- rbind(src, osrc)
miss <- setdiff(used_sid, c(src$source_id, MERGED()$merged_source_id)); if (length(miss)) stop("no source row for: ", paste(miss, collapse = ", "))

## ---------------- taxa ----------------
pfam <- sq$family[match(en$pred_genus, sq$genus)]
tx_pred <- taxon_row(pred_id, pbin, "species", cls = ifelse(en$pred_class == "Reptilia" & en$pred_order == "Crocodylia", "Crocodylia", en$pred_class),
  ord = en$pred_order, fam = ifelse(is.na(pfam), "", pfam), clade = pred_clade(ifelse(en$pred_order == "Crocodylia", "Crocodylia", en$pred_class), en$pred_genus, sq), vert = TRUE)
qcl <- ifelse(en$prey_class == "Reptilia" & en$prey_order %in% c("Squamata", "Testudines", "Crocodylia"), en$prey_order, en$prey_class)
tx_prey <- taxon_row(prey_id, ifelse(prey_id == "Animalia_pooled", "Animalia pooled", prey_id), cls = qcl, ord = ifelse(en$prey_class == "Reptilia", "", en$prey_order),
  fam = ifelse(is.na(en$prey_family), "", en$prey_family), vert = ifelse(prey_id == "Animalia_pooled", NA, prey_type(en$prey_class) == "vert"),
  notes = ifelse(prey_id == "Animalia_pooled", "pooled diet of many prey taxa (mean prey mass reported over the whole sample)", ""))
taxa <- rbind(tx_pred[keep, ], tx_prey[keep, ]); taxa <- taxa[!duplicated(taxa$taxon_id), ]

log_dropped(OWNER, dropped)
unk <- pairs[pairs$pred_mass_method == "unknown" | pairs$prey_mass_method == "unknown", c("pair_id", "source_id", "pred_taxon_id", "prey_taxon_id", "pred_mass_method", "prey_mass_method", "pred_mass_basis", "prey_mass_basis")]
write.csv(unk, file.path(MIG, paste0("unknown_", OWNER, ".csv")), row.names = FALSE)
log_input(OWNER, n, "rows of extant_new_observations.csv")
commit(OWNER, OWNS, src, pairs, M, taxa, methods_rows(c(pairs$pred_mass_method_id, pairs$prey_mass_method_id)), pair_tag = "_ref[0-9]+$")
