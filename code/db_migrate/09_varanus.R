###################################################################
# 09_varanus.R -- migrate the two Varanus summary tables into db/.
#  * data/structured/varanus_niloticus_summary.csv (Mazzotti et al. 2020) points to its prey list,
#    pdfs/extant_to_ingest/ai_found/nilemonitor/mazzotti2020_table2_prey_items.csv (88 prey taxa,
#    n GI tracts each): one population-level link per prey taxon, no masses (occurrence data).
#  * data/structured/varanus_varius_summary.csv (Weavers 1989, merged source Weavers1989): the seven prey groups listed in
#    'top_prey_occurrence_allsites': one link each, no masses. The one individual record in that
#    paper (1.2 kg monitor, 500 g rabbit) is migrated by 03 from extant_new_observations.csv.
# Predator mass ranges and Guarino 2002 masses in these tables are not pairs and are not migrated.
# Owns: sources with extracted_from starting "data/structured/varanus_".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "09_varanus"; OWNS <- "data/structured/varanus_"
nil <- read.csv(file.path(ROOT, "data", "structured", "varanus_niloticus_summary.csv"), colClasses = "character")
var <- read.csv(file.path(ROOT, "data", "structured", "varanus_varius_summary.csv"), colClasses = "character")
t2f <- file.path(ROOT, "pdfs", "extant_to_ingest", "ai_found", "nilemonitor", "mazzotti2020_table2_prey_items.csv")
t2 <- read.csv(t2f, colClasses = "character", na.strings = character(0)); t2$k <- seq_len(nrow(t2)) + 1L
src <- rbind(
  mk("sources", source_id = "Mazzotti2020", citation = "Mazzotti FJ et al. (2020) Journal of Herpetology 54:189 (diet of introduced Nile monitors, Varanus niloticus, in Palm Beach and Broward counties, Florida)",
     doi = "10.1670/18-115", file_name = "Mazzottietal2020.pdf; mazzotti2020_table2_prey_items.csv", data_type = "journal_article",
     extracted_from = "data/structured/varanus_niloticus_summary.csv (prey list in pdfs/extant_to_ingest/ai_found/nilemonitor/mazzotti2020_table2_prey_items.csv)",
     notes = "68 GI tracts, 65 with food; predator SVL 19.1-78.7 cm, mass 110-12300 g (not per prey)"))
## Nile monitor
prey_name <- ifelse(!blank(t2$species), t2$species, ifelse(!blank(t2$family), t2$family, ifelse(!blank(t2$order), t2$order, ifelse(!blank(t2$class), t2$class, "Amniota"))))
egg <- t2$prey_category == "Egg"
pn <- make_id(prey_name)
nil_pairs <- mk("pairs", pair_id = paste0("Mazzotti2020_t2r", t2$k), source_id = "Mazzotti2020", pred_taxon_id = "Varanus_niloticus", prey_taxon_id = pn,
  observation_type = "stomach_content", pair_resolution = tier_of("", ""), individual_pair = "FALSE", interaction_outcome = "consumed", is_fossil = "FALSE",
  prey_stage = ifelse(egg, "egg", ""),
  quote_or_locator = sprintf("Mazzotti et al. 2020 Table 2 (staged mazzotti2020_table2_prey_items.csv line %d): %s, in %s GI tract(s)", t2$k, prey_name, t2$n_gi_tracts),
  notes = paste0("occurrence data: present in ", t2$n_gi_tracts, " of 68 GI tracts (not an item count); prey category '", t2$prey_category, "'",
                 ifelse(t2$nonnative == "1", "; non-native prey", ""), ifelse(egg & blank(t2$class), "; egg of an unidentified amniote (no class in Table 2)", "")))
pt_n <- prey_type(t2$class)
## lace monitor
tp <- strsplit(var$value[var$metric == "top_prey_occurrence_allsites"], ";")[[1]]
lab <- trimws(sub(" *[0-9]+ *(pct)?$", "", tp)); pct <- sub(".* ([0-9]+) *(pct)?$", "\\1", tp)
TAXV <- c(Oryctolagus = "Oryctolagus", "Macropus giganteus" = "Macropus_giganteus", birds = "Aves", Wallabia = "Wallabia", "Equus(carrion)" = "Equus", Lepidoptera = "Lepidoptera", Orthoptera = "Orthoptera")
if (any(!lab %in% names(TAXV))) stop("unmapped Weavers prey: ", paste(lab[!lab %in% names(TAXV)], collapse = ", "))
carrion <- lab %in% c("Equus(carrion)", "Macropus giganteus")
## Weavers1989 is a merged source (00_merged_sources.R), shared with extant_new ref 90
var_pairs <- mk("pairs", pair_id = paste0("Weavers1989_occ_", make_id(TAXV[lab])), source_id = "Weavers1989", pred_taxon_id = "Varanus_varius", prey_taxon_id = TAXV[lab],
  observation_type = "unknown", pair_resolution = tier_of("", ""), individual_pair = "FALSE", interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("varanus_varius_summary.csv, top_prey_occurrence_allsites: '%s' (%s%% occurrence)", trimws(tp), pct),
  notes = paste0("percentage occurrence across 50 samples; Weavers pooled scats and handling-induced regurgitates ('I collected scats and regurgitated stomach contents'), so the method per prey is not separable",
                 ifelse(carrion, "; large mammals were carrion according to the paper (staged carrion_note)", "")))
pairs <- rbind(nil_pairs, var_pairs)
taxa <- rbind(taxon_row(c("Varanus_niloticus", "Varanus_varius"), c("Varanus niloticus", "Varanus varius"), "species", cls = "Squamata", fam = "Varanidae", clade = "Lacertilia", vert = TRUE),
  taxon_row(pn, prey_name, guess_rank(prey_name), cls = t2$class, ord = t2$order, fam = t2$family, vert = ifelse(is.na(pt_n), NA, pt_n == "vert")),
  taxon_row(unname(TAXV), gsub("_", " ", unname(TAXV)), guess_rank(unname(TAXV)), cls = c("Mammalia", "Mammalia", "Aves", "Mammalia", "Mammalia", "Insecta", "Insecta"),
            vert = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
log_dropped(OWNER, data.frame(source_file = character(0), source_row = character(0), reason = character(0), detail = character(0)))
log_input(OWNER, nrow(t2) + length(tp), "prey taxa in the two Varanus tables (88 Mazzotti Table 2 rows + 7 Weavers top prey)")
commit(OWNER, OWNS, src, pairs, NULL, taxa, pair_tag = "^(Mazzotti2020_t2r[0-9]+|Weavers1989_occ_.+)$")
