###################################################################
# 07_glaudas2019.R -- migrate data/structured/glaudas2019_snake_rpm.csv (Glaudas et al. 2019,
# species-mean relative prey mass, RPM = prey mass / snake mass, for 115 snakes) into db/.
# Decision 3: tier E, excluded from individual-level fits, used in species-level analyses.
# Predator mass: Feldman & Meiri 2013 Table 1 (family row; Colubrinae, Dipsadinae, Natricinae and
# Grayiinae use their Colubridae row; families without a row use 'All') applied to the species-mean
# SVL in the table, so it is the mass of a snake of mean SVL, not a mean mass.
# Prey mass: ratio_derived = RPM_mean x predator mass; interval = RPM_min and RPM_max x predator mass.
# Prey taxon: Vertebrata_pooled when the diet has no invertebrates, else Animalia_pooled.
# Owns: sources with extracted_from starting "data/structured/glaudas2019_snake_rpm.csv".
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "07_glaudas2019"; OWNS <- "data/structured/glaudas2019_snake_rpm.csv"
g <- read.csv(file.path(ROOT, "data", "structured", "glaudas2019_snake_rpm.csv"), colClasses = "character", na.strings = character(0))
g$k <- seq_len(nrow(g)) + 1L
SID <- "Glaudas2019"
src <- mk("sources", source_id = SID, citation = "Glaudas X, Glennon KL, Martins M, Luiselli L, Fearn S, Trembath DF, Jelic D, Alexander GJ (2019) Foraging mode, relative prey size and diet breadth: a phylogenetically explicit analysis of snake feeding ecology. Journal of Animal Ecology 88(5):757-767",
  doi = "10.1111/1365-2656.12972", file_name = "Glaudas_etal_2019_JAnimEcol_snake_relative_prey_mass.pdf; Archived_data.xlsx", data_type = "dataset",
  extracted_from = "data/structured/glaudas2019_snake_rpm.csv (from the authors' Archived_data.xlsx)", notes = "RPM per species from museum dissection or field regurgitation and from 47 published studies (Methods 2.1)")
FM <- c(Colubrinae = "Colubridae", Dipsadinae = "Colubridae", Natricinae = "Colubridae", Grayiinae = "Colubridae", Colubridae = "Colubridae",
        Elapidae = "Elapidae", Homalopsidae = "Homalopsidae", Lamprophiidae = "Lamprophiidae", Pythonidae = "Pythonidae", Viperidae = "Viperidae", Boidae = "Boidae")
fam <- ifelse(g$family %in% names(FM), FM[g$family], "All")
mid <- paste0("FeldmanMeiri2013_T1_", fam, "_SVL")
svl_mm <- num(g$svl_cm) * 10
pm <- eq_mass(mid, svl_mm)
r0 <- num(g$rpm_mean); rl <- num(g$rpm_min); rh <- num(g$rpm_max)
okint <- is.finite(rl) & is.finite(rh) & rl > 0 & rl <= r0 & r0 <= rh
qm <- r0 * pm
inv <- num(g$prop_invertebrates); prey_tax <- ifelse(is.finite(inv) & inv == 0, "Vertebrata_pooled", "Animalia_pooled")
pid <- paste0(SID, "_", make_id(g$species))
pairs <- mk("pairs", pair_id = pid, source_id = SID, pred_taxon_id = make_id(g$species), prey_taxon_id = prey_tax,
  observation_type = ifelse(g$method == "REGURGITATION", "stomach_content", "stomach_content"),
  pair_resolution = tier_of("regressed_from_length", "ratio_derived"), individual_pair = "FALSE",
  pred_mass_g = fnum(pm), pred_mass_method = "regressed_from_length", pred_mass_method_id = mid,
  pred_mass_basis = sprintf("species-mean SVL %s mm (svl_cm %s; mean of the snakes that contained prey, or the published species mean, Methods 2.1); mass at mean SVL, not a mean mass; %s row of Feldman & Meiri 2013 Table 1%s",
    fnum(svl_mm), g$svl_cm, fam, ifelse(fam == "Colubridae" & g$family != "Colubridae", paste0(" (Glaudas family '", g$family, "')"), ifelse(fam == "All", paste0(" (no row for ", g$family, ")"), ""))),
  prey_mass_g = fnum(qm), prey_mass_lo_g = fnum(ifelse(okint, rl * pm, NA)), prey_mass_hi_g = fnum(ifelse(okint, rh * pm, NA)),
  prey_mass_method = "ratio_derived", prey_mass_method_id = "Glaudas2019_RPM",
  prey_mass_basis = sprintf("RPM mean %s x predator mass (RPM min %s, max %s, n = %s prey; %s)%s", g$rpm_mean, g$rpm_min, g$rpm_max, g$n_rpm, g$rpm_reference,
    ifelse(okint, "; interval = RPM min and max x predator mass", "; no interval (min or max missing, zero or inconsistent)")),
  n_items = ifelse(is.finite(num(g$n_rpm)) & num(g$n_rpm) >= 1, as.character(floor(num(g$n_rpm))), ""), interaction_outcome = "consumed", is_fossil = "FALSE",
  quote_or_locator = sprintf("Glaudas et al. 2019 Archived_data.xlsx: %s (%s), method %s; glaudas2019_snake_rpm.csv line %d", g$species, g$family, g$method, g$k),
  notes = sprintf("species-mean ratio data (decision 3); method %s = %s; diet n = %s, proportion invertebrates %s; foraging mode %s",
    g$method, ifelse(g$method == "REGURGITATION", "regurgitation induced by palpation (Methods 2.1), counted as stomach_content per SCHEMA.md", "dissection of museum specimens"),
    g$n_diet, g$prop_invertebrates, g$foraging_mode))
meas <- mk("measurements", measurement_id = paste0(pid, "_SVL"), source_id = SID, pair_id = pid, taxon_id = make_id(g$species), role = "predator", element = "body",
  dimension = "SVL (species mean)", value = g$svl_cm, unit = "cm", value_type = "derived", locator = sprintf("Archived_data.xlsx, %s, column SVL", g$species))
taxa <- rbind(taxon_row(make_id(g$species), g$species, "species", cls = "Squamata", fam = ifelse(g$family %in% c("Colubrinae", "Dipsadinae", "Natricinae", "Grayiinae"), "Colubridae", g$family),
                        clade = "Serpentes", vert = TRUE, notes = ifelse(g$family %in% c("Colubrinae", "Dipsadinae", "Natricinae", "Grayiinae"), paste0("subfamily ", g$family, " (Glaudas et al. 2019)"), "")),
  taxon_row("Vertebrata_pooled", "Vertebrata pooled", "unranked_pool", vert = TRUE, notes = "pooled diet of several vertebrate prey taxa"),
  taxon_row("Animalia_pooled", "Animalia pooled", "unranked_pool", notes = "pooled diet of many prey taxa (mean prey mass reported over the whole diet)"))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
log_dropped(OWNER, data.frame(source_file = character(0), source_row = character(0), reason = character(0), detail = character(0)))
log_input(OWNER, nrow(g), "rows of glaudas2019_snake_rpm.csv (one per snake species)")
commit(OWNER, OWNS, src, pairs, meas, taxa, methods_rows(c(mid, "Glaudas2019_RPM")), pair_tag = "^Glaudas2019_")
