###################################################################
# 10_fossil_step2.R -- Phase 3 (fossil step 2): body-mass estimates for every step-1 gut entry,
# and the fossil rows of db/.
#
# Inputs (read only): data/fossil_rebuild/step1_*.csv; Benson et al. 2014 Dataset S1
# (data/mass_references/journal.pbio.1001853.s011.XLS) for reference masses. The old fossil tables
# (data/fossil_predprey.csv, fossil_ppmr_points.csv, fossil_diet_extractions.csv) are read ONLY
# at the end, to build the >25% disagreement table; nothing in db/ comes from them.
#
# Every number below is either read from a source (locator given) or computed here from a
# registered equation. Decisions and evidence: data/fossil_rebuild/step2_mass_estimates.md and
# step2_conflicts.md.
#
# Owns: sources whose extracted_from starts with "data/fossil_rebuild/". Idempotent; it does not
# touch rows owned by the Phase 2 scripts. run_all.R runs it after 00-09 (since 2026-11-12).
# Outputs besides db/: data/fossil_rebuild/step2_estimates.csv, step2_crosschecks.csv,
# step2_old_table_comparison.csv; db/excluded_fossil_records.csv.
###################################################################
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "_common.R"))
OWNER <- "10_fossil_step2"; OWNS <- "data/fossil_rebuild/"
FR <- file.path(ROOT, "data", "fossil_rebuild")
rd <- function(f) read.csv(file.path(FR, f), colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8")
GE <- rd("step1_gut_entries.csv"); PA <- rd("step1_papers.csv"); ME1 <- rd("step1_measurements.csv")

## ------------------------------------------------------------------
## Reference masses from Benson et al. 2014 Dataset S1 (read, not retyped)
## ------------------------------------------------------------------
S11f <- file.path(ROOT, "data", "mass_references", "journal.pbio.1001853.s011.XLS")
s11a <- as.data.frame(readxl::read_excel(S11f, sheet = "Full data", .name_repair = "minimal"))
s11b <- as.data.frame(readxl::read_excel(S11f, sheet = "Mass estimates", .name_repair = "minimal"))
stopifnot(identical(s11a$Taxon, s11b$Taxon))
s11 <- function(taxon, spec) {
  i <- which(s11a$Taxon == taxon & !is.na(s11a$Specimen) & s11a$Specimen == spec & !is.na(s11b[[3]]))[1]
  if (is.na(i)) stop("S11 row not found: ", taxon, " ", spec)
  list(taxon = taxon, spec = spec, g = 1000 * num(s11b[[3]][i]), note = s11b$Notes[i],
       FL = num(s11a[["Femur length - FL"]][i]), TL = num(s11a[["Tibia length (or estimate based on tibiotarsus) - TL_TLE"]][i]),
       HL = num(s11a[["Humerus length - HL"]][i]), FC = num(s11a[["Femur shaft circumference - FC"]][i]),
       FAP = num(s11a[["Femur shaft anteroposterior diameter - FAP"]][i]), FML = num(s11a[["Femur shaft mediolateral diameter - FML"]][i]))
}
R <- list(
  NIGP127587 = s11("Sinosauropteryx_prima", "NIGP 127587"),
  BSP563     = s11("Compsognathus_longipes", "BSP ASI 563"),
  CNJ79      = s11("Compsognathus_corallestris", "MNHN CNJ 79"),
  JMP        = s11("Sinocalliopteryx_gigas", "JMP-V-05-8-01"),
  CAGS301    = s11("Huaxiagnathus_orientalis", "CAGS-IG02-301"),
  MPDC10025  = s11("Velociraptor_mongoliensis", "MPD-C 100/25"),
  AMNH7223   = s11("Coelophysis_bauri", "AMNH 7223"),
  AMNH2704   = s11("Coelophysis_bauri", "AMNH 2704"),
  V13352     = s11("Microraptor_gui", "IVPP V13352"),
  CAGS20     = s11("Microraptor_zhaoianus", "CAGS 20-7-004, 001"),
  LPMB169    = s11("Anchiornis_huxleyi", "LPM-B00169"),
  Mei        = s11("Mei_long", "IVPP V12733"),
  Dilong     = s11("Dilong_paradoxus", "IVPP V14243"),
  NMC2120    = s11("Gorgosaurus_libratus", "NMC 2120"),
  TMP79      = s11("Chirostenotes_pergracilis", "TMP 79.20.1"),
  Sucho      = s11("Suchomimus_tenerensis", "MNN GDF 500"),
  Oro        = s11("Orodromeus_makelai", "MOR 473"),
  Haya       = s11("Haya_griva", "IGM 100/2015"),
  AMNH5730   = s11("Edmontosaurus_annectens", "AMNH 5730"),
  V16708     = s11("Jianchangornis_microdonta", "IVPP V16708"))
i_sc <- which(s11a$Taxon == "Scipionyx_samniticus")[1]
R$Scip <- list(taxon = "Scipionyx_samniticus", spec = "(no number; Dal Sasso & Signore 1998)", g = 1000 * num(s11b[[3]][i_sc]),
               note = s11b$Notes[i_sc], FL = num(s11a[["Femur length - FL"]][i_sc]), FAP = num(s11a[["Femur shaft anteroposterior diameter - FAP"]][i_sc]))
i_je <- which(s11a$Taxon == "Jeholosaurus_shangyuanensis")[1]
R$Jehol <- list(taxon = "Jeholosaurus_shangyuanensis", spec = s11a$Specimen[i_je], g = 1000 * num(s11b[[3]][i_je]),
                TL = num(s11a[["Tibia length (or estimate based on tibiotarsus) - TL_TLE"]][i_je]))
## check: the measured-circumference S11 masses reproduce C&E 2012 eqn 1 with the sqrt(2) biped correction
qe1_biped <- function(fc) 10^(2.749 * log10(sqrt(2) * fc) - 1.104)
stopifnot(abs(qe1_biped(R$AMNH7223$FC) / R$AMNH7223$g - 1) < 0.002, abs(qe1_biped(R$MPDC10025$FC) / R$MPDC10025$g - 1) < 0.002)

## ------------------------------------------------------------------
## Equations (coefficients transcribed from the method files; see METHODS below)
## ------------------------------------------------------------------
K_CE   <- 10^(1.96 * 0.134)                                      # 95% factor, C&E 2012 eqn 1 SEE 0.134 (Fig. 4A)
dme    <- function(m_ref, l_ref, l) m_ref * (l / l_ref)^3       # Erickson & Tumanova 2000 developmental mass extrapolation
cqe7   <- function(fc) 10^(2.754 * log10(fc) - 0.683)            # Campione et al. 2014 eqn 7, g, mm
qe1    <- function(chf) 10^(2.749 * log10(chf) - 1.104)          # Campione & Evans 2012 eqn 1, g, mm
fieldHL  <- function(x) exp(2.07 * log(x) - 2.77)                # Field et al. 2013 Table 1, ln g / ln mm
fieldFL  <- function(x) exp(2.82 * log(x) - 4.74)
fieldTaL <- function(x) exp(2.23 * log(x) - 2.87)
farlowTL <- function(tl) 1000 * 10^(3.47 * log10(tl) - 10.03)    # Farlow et al. 2005 Table 3, kg -> g, TL mm (wild)
farlowSK <- function(sk) 1000 * 10^(3.48 * log10(sk) - 6.97)     # Farlow et al. 2005 Table 3, kg -> g, skull mm (all)
K_FTL <- 10^(1.96 * 0.0553); K_FSK <- 10^(1.96 * 0.0581)
fmTL   <- function(tl, a, b) 10^(a + b * log10(tl))              # Feldman & Meiri 2013 Table 1, g, mm
meiriS <- function(svl) 10^(-4.717 + 3.027 * log10(svl))         # Meiri 2010 Table 3 Sauria, g, mm
cfFL   <- function(fl) 1000 * 10^(3.22 * log10(fl) - 6.288)      # Christiansen & Farina 2004 as printed by Therrien 2023 SI (unverified)
FR_A <- c(fusiform = 0.0137, elongated = 0.00838, shortdeep = 0.0187)   # Froese 2006 Table 1 median a3.0 (verified)
FR_P05 <- c(fusiform = 0.0061, elongated = 0.00293, shortdeep = 0.0077); FR_P95 <- c(fusiform = 0.0240, elongated = 0.0178, shortdeep = 0.0336)
froese <- function(a, Lmm) a * (Lmm / 10)^3

## ------------------------------------------------------------------
## Methods registered by this script
## ------------------------------------------------------------------
reg <- METHOD_REG()
mrow <- function(...) mk("methods", ...)
METHODS <- rbind(
  mrow(method_id = "Benson2014_S11_FCmeasured", mass_method = "regressed_from_bone",
       description = "Body mass of a dinosaur specimen as published in Benson et al. 2014 Dataset S1, where the femoral (and humeral) shaft circumference was measured",
       equation = "bipeds: log10(BM) = 2.749 * log10(sqrt(2) * C_femur) - 1.104; quadrupeds: log10(BM) = 2.749 * log10(C_humerus + C_femur) - 1.104",
       response = "body mass, g (S11 lists kg)", predictor = "minimum femoral (and humeral) shaft circumference, mm",
       taxon_scope = "non-avian dinosaurs and Mesozoic birds in Benson et al. 2014; the equation is calibrated on extant quadrupedal mammals and reptiles",
       valid_range = "not stated as a numeric range; Campione & Evans 2012 calibration sample of 245 extant mammals and non-avian reptiles (Fig. 4A)",
       reported_error = "SEE 0.134 log10 units, mean PPE 25.6% (Campione & Evans 2012 Fig. 4A); Benson et al. 2014 Table 1 caption: 'The standard error of all mass estimates is 0.135 log10(kg)'",
       method_file = "Benson_etal_2014_PLoSBiol_dinosaur_body_mass.pdf",
       locator = "S11 sheet 'Mass estimates' column 'Mass 1'; equation = Campione & Evans 2012 eqn 1 (p.15) with alpha = sqrt(2) from Campione et al. 2014 eqn 5-6 (p.915)",
       verified = "TRUE",
       notes = "Masses are read from S11, not recomputed. The equation was checked by reproducing S11: AMNH 7223 FC 49 mm -> 9.04 kg; MPD-C 100/25 FC 64 mm -> 18.84 kg (within 0.2%)."),
  mrow(method_id = "Benson2014_S11_FCestimated", mass_method = "regressed_from_bone",
       description = "Body mass as published in Benson et al. 2014 Dataset S1 where the femoral circumference was estimated from a shaft diameter ('FC estimated')",
       equation = "as Benson2014_S11_FCmeasured, after converting a femoral shaft diameter (AP or ML) to circumference by the regressions in Benson et al. 2014 Appendix S1",
       response = "body mass, g (S11 lists kg)", predictor = "femoral shaft anteroposterior or mediolateral diameter, mm (several measured from published figures)",
       taxon_scope = "as Benson2014_S11_FCmeasured",
       valid_range = "as Benson2014_S11_FCmeasured",
       reported_error = "as Benson2014_S11_FCmeasured plus the unreported error of the diameter-to-circumference step",
       method_file = "Benson_etal_2014_PLoSBiol_dinosaur_body_mass.pdf",
       locator = "S11 sheet 'Mass estimates' column 'Mass 1', Notes 'FC estimated'; conversion in Appendix S1 (not on disk)",
       verified = "FALSE",
       notes = "The diameter-to-circumference regressions are in Benson et al. 2014 Appendix S1, which is not on disk (requested). Back-calculation shows they are not C = pi*D (implied C/D about 2.67 for AP and 3.05 for ML diameters)."),
  mrow(method_id = "Campione2014_eqn7", mass_method = "regressed_from_bone",
       description = "Corrected quadrupedal equation (cQE) for non-avian bipeds",
       equation = "log10(BM) = 2.754 * log10(C_femur) - 0.683",
       response = "body mass, g", predictor = "minimum femoral shaft circumference, mm",
       taxon_scope = "non-avian bipedal tetrapods (derived from the phylogenetically corrected quadrupedal equation of Campione & Evans 2012 with alpha = sqrt(2))",
       valid_range = "not stated as a numeric range; parent equation calibrated on extant terrestrial quadrupeds (Campione et al. 2014 Table 1: Tetrapoda N = 255)",
       reported_error = "not restated for eqn 7; parent QE SEE 0.134 log10 (Campione & Evans 2012 Fig. 4A, OLS version)",
       method_file = "Campione_etal_2014_MEE_biped_limb_mass.pdf",
       locator = "eqn 7, p. 915 (rendered page; the text layer drops minus signs and decimal points)",
       verified = "TRUE",
       notes = "Units g and mm confirmed by reproducing Campione & Evans 2012 Table 6 (Iguanodon IRSNB R51, CH+CF 827.5 mm -> 8680 kg with eqn 2)."),
  mrow(method_id = "CampioneEvans2012_eqn1", mass_method = "regressed_from_bone",
       description = "Quadrupedal equation (QE, OLS) from combined humeral and femoral circumference",
       equation = "log10(BM) = 2.749 * log10(C_humerus + C_femur) - 1.104",
       response = "body mass, g", predictor = "sum of minimum humeral and femoral shaft circumferences, mm",
       taxon_scope = "terrestrial quadrupedal tetrapods (245 extant mammals and non-avian reptiles)",
       valid_range = "not stated as a numeric range; calibration sample in Fig. 4A",
       reported_error = "R2 0.988; SEE 0.134; mean PPE 25.6% (Fig. 4A)",
       method_file = "Campione_Evans_2012_BMCBiol_quadruped_limb_mass.pdf",
       locator = "eqn 1, p. 15; Fig. 4A", verified = "TRUE",
       notes = "Used here only as a cross-check (Repenomamus IVPP V13605)."),
  mrow(method_id = "ErickTum2000_DME_bone", mass_method = "regressed_from_bone",
       description = "Developmental mass extrapolation: isometric (cube) scaling of a reference specimen's mass by the ratio of a skeletal length",
       equation = "BM = BM_ref * (L / L_ref)^3",
       response = "body mass, g (unit of BM_ref)", predictor = "a limb-bone or skull length of the target and of a named reference specimen of known mass, mm",
       taxon_scope = "the authors scaled femur length within one species' growth series (Psittacosaurus mongoliensis)",
       valid_range = "assumes isometry between target and reference",
       reported_error = "none stated; each use here carries the reference mass's own interval",
       method_file = "Erickson_Tumanova_2000_ZJLS_Psittacosaurus_growth_REFERENCE.pdf",
       locator = "Methods ('we took the cube of femoral length ... multiplied the fractional values by the adult mass estimate'); Table 2",
       verified = "TRUE",
       notes = "Applying it to an element other than the femur, or across species, is an extension of the method; each pair's basis says which element and which reference."),
  mrow(method_id = "ErickTum2000_DME_bodylength", mass_method = "regressed_from_length",
       description = "Developmental mass extrapolation applied to an estimated total body length",
       equation = "BM = BM_ref * (TL / TL_ref)^3",
       response = "body mass, g (unit of BM_ref)", predictor = "total body length of the target and of a named reference specimen, mm",
       taxon_scope = "as ErickTum2000_DME_bone", valid_range = "assumes isometry; juveniles of most dinosaurs are not isometric",
       reported_error = "none stated", method_file = "Erickson_Tumanova_2000_ZJLS_Psittacosaurus_growth_REFERENCE.pdf",
       locator = "Methods; Table 2", verified = "TRUE", notes = "Extension of the method to body length."),
  mrow(method_id = "Field2013_T1_HL", mass_method = "regressed_from_bone",
       description = "Flying-bird body mass from maximum humerus length (combined dataset)",
       equation = "ln(BM) = 2.07 * ln(HL) - 2.77", response = "body mass, g", predictor = "maximum humerus length, mm",
       taxon_scope = "extant volant birds (863 specimens); the authors caution against applying crown-bird equations to distant stem taxa",
       valid_range = "not stated numerically; extant flying birds", reported_error = "R2 0.94; 95% prediction interval +/- 0.89 ln units; mean PPE 40.81%",
       method_file = "Field_etal_2013_PLoSONE_bird_skeletal_mass.pdf", locator = "Table 1, p. 6 (rendered page); natural logs (Methods p. 2); mass in g (Table 2)",
       verified = "TRUE", notes = "Field et al. find coracoid facet (HAF) best; no gut-bearing bird here has an HAF measurement. Humerus length has the lowest PPE among the lengths available."),
  mrow(method_id = "Field2013_T1_FL", mass_method = "regressed_from_bone",
       description = "Flying-bird body mass from maximum femur length", equation = "ln(BM) = 2.82 * ln(FL) - 4.74",
       response = "body mass, g", predictor = "maximum femur length, mm", taxon_scope = "extant volant birds",
       valid_range = "not stated numerically", reported_error = "R2 0.90; 95% PI +/- 1.11 ln units; mean PPE 51.01%",
       method_file = "Field_etal_2013_PLoSONE_bird_skeletal_mass.pdf", locator = "Table 1, p. 6 (rendered page)", verified = "TRUE", notes = "Cross-check only."),
  mrow(method_id = "Field2013_T1_TaL", mass_method = "regressed_from_bone",
       description = "Flying-bird body mass from maximum tarsometatarsus length", equation = "ln(BM) = 2.23 * ln(TaL) - 2.87",
       response = "body mass, g", predictor = "maximum tarsometatarsus length, mm", taxon_scope = "extant volant birds",
       valid_range = "not stated numerically", reported_error = "R2 0.65; 95% PI +/- 2.11 ln units; mean PPE 128.11% (the weakest correlate)",
       method_file = "Field_etal_2013_PLoSONE_bird_skeletal_mass.pdf", locator = "Table 1, p. 6 (rendered page)", verified = "TRUE", notes = "Used only for STM9-31, which has no other limb length."),
  mrow(method_id = "Farlow2005_T3_logM_logTL_wild", mass_method = "regressed_from_length",
       description = "Alligator mississippiensis body mass from total length (wild animals)", equation = "log10(M) = 3.47 * log10(TL) - 10.03",
       response = "body mass, kg", predictor = "total length, mm", taxon_scope = "Alligator mississippiensis (wild, N = 58)",
       valid_range = "not stated; calibration alligators about 0.6-4 m TL (Fig. 3)", reported_error = "r 0.997; SEE 0.0553 log10 units",
       method_file = "Farlow_etal_2005_JVP_alligator_femur_body_size.pdf",
       locator = "Table 3, p. 359 (coefficients read from the rendered page image; PDF is image-only; kg from Figs 6-7 axis 'Log 10 Body Mass (kg)')",
       verified = "TRUE", notes = "Applied to notosuchian and other mesoeucrocodylian TL estimates; body proportions differ from Alligator."),
  mrow(method_id = "Farlow2005_T3_logM_logSKULL_all", mass_method = "regressed_from_bone",
       description = "Alligator mississippiensis body mass from skull length (captive and wild)", equation = "log10(M) = 3.48 * log10(SKULL) - 6.97",
       response = "body mass, kg", predictor = "skull length, snout to posterior edge of the occipital table (medial), mm",
       taxon_scope = "Alligator mississippiensis (captive and wild, N = 27)", valid_range = "not stated", reported_error = "r 0.996; SEE 0.0581 log10 units",
       method_file = "Farlow_etal_2005_JVP_alligator_femur_body_size.pdf",
       locator = "Table 3, p. 359 (rendered page image); skull length defined p. 357",
       verified = "TRUE", notes = "The OCR text gives the slope as 3.4; the page image reads 3.48. Skull length in the fossil papers may be measured differently (see basis)."),
  mrow(method_id = "FeldmanMeiri2013_T1_All_TL", mass_method = "regressed_from_length",
       description = "Snake mass from total length, all snakes (non-phylogenetic)", equation = "log10(M) = -5.465 + 2.597 * log10(TL)",
       response = "body mass, g", predictor = "total length, mm", taxon_scope = "snakes (337 species)",
       valid_range = "whole dataset spans TL 99.4-3735 mm (Results p.164)", reported_error = "R2 0.789; n = 337 species",
       method_file = "Feldman_Meiri_2013_BiolJLinnSoc_snake_length_mass.pdf", locator = "Table 1, p. 164 (rendered page)", verified = "TRUE", notes = ""),
  mrow(method_id = "FeldmanMeiri2013_T1_Boidae_TL", mass_method = "regressed_from_length",
       description = "Snake mass from total length, Boidae", equation = "log10(M) = -5.886 + 2.856 * log10(TL)",
       response = "body mass, g", predictor = "total length, mm", taxon_scope = "Boidae (13 species)",
       valid_range = "not stated per family", reported_error = "R2 0.871; n = 13 species",
       method_file = "Feldman_Meiri_2013_BiolJLinnSoc_snake_length_mass.pdf", locator = "Table 1, p. 164 (rendered page)", verified = "TRUE", notes = "Bracket only."),
  mrow(method_id = "FeldmanMeiri2013_T1_Pythonidae_TL", mass_method = "regressed_from_length",
       description = "Snake mass from total length, Pythonidae", equation = "log10(M) = -5.131 + 2.611 * log10(TL)",
       response = "body mass, g", predictor = "total length, mm", taxon_scope = "Pythonidae (12 species)",
       valid_range = "not stated per family", reported_error = "R2 0.909; n = 12 species",
       method_file = "Feldman_Meiri_2013_BiolJLinnSoc_snake_length_mass.pdf", locator = "Table 1, p. 164 (rendered page)", verified = "TRUE", notes = "Bracket only."),
  reg[reg$method_id == "Meiri2010_T3_Sauria", COLS$methods],
  mrow(method_id = "ChristiansenFarina2004_FL", mass_method = "regressed_from_bone",
       description = "Theropod body mass from femur length (Christiansen & Farina 2004), as used by Therrien et al. 2023 and Hone et al. 2022",
       equation = "log10(BM) = 3.22 * log10(FL) - 6.288 (as printed in Therrien et al. 2023 SI section 5, which omits 'log10' on the left)",
       response = "body mass, kg", predictor = "femur length, mm", taxon_scope = "non-avian theropods (16 species with volumetric model masses, per Campione et al. 2014 p. 914)",
       valid_range = "unknown (paper not on disk)", reported_error = "unknown (paper not on disk); Campione et al. 2014 report that the CF model overestimates mass in extant birds",
       method_file = "Christiansen_Farina_2004_HistBiol_theropod_mass.pdf",
       locator = "secondary: Therrien et al. 2023 SI section 5 and Table S2; Hone et al. 2022 Table 2 caption",
       verified = "FALSE",
       notes = "Not on disk (NEEDED). Values are read from Therrien 2023 and Hone 2022; the printed equation reproduces them within 1.3% (Therrien) and 1% (Hone)."),
  mrow(method_id = "Hu2005_SI_Alexander1979_VanValkenburgh1990", mass_method = "regressed_from_bone",
       description = "Hu et al. 2005 SI: Repenomamus mass from limb-bone lengths and diameters (Alexander et al. 1979) and skull and head-body length (Van Valkenburgh 1990), judged together as a range",
       equation = "as printed by Hu et al. 2005 SI: log M = (log FD - log 5.2)/0.36; log M = (log HD - log 4.9)/0.38; log M = 3.13 log SKL - 5.59; log M = 2.88 log HBL - 7.24 (M kg, lengths mm)",
       response = "body mass, kg", predictor = "femoral and humeral midshaft diameter; skull length; head-body length, mm",
       taxon_scope = "extant mammals (Alexander et al. 1979); extant carnivores (Van Valkenburgh 1990)", valid_range = "unknown (papers not on disk)",
       reported_error = "not reported", method_file = "Alexander_etal_1979_JZool_mammal_limb_allometry.pdf",
       locator = "secondary: Hu et al. 2005 SI section I and Tables 2 and 4", verified = "FALSE",
       notes = "Neither source paper is on disk (both NEEDED; Van Valkenburgh 1990 is VanValkenburgh_1990_BodySizeMammPaleobiol_carnivore_mass.pdf). The authors' final value is a judgement ('we consider the body mass to be ... 4-6 kg for R. robustus')."),
  mrow(method_id = "Han2023_DME_CampioneEvans2012", mass_method = "regressed_from_bone",
       description = "Han et al. 2023: DME (femur length) from Repenomamus robustus IVPP V12728 at 5.54 kg, itself from combined stylopodial circumference",
       equation = "BM = 5.54 kg * (FL / FL_V12728)^3; reference mass by Campione & Evans 2012 (QE)",
       response = "body mass, kg", predictor = "femur length 58.8 mm; reference specimen circumferences and femur length not reported",
       taxon_scope = "terrestrial quadrupeds (QE); DME within Repenomamus robustus", valid_range = "as Campione & Evans 2012",
       reported_error = "'3.43 +/- 1.42 kg' (the +/- is not defined)", method_file = "Campione_Evans_2012_BMCBiol_quadruped_limb_mass.pdf",
       locator = "Han et al. 2023 Methods p. 7 and SI Table S1", verified = "TRUE",
       notes = "Value read from Han et al.; the reference inputs are not reported, so it cannot be recomputed."),
  mrow(method_id = "Han2023_DME_Campione2014", mass_method = "regressed_from_bone",
       description = "Han et al. 2023: DME from Psittacosaurus lujiatunensis D2591 at 23.5 kg (biped equation of Campione et al. 2014 via MASSTIMATE)",
       equation = "BM = 23.5 kg * (FL / FL_D2591)^3; reference: log10(BM) = 2.749 * log10(sqrt(2) * 69.362) - 1.104",
       response = "body mass, kg", predictor = "femur length 137.7 mm; D2591 femoral circumference 69.362 mm; D2591 femur length not reported",
       taxon_scope = "non-avian bipeds", valid_range = "as Campione et al. 2014", reported_error = "'10.6 +/- 6.0 kg' (the +/- is not defined)",
       method_file = "Campione_etal_2014_MEE_biped_limb_mass.pdf", locator = "Han et al. 2023 Methods p. 7", verified = "TRUE",
       notes = "The 23.5 kg reference mass is reproduced exactly from FC 69.362 mm with the OLS QE plus sqrt(2); the DME step cannot be recomputed (D2591 femur length not given)."),
  mrow(method_id = "Hone2022_Meiri2010_unstated", mass_method = "regressed_from_length",
       description = "Hone et al. 2022 Table 2: Indrasaurus mass 'estimated from SVL using Meri (2010)', SVL scaled from Liushusaurus",
       equation = "one of the Meiri 2010 Table 3 equations; which one, and the SVL used, are not stated",
       response = "body mass, g", predictor = "snout-vent length (not reported)", taxon_scope = "lizards",
       valid_range = "Meiri 2010 Table 3", reported_error = "not reported", method_file = "Meiri_2010_JZool_lizard_length_mass.pdf",
       locator = "Hone et al. 2022 Table 2 and caption", verified = "FALSE",
       notes = "Method paper on disk, but the equation and input are not stated, so the value cannot be checked."),
  mrow(method_id = "Froese2006_T1_fusiform", mass_method = "regressed_from_length",
       description = "Fish weight-length cube law with the median form factor a3.0 of fusiform species (Froese 2006 Table 1)",
       equation = "W = 0.0137 * L^3", response = "weight, g", predictor = "fish length, cm (Froese's L is total length unless the source states otherwise)",
       taxon_scope = "fusiform fishes (n = 451 species of the 1316 in Table 1)", valid_range = "not stated; a3.0 5th-95th percentile 0.0061-0.0240",
       reported_error = "median a3.0 0.0137, 95% CL of the median 0.0131-0.0140; 5th-95th percentile of species 0.0061-0.0240",
       method_file = "Froese_2006_JApplIchthyol_fish_length_weight.pdf", locator = "Table 1, p. 249 (PDF p. 9); the b = 3 form factor, eqn on p. 249", verified = "TRUE",
       notes = "Isometric (b = 3) equation. Applied to author-estimated 'body length' of fossil fish, which may be standard rather than total length."),
  mrow(method_id = "Froese2006_T1_elongated", mass_method = "regressed_from_length",
       description = "Fish weight-length cube law with the median form factor a3.0 of elongated species (Froese 2006 Table 1)",
       equation = "W = 0.00838 * L^3", response = "weight, g", predictor = "fish length, cm", taxon_scope = "elongated fishes (n = 403)",
       valid_range = "not stated; a3.0 5th-95th percentile 0.00293-0.0178", reported_error = "median a3.0 0.00838, 95% CL 0.00775-0.00906",
       method_file = "Froese_2006_JApplIchthyol_fish_length_weight.pdf", locator = "Table 1, p. 249 (PDF p. 9)", verified = "TRUE", notes = "Used for lower bounds of fish brackets only."),
  mrow(method_id = "Froese2006_T1_shortdeep", mass_method = "regressed_from_length",
       description = "Fish weight-length cube law with the median form factor a3.0 of short-and-deep species (Froese 2006 Table 1)",
       equation = "W = 0.0187 * L^3", response = "weight, g", predictor = "fish length, cm", taxon_scope = "short and deep fishes (n = 324)",
       valid_range = "not stated; a3.0 5th-95th percentile 0.0077-0.0336", reported_error = "median a3.0 0.0187, 95% CL 0.0172-0.0193",
       method_file = "Froese_2006_JApplIchthyol_fish_length_weight.pdf", locator = "Table 1, p. 249 (PDF p. 9)", verified = "TRUE", notes = "Registered for completeness; not used by a pair yet."))
METHODS$verified[is.na(METHODS$verified)] <- ""

## ------------------------------------------------------------------
## Estimate objects
## ------------------------------------------------------------------
E <- function(point = NA, lo = NA, hi = NA, mm = "", mid = "", basis = "", from_animal = TRUE, xcheck = "", inputs = list())
  list(point = point, lo = lo, hi = hi, mm = mm, mid = mid, basis = basis, from_animal = from_animal, xcheck = xcheck, inputs = inputs)
NONE <- function(why, from_animal = TRUE) E(basis = "", from_animal = from_animal, xcheck = why)
f0 <- function(x) ifelse(is.na(x), "", format(signif(x, 3), big.mark = ",", scientific = FALSE, trim = TRUE))
kg <- function(g) paste0(f0(g / 1000), " kg"); gg <- function(g) paste0(f0(g), " g")
## measurement spec: source, specimen, taxon, role, element, dimension, value, unit, value_type, locator, notes
M <- function(src, spec, tax, role, el, dim, val, unit = "mm", vt = "measured", loc = "", nt = "")
  list(src = src, spec = spec, tax = tax, role = role, el = el, dim = dim, val = val, unit = unit, vt = vt, loc = loc, nt = nt)
s11_basis <- function(r, inp) sprintf("Benson et al. 2014 S11 mass for %s (%s; S11 note '%s'); interval = point x/÷ %.2f (95%%, C&E 2012 eqn 1 SEE 0.134)", r$spec, inp, r$note, K_CE)
S11E <- function(r, inp, meas = TRUE, xcheck = "", inputs = list())
  E(r$g, r$g / K_CE, r$g * K_CE, "regressed_from_bone", if (meas) "Benson2014_S11_FCmeasured" else "Benson2014_S11_FCestimated", s11_basis(r, inp), TRUE, xcheck, inputs)

PRED <- list(); PREY <- list(); XC <- list()   # XC: cross-check rows for the report
xc <- function(key, side, label, g, note = "") XC[[length(XC) + 1]] <<- data.frame(key = key, side = side, estimate = label, mass_g = g, note = note)

## ---- Repenomamus IVPP V13605 (Hu 2005)
ce_hu <- qe1(28 + 26)
PRED$V13605 <- E(sqrt(4000 * 6000), 4000, 6000, "regressed_from_bone", "Hu2005_SI_Alexander1979_VanValkenburgh1990",
  "authors' range: 'we consider the body mass to be ... 4-6 kg for R. robustus' (Hu et al. 2005 SI I); point = geometric midpoint; interval = authors' range",
  TRUE, sprintf("C&E 2012 eqn 1 on V13605 midshaft circumferences (humerus 28 + femur 26 mm) = %s (95%% %s-%s); Hu SI: limb-diameter mean 4.13 kg, skull/HBL mean 3.85 kg; Han 2023 SI: HBL equation 2.08 kg",
  kg(ce_hu), kg(ce_hu / K_CE), kg(ce_hu * K_CE)),
  list(M("Hu2005", "IVPP V13605", "Repenomamus_robustus", "predator", "humerus", "midshaft circumference", 28, loc = "SI Table 1"),
       M("Hu2005", "IVPP V13605", "Repenomamus_robustus", "predator", "femur", "midshaft circumference", 26, loc = "SI Table 1"),
       M("Hu2005", "IVPP V13605", "Repenomamus_robustus", "predator", "femur", "midshaft diameter", 8, loc = "SI Table 1"),
       M("Hu2005", "IVPP V13605", "Repenomamus_robustus", "predator", "head-body", "length", 421, loc = "SI Table 1")))
xc("V13605", "pred", "C&E 2012 eqn 1 (CH 28 + CF 26 mm)", ce_hu); xc("V13605", "pred", "Hu SI limb-diameter mean (Alexander 1979)", 4130); xc("V13605", "pred", "Hu SI skull+HBL mean (Van Valkenburgh 1990)", 3850); xc("V13605", "pred", "Han 2023 SI HBL equation", 2080)
## juvenile Psittacosaurus: femur estimated from tibia (36) and humerus (21)
tf <- c(R_6253 = 156 / 154, R_neimong = 141.5 / 129.5, R_gobi = 129 / 125, R_6541 = 201.4 / 178.7)   # tibia/femur, S11 Psittacosaurus specimens
hf <- c(Han = 78.6 / 137.7, HuSI_juv = 32 / 52, S11_6253 = 121 / 154, S11_neimong = 105 / 129.5, S11_gobi = 92 / 125)
fem_pt <- 36 / mean(tf); fem_lo <- min(21 / max(hf), 36 / max(tf)); fem_hi <- max(21 / min(hf), 36 / min(tf))
ps_ref <- c(m = 19870, L = 210)   # Erickson & Tumanova 2000 Table 2, stage G: femur 21.0 cm, 19.87 kg
PREY$Hu2005_01 <- E(dme(ps_ref["m"], ps_ref["L"], fem_pt), dme(ps_ref["m"], ps_ref["L"], fem_lo), dme(ps_ref["m"], ps_ref["L"], fem_hi),
  "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("femur not reported; estimated %.1f mm from tibia 36 mm / mean tibia:femur %.3f of four Psittacosaurus in Benson 2014 S11 (bracket %.1f-%.1f mm from the tibia and from humerus 21 mm with humerus:femur %.2f-%.2f in Han 2023, Hu 2005 SI juvenile and S11); DME from Erickson & Tumanova 2000 Table 2 (femur 21.0 cm, 19.87 kg; same result from any row); interval = DME at the femur bracket ends (no error is stated for DME)",
          fem_pt, mean(tf), fem_lo, fem_hi, min(hf), max(hf)),
  TRUE, "DME from humerus via E&T Table 1 element lengths is not usable: E&T's humeri and femora come from different individuals",
  list(M("Hu2005", "IVPP V13605 (prey)", "Psittacosaurus", "prey", "tibia", "length", 36, loc = "p.151"),
       M("Hu2005", "IVPP V13605 (prey)", "Psittacosaurus", "prey", "humerus", "length", 21, loc = "p.151"),
       M("Hu2005", "IVPP V13605 (prey)", "Psittacosaurus", "prey", "femur", "length", round(fem_pt, 1), vt = "derived", loc = "this study: tibia 36 mm / mean tibia:femur ratio of Psittacosaurus in Benson 2014 S11")))

## ---- Repenomamus WZSSM VF000011 + Psittacosaurus lujiatunensis (Han 2023)
PRED$WZSSM <- E(3430, 3430 - 1420, 3430 + 1420, "regressed_from_bone", "Han2023_DME_CampioneEvans2012",
  "authors: 'a mass of 3.43 +/- 1.42 kg for the mammal' (Han et al. 2023 p.3; SI Table S1, stylopodial-circumference DME); interval = authors' +/- (undefined in the paper)", TRUE,
  "Han Table 1 'minimum shaft circumference' values (humerus 11.4, femur 7.3 mm) are not usable as circumferences (conflict 3), so no independent check",
  list(M("Han2023", "WZSSM VF000011 (mammal)", "Repenomamus_robustus", "predator", "femur", "length", 58.8, loc = "Table 1")))
han_et <- dme(19870, 210, 137.7)
PREY$Han2023_01 <- E(10600, 10600 - 6000, 10600 + 6000, "regressed_from_bone", "Han2023_DME_Campione2014",
  "authors: 'we estimate its body mass to have been 10.6 +/- 6.0 kg at time of death' (Han et al. 2023 p.3; DME from D2591, femoral circumference 69.362 mm -> 23.5 kg); interval = authors' +/- (undefined)", TRUE,
  sprintf("DME from Erickson & Tumanova 2000 (P. mongoliensis, femur 21.0 cm, 19.87 kg) with femur 137.7 mm = %s", kg(han_et)),
  list(M("Han2023", "WZSSM VF000011 (dinosaur)", "Psittacosaurus_lujiatunensis", "prey", "femur", "length", 137.7, loc = "Table 1")))
xc("Han2023_01", "prey", "DME, Erickson & Tumanova 2000 Table 2, femur 137.7 mm", han_et)

## ---- Sinosauropteryx NIGP 127587
PRED$NIGP127587 <- S11E(R$NIGP127587, "femur 86.4 mm, FAP 8.2 mm", FALSE, "",
  list(M("Benson2014", "NIGP 127587", "Sinosauropteryx_prima", "predator", "femur shaft", "anteroposterior diameter", 8.2, loc = "S11 'Full data'"),
       M("Chen1998", "NIGP 127587", "Sinosauropteryx_prima", "predator", "femur", "length", 86.4, loc = "Table 1")))

## ---- GMV 2124 (Sinosauropteryx prima per decision 5; tyrannosauroid per Cau 2024)
g_sp <- dme(R$NIGP127587$g, R$NIGP127587$FL, 108); g_ty <- dme(R$Dilong$g, R$Dilong$FL, 108)
PRED$GMV2124 <- E(g_sp, g_sp / K_CE, max(g_sp, g_ty) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("DME by femur length 108 mm (Currie & Chen 2001; attribution to NGMC 2124 by elimination, conflict 25) from Sinosauropteryx NIGP 127587 (S11 %s, femur 86.4 mm; FC estimated); interval = lower reference bound to upper bound of the alternative identification", kg(R$NIGP127587$g)),
  TRUE, sprintf("decision 5 alternative (Cau 2024: early tyrannosauroid): DME from Dilong IVPP V14243 (S11 %s, femur 179 mm, juvenile) = %s", kg(R$Dilong$g), kg(g_ty)),
  list(M("Currie2001", "NGMC 2124 (= CAGS GMV 2124)", "Sinosauropteryx_prima", "predator", "femur", "length", 108, loc = "p.1706; attribution by elimination (conflict 25)")))
xc("GMV2124", "pred", "DME from NIGP 127587 (Sinosauropteryx; decision 5 identity)", g_sp); xc("GMV2124", "pred", "DME from Dilong IVPP V14243 (tyrannosauroid identity, Cau 2024)", g_ty)

## ---- Birds: Field 2013 humerus length
birdE <- function(key, HL, src_hl, spec, tax, FL = NA, src_fl = "", extra = list(), note = "Table 1") {
  p <- fieldHL(HL); x <- if (!is.na(FL)) sprintf("Field 2013 femur-length equation (FL %s mm) = %s", FL, gg(fieldFL(FL))) else ""
  if (!is.na(FL)) xc(key, "pred", sprintf("Field 2013 FL (%s mm)", FL), fieldFL(FL))
  ins <- c(list(M(src_hl, spec, tax, "predator", "humerus", "length", HL, loc = note)), if (!is.na(FL)) list(M(src_fl, spec, tax, "predator", "femur", "length", FL, loc = note)) else list(), extra)
  E(p, p / exp(0.89), p * exp(0.89), "regressed_from_bone", "Field2013_T1_HL",
    sprintf("humerus length %s mm (%s); Field et al. 2013 humerus-length equation; interval = Field's 95%% prediction interval (+/- 0.89 ln)", HL, src_hl), TRUE, x, ins)
}
PRED$V17078 <- birdE("V17078", 74, "Zhou2014", "IVPP V17078", "Piscivoravis_lii", 56, "Zhou2014")
fe_kp <- ME1$value[ME1$specimen == "AGB 15483" & grepl("^femur", ME1$element) & ME1$dimension == "length"][1]
PRED$AGB15483 <- birdE("AGB15483", 82.93, "Huang2026", "AGB 15483", "Kunpengornis_anhuimusei", if (length(fe_kp) && !is.na(fe_kp)) as.numeric(fe_kp) else NA, "Huang2026")
PRED$V13259 <- birdE("V13259", 75.4, "Wang2020", "IVPP V13259", "Yanornis_martini", 56.1, "Wang2020", note = "Wang 2020 SI Table S1")
stm <- list(`STM9-15` = c(80, 51), `STM9-18` = c(73, 49), `STM9-19` = c(76, 58), `STM9-26` = c(78, NA), `STM9-37` = c(79, 55),
            `STM9-46` = c(80, 57), `STM9-49` = c(72, 50), `STM9-51` = c(67, 57), `STM9-52` = c(68, 50))
for (s in names(stm)) PRED[[s]] <- birdE(s, stm[[s]][1], "Zheng2014", s, "Yanornis", stm[[s]][2], "Zheng2014", note = "File S1 Table S1 (asterisked values estimated)")
p31 <- fieldTaL(52)
PRED$`STM9-31` <- E(p31, p31 / exp(2.11), p31 * exp(2.11), "regressed_from_bone", "Field2013_T1_TaL",
  "tarsometatarsus 52 mm (estimated; Zheng 2014 File S1 Table S1; outside the 31-40 mm of the other nine birds); the only limb length for this bird; interval = Field's 95% PI (+/- 2.11 ln)", TRUE,
  "skull 60 mm, the same as STM9-18 (451 g by humerus length)", list(M("Zheng2014", "STM9-31", "Yanornis", "predator", "tarsometatarsus", "length", 52, vt = "estimated", loc = "File S1 Table S1")))
PRED$V16708 <- birdE("V16708", 76, "Zhou2009", "IVPP V16708", "Jianchangornis_microdonta", 60, "Zhou2009", note = "Table 1, left")
PRED$V16708$xcheck <- paste0(PRED$V16708$xcheck, sprintf("; Wang 2020 SI humerus 75.4 mm = %s; Benson 2014 S11 = %s (cQE, FC estimated)", gg(fieldHL(75.4)), gg(R$V16708$g)))
xc("V16708", "pred", "Benson 2014 S11 (cQE, FC estimated)", R$V16708$g)

## ---- Microraptor (Hone 2022 Table 2 values; DME cross-checks)
q_d <- dme(R$V13352$g, R$V13352$FL, 109)
PRED$QMV1002 <- E(1890, min(1890, q_d / K_CE), max(1890, q_d * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
  sprintf("Hone et al. 2022 Table 2: femur 109 mm -> 1890 g (Christiansen & Farina 2004); interval = envelope of this value and the 95%% range of the DME cross-check (%s)", gg(q_d)), TRUE,
  sprintf("DME by femur from M. gui IVPP V13352 (S11 %s, femur 106 mm) = %s; O'Connor 2011 Table S1 gives V13352 femur 78.7* mm, which would give %s (conflict 28)", gg(R$V13352$g), gg(q_d), gg(dme(R$V13352$g, 78.7, 109))),
  list(M("Xing2013", "QM V1002", "Microraptor_gui", "predator", "femur", "length", 109, loc = "PDF p.2, 'femur length = 109 mm'")))
xc("QMV1002", "pred", "DME by femur from IVPP V13352 (S11 FL 106)", q_d); xc("QMV1002", "pred", "DME by femur from IVPP V13352 if FL = 78.7* (O'Connor 2011)", dme(R$V13352$g, 78.7, 109))
v_t <- dme(R$V13352$g, R$V13352$TL, 140.4); v_f <- dme(R$V13352$g, R$V13352$FL, 82.3)
PRED$V17972 <- E(760, min(760, v_t / K_CE), max(760, v_t * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
  sprintf("Hone et al. 2022 Table 2: femur 82.3 mm (estimated, O'Connor 2011 Table S1 slab B) -> 760 g (Christiansen & Farina 2004); interval = envelope of this value and the 95%% range of the tibia DME cross-check (%s)", gg(v_t)), TRUE,
  sprintf("DME by tibia 140.4 mm (measured) from IVPP V13352 (S11 %s, tibia %.1f mm; O'Connor gives 126.7) = %s; by femur 82.3* = %s; O'Connor 2011 cites a generic 1500 g for Microraptor (Turner et al. 2007), not this specimen", gg(R$V13352$g), R$V13352$TL, gg(v_t), gg(v_f)),
  list(M("OConnor2011", "IVPP V17972", "Microraptor_gui", "predator", "tibia", "length", 140.4, loc = "SI Table S1 (printed 'IVPP V13972A/B')"),
       M("OConnor2011", "IVPP V17972", "Microraptor_gui", "predator", "femur", "length", 82.3, vt = "estimated", loc = "SI Table S1 slab B")))
xc("V17972", "pred", "DME by tibia from IVPP V13352", v_t); xc("V17972", "pred", "DME by femur 82.3* from IVPP V13352", v_f); xc("V17972", "pred", "O'Connor 2011 generic Microraptor mass", 1500)
s_d <- dme(R$CAGS20$g, R$CAGS20$FL, 75)
PRED$STM532 <- E(570, min(570, s_d / K_CE), max(570, s_d * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
  sprintf("Hone et al. 2022 Table 2: femur 'est. 75' mm -> 570 g (Christiansen & Farina 2004); interval = envelope with the 95%% range of the DME cross-check (%s)", gg(s_d)), TRUE,
  sprintf("DME from M. zhaoianus CAGS 20-7-004 (S11 %s, femur 75 mm, FC estimated) = %s", gg(R$CAGS20$g), gg(s_d)),
  list(M("Hone2022", "STM5-32", "Microraptor_zhaoianus", "predator", "femur", "length", 75, vt = "estimated", loc = "Table 2")))
xc("STM532", "pred", "DME from CAGS 20-7-004", s_d)
z_lo <- dme(R$CAGS20$g, R$CAGS20$FL, 49.8); z_hi <- dme(R$CAGS20$g, R$CAGS20$FL, 53)
PRED$V12330 <- E(150, min(150, z_lo / K_CE), max(150, z_hi * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
  "Hone et al. 2022 Table 2: femur 49.8 mm -> 150 g (Christiansen & Farina 2004); interval = envelope with the 95% ranges of the DME cross-checks at femur 49.8 and 53 mm (conflict 17)", TRUE,
  sprintf("DME from CAGS 20-7-004 (S11 %s, femur 75 mm): femur 49.8 mm (Hone) = %s, femur 53 mm (Zhou & Zhang 2002; O'Connor 2011) = %s; Larsson et al. 2010 '100 and 150 g' ('metric comparisons to closely related taxa')", gg(R$CAGS20$g), gg(z_lo), gg(z_hi)),
  list(M("Hone2022", "IVPP V12330", "Microraptor_zhaoianus", "predator", "femur", "length", 49.8, loc = "Table 2"),
       M("ZhouZhang2002", "IVPP V12330", "Microraptor_zhaoianus", "predator", "femur", "length", 53, loc = "Table 1 (left)")))
xc("V12330", "pred", "DME from CAGS 20-7-004, femur 49.8", z_lo); xc("V12330", "pred", "DME from CAGS 20-7-004, femur 53", z_hi)

## ---- Anchiornis (Zheng 2018 Table 1 femora)
anchE <- function(key, fl, vt = "measured") {
  a <- dme(R$LPMB169$g, R$LPMB169$FL, fl); b <- dme(R$Mei$g, R$Mei$FL, fl); xc(key, "pred", "DME from Mei long IVPP V12733 (measured FC)", b)
  E(a, min(a, b) / K_CE, max(a, b) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
    sprintf("DME by femur %s mm (Zheng 2018 Table 1) from Anchiornis LPM-B00169 (S11 %s, femur 66.2 mm; FAP 'measured from figure', FC estimated); interval = 95%% range across this and the Mei long reference", fl, gg(R$LPMB169$g)),
    TRUE, sprintf("DME from troodontid Mei long IVPP V12733 (S11 %s, femur 77 mm, measured FC) = %s", gg(R$Mei$g), gg(b)),
    list(M("Zheng2018", key, "Anchiornis", "predator", "femur", "length", fl, vt = vt, loc = "Table 1")))
}
PRED$`STM0-179` <- anchE("STM0-179", 71); PRED$`STM0-224` <- anchE("STM0-224", 70); PRED$`STMA0-4` <- anchE("STMA0-4", 35, "estimated"); PRED$`STM0-38` <- anchE("STM0-38", 40.5)

## ---- Sinocalliopteryx
PRED$JMP <- S11E(R$JMP, "femur 250 mm and ML shaft diameter 25.5 mm 'measured from figures in Ji et al. 2007'", FALSE,
  "femur length is disputed (S11 250 mm vs Hone 2022 210 mm, conflict 22) but the S11 mass rests on the shaft diameter, not on the length",
  list(M("Benson2014", "JMP-V-05-8-01", "Sinocalliopteryx_gigas", "predator", "femur shaft", "mediolateral diameter", 25.5, loc = "S11; measured from figures in Ji et al. 2007")))
c_mt <- dme(R$JMP$g, 147.3, 206.3); c_ra <- dme(R$JMP$g, 100.7, 118.64); c_mx <- dme(R$JMP$g, 40.8, 44.5); c_gm <- exp(mean(log(c(c_mt, c_ra, c_mx))))
PRED$CAGST1 <- E(c_gm, min(c_mt, c_ra, c_mx) / K_CE, max(c_mt, c_ra, c_mx) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("no femur; DME from the holotype JMP-V-05-8-01 (S11 %s) by three Xing 2012 Table 2 ratios: metatarsal III 206.3/147.3 -> %s, radius 118.64/100.7 -> %s, maxillary height 44.5/40.8 -> %s; point = geometric mean; interval = 95%% range across the three", kg(R$JMP$g), kg(c_mt), kg(c_ra), kg(c_mx)),
  TRUE, "Xing et al. note that metatarsals grow faster than the radius or maxilla, so the element ratios disagree; Hone 2022 'est. 290' mm femur",
  list(M("Xing2012", "CAGS-IG-T1", "Sinocalliopteryx_gigas", "predator", "metatarsal III", "length", 206.3, loc = "Table 2"),
       M("Xing2012", "CAGS-IG-T1", "Sinocalliopteryx_gigas", "predator", "radius", "length", 118.64, loc = "Table 2"),
       M("Xing2012", "CAGS-IG-T1", "Sinocalliopteryx_gigas", "predator", "maxilla", "maximum height", 44.5, loc = "Table 2"),
       M("Xing2012", "JMP-V-05-8-01", "Sinocalliopteryx_gigas", "comparison", "metatarsal III", "length", 147.3, loc = "Table 2"),
       M("Xing2012", "JMP-V-05-8-01", "Sinocalliopteryx_gigas", "comparison", "radius", "length", 100.7, loc = "Table 2"),
       M("Xing2012", "JMP-V-05-8-01", "Sinocalliopteryx_gigas", "comparison", "maxilla", "maximum height", 40.8, loc = "Table 2")))
xc("CAGST1", "pred", "DME by metatarsal III", c_mt); xc("CAGST1", "pred", "DME by radius", c_ra); xc("CAGST1", "pred", "DME by maxillary height", c_mx)
sn_mi <- dme(R$V13352$g, R$V13352$TL, 155); sn_ve <- dme(R$MPDC10025$g, R$MPDC10025$TL, 155); sn_me <- dme(R$Mei$g, R$Mei$TL, 155)
PREY$Xing2012_01 <- E(sn_mi, sn_mi / K_CE, max(sn_mi, sn_ve, sn_me) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("prey tibia 155 mm (Xing 2012 'tibial length 15.5 cm'); DME by tibia from microraptorine Microraptor gui IVPP V13352 (S11 %s, tibia %.1f mm); interval = 95%% range across this, Mei long and Velociraptor references", gg(R$V13352$g), R$V13352$TL),
  TRUE, sprintf("DME by tibia from Velociraptor MPD-C 100/25 (tibia 235 mm, measured FC) = %s; from Mei long IVPP V12733 (tibia 100.5 mm) = %s; S11 has no tibia for Sinornithosaurus IVPP V12811", gg(sn_ve), gg(sn_me)),
  list(M("Xing2012", "JMP-V-05-8-01 (prey)", "Sinornithosaurus", "prey", "tibia", "length", 155, loc = "PDF p.6, 'tibial length 15.5 cm'")))
xc("Xing2012_01", "prey", "DME by tibia from Velociraptor MPD-C 100/25", sn_ve); xc("Xing2012_01", "prey", "DME by tibia from Mei long", sn_me)

## ---- Coelophysis AMNH FR 7224
co_a <- dme(R$AMNH7223$g, R$AMNH7223$FL, 203); co_b <- dme(R$AMNH2704$g, R$AMNH2704$FL, 203)
PRED$AMNH7224 <- E(co_a, min(co_a, co_b) / K_CE, max(co_a, co_b) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("DME by femur 203 mm (Nesbitt 2006) from AMNH 7223 (S11 %s, femur 209 mm, measured FC; same quarry); interval = 95%% range across AMNH 7223 and AMNH 2704", kg(R$AMNH7223$g)),
  TRUE, sprintf("DME from AMNH 2704 (S11 %s, femur 217 mm) = %s; Hone 2022 Table 1 femur 209 belongs to AMNH 7223 (conflict 23)", kg(R$AMNH2704$g), kg(co_b)),
  list(M("Nesbitt2006", "AMNH FR 7224", "Coelophysis_bauri", "predator", "femur", "length", 203, loc = "PDF p.1-2, 'femoral length=203 mm'")))
xc("AMNH7224", "pred", "DME from AMNH 2704", co_b)

## ---- Gorgosaurus TMP 2009.12.14 + Citipes
go_d <- dme(R$NMC2120$g, R$NMC2120$FL, 543.3)
PRED$TMP2009 <- E(334400, min(334400, go_d / K_CE), max(334400, go_d * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
  sprintf("authors: femur 543.3 mm -> 334.4 kg (Therrien et al. 2023 SI Table S2, Christiansen & Farina 2004); interval = envelope with the 95%% range of the DME cross-check (%s)", kg(go_d)), TRUE,
  sprintf("DME from Gorgosaurus NMC 2120 (S11 %s, femur 1040 mm, measured FC) = %s; printed C&F equation gives %s", kg(R$NMC2120$g), kg(go_d), kg(cfFL(543.3))),
  list(M("Therrien2023", "TMP 2009.12.14", "Gorgosaurus_libratus", "predator", "femur", "length", 543.3, loc = "SI Tables S2, S4")))
xc("TMP2009", "pred", "DME from NMC 2120", go_d)
citE <- function(fl, auth, vt, extra) {
  d <- dme(R$TMP79$g, R$TMP79$FL, fl)
  list(E(auth, min(auth, d / K_CE), max(auth, d * K_CE), "regressed_from_bone", "ChristiansenFarina2004_FL",
    sprintf("authors: femur %s mm -> %s kg (Therrien et al. 2023 SI Table S2, Christiansen & Farina 2004)%s; interval = envelope with the 95%% range of the DME cross-check", fl, auth / 1000, extra), TRUE,
    sprintf("DME from Chirostenotes pergracilis TMP 79.20.1 (S11 %s, femur 310 mm, measured FC) = %s", kg(R$TMP79$g), kg(d)),
    list(M("Therrien2023", "TMP 2009.12.14 (prey)", "Citipes_elegans", "prey", "femur", "length", fl, vt = vt, loc = "SI Tables S2, S4"))), d)
}
ca <- citE(178, 9200, "estimated", " (femur estimated from the metatarsal IV/femur ratio of the posterior individual)"); PREY$Therrien2023_01 <- ca[[1]]; xc("Therrien2023_01", "prey", "DME from TMP 79.20.1", ca[[2]])
cp <- citE(191, 11500, "measured", ""); PREY$Therrien2023_02 <- cp[[1]]; xc("Therrien2023_02", "prey", "DME from TMP 79.20.1", cp[[2]])

## ---- Aplestosuchus + sphagesaurid (Godoy 2014 Text S1)
ap <- farlowTL(2300)
PRED$Aplesto <- E(ap, ap / K_FTL, ap * K_FTL, "regressed_from_length", "Farlow2005_T3_logM_logTL_wild",
  sprintf("total length 2300 mm estimated by the authors 'based on Baurusuchus salgadoensis' (Text S1; preserved 1100 mm); Farlow 2005 alligator TL equation; interval = 95%% (SEE 0.0553); input is itself an estimate and a baurusuchid is not an alligator"), TRUE, "",
  list(M("Godoy2014", "LPRP/USP 0229a", "Aplestosuchus_sordidus", "predator", "body", "total length", 2300, vt = "estimated", loc = "Text S1")))
sp_sk <- farlowSK(95); sp_t1 <- farlowTL(550); sp_t2 <- farlowTL(600)
PREY$Godoy2014_01 <- E(sp_sk, min(sp_t1, sp_sk / K_FSK), max(sp_t2, sp_sk * K_FSK), "regressed_from_bone", "Farlow2005_T3_logM_logSKULL_all",
  "skull length 95 mm, estimated from parietal length 25 mm 'based on Cairpirasuchus paulistanus' (Text S1); Farlow 2005 skull equation; interval = envelope of its 95% range and the TL-based estimates", TRUE,
  sprintf("Farlow TL equation: 550 mm (Text S1) = %s; 600 mm (main text) = %s", gg(sp_t1), gg(sp_t2)),
  list(M("Godoy2014", "LPRP/USP 0229b", "Sphagesauridae", "prey", "parietal", "length", 25, loc = "Text S1"),
       M("Godoy2014", "LPRP/USP 0229b", "Sphagesauridae", "prey", "skull", "length", 95, vt = "estimated", loc = "Text S1"),
       M("Godoy2014", "LPRP/USP 0229b", "Sphagesauridae", "prey", "body", "total length", 550, vt = "estimated", loc = "Text S1")))
xc("Godoy2014_01", "prey", "Farlow TL, 550 mm", sp_t1); xc("Godoy2014_01", "prey", "Farlow TL, 600 mm", sp_t2)

## ---- Confractosuchus AODF0890 + ornithopod
cf_sk <- farlowSK(285); cf_t1 <- farlowTL(2000); cf_t2 <- farlowTL(2500)
PRED$AODF0890 <- E(cf_sk, min(cf_t1, cf_sk / K_FSK), max(cf_t2, cf_sk * K_FSK), "regressed_from_bone", "Farlow2005_T3_logM_logSKULL_all",
  "skull length 285 mm (White et al. 2022 'The skull (28.5 cm long, 19 cm wide)'; their landmark may differ from Farlow's occipital-table definition); Farlow 2005 skull equation; interval = envelope of its 95% range and the TL estimates", TRUE,
  sprintf("Farlow TL equation with the authors' 2.0-2.5 m: %s-%s", kg(cf_t1), kg(cf_t2)),
  list(M("White2022", "AODF0890", "Confractosuchus_sauroktonos", "predator", "skull", "length", 285, loc = "PDF p.4, 'The skull (28.5 cm long, 19 cm wide)'"),
       M("White2022", "AODF0890", "Confractosuchus_sauroktonos", "predator", "body", "total length", 2250, vt = "estimated", loc = "'estimated to be around 2-2.5 m in length' (midpoint entered)")))
xc("AODF0890", "pred", "Farlow TL 2.0 m", cf_t1); xc("AODF0890", "pred", "Farlow TL 2.5 m", cf_t2)
w_or <- dme(R$Oro$g, R$Oro$TL, 43); w_je <- dme(R$Jehol$g, R$Jehol$TL, 43); w_ha <- dme(R$Haya$g, R$Haya$TL, 43)
PREY$White2022_01 <- E(sqrt(1000 * 1700), 1000, 1700, "regressed_from_bone", "Campione2014_eqn7",
  "authors: 'The ornithopod's estimated body mass (1.0-1.7 kg; calculated using the bipedal formula of Campione et al. (2014))'; input femoral circumference not reported (only proximal femora preserved; conflict 4 open); point = geometric midpoint; interval = authors' range",
  TRUE, sprintf("DME by the preserved tibia (43 mm long, 10 mm circumference): from Orodromeus MOR 473 = %s, Jeholosaurus = %s, Haya IGM 100/2015 = %s; about 1/5 to 1/9 of the authors' range", gg(w_or), gg(w_je), gg(w_ha)),
  list(M("White2022", "AODF0890 (prey)", "Ornithopoda", "prey", "tibia", "length", 43, loc = "p.296"),
       M("White2022", "AODF0890 (prey)", "Ornithopoda", "prey", "tibia", "mid-length circumference", 10, loc = "p.296")))
xc("White2022_01", "prey", "DME by tibia, Orodromeus MOR 473", w_or); xc("White2022_01", "prey", "DME by tibia, Jeholosaurus", w_je); xc("White2022_01", "prey", "DME by tibia, Haya", w_ha)

## ---- Sanajeh
sa_all <- fmTL(3460, -5.465, 2.597); sa_boa <- fmTL(3460, -5.886, 2.856); sa_py <- fmTL(3460, -5.131, 2.611)
PRED$Sanajeh <- E(sa_all, min(sa_all, sa_boa, sa_py), max(sa_all, sa_boa, sa_py), "regressed_from_length", "FeldmanMeiri2013_T1_All_TL",
  "total length 3460 mm (Wilson 2010 Text S3, from skull length 95 mm by the authors' regression y = 34.2x + 215.4); Feldman & Meiri 2013 all-snake TL equation (madtsoiids belong to no extant family); interval = range across the All, Boidae and Pythonidae TL equations (no prediction error is published)",
  TRUE, sprintf("Boidae TL = %s; Pythonidae TL = %s", kg(sa_boa), kg(sa_py)),
  list(M("Wilson2010", "GSI/GC/2901-2906", "Sanajeh_indicus", "predator", "skull", "length", 95, loc = "main text; Text S3"),
       M("Wilson2010", "GSI/GC/2901-2906", "Sanajeh_indicus", "predator", "body", "total length", 3460, vt = "estimated", loc = "Text S3")))
xc("Sanajeh", "pred", "F&M Boidae TL", sa_boa); xc("Sanajeh", "pred", "F&M Pythonidae TL", sa_py)

## ---- Velociraptor, Edmontosaurus DMNH 1493, hadrosaur juveniles (Varricchio)
PRED$GISPS <- S11E(R$MPDC10025, "femur 190 mm, femoral circumference 64 mm measured", TRUE, "", list(M("Benson2014", "GI SPS 100/25 (= MPD-C 100/25)", "Velociraptor_mongoliensis", "predator", "femur shaft", "circumference", 64, loc = "S11")))
ed_m <- cqe7(mean(c(495, 424))); ed_L <- cqe7(495); ed_R <- cqe7(424); ed_lo <- ed_R / K_CE; ed_hi <- ed_L * K_CE
ed_d <- dme(R$AMNH5730$g, R$AMNH5730$FL, mean(c(1045, 1024)))
PREY$Carpenter1998_02 <- E(ed_m, ed_lo, ed_hi, "regressed_from_bone", "Campione2014_eqn7",
  sprintf("femur circumference 'below 4th trochanter' 49.5 cm (left) and 42.4 cm (right) (Carpenter 1998 Table I); cQE on the mean 459.5 mm; left alone %s, right alone %s; interval = right-side value / %.2f to left-side value x %.2f", kg(ed_L), kg(ed_R), K_CE, K_CE), TRUE,
  sprintf("DME by mean femur length 1034.5 mm from E. annectens AMNH 5730 (S11 %s, femur 1147.7 mm) = %s; no humeral circumference, so the quadrupedal equation cannot be used", kg(R$AMNH5730$g), kg(ed_d)),
  list(M("Carpenter1998", "DMNH 1493", "Edmontosaurus_annectens", "prey", "femur (left)", "circumference below 4th trochanter", 49.5, "cm", loc = "Table I"),
       M("Carpenter1998", "DMNH 1493", "Edmontosaurus_annectens", "prey", "femur (right)", "circumference below 4th trochanter", 42.4, "cm", loc = "Table I"),
       M("Carpenter1998", "DMNH 1493", "Edmontosaurus_annectens", "prey", "femur (left)", "length", 104.5, "cm", loc = "Table I"),
       M("Carpenter1998", "DMNH 1493", "Edmontosaurus_annectens", "prey", "skeleton (mounted, along neural spines)", "length", 699.5, "cm", vt = "estimated", loc = "Table I")))
xc("Carpenter1998_02", "prey", "DME by femur length from AMNH 5730", ed_d)
r3 <- (3000 / 6995)^3; r10 <- (1000 / 6995)^3; r11 <- (1100 / 6995)^3; r12 <- (1200 / 6995)^3
PREY$Varricchio2001_01 <- E(ed_m * r3, ed_lo * r3, ed_hi * r3, "regressed_from_length", "ErickTum2000_DME_bodylength",
  sprintf("authors: caudals 'represent individuals about 3 m in total body length'; DME by body length from Edmontosaurus DMNH 1493 (6995 mm mounted length, Carpenter 1998 Table I; %s by cQE, this study); interval carries the reference's interval; isometry is a poor assumption for juvenile hadrosaurs", kg(ed_m)), TRUE,
  "no hadrosaur length-mass equation is on disk", list(M("Varricchio2001", "OTM 201 (caudals)", "Hadrosauridae", "prey", "body", "total length", 3000, vt = "estimated", loc = "PDF p.4, figure caption 'about 3 m in total body length'")))
PREY$Varricchio2001_02 <- E(ed_m * r11, ed_lo * r10, ed_hi * r12, "regressed_from_length", "ErickTum2000_DME_bodylength",
  "authors: dentary 'indicates a very young animal roughly 1 to 1.2 m in length'; DME by body length (point at 1.1 m) from Edmontosaurus DMNH 1493 as for Varricchio2001_01; interval = 1.0 m with the reference lower bound to 1.2 m with the upper bound", TRUE,
  "", list(M("Varricchio2001", "OTM 201 (dentary)", "Hadrosauridae", "prey", "body", "total length", 1100, vt = "estimated", loc = "PDF p.3, 'roughly 1 to 1.2 m in length' (midpoint entered)")))
PREY$DePalma2013_01 <- E(sqrt(ed_m * R$AMNH5730$g), ed_m, R$AMNH5730$g, "species_average_literature", "",
  sprintf("no element of this individual allows an estimate (two coalesced caudals); DePalma et al. call it 'an adult hadrosaur'; bracket = two adult E. annectens with limb-based masses: DMNH 1493 (%s, this study) and AMNH 5730 (Benson 2014 S11, %s); point = geometric mean", kg(ed_m), kg(R$AMNH5730$g)),
  FALSE, "")

## ---- Huadanosaurus, Compsognathus, Scipionyx, Huaxiagnathus, Baryonyx
hu_s <- dme(R$NIGP127587$g, R$NIGP127587$FL, 105.69); hu_c <- dme(R$CNJ79$g, R$CNJ79$FL, 105.69)
PRED$V14202 <- E(hu_s, min(hu_s, hu_c) / K_CE, max(hu_s, hu_c) * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("DME by femur 105.69 mm (Qiu 2025 SI Table S2) from compsognathid Sinosauropteryx NIGP 127587 (S11 %s, femur 86.4 mm; same formation); interval = 95%% range across this and Compsognathus MNHN CNJ 79", gg(R$NIGP127587$g)), TRUE,
  sprintf("DME from Compsognathus MNHN CNJ 79 (S11 %s, femur 110 mm) = %s", gg(R$CNJ79$g), gg(hu_c)),
  list(M("Qiu2025", "IVPP V 14202", "Huadanosaurus_sinensis", "predator", "femur", "length", 105.69, loc = "SI Table S2")))
xc("V14202", "pred", "DME from MNHN CNJ 79", hu_c)
PRED$BSP563 <- S11E(R$BSP563, "femur 71.5 mm (MTC), FAP 9.65 mm", FALSE, "Ostrom 1978 p.109 'estimated live weight, 3-3.5 kg' (no method given)",
  list(M("Benson2014", "SNSB-BSPG AS I 563", "Compsognathus_longipes", "predator", "femur shaft", "anteroposterior diameter", 9.65, loc = "S11")))
xc("BSP563", "pred", "Ostrom 1978 'estimated live weight' (no method)", 3250)
xc("Scip", "pred", "Dal Sasso & Maganuco 2011 upper bound 'no more than 0.2 kg' (no equation)", 200)
PRED$CNJ79 <- S11E(R$CNJ79, "femur 110 mm, FAP 11.1 mm", FALSE, "", list(M("Peyer2006", "MNHN CNJ 79", "Compsognathus_longipes", "predator", "femur", "minimum shaft diameter", 11.1, loc = "Table 1")))
PRED$Scip <- S11E(R$Scip, "femur 37.3 mm, FAP 4.8 mm; juvenile", FALSE, "Dal Sasso & Maganuco 2011 (PDF p.177, 'Body length and body mass'): 'tentatively estimate a weight in life of no more than 0.2 kg' from body proportions of small coelurosaurs (no equation); total length 'did not exceed 50 cm'", list(M("DalSasso2011", "SBA-SA 163760", "Scipionyx_samniticus", "predator", "femur (right)", "length", 37.3, loc = "Table 1")))
PRED$CAGS301 <- S11E(R$CAGS301, "femur 164 mm, FAP 24 mm 'Measured from Hwang et al. (2004)'", FALSE, "", list(M("Benson2014", "CAGS-IG02-301", "Huaxiagnathus_orientalis", "predator", "femur shaft", "anteroposterior diameter", 24, loc = "S11")))
ba_f <- dme(R$Sucho$g, R$Sucho$FL, 1200); ba_h <- dme(R$Sucho$g, R$Sucho$HL, 463); ba_g <- sqrt(ba_f * ba_h)
PRED$R9951 <- E(ba_g, ba_h / K_CE, ba_f * K_CE, "regressed_from_bone", "ErickTum2000_DME_bone",
  sprintf("DME from Suchomimus MNN GDF 500 (S11 %s, femur 1075, humerus 560 mm, measured FC): femur 1200 mm ('probably a reasonable approximation', Charig & Milner p.51) -> %s; humerus 463 mm -> %s; point = geometric mean; interval = 95%% range across the two", kg(R$Sucho$g), kg(ba_f), kg(ba_h)),
  TRUE, "R9951 is a juvenile (Charig & Milner); the humerus and femur ratios disagree by 2.5 times",
  list(M("Charig1997", "BMNH R9951", "Baryonyx_walkeri", "predator", "humerus (left)", "length", 463, loc = "p.43"),
       M("Charig1997", "BMNH R9951", "Baryonyx_walkeri", "predator", "femur", "length", 1200, vt = "estimated", loc = "p.51")))
xc("R9951", "pred", "DME by femur", ba_f); xc("R9951", "pred", "DME by humerus", ba_h)

## ---- small prey
in_lo <- 6; in_hi <- 12
PREY$OConnor2019_01 <- E(sqrt(in_lo * in_hi), in_lo, in_hi, "regressed_from_length", "Hone2022_Meiri2010_unstated",
  "Hone et al. 2022 Table 2: Indrasaurus '6-12 g', 'estimated from SVL using Meri (2010); SVL estimated based on closely related and similar sized (femur length 83% of that of Indrasaurus) specimen of Liushusaurus'; SVL and equation not stated; point = geometric midpoint", TRUE, "",
  list(M("OConnor2019", "STM5-32 (prey)", "Indrasaurus_wangi", "prey", "femur (right)", "length", 13.4, loc = "SI Table S1")))
bv <- meiriS(75); bv_lo <- meiriS(91 * 16.9 / 21.9); bv_hi <- meiriS(max(75, 91 * 18.2 / 22))
PREY$Conrad2017_01 <- E(bv, bv_lo, bv_hi, "regressed_from_length", "Meiri2010_T3_Sauria",
  sprintf("precaudal length (= SVL) 'approximately 75' mm reconstructed by Conrad 2017; Meiri 2010 Sauria; bracket: PCL %.1f-%.1f mm from the Bavarisaurus type (PCL 91 mm) scaled by femur ratios 16.9/21.9 (text) and 18.2/22 (Table 1); Meiri gives no prediction interval, so the bracket covers the input only", 91 * 16.9 / 21.9, max(75, 91 * 18.2 / 22)),
  TRUE, "Ostrom's 'body length 46-47 mm' is not an SVL: his value for the Bavarisaurus type is 52 mm against Conrad's PCL 91 mm (conflict 20)",
  list(M("Conrad2017", "SNSB-BSPG AS I 563b", "Bavarisaurus", "prey", "body", "precaudal length", 75, vt = "estimated", loc = "PDF p.46, 'reconstructed as approximately 75 and 250 mm'"),
       M("Conrad2017", "SNSB-BSPG AS I 563b", "Bavarisaurus", "prey", "femur", "length", 18.2, loc = "Table 1 (text: 16.9)")))
fishE <- function(Llo, Lhi, src, spec, tax, loc) E(froese(FR_A["fusiform"], (Llo + Lhi) / 2), froese(FR_P05["fusiform"], Llo), froese(FR_P95["fusiform"], Lhi), "regressed_from_length", "Froese2006_T1_fusiform",
  sprintf("RUNG 1. Fish body length %s-%s mm estimated by the authors (taken as total length); W = a3.0 L^3 with the fusiform median a3.0 0.0137 (Froese 2006 Table 1) at the midpoint; interval = 5th percentile a3.0 (0.0061) at the lower length to 95th percentile (0.0240) at the upper, so it covers form variation and the length bracket, not the unknown length error", Llo, Lhi),
  TRUE, "", list(M(src, spec, tax, "prey", "body", "length", (Llo + Lhi) / 2, vt = "estimated", loc = loc)))
PREY$DalSasso2011_07 <- fishE(40, 50, "DalSasso2011", "SBA-SA 163760 (prey 1)", "Teleostei", "p.182 '4-5 cm long from the size (0.9 mm) of the scales'")
PREY$DalSasso2011_05 <- fishE(20, 30, "DalSasso2011", "SBA-SA 163760 (prey 2)", "Clupeomorpha", "p.182 'no more than 2-3 cm in length'")
PREY$Zheng2014_01 <- fishE(70, 80, "Zheng2014", "STM9-15 (crop fish)", "Jinanichthys", "Results, Fig. 1 text: 'would have measured 70-80 mm in body length'")
PREY$OConnor2011_01 <- E(65, 60, 70, "species_average_literature", "",
  "O'Connor et al. 2011: 'the enantiornithine, roughly estimated to be 60-70 g (30)', ref 30 = Sanz et al. 2002 (wing loading); a literature value for enantiornithines, not this individual; point = midpoint", FALSE,
  "Hone 2022 gives the prey ulna as 10.5 mm (not checked against the figure)")
PREY$Hone2022_01 <- E(sqrt(21 * 30), 21, 30, "species_average_literature", "",
  "Hone et al. 2022: 'The mid range estimates for Eomaia (digit length 9.1 mm) at 30 g and that of Yanoconodon (digit length 7.5mm) at 21 g define a reasonable size bracket' (masses from Meng et al. 2017, not on disk); full range 13-43 g; point = geometric midpoint", FALSE,
  sprintf("DME by pedal digit I (about 9 mm) from Eomaia = %s and Yanoconodon = %s; Larsson et al. 2010: '20 - 25 g'", gg(dme(30, 9.1, 9)), gg(dme(21, 7.5, 9))),
  list(M("Hone2022", "IVPP V12330 (prey)", "Mammalia", "prey", "pes digit I (including ungual)", "length", 9, vt = "approximate", loc = "PDF p.3, 'about 9 mm'")))

## ------------------------------------------------------------------
## Entries: one row per step-1 entry (pairs and exclusions)
## ------------------------------------------------------------------
AGE <- list(   # age_ma, lo, hi, note
  Hu2005 = c(133.5, 128, 139, "basal Yixian Fm: 'older than 128 and younger than 139 million years' (Hu et al. 2005)"),
  Han2023 = c(133.5, 128, 139, "Lujiatun bed, Yixian Fm; Han et al. give no number; range from Hu et al. 2005 for the same bed"),
  Y125 = c(125, NA, NA, "Yixian Fm; 125 Ma as stated for the formation by Hwang et al. 2004 ('125 Myr') and Qiu et al. 2025 ('125 Ma')"),
  J120 = c(120, NA, NA, "Jiufotang Fm; 'approximately 120 Ma' (Zhou et al. 2009, 2014; O'Connor et al. 2019)"),
  Dalsatt2006 = c(120.3, 119.6, 121, "'40Ar/39Ar date of 120.3 +/- 0.7' for the Jiufotang Fm at Chaoyang (Dalsatt et al. 2006)"),
  Kear2006 = c(106.75, 100.5, 113, "Albian (Toolebuc Fm); stage bounds from the ICS chart (not on disk)"),
  Zheng2018 = c(158.15, 154.8, 161.5, "'Oxfordian Tiaojishan Formation' (Zheng et al. 2018); stage bounds from the ICS chart (not on disk)"),
  Nesbitt2006 = c(210, NA, NA, "'Upper Triassic (210 Myr old) Chinle Formation' (Nesbitt et al. 2006)"),
  Varricchio2001 = c(77.85, 72.1, 83.6, "Campanian (lower Two Medicine Fm); stage bounds from the ICS chart (not on disk)"),
  Therrien2023 = c(75.3, NA, NA, "'Dinosaur Park Formation (~75.3 Ma)' (Therrien et al. 2023)"),
  Godoy2014 = c(88.75, 83.6, 93.9, "Adamantina Fm, 'Late Cretaceous'; Turonian-Santonian as given in the title of the Adamantina paper Godoy et al. cite (Iori & Carvalho 2011); ICS bounds"),
  Wilson2010 = c(67.5, 66, 72.1, "Lameta Fm, Maastrichtian; '67.5 million years' is the Deccan onset Wilson et al. use to date it; range = Maastrichtian (ICS)"),
  LateMaas = c(67.5, 66, 69.05, "Hell Creek Fm, 'Late/Upper Maastrichtian'; upper half of the Maastrichtian (ICS chart, not on disk)"),
  MidCamp = c(77.85, 72.1, 83.6, "'middle Campanian' Djadokhta Fm (Carpenter 1998); Campanian bounds (ICS)"),
  White2022 = c(93, 92.5, 104, "'ca 93 Ma'; 'Cenomanian (92.5-104 Ma)' (White et al. 2022)"),
  Tith = c(147.1, 145, 149.2, "Solnhofen Fm, Tithonian (Conrad 2017; Ostrom 1978); ICS bounds"),
  UTith = c(146.05, 145, 147.1, "'upper Tithonian of Canjuers' (Peyer 2006); upper half of the Tithonian (ICS)"),
  Albian = c(106.75, 100.5, 113, "'Lower Cretaceous (Albian) of Pietraroja' (Dal Sasso & Maganuco 2011); ICS bounds"),
  Yan = c(122.5, 120, 125, "horizon not stated; yanornithids come from the 'Yixian and Jiufotang formations (125 Ma-120 Ma)' (Wang et al. 2020)"),
  Jehol = c(125.5, 120, 131, "Jehol Biota '131-120 Ma' (Zhou & Wang 2010); no horizon for this specimen"),
  Barr = c(123.6, 121.4, 125.8, "'Wealden (Barremian, Lower Cretaceous)' (Charig & Milner 1997); ICS bounds"))

# one list per counted entry: entry, source, pred taxon, prey taxon, pred specimen, prey specimen, obs, outcome, ind, n, stages, age key, pred est key, prey est (or NONE), quote, notes
P <- list()
A <- function(entry, src, pt, yt, ps, ys, obs, out, ind, n, pst, yst, age, pk, prey, quote, notes = "", pred_present = TRUE)
  P[[length(P) + 1]] <<- list(entry = entry, src = src, pt = pt, yt = yt, ps = ps, ys = ys, obs = obs, out = out, ind = ind, n = n, pst = pst, yst = yst,
                              age = age, pk = pk, prey = prey, quote = quote, notes = notes, pred_present = pred_present)
noP <- function(why) NONE(why)

A("Hu2005_01", "Hu2005", "Repenomamus_robustus", "Psittacosaurus", "IVPP V13605", "", "stomach_content", "consumed", TRUE, 1, "adult", "juvenile", "Hu2005", "V13605", PREY$Hu2005_01,
  "'a patch of small bones was revealed within the ribcage, on the ventral sides of the posterior left thoracic ribs and vertebrae, where the stomach is positioned in extant mammals' (p.151)",
  "Prey stage: Hu et al. 'not from an embryo' (worn teeth); Han et al. 2023 call it 'perinatal' without new evidence (conflict 7).")
A("Han2023_01", "Han2023", "Repenomamus_robustus", "Psittacosaurus_lujiatunensis", "WZSSM VF000011 (mammal)", "WZSSM VF000011 (dinosaur)", "combat_association", "unknown", TRUE, 1, "subadult", "juvenile", "Han2023", "WZSSM", PREY$Han2023_01,
  "'The mammal died while biting two of the dinosaur's left anterior dorsal ribs' (p.2)",
  "Prey about 3 times the predator's mass. Prey stage: 'at least 6.5 years, and more likely closer to 10 years' (immature).")
A("Chen1998_01", "Chen1998", "Sinosauropteryx_prima", "Lacertilia", "NIGP 127587", "", "stomach_content", "consumed", TRUE, 1, "adult", "unknown", "Y125", "NIGP127587", noP("no prey measurement is published; Smithwick 2017 Fig. S2 (20 mm scale bar) could be measured, but no lizard femur-to-mass or skull-to-mass equation is on disk"),
  "'stomach contents preserved within the rib cage. This consists of a semi-articulated skeleton of a lizard, complete with skull' (Chen et al. 1998)",
  "Smithwick et al. 2017: closest to Dalinghosaurus (cf.); Currie2001_01 and Smithwick2017_01 are the same item. Predator stage: Currie & Chen 2001 'young adult'.")
A("Hurum2006_01", "Hurum2006", "Sinosauropteryx_prima", "Zhangheotherium", "CAGS GMV 2124", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Y125", "GMV2124", noP("no measurement of the jaws is published; no literature mass for Zhangheotherium is on disk"),
  "'in the abdomen of a specimen (CAGS GMV 2124) of the feathered dinosaur Sinosauropteryx prima, three lower jaws of mammals have been preserved ... Two of them belong to Zhangheotherium, the third to the multituberculate Sinobaatar' (Hurum et al. 2006)",
  "Decision 5: predator kept as S. prima (Ji & Ji 1997b; Currie & Chen 2001; Hurum 2006); Cau 2024 (followed by Qiu 2025) places NGMC 2124 among early tyrannosauroids. Mass reported under both identities (see basis).")
A("Hurum2006_02", "Hurum2006", "Sinosauropteryx_prima", "Sinobaatar", "CAGS GMV 2124", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Y125", "GMV2124", noP("no measurement of the jaw is published; no literature mass for Sinobaatar is on disk"),
  "'the third to the multituberculate Sinobaatar (Hu and Wang 2002)' (Hurum et al. 2006)", "As Hurum2006_01.")
A("Dalsatt2006_01", "Dalsatt2006", "Confuciusornis_sanctus", "Jinanichthys", "IVPP V13313", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Dalsatt2006", "", noP("seven to nine vertebrae in a 6 mm cluster; no fish length"),
  "'Partly underlying the ventral region of the seventh and eighth cervical vertebrae is a small cluster of fish bones' (Dalsatt et al. 2006)",
  "Authors: a pellet 'about to be regurgitated' or crop contents; coded stomach_content (inside the body). Prey 'tentatively' Jinanichthys. Predator mass empty: the paper gives no measurements of IVPP V13313.")
for (k in 1:3) A(sprintf("Kear2006_%02d", k), "Kear2006", "Notochelone", "Inoceramidae", c("AM F87832", "QM F49190", "SAM P41107")[k], "", "stomach_content", "consumed", TRUE, NA, "unknown", "unknown", "Kear2006", "",
  noP("bivalve shell fragments 5-20 mm; no estimate of individual bivalve size or number"),
  "'The turtle gastric residues ... typically comprise irregular, light grey phosphatic masses (between 20 and 30 mm thick) positioned in the posterior section of the body cavity' (Kear 2006)",
  "Invertebrate prey (inoceramid bivalves). Predator mass empty: 'small (<1 m total length)' only, and no turtle equation is on disk; the 'maximum carapace lengths ~50-60 mm' is printed as mm (conflict 9).")
A("Zhou2014_01", "Zhou2014", "Piscivoravis_lii", "Teleostei", "IVPP V17078", "", "stomach_content", "consumed", TRUE, 1, "subadult", "unknown", "J120", "V17078", noP("13 x 8 mm cluster of disarticulated bones; no fish length"),
  "'A small cluster of fish bones is present near the dentary; approximately four to five vertebrae and a few ribs are visible' (Zhou et al. 2014)", "Oral/oesophageal cluster; relation to the stomach cluster (Zhou2014_02) unknown.")
A("Zhou2014_02", "Zhou2014", "Piscivoravis_lii", "Teleostei", "IVPP V17078", "", "stomach_content", "consumed", TRUE, 1, "subadult", "unknown", "J120", "V17078", noP("no fish length"),
  "'A much larger cluster of fish bones is located in the abdomen, laterally overlain by the thoracic ribs and uncinate processes' (Zhou et al. 2014)", "")
A("Huang2026_01", "Huang2026", "Kunpengornis_anhuimusei", "Teleostei", "AGB 15483", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "J120", "AGB15483", noP("no fish length"),
  "'A cluster of disarticulated and fragile fish vertebrae and ribs can be recognized in the abdominal cavity' (Huang et al. 2026)", "Ostracods in the abdomen excluded by the authors as not digested.")
A("Xing2013_01", "Xing2013", "Microraptor_gui", "Teleostei", "QM V1002", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "J120", "QMV1002",
  noP("disarticulated fin rays (up to 6.7 mm), vertebrae and cranial fragments; Hone 2022's 90 g rests on comparing the 3.5 x 1.6 cm bone mass with an Anchiornis pellet (Freimuth et al. 2021, not on disk), which is not a body-size estimate"),
  "'a dense elliptical mass of disarticulated osseous elements ... primarily composed of long (up to 6.7 mm) and thin (0.1-0.3 mm in diameter) fin rays, ribs, and neural/hemal spines of fish' (Xing et al. 2013)",
  "Intestinal position ('most probably within or near the jejunum').")
A("OConnor2011_01", "OConnor2011", "Microraptor_gui", "Enantiornithes", "IVPP V17972", "", "stomach_content", "consumed", TRUE, 1, "unknown", "adult", "J120", "V17972", PREY$OConnor2011_01,
  "'Inside the rib cage of the theropod, bird remains consist of an articulated left humerus and antebrachium and both feet' (O'Connor et al. 2011)",
  "Printed 'IVPP V13972' in SI Table S1 (conflict 10). Prey 'tentatively' Cathayornithiformes; 'the animal was an adult, or very near so'.")
A("OConnor2019_01", "OConnor2019", "Microraptor_zhaoianus", "Indrasaurus_wangi", "STM5-32", "STM5-32 (Indrasaurus holotype)", "stomach_content", "consumed", TRUE, 1, "unknown", "juvenile", "J120", "STM532", PREY$OConnor2019_01,
  "'partial articulated skeleton preserved in the stomach of a nearly complete and fully articulated Microraptor' (O'Connor et al. 2019)", "Prey 'juvenile to subadult'.")
for (k in 1:3) A(sprintf("Zheng2018_%02d", k), "Zheng2018", "Anchiornis", "Lacertilia", "STM0-179", "", "pellet", "consumed", TRUE, 1, "unknown", "unknown", "Zheng2018", "STM0-179",
  noP("femora and humeri are drawn in Fig. 1c (10 mm scale) but not measured, and no lizard limb-to-mass equation is on disk"),
  "'a coherent oval structure, lying ventral to the anterior cervical vertebrae, that we interpret as a pellet lodged in the oesophagus' (Zheng et al. 2018)",
  paste0(c("Large lizard A", "Large lizard B", "Small lizard")[k], " of at least three in one pellet. Coded pellet: a formed pellet in transit; it lies inside the body. Pellet size disputed (conflict 2)."))
for (k in 4:6) A(sprintf("Zheng2018_%02d", k), "Zheng2018", "Anchiornis", "Ptycholepidae", c("STM0-224", "STMA0-4", "STM0-38")[k - 3], "", "pellet", "consumed", TRUE, NA, "unknown", "unknown", "Zheng2018", c("STM0-224", "STMA0-4", "STM0-38")[k - 3],
  noP("fish scales and fragments only"),
  "'In each of the Anchiornis specimens STM0-38, STM0-224 and STMA0-4, a highly compact oval structure is preserved near the skeleton' (Zheng et al. 2018)",
  if (k == 5) "STMA0-4 'displays poorly fused skeletal elements' (juvenile); femur 35 mm estimated." else "")
for (k in 7:8) A(sprintf("Zheng2018_%02d", k), "Zheng2018", "Anchiornis", "Ptycholepidae", "", c("STM0-227", "STM0-228")[k - 6], "pellet", "consumed", FALSE, NA, "unknown", "unknown", "Zheng2018", "",
  noP("fish scales and fragments only"),
  "'STM0-227 and STM0-228 ... discovered in isolation ... almost entirely composed of ptycholepid fish scales with a few fish bone fragments' (Zheng et al. 2018)",
  "Isolated pellet attributed to Anchiornis by context; producer individual unknown, so individual_pair FALSE.", pred_present = FALSE)
A("Xing2012_01", "Xing2012", "Sinocalliopteryx_gigas", "Sinornithosaurus", "JMP-V-05-8-01", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Y125", "JMP", PREY$Xing2012_01,
  "'The visible limb elements include the tibia, fibula, metatarsals III and IV, and numerous phalanges' ... 'it is to this genus [Sinornithosaurus] that the limb is tentatively referred' (Xing et al. 2012)",
  "Prey ID tentative (cf.). The feather mass Xing2012_02 is excluded as probably the same prey.")
for (k in 4:5) A(sprintf("Xing2012_%02d", k), "Xing2012", "Sinocalliopteryx_gigas", "Confuciusornis_sanctus", "CAGS-IG-T1", "", "stomach_content", "consumed", TRUE, 1, "unknown", "adult", "Y125", "CAGST1",
  noP("no measurement of the confuciusornithid bones is published; no species mean for C. sanctus is on disk (Benson 2014 S11 lists only MOR 1063, 'near-maximal-sized', 0.475 kg)"),
  "'Four scapulocoracoids indicate the presence of at least two individuals' (Xing et al. 2012)", "Fused scapulocoracoids; at least two individuals, one row each.")
A("Xing2012_06", "Xing2012", "Sinocalliopteryx_gigas", "Ornithischia", "CAGS-IG-T1", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Y125", "CAGST1",
  noP("one incomplete scapula (135 x 65 mm, preserved); identity 'contentious' and no scapula-based equation is on disk"),
  "'The largest bone, tentatively identified as a scapula, measures 13.5 cm in maximum length and is 6.5 cm wide' (Xing et al. 2012)", "Eaten before the confuciusornithids (digestion disparity).")
A("Nesbitt2006_01", "Nesbitt2006", "Coelophysis_bauri", "Crocodylomorpha", "AMNH FR 7224", "AMNH FR 30616", "stomach_content", "consumed", TRUE, 1, "adult", "unknown", "Nesbitt2006", "AMNH7224",
  noP("proximal femora, ilium and a sacral vertebra only; no length is published (the old table's 71.2 mm femur is untraced, conflict 11)"),
  "'We have identified well-preserved remains consisting of left and right proximal ends of femora, a left ilium and a sacral vertebra within the stomach cavity' (Nesbitt et al. 2006)", "cf. Hesperosuchus (ESM).")
A("Varricchio2001_01", "Varricchio2001", "Daspletosaurus", "Hadrosauridae", "OTM 200", "OTM 201 (caudals)", "stomach_content", "consumed", TRUE, 1, "unknown", "juvenile", "Varricchio2001", "", PREY$Varricchio2001_01,
  "'All three vertebrae represent individuals about 3 m in total body length' (Varricchio 2001)",
  "Predator mass empty: OTM 200 has no limb bones ('Notably absent are the cranium, shoulder girdle and all limb elements'); Hone 2022's femur 1030 mm is AMNH 5438 (conflict 23). Association with digestive corrosion, not enclosed by ribs.")
A("Varricchio2001_02", "Varricchio2001", "Daspletosaurus", "Hadrosauridae", "OTM 200", "OTM 201 (dentary)", "stomach_content", "consumed", TRUE, 1, "unknown", "juvenile", "Varricchio2001", "", PREY$Varricchio2001_02,
  "'Its small size (maximum anterior-posterior breadth of the coronoid = 15.8 mm) indicates a very young animal roughly 1 to 1.2 m in length' (Varricchio 2001)", "As Varricchio2001_01.")
A("Therrien2023_01", "Therrien2023", "Gorgosaurus_libratus", "Citipes_elegans", "TMP 2009.12.14", "anterior Citipes", "stomach_content", "consumed", TRUE, 1, "juvenile", "juvenile", "Therrien2023", "TMP2009", PREY$Therrien2023_01,
  "'postcranial elements, primarily from the hindlimbs, of two separate individuals of the small caenagnathid theropod Citipes elegans' (Therrien et al. 2023)", "Printed 'TMP 2009.14.12' in SI Table S2. Both Citipes 'within their first year of life'.")
A("Therrien2023_02", "Therrien2023", "Gorgosaurus_libratus", "Citipes_elegans", "TMP 2009.12.14", "posterior Citipes", "stomach_content", "consumed", TRUE, 1, "juvenile", "juvenile", "Therrien2023", "TMP2009", PREY$Therrien2023_02,
  "'the posterior individual was consumed first' (Therrien et al. 2023)", "")
A("Godoy2014_01", "Godoy2014", "Aplestosuchus_sordidus", "Sphagesauridae", "LPRP/USP 0229a", "LPRP/USP 0229b", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Godoy2014", "Aplesto", PREY$Godoy2014_01,
  "'the sphagesaurid remains positioned between the gastralia and the left ribs are best interpreted as abdominal contents' (Godoy et al. 2014)", "Prey length 55 cm (Text S1) vs 'about 60 cm' (main text), conflict 8.")
A("Wilson2010_01", "Wilson2010", "Sanajeh_indicus", "Titanosauria", "GSI/GC/2901-2906", "GSI/GC/2904", "combat_association", "unknown", TRUE, 1, "adult", "hatchling", "Wilson2010", "Sanajeh",
  noP("the hatchling is 'approximately 0.5 m long' with no measured element and no sauropod length-mass equation is on disk; the associated egg GSI/GC/2905 is 2,316 g (Text S5), which bounds a fresh hatchling only if it had not yet grown"),
  "'The titanosaur hatchling was approximately 0.5 m long, or one-seventh the length of Sanajeh (3.5 m long)' (Wilson et al. 2010)", "Ethofossil: snake coiled around a crushed egg next to the hatchling; no ingestion.")
A("DePalma2013_01", "DePalma2013", "Tyrannosaurus_rex", "Edmontosaurus", "PBMNH.P.09.039 (tooth crown embedded in the prey)", "two coalesced caudals", "bite_trace", "attempted_failed", TRUE, 1, "subadult", "adult", "LateMaas", "",
  PREY$DePalma2013_01,
  "'Two coalesced hadrosaur (compare with Edmontosaurus annectens) caudal vertebrae' with an embedded tooth 'indistinguishable in morphology, size, and denticle character from known T. rex subadults' (DePalma et al. 2013)",
  "Predator mass empty: only a tooth crown, and no tooth-to-mass equation is on disk. Prey survived (healed).")
A("Carpenter1998_01", "Carpenter1998", "Velociraptor_mongoliensis", "Protoceratops_andrewsi", "GI SPS 100/25", "GI SPS 100/512", "combat_association", "unknown", TRUE, 1, "unknown", "unknown", "MidCamp", "GISPS",
  noP("only a body length ('120-130 cm long', from Barsbold 1974) is given and no Protoceratops length-mass equation is on disk"),
  "'The Velociraptor (GI SPS 100/25; 170 cm long ...) is lying on its right side ... almost parallel to the Protoceratops (GI SPS 100/512; 120-130 cm long ...)' (Carpenter 1998)", "Benson 2014 S11 lists the same Velociraptor as MPD-C 100/25.")
A("Carpenter1998_02", "Carpenter1998", "Tyrannosaurus_rex", "Edmontosaurus_annectens", "", "DMNH 1493", "bite_trace", "attempted_failed", FALSE, 1, "unknown", "adult", "LateMaas", "", PREY$Carpenter1998_02,
  "'neural spines of caudal vertebrae 13-17 have suffered trauma' ... 'The hadrosaur undoubtedly survived the attack' (Carpenter 1998)",
  "Attacker inferred from bite size ('believed to have been Tyrannosaurus'); no predator specimen, so individual_pair FALSE and no predator mass.", pred_present = FALSE)
A("Qiu2025_01", "Qiu2025", "Huadanosaurus_sinensis", "Gobiconodontidae", "IVPP V 14202", "", "stomach_content", "consumed", TRUE, 1, "juvenile", "unknown", "Y125", "V14202",
  noP("two mandibles in SI Fig. S7 (5 mm scale bar) but no measurement; no literature mass for Meemannodon is on disk"),
  "'the dorsal patch consists of the skeletal elements of the eutriconodonts, which are more robust than those of the eutherian in the ventral patch' (Qiu et al. 2025)", "Closest to Meemannodon lujiatunensis. Predator 'may represent a juvenile'.")
A("Qiu2025_02", "Qiu2025", "Huadanosaurus_sinensis", "Eutheria", "IVPP V 14202", "", "stomach_content", "consumed", TRUE, 1, "juvenile", "unknown", "Y125", "V14202",
  noP("'the eutherian content is badly preserved'; no measurement"), "'the mammals were swallowed in as a whole' (Qiu et al. 2025)", "")
A("White2022_01", "White2022", "Confractosuchus_sauroktonos", "Ornithopoda", "AODF0890", "", "stomach_content", "consumed", TRUE, 1, "unknown", "juvenile", "White2022", "AODF0890", PREY$White2022_01,
  "'The partially-digested remains of a juvenile ornithopod dinosaur were found concentrated in the anterior part of the abdominal cavity' (White et al. 2022)",
  "Conflict 4 open: the authors' 1.0-1.7 kg cannot be traced to an input, and the preserved tibia implies roughly 0.19-0.24 kg by DME. Use with caution.")
A("Conrad2017_01", "Conrad2017", "Compsognathus_longipes", "Bavarisaurus", "SNSB-BSPG AS I 563", "SNSB-BSPG AS I 563b", "stomach_content", "consumed", TRUE, 1, "subadult", "juvenile", "Tith", "BSP563", PREY$Conrad2017_01,
  "'Compsognathus longipes clearly ate S. dyspepsia. As preserved, the S. dyspepsia clearly overlies the right ribs of C. longipes and is overlaid by the left ribs' (Conrad 2017)",
  "Decision 5: prey kept as Bavarisaurus cf. macrodactylus (Ostrom 1978, first identification); Conrad 2017 names it the holotype of Schoenesmahl dyspepsia (one paper). Ostrom1978_01 is the same item. Predator stage: Ostrom 'suggest, but do not prove, that this is not a fully adult individual'.")
dsq <- c(`01` = "Vertebrata_indet", `02` = "Lepidosauria", `03` = "Lepidosauria", `04` = "Osteichthyes", `05` = "Clupeomorpha", `06` = "Vertebrata_indet", `07` = "Teleostei")
dsquote <- c(`01` = "'a small and elongated cluster of tiny yellowish, flat, sometimes angular, shiny elements' in the oesophagus",
             `02` = "'we interpret mesopodium and metapodium as belonging to a still-articulated ankle and pes, with a mediolateral width less than 3 mm' (stomach)",
             `03` = "'Two adjacent clusters ... polygonal-to-roundish elements ... integumentary squamation' (duodenum)",
             `04` = "'a fish vertebra in dorsal view' (duodenum, descending loop)",
             `05` = "'a rounded, 3 mm-diameter cluster of dozens of small cylindrical elements ... likely vertebral centra of a very small fish' (jejunum)",
             `06` = "'Two even smaller roundish fragments ... origin is impossible to determine' (jejunum-ileum)",
             `07` = "'a faecal pellet with a number of tightly bound, scaly elements ... diagnostic of teleostean fishes' (rectum)")
dsnone <- c(`01` = "indeterminate elements; probably the authors' prey 5 ('smaller vertebrates of uncertain affinities')",
            `02` = "authors give total length 15-40 cm ('half that of Scipionyx'); no SVL and no length-mass equation for total length is on disk",
            `03` = "authors give 'no more than 10-12 cm long' (total length) from scale size; no SVL", `04` = "isolated vertebra; possibly part of prey 2 or 5",
            `06` = "origin indeterminate")
for (k in names(dsq)) A(paste0("DalSasso2011_", k), "DalSasso2011", "Scipionyx_samniticus", dsq[[k]], "SBA-SA 163760", "", "stomach_content", "consumed", TRUE,
  if (k %in% c("01", "04", "06")) NA else 1, "hatchling", "unknown", "Albian", "Scip", if (!is.null(PREY[[paste0("DalSasso2011_", k)]])) PREY[[paste0("DalSasso2011_", k)]] else noP(dsnone[[k]]),
  paste0(dsquote[[k]], " (Dal Sasso & Maganuco 2011 pp.176-183)"),
  c(`01` = "Oesophagus; possibly regurgitated; probably the authors' prey 5.", `02` = "Authors' prey 4.", `03` = "Authors' prey 3.", `04` = "Not assigned by the authors.",
    `05` = "Authors' prey 2.", `06` = "Not assigned by the authors.", `07` = "Authors' prey 1 ('one or more teleostean fish').")[[k]])
A("Zhou2002_01", "Zhou2002", "Yanornis_martini", "Teleostei", "IVPP V13259", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "Yan", "V13259", noP("macerated fish remains; no fish length"),
  "'contains preserved macerated fish remains, including a teleost vertebra, fin rays and opercular fragments' (Zhou et al. 2002)",
  "Decision 5: kept as Yanornis martini; Wang et al. 2020 refer V13259 to Abitusavis lii (one paper; Huang 2026 by citation). Zheng2014_20 and Wang2020_01 are the same item. Ventriculus position (Zheng 2014 Table 1).")
crop <- list(`01` = c("STM9-15", "Jinanichthys", "crop", 2, "'STM9-15 preserves at least two complete fish, which we estimate would have measured 70-80 mm in body length'"),
             `02` = c("STM9-15", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"),
             `03` = c("STM9-18", "Teleostei", "crop", 1, "Table 1: crop 'Whole fish, disarticulated fish bones'"),
             `04` = c("STM9-18", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"),
             `05` = c("STM9-19", "Protopsephurus", "crop", 1, "'a large patch of microctenoid fish scales are preserved in articulation, referable to the basal paddlefish Protopspherus sp.'"),
             `06` = c("STM9-19", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"),
             `07` = c("STM9-26", "Jinanichthys", "crop", 1, "Table 1: crop '>=1 whole fish, soft tissue'"),
             `08` = c("STM9-31", "Jinanichthys", "crop", 1, "Fig. 3C: 'an entire fish, referable to Jinanichthys, preserved in the crop of STM9-31'"),
             `09` = c("STM9-31", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"),
             `10` = c("STM9-37", "Teleostei", "crop", 1, "Table 1: crop 'Fish bones'"),
             `11` = c("STM9-46", "Teleostei", "crop", 1, "Table 1: crop 'Disarticulated fish bones'"),
             `12` = c("STM9-46", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"),
             `13` = c("STM9-49", "Jinanichthys", "crop", 1, "Table 1: crop '1 whole fish'"),
             `14` = c("STM9-49", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones (fine)'"),
             `15` = c("STM9-51", "Teleostei", "crop", 1, "Table 1: crop 'Disarticulated fish bones'"),
             `16` = c("STM9-51", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones and gastroliths'"),
             `17` = c("STM9-52", "Teleostei", "crop", 1, "Table 1: crop 'Disarticulated fish bones'"),
             `18` = c("STM9-52", "Teleostei", "ventriculus", 1, "Table 1: ventriculus 'Disarticulated fish bones'"))
for (k in names(crop)) { cc <- crop[[k]]; e <- paste0("Zheng2014_", k)
  A(e, "Zheng2014", "Yanornis", cc[2], cc[1], "", "stomach_content", "consumed", TRUE, as.integer(cc[4]), "unknown", "unknown", "Yan", cc[1],
    if (!is.null(PREY[[e]])) PREY[[e]] else noP("no fish length is published"), paste0(cc[5], " (Zheng et al. 2014)"),
    paste0(if (cc[3] == "crop") "Crop contents." else "Ventriculus contents; relation to the crop fish unknown.",
           if (cc[2] %in% c("Jinanichthys", "Protopsephurus") && k != "08") " Prey ID tentative (cf.)." else "",
           " Decision 5: kept as Yanornis sp.; Wang et al. 2020 call the STM9 birds Yanornithidae indet."))
}
A("Zhou2009_01", "Zhou2009", "Jianchangornis_microdonta", "Teleostei", "IVPP V16708", "", "stomach_content", "consumed", TRUE, 1, "subadult", "unknown", "J120", "V16708", noP("fragmentary fish remains; no length"),
  "'Although there is no compelling evidence to argue that they were the diets of the new avian, their extremely fragmentary status, their position not much distant from the stomach region ... may suggest that this bird was piscivorous' (Zhou et al. 2009, English summary)",
  "WEAK: associated, not demonstrably ingested; fish lie near the left femur and tibiotarsus. Possibly chimaeric holotype (O'Connor 2019 per Huang 2026; not on disk).")
A("Zhou2010_01", "Zhou2010", "Yabeinosaurus", "Teleostei", "", "", "stomach_content", "consumed", FALSE, NA, "unknown", "unknown", "Jehol", "", NONE("no description; no specimen", from_animal = FALSE),
  "'one newly discovered fossil actually preserved in its stomach some fish remains, suggesting at least some of them were piscivorous (Y. Wang, pers. observ.)' (Zhou & Wang 2010, p.1900)",
  "WEAK: personal observation in a review; no specimen number, figure or measurement. individual_pair FALSE because no specimen can be named.", pred_present = FALSE)
A("Hone2022_01", "Hone2022", "Microraptor_zhaoianus", "Mammalia", "IVPP V12330", "", "stomach_content", "consumed", TRUE, 1, "unknown", "unknown", "J120", "V12330", PREY$Hone2022_01,
  "'An articulated mammalian right foot is preserved within the thoracic cavity' (Hone et al. 2022)",
  "Larsson2010_01 is the same item (superseded abstract). Hone et al.: 'the pes was ingested during late-stage carcass consumption, and potentially in a scavenging event'.")
A("Peyer2006_01", "Peyer2006", "Compsognathus_longipes", "Lepidosauria", "MNHN CNJ 79", "", "stomach_content", "consumed", TRUE, 2, "unknown", "unknown", "UTith", "CNJ79",
  noP("imprints; 'Their description and interpretation is, however, beyond the scope of this paper'"),
  "'Disparate sizes of cervical and dorsal vertebrae, as well as scattered placement of a coracoid and pelvic elements among the gastric contents, suggests multiple individuals within the gastric area. The remains might belong to sphenodontids or lizards' (Peyer 2006, p.895)",
  "n_items = 2 is the minimum implied by 'multiple individuals'. Ostrom1978_02 is the same item. Primary reports (Bidar et al. 1972a,b; Michard 1991) not on disk.")
A("Hwang2004_01", "Hwang2004", "Huaxiagnathus_orientalis", "Vertebrata_indet", "CAGS-IG02-301", "", "stomach_content", "consumed", TRUE, NA, "juvenile", "unknown", "Y125", "CAGS301", noP("unidentified rounded objects averaging 7 mm"),
  "'The objects are most likely chunks of a partially digested meal' (Hwang et al. 2004, p.18)", "Prey taxon unknown; texture 'the same as the bone of the skeleton'.")
A("Charig1997_01", "Charig1997", "Baryonyx_walkeri", "Lepidotes", "BMNH R9951", "", "stomach_content", "consumed", TRUE, 1, "juvenile", "unknown", "Barr", "R9951", noP("scales and teeth only; no size"),
  "'Acid-etched scales and teeth of the common Mesozoic fish Lepidotes (Fig. 47)' (Charig & Milner 1997, p.61)", "")
A("Charig1997_02", "Charig1997", "Baryonyx_walkeri", "Iguanodon", "BMNH R9951", "", "stomach_content", "consumed", TRUE, 1, "juvenile", "juvenile", "Barr", "R9951",
  noP("'a young Iguanodon'; elements listed by block (Appendix B) but none measured"),
  "'The disarticulated skeletal remains of a young Iguanodon' (Charig & Milner 1997, p.61)", "'whether this was the result of active predation or scavenging cannot be determined'.")

## ------------------------------------------------------------------
## Build pairs
## ------------------------------------------------------------------
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "10b_fossil_topup.R"))   # Phase 3b: TOPUP
nz <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
RUNG <- list()
pr_rows <- list(); ent_rows <- list(); meas_specs <- list()
for (p in P) {
  pe <- if (nzchar(p$pk)) PRED[[p$pk]] else NONE("", from_animal = p$pred_present)
  if (is.null(pe)) stop("no predator estimate ", p$pk)
  if (!nzchar(p$pk) && p$pred_present && !nzchar(pe$xcheck)) pe$xcheck <- "no predator mass (see notes)"
  ye <- p$prey
  tpx <- TOPUP[[p$entry]]
  if (!is.null(tpx$pred)) pe <- tpx$pred
  if (!is.null(tpx$prey)) ye <- tpx$prey
  rp <- if (!is.null(tpx) && !is.na(tpx$rung_pred)) tpx$rung_pred else ""
  ry <- if (!is.null(tpx) && !is.na(tpx$rung_prey)) tpx$rung_prey else ""
  pm <- if (is.na(pe$point) && is.na(pe$lo)) "" else pe$mm; ym <- if (is.na(ye$point) && is.na(ye$lo)) "" else ye$mm
  tier <- tier_of(pm, ym, pred_none_from_animal = p$pred_present, prey_none_from_animal = isTRUE(ye$from_animal))
  ag <- AGE[[p$age]]
  pr_rows[[length(pr_rows) + 1]] <- mk("pairs", pair_id = p$entry, source_id = p$src, pred_taxon_id = p$pt, prey_taxon_id = p$yt,
    pred_specimen = p$ps, prey_specimen = p$ys, observation_type = p$obs, pair_resolution = tier, individual_pair = if (p$ind) "TRUE" else "FALSE",
    pred_mass_g = fnum(pe$point, 4), pred_mass_lo_g = fnum(pe$lo, 4), pred_mass_hi_g = fnum(pe$hi, 4), pred_mass_method = pm,
    pred_mass_method_id = if (nzchar(pm)) pe$mid else "", pred_mass_basis = if (nzchar(pm)) pe$basis else "",
    prey_mass_g = fnum(ye$point, 4), prey_mass_lo_g = fnum(ye$lo, 4), prey_mass_hi_g = fnum(ye$hi, 4), prey_mass_method = ym,
    prey_mass_method_id = if (nzchar(ym)) ye$mid else "", prey_mass_basis = if (nzchar(ym)) ye$basis else "",
    pred_stage = p$pst, prey_stage = p$yst, n_items = if (is.na(p$n)) "" else as.character(p$n), interaction_outcome = p$out, is_fossil = "TRUE",
    age_ma = ag[1], age_ma_lo = nz(ag[2]), age_ma_hi = nz(ag[3]), quote_or_locator = p$quote,
    notes = trimws(paste(p$notes, if (nzchar(pe$xcheck)) paste0("Predator cross-check: ", pe$xcheck, ".") else "",
                         if (nzchar(ye$xcheck)) paste0("Prey: ", ye$xcheck, ".") else "",
                         if (nzchar(ry)) paste0("Phase 3b prey mass rung ", ry, " (see data/fossil_rebuild/step3b_topup.md).") else "",
                         if (nzchar(rp)) paste0("Phase 3b predator mass rung ", rp, ".") else "", paste0("Age: ", ag[4], "."))))
  ent_rows[[length(ent_rows) + 1]] <- data.frame(entry_id = p$entry, pred_specimen = p$ps, pred_taxon = p$pt, prey_taxon = p$yt, observation_type = p$obs, tier = tier,
    pred_mass_g = pe$point, pred_lo_g = pe$lo, pred_hi_g = pe$hi, pred_method_id = if (nzchar(pm)) pe$mid else "", pred_basis = pe$basis, pred_crosscheck = pe$xcheck,
    prey_mass_g = ye$point, prey_lo_g = ye$lo, prey_hi_g = ye$hi, prey_method_id = if (nzchar(ym)) ye$mid else "", prey_basis = ye$basis, prey_note = ye$xcheck,
    ppmr_log10 = if (!is.na(pe$point) && !is.na(ye$point)) log10(pe$point / ye$point) else NA, age_ma = as.numeric(ag[1]))
  hv <- function(e) !(is.na(e$point) && is.na(e$lo))
  why_p <- if (!is.null(tpx) && nzchar(tpx$empty_pred)) tpx$empty_pred else if (!hv(pe)) pe$xcheck else ""
  why_y <- if (!is.null(tpx) && nzchar(tpx$empty_prey)) tpx$empty_prey else if (!hv(ye)) ye$xcheck else ""
  RUNG[[length(RUNG) + 1]] <- data.frame(pair_id = p$entry, predator = p$pt, pred_specimen = p$ps, prey = p$yt, observation_type = p$obs, tier = tier,
    pred_rung = if (nzchar(rp)) rp else if (hv(pe)) "0" else "none", pred_g = pe$point, pred_lo_g = pe$lo, pred_hi_g = pe$hi, pred_method_id = if (nzchar(pm)) pe$mid else "", pred_method = pm,
    pred_basis = if (hv(pe)) pe$basis else "", pred_empty_reason = if (hv(pe)) "" else why_p,
    prey_rung = if (nzchar(ry)) ry else if (hv(ye)) "0" else "none", prey_g = ye$point, prey_lo_g = ye$lo, prey_hi_g = ye$hi, prey_method_id = if (nzchar(ym)) ye$mid else "", prey_method = ym,
    prey_basis = if (hv(ye)) ye$basis else "", prey_empty_reason = if (hv(ye)) "" else why_y)
  for (m in pe$inputs) meas_specs[[length(meas_specs) + 1]] <- c(m, pair = "")
  for (m in ye$inputs) meas_specs[[length(meas_specs) + 1]] <- c(m, pair = p$entry)
}
pairs <- do.call(rbind, pr_rows); ENT <- do.call(rbind, ent_rows)
stopifnot(!anyDuplicated(pairs$pair_id))

## ------------------------------------------------------------------
## Exclusions
## ------------------------------------------------------------------
counted <- pairs$pair_id
ex <- GE[!GE$entry_id %in% counted, ]
reason <- function(e, ev) {
  if (grepl("^no_gut_contents", ev)) return("no gut contents (no-contents specimen)")
  if (grepl("rejected", ev)) return("rejected by the source as not demonstrably gut contents")
  if (grepl("abdominal_inclusion_probably_not_prey", ev)) return("not prey: the authors' unlaid eggs, or intestinal remnants (Wang 2022; Cau 2024); conflict 6")
  if (grepl("gut_content_plant", ev)) return("plant diet; not a predator-prey record")
  if (grepl("gastrolith", ev)) return("gastroliths or sediment only; no food item")
  if (grepl("same item|superseded|identity statement", ev)) return("repeat observation of an item counted under another entry")
  if (e == "Xing2012_02") return("integument mass probably from the same prey as Xing2012_01 (the dromaeosaurid leg transects it); not counted separately")
  "other"
}
counted_as <- c(Smithwick2017_01 = "Chen1998_01", Currie2001_01 = "Chen1998_01", Currie2001_02 = "Chen1998_02 (also excluded)", Currie2001_03 = "Hurum2006_01; Hurum2006_02",
                Cau2024_01 = "Hurum2006_01; Hurum2006_02", Zheng2014_20 = "Zhou2002_01", Wang2020_01 = "Zhou2002_01", Larsson2010_01 = "Hone2022_01",
                Ostrom1978_01 = "Conrad2017_01", Ostrom1978_02 = "Peyer2006_01", Zheng2014_19 = "Zhou2004_01 (also excluded)", Zheng2018b_01 = "Zheng2011_04 (also excluded)",
                Zheng2018b_02 = "Zheng2011_05 (also excluded)", Xing2012_02 = "Xing2012_01")
EXC <- data.frame(entry_id = ex$entry_id, paper_id = ex$paper_id, pred_specimen = ex$pred_specimen, pred_taxon = ex$pred_taxon, gut_item = ex$gut_taxon,
                  step1_evidence_type = ex$evidence_type, reason = mapply(reason, ex$entry_id, ex$evidence_type),
                  counted_as = ifelse(ex$entry_id %in% names(counted_as), counted_as[ex$entry_id], ""), notes = gsub("[\r\n]+", " ", ex$notes))
stopifnot(!any(EXC$reason == "other"), nrow(EXC) + nrow(pairs) == nrow(GE))
write.csv(EXC, file.path(DB, "excluded_fossil_records.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

## ------------------------------------------------------------------
## Sources
## ------------------------------------------------------------------
src_ids <- unique(c(pairs$source_id, vapply(meas_specs, `[[`, "", "src"), "Benson2014"))
pp <- PA[match(src_ids, PA$paper_id), ]
if (anyNA(pp$paper_id)) stop("source not in step1_papers: ", paste(src_ids[is.na(pp$paper_id)], collapse = ", "))
sources <- mk("sources", source_id = pp$paper_id, citation = paste0(pp$citation, ". ", pp$title),
  doi = pp$doi, file_name = ifelse(pp$paper_id == "Benson2014", "journal.pbio.1001853.s011.XLS", pp$file_renamed),
  data_type = ifelse(pp$paper_id == "Benson2014", "dataset", ifelse(pp$paper_id == "Larsson2010", "report", "journal_article")),
  extracted_from = ifelse(pp$paper_id == "Benson2014", "data/fossil_rebuild/step1_measurements.csv (paper_id Benson2014); data/mass_references/journal.pbio.1001853.s011.XLS",
                          paste0("data/fossil_rebuild/step1_gut_entries.csv and step1_measurements.csv; pdfs/fossil_cases/", pp$file_renamed)),
  notes = ifelse(pp$paper_id == "Larsson2010", "conference abstract (SVP 2010)", ""))

## ------------------------------------------------------------------
## Taxa
## ------------------------------------------------------------------
T_ <- function(id, name, rank, cls, ord = "", fam = "", clade = "", vert = TRUE, notes = "")
  mk("taxa", taxon_id = id, accepted_name = name, rank = rank, class = cls, order = ord, family = fam, analysis_clade = clade, is_vertebrate = if (is.na(vert)) "" else if (vert) "TRUE" else "FALSE", notes = notes)
taxa <- rbind(
  T_("Repenomamus_robustus", "Repenomamus robustus", "species", "Mammalia", "Eutriconodonta", "Gobiconodontidae", "Mammalia", notes = "Repenomamidae in Hu et al. 2005; Gobiconodontidae in Han et al. 2023"),
  T_("Sinosauropteryx_prima", "Sinosauropteryx prima", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda", notes = "Fossil. CAGS GMV 2124 (= NGMC 2124) is kept here under decision 5; Cau 2024 places it among early tyrannosauroids as an unnamed taxon"),
  T_("Confuciusornis_sanctus", "Confuciusornis sanctus", "species", "Aves", "Confuciusornithiformes", "Confuciusornithidae", "Avialae", notes = "Fossil Mesozoic bird"),
  T_("Notochelone", "Notochelone", "genus", "Reptilia", "Testudines", "Protostegidae", "Testudines", notes = "Kear 2006: cf. Notochelone sp."),
  T_("Piscivoravis_lii", "Piscivoravis lii", "species", "Aves", "", "", "Avialae", notes = "Fossil ornithuromorph"),
  T_("Kunpengornis_anhuimusei", "Kunpengornis anhuimusei", "species", "Aves", "", "", "Avialae", notes = "Fossil euornithine (Huang et al. 2026)"),
  T_("Microraptor_gui", "Microraptor gui", "species", "Reptilia", "Saurischia", "Dromaeosauridae", "Theropoda"),
  T_("Microraptor_zhaoianus", "Microraptor zhaoianus", "species", "Reptilia", "Saurischia", "Dromaeosauridae", "Theropoda", notes = "O'Connor 2011 Table S1 prints the holotype IVPP V12330 as 'Sinornithosaurus zhaoianus'"),
  T_("Anchiornis", "Anchiornis", "genus", "Reptilia", "Saurischia", "Troodontidae", "Theropoda", notes = "Zheng et al. 2018: Anchiornis sp. (cf. A. huxleyi), 'the troodontid Anchiornis'; often placed in Anchiornithidae"),
  T_("Sinocalliopteryx_gigas", "Sinocalliopteryx gigas", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda"),
  T_("Coelophysis_bauri", "Coelophysis bauri", "species", "Reptilia", "Saurischia", "Coelophysidae", "Theropoda"),
  T_("Daspletosaurus", "Daspletosaurus", "genus", "Reptilia", "Saurischia", "Tyrannosauridae", "Theropoda", notes = "Varricchio 2001: Daspletosaurus sp."),
  T_("Gorgosaurus_libratus", "Gorgosaurus libratus", "species", "Reptilia", "Saurischia", "Tyrannosauridae", "Theropoda"),
  T_("Aplestosuchus_sordidus", "Aplestosuchus sordidus", "species", "Reptilia", "Crocodyliformes", "Baurusuchidae", "Crocodyliformes", notes = "Fossil notosuchian; not crown Crocodylia"),
  T_("Sanajeh_indicus", "Sanajeh indicus", "species", "Reptilia", "Squamata", "Madtsoiidae", "Serpentes"),
  T_("Tyrannosaurus_rex", "Tyrannosaurus rex", "species", "Reptilia", "Saurischia", "Tyrannosauridae", "Theropoda"),
  T_("Velociraptor_mongoliensis", "Velociraptor mongoliensis", "species", "Reptilia", "Saurischia", "Dromaeosauridae", "Theropoda"),
  T_("Huadanosaurus_sinensis", "Huadanosaurus sinensis", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda"),
  T_("Confractosuchus_sauroktonos", "Confractosuchus sauroktonos", "species", "Reptilia", "Crocodyliformes", "", "Crocodyliformes", notes = "Fossil eusuchian-grade crocodyliform; not crown Crocodylia"),
  T_("Compsognathus_longipes", "Compsognathus longipes", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda", notes = "MNHN CNJ 79 was described as C. corallestris by Bidar et al. 1972; referred to C. longipes by Ostrom 1978 and Peyer 2006"),
  T_("Scipionyx_samniticus", "Scipionyx samniticus", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda"),
  T_("Yanornis_martini", "Yanornis martini", "species", "Aves", "", "Yanornithidae", "Avialae", notes = "IVPP V13259 kept here under decision 5; Wang et al. 2020 refer it to Abitusavis lii"),
  T_("Yanornis", "Yanornis", "genus", "Aves", "", "Yanornithidae", "Avialae", notes = "Zheng et al. 2014 STM9 birds, 'Yanornis sp.'; Wang et al. 2020: Yanornithidae indet."),
  T_("Jianchangornis_microdonta", "Jianchangornis microdonta", "species", "Aves", "", "", "Avialae"),
  T_("Yabeinosaurus", "Yabeinosaurus", "genus", "Reptilia", "Squamata", "", "Lacertilia"),
  T_("Huaxiagnathus_orientalis", "Huaxiagnathus orientalis", "species", "Reptilia", "Saurischia", "Compsognathidae", "Theropoda"),
  T_("Baryonyx_walkeri", "Baryonyx walkeri", "species", "Reptilia", "Saurischia", "Spinosauridae", "Theropoda"),
  # prey
  T_("Psittacosaurus", "Psittacosaurus", "genus", "Reptilia", "Ornithischia", "Psittacosauridae"),
  T_("Psittacosaurus_lujiatunensis", "Psittacosaurus lujiatunensis", "species", "Reptilia", "Ornithischia", "Psittacosauridae"),
  T_("Lacertilia", "Lacertilia", "suborder", "Reptilia", "Squamata"),
  T_("Zhangheotherium", "Zhangheotherium", "genus", "Mammalia", "", "Zhangheotheriidae", notes = "Hurum et al. 2006: cf. Z. quinquecuspidens"),
  T_("Sinobaatar", "Sinobaatar", "genus", "Mammalia", "Multituberculata", "Eobaataridae"),
  T_("Jinanichthys", "Jinanichthys", "genus", "Actinopterygii", "", "", notes = "Fossil osteoglossomorph; most identifications in the gut papers are 'cf.' or 'tentatively'"),
  T_("Inoceramidae", "Inoceramidae", "family", "Bivalvia", "", "Inoceramidae", vert = FALSE, notes = "Kear 2006: Inoceramus and/or Anopaea"),
  T_("Teleostei", "Teleostei", "infraclass", "Actinopterygii"),
  T_("Enantiornithes", "Enantiornithes", "unranked_clade", "Aves", notes = "O'Connor et al. 2011: tentatively Cathayornithiformes"),
  T_("Indrasaurus_wangi", "Indrasaurus wangi", "species", "Reptilia", "Squamata"),
  T_("Ptycholepidae", "Ptycholepidae", "family", "Actinopterygii", "", "Ptycholepidae"),
  T_("Sinornithosaurus", "Sinornithosaurus", "genus", "Reptilia", "Saurischia", "Dromaeosauridae", notes = "Xing et al. 2012: tentatively referred"),
  T_("Ornithischia", "Ornithischia", "order", "Reptilia", "Ornithischia"),
  T_("Crocodylomorpha", "Crocodylomorpha", "unranked_clade", "Reptilia", notes = "Nesbitt et al. 2006: cf. Hesperosuchus agilis"),
  T_("Hadrosauridae", "Hadrosauridae", "family", "Reptilia", "Ornithischia", "Hadrosauridae"),
  T_("Citipes_elegans", "Citipes elegans", "species", "Reptilia", "Saurischia", "Caenagnathidae"),
  T_("Sphagesauridae", "Sphagesauridae", "family", "Reptilia", "Crocodyliformes", "Sphagesauridae"),
  T_("Titanosauria", "Titanosauria", "unranked_clade", "Reptilia", "Saurischia"),
  T_("Edmontosaurus", "Edmontosaurus", "genus", "Reptilia", "Ornithischia", "Hadrosauridae", notes = "DePalma et al. 2013: 'compare with Edmontosaurus annectens'"),
  T_("Edmontosaurus_annectens", "Edmontosaurus annectens", "species", "Reptilia", "Ornithischia", "Hadrosauridae"),
  T_("Protoceratops_andrewsi", "Protoceratops andrewsi", "species", "Reptilia", "Ornithischia", "Protoceratopsidae"),
  T_("Gobiconodontidae", "Gobiconodontidae", "family", "Mammalia", "Eutriconodonta", "Gobiconodontidae", notes = "Qiu et al. 2025: closest to Meemannodon lujiatunensis"),
  T_("Eutheria", "Eutheria", "unranked_clade", "Mammalia"),
  T_("Ornithopoda", "Ornithopoda", "unranked_clade", "Reptilia", "Ornithischia", notes = "White et al. 2022: early-diverging, non-iguanodontian, juvenile"),
  T_("Bavarisaurus", "Bavarisaurus", "genus", "Reptilia", "Squamata", notes = "Ostrom 1978: Bavarisaurus cf. macrodactylus. Conrad 2017 makes the Compsognathus gut lizard the holotype of Schoenesmahl dyspepsia (one paper; decision 5 keeps Ostrom's identification)"),
  T_("Lepidosauria", "Lepidosauria", "superorder", "Reptilia"),
  T_("Osteichthyes", "Osteichthyes", "unranked_clade", "Actinopterygii", notes = "Dal Sasso & Maganuco 2011: 'Osteichthyes indet.'"),
  T_("Clupeomorpha", "Clupeomorpha", "unranked_clade", "Actinopterygii", notes = "Dal Sasso & Maganuco 2011: 'clupeomorph-like', cf. Clupavus"),
  T_("Vertebrata_indet", "Vertebrata indet.", "unranked_clade", "", vert = TRUE),
  T_("Mammalia", "Mammalia", "class", "Mammalia"),
  T_("Protopsephurus", "Protopsephurus", "genus", "Actinopterygii", "Acipenseriformes", "Polyodontidae", notes = "Zheng et al. 2014 print 'Protopspherus'"),
  T_("Lepidotes", "Lepidotes", "genus", "Actinopterygii", "", "Lepidotidae"),
  T_("Iguanodon", "Iguanodon", "genus", "Reptilia", "Ornithischia", "Iguanodontidae"))
taxa <- taxa[!duplicated(taxa$taxon_id), ]
stopifnot(all(c(pairs$pred_taxon_id, pairs$prey_taxon_id) %in% taxa$taxon_id))

## ------------------------------------------------------------------
## Measurements
## ------------------------------------------------------------------
ms <- do.call(rbind, lapply(meas_specs, function(m) data.frame(src = m$src, spec = m$spec, tax = m$tax, role = m$role, el = m$el, dim = m$dim, val = m$val, unit = m$unit, vt = m$vt, loc = m$loc, nt = m$nt, pair = m$pair)))
ms <- ms[!duplicated(ms[, c("src", "spec", "el", "dim", "val", "pair")]), ]
need_tax <- setdiff(ms$tax, taxa$taxon_id)
if (length(need_tax)) stop("measurement taxa missing: ", paste(need_tax, collapse = ", "))
ms$role[ms$role == "comparison"] <- "comparison"
ms$pair[ms$role != "prey"] <- ""
ms <- ms[order(ms$src), ]
ms$id <- ave(seq_len(nrow(ms)), ms$src, FUN = seq_along)
meas <- mk("measurements", measurement_id = sprintf("%s_m%02d", ms$src, ms$id), source_id = ms$src, pair_id = ms$pair, specimen = ms$spec, taxon_id = ms$tax,
           role = ms$role, element = ms$el, dimension = ms$dim, value = fnum(ms$val), unit = ms$unit, value_type = ms$vt, locator = ms$loc, notes = ms$nt)
missing_src <- setdiff(meas$source_id, sources$source_id); if (length(missing_src)) stop("measurement sources missing: ", paste(missing_src, collapse = ", "))

## ------------------------------------------------------------------
## Write db/ and the step-2 side tables
## ------------------------------------------------------------------
used_mid <- unique(c(pairs$pred_mass_method_id, pairs$prey_mass_method_id)); used_mid <- used_mid[nzchar(used_mid)]
stopifnot(all(used_mid %in% METHODS$method_id))
commit(OWNER, OWNS, sources, pairs, meas, taxa, METHODS, pair_tag = "^[A-Za-z]+[0-9]{4}[a-z]?_[0-9]{2}$")
## Phase 3b: the superseded generic fish method row is no longer used by any pair
{ ME0 <- read_db("methods"); ME0 <- ME0[ME0$method_id != "Froese2006_cube_generic", , drop = FALSE]; write_db("methods", ME0) }
RUNGdf <- do.call(rbind, RUNG); write.csv(RUNGdf, file.path(FR, "step3b_topup.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(ENT, file.path(FR, "step2_estimates.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
XCdf <- do.call(rbind, XC); write.csv(XCdf, file.path(FR, "step2_crosschecks.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

## ------------------------------------------------------------------
## Comparison with the old fossil tables (read only here)
## ------------------------------------------------------------------
oldp <- read.csv(file.path(ROOT, "data", "fossil_ppmr_points.csv"), colClasses = "character")
oldq <- read.csv(file.path(ROOT, "data", "fossil_predprey.csv"), colClasses = "character", check.names = FALSE)
names(oldq)[duplicated(names(oldq))] <- paste0(names(oldq)[duplicated(names(oldq))], "_2")
olde <- read.csv(file.path(ROOT, "data", "fossil_diet_extractions.csv"), colClasses = "character")
map <- list(  # entry -> list(table, pred match, prey match)
  Godoy2014_01 = list(p = c("LPRP/USP 0229a", "Sphagesauridae_indet")), White2022_01 = list(p = c("AODF0890", "Ornithopoda_indet_juvenile")),
  Hu2005_01 = list(p = c("IVPP V13605", "Psittacosaurus_juvenile")), Han2023_01 = list(p = c("NA", "Psittacosaurus_lujiatunensis")),
  Wilson2010_01 = list(p = c("GSI/GC/2901-2906", "Titanosauria_hatchling_indet")), Xing2013_01 = list(p = c("Microraptor_fish_gut", "Osteichthyes_indet"), q = c("QM-V1002", "")),
  Zheng2018_01 = list(p = c("Anchiornis_gut", "Squamata_indet"), q = c("STM0-179", "")), Varricchio2001_01 = list(p = c("Daspletosaurus_gut", "Hadrosauridae_juv_large"), q = c("OTM-200", "Hadrosaur")),
  Varricchio2001_02 = list(p = c("Daspletosaurus_gut", "Hadrosauridae_juv_small")), Hurum2006_01 = list(p = c("Sinosauropteryx_gut", "Zhangheotherium"), q = c("GMV-2124", "Zhangheotherium")),
  Hurum2006_02 = list(p = c("Sinosauropteryx_gut", "Sinobaatar"), q = c("GMV-2124", "Sinobaatar")), DalSasso2011_05 = list(p = c("Scipionyx_gut", "Osteichthyes_indet_b"), q = c("SALERNO", "Indet")),
  DalSasso2011_07 = list(p = c("Scipionyx_gut", "Osteichthyes_indet_a")), DalSasso2011_03 = list(p = c("Scipionyx_gut", "Squamata_indet_small")),
  DalSasso2011_02 = list(p = c("Scipionyx_gut", "Squamata_indet_large")), Dalsatt2006_01 = list(p = c("IVPP V13313", "cf_Jinanichthys")),
  Xing2012_04 = list(p = c("CAGS-IG-T1", "Confuciusornis"), q = c("CAGS-IG-T1", "Confuciusornis")), Qiu2025_01 = list(p = c("Huadanosaurus_gut", "Gobiconodontidae_cf_Meemannodon")),
  Qiu2025_02 = list(p = c("Huadanosaurus_gut", "Eutheria_indet")), Conrad2017_01 = list(p = c("Compsognathus_gut", "Schoenesmahl_dyspepsia"), q = c("SNSB-BSPG-AS-I-563", "Schoenesmahl_dyspepsia")),
  Chen1998_01 = list(p = c("NIGP-127587", "Dalinghosaurus"), q = c("NIGP-127587", "Dalinghosaurus")), Zhou2014_01 = list(p = c("IVPP V17078", "fish_indet")),
  Therrien2023_01 = list(p = c("TMP 2009.12.14", "Citipes_elegans_yearling_anterior")), Therrien2023_02 = list(p = c("TMP 2009.12.14", "Citipes_elegans_yearling_posterior")),
  OConnor2011_01 = list(p = c("IVPP-V17972", "Enantiornithes"), q = c("IVPP-V17972", "Enantiornithes")), Nesbitt2006_01 = list(p = c("Coelophysis_gut", "Hesperosuchus_crocodylomorph"), q = c("AMNH-FR-7224", "Hesperosuchus")),
  Xing2012_01 = list(p = c("JMP-V-05-8-01", "Sinornithosaurus"), q = c("JMP-V-05-8-01", "Sinornithosaurus")), Carpenter1998_01 = list(p = c("GI SPS 100/25", "Protoceratops_andrewsi"), q = c("GI-SPS-100/25", "Protoceratops_andrewsi")))
cmp <- list()
addc <- function(e, side, tab, old, new) {
  old <- suppressWarnings(as.numeric(old)); if (is.na(old) && is.na(new)) return()
  d <- if (!is.na(old) && !is.na(new)) new / old - 1 else NA
  cmp[[length(cmp) + 1]] <<- data.frame(entry_id = e, side = side, old_table = tab, old_g = old, new_g = new, rel_diff = d,
    flag = ifelse(is.na(d), if (is.na(new)) "new value empty" else "old value empty", ifelse(abs(d) > 0.25, ">25%", "")))
}
for (e in names(map)) {
  m <- map[[e]]; n <- ENT[ENT$entry_id == e, ]
  if (!is.null(m$p)) { k <- which(oldp$pred_specimen == m$p[1] & oldp$prey == m$p[2])[1]
    if (!is.na(k)) { addc(e, "predator", "fossil_ppmr_points", oldp$pred_mass_g[k], n$pred_mass_g); addc(e, "prey", "fossil_ppmr_points", oldp$prey_mass_g[k], n$prey_mass_g) } }
  if (!is.null(m$q)) { k <- which(oldq$specimen_pred == m$q[1] & (m$q[2] == "" | oldq$name_prey == m$q[2]))[1]
    if (!is.na(k)) { addc(e, "predator", "fossil_predprey", oldq$mass_pred[k], n$pred_mass_g); if (nzchar(oldq$mass_prey[k])) addc(e, "prey", "fossil_predprey", oldq$mass_prey[k], n$prey_mass_g) } }
  k <- which(olde$specimen_pred == n$pred_specimen | (e == "Han2023_01" & olde$name_prey == "Psittacosaurus_lujiatunensis") | (e == "Godoy2014_01" & olde$specimen_pred == "LPRP/USP 0229a"))
  k <- k[olde$name_prey[k] == "" | grepl(sub("_.*", "", n$prey_taxon), olde$name_prey[k]) | e %in% c("Han2023_01", "Kear2006_01", "Zhou2014_01", "Dalsatt2006_01", "White2022_01", "Hu2005_01", "Godoy2014_01", "Wilson2010_01", "Carpenter1998_01")][1]
  if (!is.na(k)) { addc(e, "predator", "fossil_diet_extractions", olde$pred_mass_g[k], n$pred_mass_g); addc(e, "prey", "fossil_diet_extractions", olde$prey_mass_g[k], n$prey_mass_g) }
}
CMP <- do.call(rbind, cmp); CMP <- CMP[!duplicated(CMP[, c("entry_id", "side", "old_table")]), ]
write.csv(CMP, file.path(FR, "step2_old_table_comparison.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("pairs %d (tiers: %s); excluded %d; measurements %d; methods %d; comparisons %d (%d over 25%%)\n", nrow(pairs),
            paste(names(table(pairs$pair_resolution)), table(pairs$pair_resolution), collapse = ", "), nrow(EXC), nrow(meas), nrow(METHODS), nrow(CMP), sum(CMP$flag == ">25%")))
