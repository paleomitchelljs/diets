###################################################################
# 10b_fossil_topup.R -- Phase 3b: mass top-up for fossil pairs that lack a predator or prey mass.
# Sourced by 10_fossil_step2.R just before it builds the pairs (it needs that script's objects:
# E(), M(), R, PRED, PREY, meiriS, farlowTL, K_CE, gg, kg). It does not write to db/ itself.
#
# Ladder (docs/ASSESSMENT_AND_PLAN_2026-11.md, Prompt 3b), stop at the first rung that applies:
#   1 published equation on a published measurement
#   2 measurement taken from a published figure with a scale bar, then an equation
#   3 taxon-average mass from published estimates (species_average_literature), interval >= x/÷3
#   4 bracket (lo, hi, no point) from element size, cluster size, comparison specimen, swallow limit
#   5 weak bracket (physical floor or ceiling only), or a taxon range for a predator without limb bones
# Rungs 4 and 5 that rest on a comparison or an assumption, not on a measurement of the animal plus a
# published equation, use mass_method = assigned_approximate (forces tier E). Nothing here is a measurement.
#
# TOPUP[[entry]] = list(prey = E(), pred = E(), rung_prey, rung_pred, why_empty_prey, why_empty_pred).
# RUNG records every pair for data/fossil_rebuild/step3b_topup.csv (built in 10_fossil_step2.R).
###################################################################
TOPUP <- list()
METHODS <- rbind(METHODS,
  mrow(method_id = "Seebacher2001_T3_Ceratopsia", mass_method = "regressed_from_length",
       description = "Ceratopsian body mass from total length (Seebacher 2001, polynomial-method masses regressed on length)",
       equation = "M = 12.58 * TL^2.90", response = "body mass, kg", predictor = "total length, m", taxon_scope = "Ceratopsia (Table 3). The sample is not itemised for the group; the Table 2 Marginocephalia block has 9 reconstructions (Centrosaurus, Chasmosaurus, Homocephale, Monoclonius, Pentaceratops, Protoceratops, Psittacosaurus, Stegoceras, Triceratops), two of them pachycephalosaurs",
       valid_range = "total length 1.4-8.0 m (Table 2 Marginocephalia block)", input_min = "1.4", input_max = "8.0", reported_error = "exponent 2.90 +/- 0.94 (95% CI); R2 0.98",
       method_file = "Seebacher_2001_JVP_dinosaur_length_mass.pdf", locator = "Table 3, p. 55 (rendered page); sample in Table 2, p. 53-54", verified = "TRUE",
       notes = "Masses are from graphical reconstructions, not weighed animals. Seebacher's own Protoceratops AMNH 6467 (1.4 m) is 23.7 kg. The prediction error of the line is not given, only the exponent's CI."),
  mrow(method_id = "Seebacher2001_T3_Ornithopoda", mass_method = "regressed_from_length",
       description = "Ornithopod (including hadrosaur) body mass from total length (Seebacher 2001)",
       equation = "M = 11.81 * TL^2.66", response = "body mass, kg", predictor = "total length, m", taxon_scope = "Ornithopoda (20 reconstructions in Table 2, hypsilophodontids to hadrosaurs)",
       valid_range = "total length 0.65-17 m (Table 2 sample)", input_min = "0.65", input_max = "17", reported_error = "exponent 2.66 +/- 0.14 (95% CI); R2 0.99",
       method_file = "Seebacher_2001_JVP_dinosaur_length_mass.pdf", locator = "Table 3, p. 55 (rendered page); sample in Table 2, p. 53-54", verified = "TRUE",
       notes = "Masses are from graphical reconstructions of adults and subadults. Table 2 prints 'Anatotitan 1.20 m, 7,594 kg', evidently a typo for 12.0 m; not used."))

tp <- function(entry, prey = NULL, pred = NULL, rung_prey = NA, rung_pred = NA, empty_prey = "", empty_pred = "")
  TOPUP[[entry]] <<- list(prey = prey, pred = pred, rung_prey = rung_prey, rung_pred = rung_pred, empty_prey = empty_prey, empty_pred = empty_pred)

## ---- reference values -------------------------------------------------------------------------
## Froese 2006 Table 1 (J Appl Ichthyol 22:241-253, p.249): median form factor a3.0 by body shape; W = a3.0 L^3, g and cm
FR_A <- c(fusiform = 0.0137, elongated = 0.00838, shortdeep = 0.0187)
FR_P05 <- c(fusiform = 0.0061, elongated = 0.00293, shortdeep = 0.0077)
FR_P95 <- c(fusiform = 0.0240, elongated = 0.0178, shortdeep = 0.0336)
froese <- function(a, Lmm) a * (Lmm / 10)^3
R$Proto <- s11("Protoceratops_andrewsi", "AMNH 6424")
R$Dasp_small <- s11("Daspletosaurus_torosus", "NMC 11315")
R$Dasp_big <- s11("Daspletosaurus_torosus", "UA 11")
LMAX_FISH <- 160   # mm: twice the longest fish a yanornithid is known to have swallowed (Zheng et al. 2014: 70-80 mm)

## ---- fish ----------------------------------------------------------------------------------------
## bracket: lower bound = elongated-body median a at Lmin; upper = fusiform 95th-percentile a at Lmax
fish_bracket <- function(Lmin, Lmax = LMAX_FISH, lo_why, hi_why = "")
  E(NA, froese(FR_A["elongated"], Lmin), froese(FR_P95["fusiform"], Lmax), "assigned_approximate", "",
    sprintf("BRACKET, no point value (rung 4/5). Lower bound %s mg: %s, as an elongated-bodied fish (Froese 2006 Table 1 median a3.0 0.00838) of total length %s mm. Upper bound %s g: fish of %s mm (%s) with the 95th-percentile fusiform form factor (0.0240; Froese 2006 Table 1). Neither length is a measurement of this fish; the bracket is a comparison and swallow-size limit, not an estimate.",
            signif(1000 * froese(FR_A["elongated"], Lmin), 2), lo_why, Lmin, signif(froese(FR_P95["fusiform"], Lmax), 2), Lmax,
            if (nzchar(hi_why)) hi_why else "twice the 70-80 mm fish that Zheng et al. 2014 measured in STM9-15, the only measured fish ingested by a yanornithid"),
    FALSE, "")
FB_YAN <- fish_bracket(35, lo_why = "half the 70-80 mm fish measured in STM9-15 (Zheng et al. 2014); the fish bones here are disarticulated and cannot be measured")
for (k in c("02", "03", "04", "05", "06", "09", "10", "11", "12", "14", "15", "16", "17", "18"))
  tp(paste0("Zheng2014_", k), prey = FB_YAN, rung_prey = "4")
## Jinanichthys-identified fish in Yanornis crops with no length of their own: taxon average, rung 3
jin_pt <- froese(FR_A["fusiform"], 75)
JIN <- function(div = 3, extra = "")
  E(jin_pt, jin_pt / div, jin_pt * div, "species_average_literature", "",
    sprintf("taxon average: Jinanichthys as measured by Zheng et al. 2014 ('at least two complete fish, which we estimate would have measured 70-80 mm in body length', STM9-15; identification 'tentative'); W = a3.0 L^3 with the fusiform median a3.0 0.0137 (Froese 2006 Table 1) at 75 mm; interval = point x/÷ %g (rule: at least a factor of three)%s", div, extra),
    FALSE, "")
for (k in c("07", "08", "13")) tp(paste0("Zheng2014_", k), prey = JIN(), rung_prey = "3")
tp("Dalsatt2006_01", prey = JIN(10, "; widened to x/÷ 10 because the cluster (seven to nine vertebrae in 6 mm, Dalsatt et al. 2006) implies vertebrae of about 0.7 mm, small for a 75 mm fish"), rung_prey = "3",
   pred = E(NA, 180, 1620, "species_average_literature", "",
     "BRACKET, no point (rung 5, taxon range for a predator with no limb bones): Confuciusornis sanctus, stage not stated. Published values: 615 g (range 511-719) for NHMW 1997z0112/0001 by the confuciusornithid multiple regression of Serrano et al. 2015 (Chiappe et al. 2023, Spanish J Palaeontol 38:101-122); 475 g for MOR 1063, 'near-maximal-sized' (Benson 2014 S11). Point-free interval = geometric mean of the two (540 g) x/÷ 3. IVPP V13313 has no published measurement.", FALSE, ""),
   rung_pred = "5")
tp("Zhou2002_01", prey = fish_bracket(35, lo_why = "half the 70-80 mm fish of Zheng et al. 2014; the fish remains are macerated"), rung_prey = "4")
tp("Zhou2014_01", prey = fish_bracket(13, lo_why = "the 13 x 8 mm bone cluster (Zhou et al. 2014 p.616) is at least that long"), rung_prey = "4")
tp("Zhou2014_02", prey = fish_bracket(35, lo_why = "half the 70-80 mm fish of Zheng et al. 2014; no size published"), rung_prey = "5")
tp("Huang2026_01", prey = fish_bracket(35, lo_why = "half the 70-80 mm fish of Zheng et al. 2014; no size published"), rung_prey = "5")
tp("Xing2013_01", prey = fish_bracket(6.7, lo_why = "the longest preserved fin ray (6.7 mm, Xing et al. 2013) cannot exceed the fish's length",
   hi_why = "twice the 70-80 mm fish of Zheng et al. 2014; Microraptor (1.9 kg) is larger than a yanornithid, so this ceiling may be low"), rung_prey = "4")

## ---- lizards -----------------------------------------------------------------------------------------
## Chen1998_01: lizard in NIGP 127587, snout to pelvis measured on Smithwick et al. 2017 SI Fig. S2C (rung 2, then Meiri 2010)
px_bar <- 89; mm_bar <- 20                       # scale bar 871-959 px = 20 mm on a 150 dpi render of SI p.3
svl_fig <- 515 / (px_bar / mm_bar)               # snout (x 955) to pelvis (x 440): 515 px
svl_lo <- 95; svl_hi <- 140
PREY_CHEN <- E(meiriS(svl_fig), meiriS(svl_lo), meiriS(svl_hi), "regressed_from_length", "Meiri2010_T3_Sauria",
  sprintf("RUNG 2. Snout-vent length %.0f mm measured from Smithwick et al. 2017 SI Fig. S2C against its 20 mm scale bar (89 px at 150 dpi; snout to the pelvic bones, 515 px), by eye on the rendered page, so +/- 20%%; the snout may run past the figure edge, so the value is more likely low than high. Meiri 2010 Table 3 Sauria equation. Interval = Meiri at SVL %d-%d mm; Meiri gives no prediction interval, so it covers the measurement only. Chen et al.'s own femur (22.31 mm, SVL 106) gave about 26 g under the same equation.",
          svl_fig, svl_lo, svl_hi), TRUE, sprintf("Meiri at SVL 106 mm = %s", gg(meiriS(106))),
  list(M("Smithwick2017", "NIGP 127587 (prey)", "Lacertilia", "prey", "body", "snout-vent length", round(svl_fig), vt = "estimated",
         loc = "SI Fig. S2C (page 3 of the SI PDF); scale bar 20 mm = 89 px; measured from the figure, +/- 20%", nt = "measured from the figure by eye; the right edge of the image may cut the skull")))
tp("Chen1998_01", prey = PREY_CHEN, rung_prey = "2")
## Zheng2018_01-03: pellet lizards. Femur lengths from Fig. 1c (10 mm bar = 77 px at 200 dpi). No femur-to-SVL equation on disk:
## SVL from the Bavarisaurus femur/PCL ratio (21.9/91 = 0.24, Conrad 2017), bracketed 0.18-0.30; then Meiri 2010.
lz <- function(f, flo, fhi, who)
  E(NA, meiriS(flo / 0.30), meiriS(fhi / 0.18), "assigned_approximate", "",
    sprintf("BRACKET, no point (rung 4, comparison specimen). %s femur about %g mm (%g-%g) measured from Zheng et al. 2018 Fig. 1c against its 10 mm bar (77 px at 200 dpi), by eye, +/- 20%%. SVL = femur / ratio, ratio 0.18-0.30 chosen around the one on-disk comparison, Bavarisaurus (femur 21.9 mm, precaudal length 91 mm = 0.24; Conrad 2017) - the range is mine, not published. Mass = Meiri 2010 Table 3 Sauria at that SVL (%.0f-%.0f mm). No femur-based lizard equation is on disk.",
            who, f, flo, fhi, flo / 0.30, fhi / 0.18), FALSE, "",
    list(M("Zheng2018", "STM0-179 (lizard pellet)", "Lacertilia", "prey", "femur", "length", f, vt = "estimated", loc = "Fig. 1c, 10 mm scale bar; measured from the figure, +/- 20%", nt = paste(who, "- measured from the figure by eye"))))
tp("Zheng2018_01", prey = lz(10, 8, 12, "Large lizard A"), rung_prey = "4")
tp("Zheng2018_02", prey = lz(9, 7, 11, "Large lizard B"), rung_prey = "4")
tp("Zheng2018_03", prey = lz(4, 3, 5, "Small lizard"), rung_prey = "4")
## DalSasso2011_02/_03: lepidosaurs with a total length from the authors. SVL = TL x 0.3-0.6 (assumed tail share), then Meiri.
lep <- function(tl_lo, tl_hi, lo_why)
  E(NA, meiriS(0.3 * tl_lo), meiriS(0.6 * tl_hi), "assigned_approximate", "",
    sprintf("BRACKET, no point (rung 4/5). Total length %g-%g mm (%s). SVL taken as 0.3-0.6 of total length (assumed tail share; my range, not published), then Meiri 2010 Table 3 Sauria: %.0f-%.0f mm SVL. Intact tails are rarely more than twice the SVL and many are shorter, but a regenerated or lost tail would put SVL above this range.",
            tl_lo, tl_hi, lo_why, 0.3 * tl_lo, 0.6 * tl_hi), FALSE, "")
tp("DalSasso2011_02", prey = lep(150, 400, "Dal Sasso & Maganuco 2011: 15-40 cm, 'half that of Scipionyx'"), rung_prey = "4")
tp("DalSasso2011_03", prey = lep(50, 120, "upper: 'no more than 10-12 cm long' from scale size; the 50 mm floor is mine, a hatchling-sized lizard, not from the source"), rung_prey = "5")

## ---- mammals (Jehol) ----------------------------------------------------------------------------------
## No published mass for Zhangheotherium, Sinobaatar or Gobiconodontidae is on disk, and no jaw-to-mass equation for Mesozoic mammals.
## Comparison: Hone et al. 2022 give 13-43 g for the Jehol mammals Eomaia and Yanoconodon (from Meng et al. 2017); widened x/÷ 3.
JEHOL <- function(why)
  E(NA, 13 / 3, 43 * 3, "assigned_approximate", "",
    sprintf("BRACKET, no point (rung 4). Comparison taxa only: Hone et al. 2022 give 'a reasonable size bracket' of 21-30 g and a full range of 13-43 g for the Jehol mammals Eomaia and Yanoconodon (from Meng et al. 2017, not on disk); widened x/÷ 3 to %.1f-%.0f g. %s No published mass for this taxon is on disk. Huang et al. 2024 (Dryad file, now on disk) covers Eutriconodonta only, not symmetrodonts, multituberculates or eutherians.", 13 / 3, 43 * 3, why),
    FALSE, "")
tp("Hurum2006_01", prey = JEHOL("Zhangheotherium is a symmetrodont known from skeletons of roughly hamster size (not quantified in the paper)."), rung_prey = "4")
tp("Hurum2006_02", prey = JEHOL("Sinobaatar is a multituberculate; the jaw is smaller than the Zhangheotherium jaws in Hurum et al. Fig. 8 (not measured)."), rung_prey = "4")
## Qiu2025_01: gobiconodontid. Huang et al. 2024 (data/mass_references/Huang_etal_2024_mesozoic_mammal_mass/allmass.csv; units not stated in
## the README, read as grams) give per-predictor mass predictions for gobiconodontids. Species medians: Gobiconodon bathoniensis 14.0 g (lowest),
## G. borissiaki 77.3, Fuxinoconodon changi 77.5, Meiconodon lii 93.6 g (highest). Bracket = lowest to highest median, widened x/÷ 3.
GOBI_LO <- 14.0; GOBI_HI <- 93.6
tp("Qiu2025_01", rung_prey = "4", prey = E(NA, GOBI_LO / 3, GOBI_HI * 3, "assigned_approximate", "",
  sprintf("BRACKET, no point (rung 4, comparison taxa). Species-median body-mass predictions for gobiconodontids in Huang et al. 2024 allmass.csv (units not stated, read as g): Gobiconodon bathoniensis %.1f g to Meiconodon lii %.1f g (Fuxinoconodon changi 77.5 g); widened x/÷ 3 to %.1f-%.0f g. The prey is 'closest to Meemannodon' (family level only) and Meemannodon is not in that file, so it may sit outside the range. Qiu et al. SI Fig. S7c-d draws the mandibles at a 5 mm scale bar and no jaw-to-mass step was done.", GOBI_LO, GOBI_HI, GOBI_LO / 3, GOBI_HI * 3), FALSE, ""))
tp("Qiu2025_02", prey = JEHOL("The eutherian is 'badly preserved'; Fig. S7a-b draws its mandible and maxilla at a 5 mm scale bar."), rung_prey = "5")

## ---- dinosaurs and others ------------------------------------------------------------------------------
## Seebacher 2001 Table 3 (verified on the rendered page 55): M(kg) = a TL(m)^b, b +/- 95% CI. Ceratopsia 12.58, 2.90, 0.94; Ornithopoda 11.81, 2.66, 0.14.
seeb <- function(a, b, TL) 1000 * a * TL^b
SEEB_C <- c(a = 12.58, b = 2.90, ci = 0.94); SEEB_O <- c(a = 11.81, b = 2.66, ci = 0.14)
seebE <- function(S, TLmid, TLlo, THhi_unused = NULL, TLhi, mid, basis, xcheck, inputs)
  E(seeb(S["a"], S["b"], TLmid), seeb(S["a"], S["b"] - S["ci"], TLlo), seeb(S["a"], S["b"] + S["ci"], TLhi), "regressed_from_length", mid, basis, TRUE, xcheck, inputs)
tp("Carpenter1998_01", rung_prey = "1", prey = seebE(SEEB_C, 1.25, 1.2, TLhi = 1.3, mid = "Seebacher2001_T3_Ceratopsia",
  basis = paste0("RUNG 1. Body length 120-130 cm ('120-130 cm long', Barsbold 1974, quoted by Carpenter 1998 p.139; entered as 1.25 m and taken as total length, which the paper does not say) through the Seebacher 2001 Table 3 Ceratopsia equation, M = 12.58 TL^2.90 kg. Interval = length 1.2 m with the exponent's lower 95% bound (2.90 - 0.94) to 1.3 m with its upper bound (2.90 + 0.94). The calibration sample runs from 1.4 m (Protoceratops AMNH 6467, 23.7 kg) to 8.0 m, so 1.25 m is a slight extrapolation below it."),
  xcheck = sprintf("Seebacher's own Protoceratops AMNH 6467 (TL 1.4 m) = 23.7 kg; adult Protoceratops AMNH 6424 by limb circumference (Benson 2014 S11) = %s; Colbert 1962 (as quoted in Seebacher Table 4) = 177 kg for an adult", kg(R$Proto$g)),
  inputs = list(M("Carpenter1998", "Protoceratops of the fighting pair", "Protoceratops_andrewsi", "prey", "body", "total length", 1250, vt = "approximate", loc = "p.139 and caption, '120-130 cm long; Barsbold 1974' (midpoint entered)"))))
## Wilson2010: hatchling bounded below by an alligator of the same length and above by the egg
wl_lo <- farlowTL(500); wl_hi <- 2316
tp("Wilson2010_01", rung_prey = "4", prey = E(NA, wl_lo, wl_hi, "assigned_approximate", "",
  sprintf("BRACKET, no point (rung 4). Lower %.0f g: Farlow et al. 2005 wild-alligator total-length equation at the authors' 'approximately 0.5 m' - a crocodylian proxy; a stocky titanosaur hatchling should be heavier than an alligator of that length. Upper 2316 g: the mass of the associated egg GSI/GC/2905 (Wilson et al. 2010 Text S5); a hatchling cannot weigh more than the fresh egg it left.", wl_lo), FALSE, ""))
## Daspletosaurus OTM 200 (no limb bones): taxon range from S11
PRED_DASP <- E(NA, R$Dasp_small$g / K_CE, R$Dasp_big$g * K_CE, "species_average_literature", "",
  sprintf("BRACKET, no point (rung 5, taxon range): OTM 200 has 'no limb elements' and its ontogenetic stage is not stated. Range of Daspletosaurus torosus masses in Benson et al. 2014 S11, from the smallest (NMC 11315, %s, femur %g mm) to the largest (UA 11, %s, femur %g mm), each widened by the 95%% factor x/÷ %.2f. The tyrannosaurid is probably a subadult or small adult from its dentary size, so the lower half is more likely; that is a judgement, not a measurement.",
          kg(R$Dasp_small$g), R$Dasp_small$FL, kg(R$Dasp_big$g), R$Dasp_big$FL, K_CE), FALSE, "")
v1 <- PREY$Varricchio2001_01; v2 <- PREY$Varricchio2001_02
tp("Varricchio2001_01", rung_pred = "5", pred = PRED_DASP, rung_prey = "1", prey = seebE(SEEB_O, 3, 2.7, TLhi = 3.3, mid = "Seebacher2001_T3_Ornithopoda",
  basis = "RUNG 1. Total body length 'about 3 m' (Varricchio et al. 2001, juvenile hadrosaur from caudal size) through the Seebacher 2001 Table 3 Ornithopoda equation, M = 11.81 TL^2.66 kg (R2 0.99, calibration 0.65-17 m). Interval = 2.7 m (my +/- 10% on 'about') with the exponent's lower 95% bound (2.66 - 0.14) to 3.3 m with its upper bound. The equation is fitted to reconstructed adults and subadults; a juvenile has a relatively larger head and thinner body, so it is more likely to overestimate than underestimate.",
  xcheck = sprintf("Phase 3 DME from Edmontosaurus DMNH 1493 by body length = %s (%s-%s); replaced because a published ornithopod length-mass equation is now on disk", kg(v1$point), kg(v1$lo), kg(v1$hi)),
  inputs = v1$inputs))
tp("Varricchio2001_02", rung_pred = "5", pred = PRED_DASP, rung_prey = "1", prey = seebE(SEEB_O, 1.1, 1.0, TLhi = 1.2, mid = "Seebacher2001_T3_Ornithopoda",
  basis = "RUNG 1. Total body length 'roughly 1 to 1.2 m' (Varricchio et al. 2001, from the dentary coronoid) through the Seebacher 2001 Table 3 Ornithopoda equation, M = 11.81 TL^2.66 kg, at the 1.1 m midpoint. Interval = 1.0 m with the exponent's lower 95% bound to 1.2 m with its upper bound. Calibration runs from 0.65 m (Gasparinisaura, 0.98 kg), so the length is inside it; the shape of a hatchling-sized hadrosaur is not represented.",
  xcheck = sprintf("Phase 3 DME from Edmontosaurus DMNH 1493 by body length = %s (%s-%s)", kg(v2$point), kg(v2$lo), kg(v2$hi)),
  inputs = v2$inputs))
## Confuciusornis prey (Xing 2012). Rung 2 was tried and failed: Fig. 3E draws the humerus at about 26 mm against a 10 mm bar, which Field et al. 2013 would turn into 53 g,
## a tenth of any published Confuciusornis mass, so the element is incomplete or mis-identified in the drawing; the 38 px bar is too coarse to trust. Rung 3 instead.
CONF <- E(sqrt(475 * 615), sqrt(475 * 615) / 3, sqrt(475 * 615) * 3, "species_average_literature", "",
  "RUNG 3 (rung 2 tried and abandoned: Fig. 3E of Xing et al. 2012 is too small to measure the humerus reliably). Taxon average: Confuciusornis sanctus 615 g (range 511-719) for NHMW 1997z0112/0001 from the confuciusornithid regression of Serrano et al. 2015 (Chiappe et al. 2023, Spanish J Palaeontol 38:101-122) and 475 g for MOR 1063, 'near-maximal-sized' (Benson 2014 S11); point = geometric mean 540 g; interval = x/÷ 3. Chinsamy et al. 2013 report mature individuals as small as 23% of the largest, so the interval is not generous.",
  FALSE, "")
tp("Xing2012_04", prey = CONF, rung_prey = "3"); tp("Xing2012_05", prey = CONF, rung_prey = "3")

## ---- entries that stay without a mass (reason shown in step3b_topup.md) ----------------------------------------------
tp("Charig1997_01", empty_prey = "Lepidotes scales and teeth only; no size in the paper; no Lepidotes mass or length on disk (a genus-level figure of about 30 cm exists only on a Wikipedia mirror, not a published source)")
tp("Charig1997_02", empty_prey = "'a young Iguanodon', elements listed by block (Appendix B) and not measured; no stage-specific reference on disk; the only bound is the adult (I. bernissartensis IRSNB R51, 8.27 t), which would span four orders of magnitude")
tp("DalSasso2011_01", empty_prey = "indeterminate elements, probably part of another prey item; no size information")
tp("DalSasso2011_04", empty_prey = "isolated vertebra, probably part of prey 2 or 5; no size information")
tp("DalSasso2011_06", empty_prey = "origin indeterminate")
tp("Hwang2004_01", empty_prey = "unidentified rounded objects averaging 7 mm; not identified as vertebrate prey")
tp("Peyer2006_01", empty_prey = "imprints; description 'beyond the scope of this paper'; no measurement")
tp("Nesbitt2006_01", empty_prey = "proximal femora, ilium and a sacral vertebra only; no length published; the femur is drawn at a 5 mm scale (Fig. 2) but no crocodylomorph (cf. Hesperosuchus) femur-to-mass equation or comparison mass is on disk")
tp("Xing2012_06", empty_prey = "one incomplete scapula (135 x 65 mm); identity 'contentious' (Psittacosaurus or Yueosaurus-like); no ornithischian scapula-to-mass equation or comparison scapula on disk")
for (k in c("04", "05", "06")) tp(paste0("Zheng2018_", k), empty_prey = "fish scales and fragments only (Ptycholepidae); no size, and no fish-scale size-to-length relationship on disk")
for (k in c("07", "08")) tp(paste0("Zheng2018_", k), empty_prey = "isolated pellet with no producer specimen; excluded by rule", empty_pred = "isolated pellet; producer unknown")
tp("Zhou2009_01", empty_prey = "marked WEAK in step 1 (association only; 'no compelling evidence' of ingestion); excluded by rule")
tp("Zhou2010_01", empty_prey = "marked WEAK in step 1 (one-line personal observation, no specimen); excluded by rule", empty_pred = "no specimen")
for (k in 1:3) tp(sprintf("Kear2006_%02d", k), empty_prey = "invertebrate prey (inoceramid bivalve); excluded by rule", empty_pred = "turtle with no equation on disk; turtles are low priority (decision 6)")
tp("DePalma2013_01", empty_pred = "bite trace; excluded by rule (the prey bracket from Phase 3 stands)")
tp("Carpenter1998_02", empty_pred = "bite trace; excluded by rule (the prey estimate from Phase 3 stands)")

## ---- rung 1: fish with an author-estimated length, re-based from the assumed cube law to Froese 2006 Table 1 (see fishE in 10_fossil_step2.R)
for (k in c("DalSasso2011_05", "DalSasso2011_07", "Zheng2014_01")) tp(k, rung_prey = "1")
