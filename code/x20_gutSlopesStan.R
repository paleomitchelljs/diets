###################################################################
# x20_gutSlopesStan.R   — EXTANT-ONLY prey~predator mass scaling
#
# Per JM (2026-06) redesign:
#  * Fit EXTANT data only; fossils are NOT in the likelihood. They are placed
#    against the extant fits by posterior prediction (see x07) — comparison, not
#    contamination. (Theropods/crocs/snakes overlaid here for context only.)
#  * Proper regression form: ln(preyMass) ~ ln(predMass) + clade + OBSTYPE; PPMR
#    is a DERIVED display quantity, never the response (regressing the ratio on its
#    own denominator induces a spurious ~-1 slope).
#  * OBSTYPE (observation/preservation process) in BOTH mean and sigma — the
#    identifiable, mechanistic version of a "fossil factor": gut/scat/follow all
#    exist in extant data, so the process effect is estimable and then applies to
#    fossils (predicted at the Stomach/gut level).
#  * One individual-or-study grouping factor (pred_specimen = individual where the
#    data carry it, else study) for non-independence (multi-prey per individual).
#  * Broaden + subsample: include Stomach/Pellet/Scat/Follow (bird "Scat" in longform = raptor/owl
#    PELLETS -> recoded Pellet, its own process), capped per clade x obstype
#    for parity so the 134k scat rows don't swamp the 19k stomach.
#  * TIGHT high-level priors: prey~pred scaling slope ~ normal(0.85, 0.25), anchored
#    on Tucker & Rogers 2014 (108 mammals, terrestrial slope ~1) and Costa et al.
#    2008 (159 lizards, mean-prey slope 0.77-0.93). Both also show the niche shift
#    (big predators add large prey, drop small) -> supports sigma~predmass + slope<1.
#
# Model: ln(prey) ~ ln(pred_c) + (1 + ln(pred_c) | clade) + obstype
#                   + (1|species) + (1|pred_specimen)
#        sigma   ~ 0 + clade + ln(pred_c) + obstype          family = gaussian
#        clade = {Aves, Crocodylia, Lacertilia, Mammalia, Serpentes}  (extant only)
#
# Saves data/fit_gutslopes<suffix>.rds after sampling; re-run with fit present skips.
###################################################################
suppressWarnings(suppressPackageStartupMessages({ library(brms); library(ggplot2); library(ggrepel) }))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
flag <- function(v) tolower(Sys.getenv(v, "")) %in% c("1","true","t","yes","y")   # robust bool env (as.logical("1") is NA!)
splitsq <- function(cl, sub) ifelse(cl!="Squamata", cl, ifelse(sub=="Serpentes","Serpentes","Lacertilia"))
## prey TYPE from a prey-class string (same rule as x00): vert = backboned animal prey (incl. carrion/egg),
## invert = animal prey w/o, NA = non-animal "prey" (plant/fungus/placeholder). NA rows are dropped from the
## predator-prey fit -- frugivory/herbivory is not predation (the tegu/monitor plant-item contamination).
ptype <- function(cls) ifelse(grepl("Actinopter|Acinopter|Chondrich|Sarcopter|Osteichthy|Pisces|Teleost|Mammal|Aves|Reptil|Amphib|Squamat|Anura|Chelon|Testud|Serpent|Lacert|Crocodyl|Amniota|Carrion|Egg|fish|vertebr", cls, ignore.case=TRUE), "vert",
                       ifelse(grepl("Insect|Arachn|Crustac|Malacostr|Myriapod|Chilopod|Diplopod|Gastropod|Mollusc|Annelid|Hexapod|Arthropod|Bivalv|Oligochaet|Clitellat|Branchiopod|Collembol|Decapod|Isopod|Orthopter|Coleopter|Hymenopt|Lepidopt|Hemipter|Cephalopod|Scorpion|Solifug|invert", cls, ignore.case=TRUE), "invert", NA))

SMOKE   <- flag("SMOKE")                                       # fast end-to-end test: small cap + 1 chain
SUFFIX  <- Sys.getenv("GUT_SUFFIX","")
CAP     <- if (SMOKE) 120L else as.integer(Sys.getenv("GUT_SUBSAMPLE","1500")) # cap per clade x obstype x PREY_TYPE cell (parity); 0 = use all
NOCOR     <- flag("GUT_NOCOR")                                  # (1+pred||clade): drop LKJ cor if the 5-clade slope cov funnels
TREEDEPTH <- as.integer(Sys.getenv("GUT_TREEDEPTH","12"))
OBMAP <- c(Stomach="Stomach", Stomach_DNA_metabarcoding="Stomach", Gizzard="Stomach",
           Regurgitation="Stomach",          # expelled RECENT whole prey (minimally digested) = gut-grade, NOT a digestive pellet
           prey_remains_at_nest="Follow",     # observed nest remains (large-prey-biased) ~ observation; debatable, between Follow & Pellet
           Pellet="Pellet",                   # literal regurgitated pellet (e.g. NZ kingfisher, ref95)
           Scat="Scat", Fecal_DNA_metabarcoding="Scat",
           Observation="Follow", Telemetry_radio_tracking="Follow")

## ================= 1. EXTANT DATA ASSEMBLY =================
## a) longform, ALL clades, with a CRUCIAL obstype recode:
##    Bird "Scat"/"Larder" in longform are raptor/owl PELLETS (Tyto 42k, Bubo, Strix, Falco, Athene, Asio...)
##    + shrike larders -- NOT feces. Pellets are their own observation process (most complete, least variable;
##    observation_bias_notes.md) and the fossil-relevant mode (Anchiornis is a pellet). Mammal/lizard/snake
##    "Scat" = true feces. Aves Stomach comes from ADB below (AVONET-massed) -> drop longform Aves Stomach.
lf <- read.csv(file.path(ROOT,"data","longform.csv"))
lf$clade <- splitsq(lf$clade, lf$subclade)
lf <- subset(lf, is.finite(preyMass) & is.finite(predmass))
lf$obs2 <- with(lf, ifelse(clade=="Aves" & obstype %in% c("Scat","Larder"), "Pellet", obstype))
lf <- subset(lf, clade %in% c("Aves","Crocodylia","Lacertilia","Mammalia","Serpentes") &
                 obs2 %in% c("Stomach","Scat","Follow","Pellet") &
                 !(clade=="Aves" & obs2=="Stomach"))            # Aves Stomach <- ADB (cleaner)
## CERTAINTY (re-curated 2026-06): longform is LITERATURE-mass-derived by construction -- prey mass is one value
## per prey TAXON (raw_data row 3), predator mass is per-species for 81% of spp. Weakest-link = the taxon-mean
## prey mass -> 0.75 ("species-literature"); WITHMASS studies (96,110,133,135; see data/longform_refs.csv) that
## report measured masses -> 0.85. (Was a blanket, unearned 1.0.)
ext <- data.frame(preyMass=lf$preyMass, predmass=lf$predmass, clade=lf$clade, prey_type=lf$prey_type,
                  obstype=lf$obs2, species=lf$species, study=lf$study, pred_specimen=lf$study,
                  certainty=ifelse(as.integer(as.character(lf$study)) %in% c(96,110,133,135), 0.85, 0.75))

## b) Aves = Avian Diet DB gut (AVONET-massed, clean), obstype=Stomach
adb <- read.csv(file.path(ROOT,"data","structured","aviandiet_gut_ppmr.csv"))
adb <- adb[is.finite(adb$pred_mass_g) & is.finite(adb$prey_mass_g) & adb$pred_mass_g>0 & adb$prey_mass_g>0,]
aves <- data.frame(preyMass=log(adb$prey_mass_g), predmass=log(adb$pred_mass_g), clade="Aves", prey_type=ptype(adb$prey_class),
                   obstype="Stomach", species=adb$bird_species, study=adb$source_file,
                   pred_specimen=adb$source_file, certainty=0.75)  # ADB = AVONET/published species masses (literature-derived); prey all Mammalia/Aves = vert

## c) new individual-resolved records (extant_new_observations) -> gives the specimen factor real individual structure
en <- read.csv(file.path(ROOT,"data","extant_new_observations.csv"), stringsAsFactors=FALSE)
en$pm <- num(en$pred_mass_g); en$qm <- num(en$prey_mass_g)
en <- en[en$obs_type %in% names(OBMAP) & is.finite(en$pm) & is.finite(en$qm) & en$pm>0 & en$qm>0,]
snake_gen <- "Elaphe|Acrochordus|Lampropeltis|Pituophis|Hemorrhois|Ophiophagus|Coluber|Crotalus|Sistrurus|Morelia|Python|Naja|Vipera|Natrix|Boa|Thamnophis|Nerodia|Masticophis|Bothrops|Philodryas"
en_clade <- ifelse(en$pred_class=="Aves","Aves",
            ifelse(en$pred_class=="Mammalia","Mammalia",
            ifelse(en$pred_class=="Reptilia" & grepl("Crocodyl",en$pred_order),"Crocodylia",
            ifelse(en$pred_class=="Reptilia" & grepl(snake_gen, en$pred_genus),"Serpentes",
            ifelse(en$pred_class=="Reptilia","Lacertilia",NA)))))    # snake genera -> Serpentes; ALL other Squamata (Tropidurus, Ameiva, Crotaphytus, Varanus...) -> Lacertilia
newobs <- data.frame(preyMass=log(en$qm), predmass=log(en$pm), clade=en_clade, prey_type=ptype(en$prey_class),
                     obstype=unname(OBMAP[en$obs_type]),
                     species=paste0(en$pred_genus,"_",en$pred_species),
                     study=en$source, pred_specimen=paste0(en$source,"_",en$ref_id),  # record-level = individual where available
                     certainty=ifelse(is.na(num(en$certainty)), 0.7, num(en$certainty)))  # per-record quality flag (0.35-1.0)
newobs <- newobs[!is.na(newobs$clade), ]

dat <- rbind(ext, aves, newobs)
dat <- dat[is.finite(dat$preyMass) & is.finite(dat$predmass), ]
## DROP non-animal "prey" (prey_type NA = plant/fungus/fruit/placeholder) -- frugivory/herbivory is not
## predation. This strips the omnivorous-lizard contamination (Salvator merianae tegu fruit, Varanus plant/egg)
## that was inflating Lacertilia "prey mass". Applied to ALL variants -> a pure data-quality fix, not a model knob.
n_pre <- nrow(dat); dat <- dat[dat$prey_type %in% c("invert","vert"), ]
cat(sprintf("dropped %d non-animal-prey rows (plant/fungus/unknown); kept %d animal-prey rows\n", n_pre-nrow(dat), nrow(dat)))
cat(sprintf("assembled extant: %d rows | sources: longform=%d ADB=%d newobs=%d\n",
            nrow(dat), nrow(ext), nrow(aves), nrow(newobs)))
cat("prey_type x clade (post-drop):\n"); print(table(dat$clade, dat$prey_type))

## ============ MATCHED-FILTER: VERTEBRATE-PREY ONLY (default) ============
## Fossilisation preferentially preserves VERTEBRATE prey in vertebrate guts (bone survives digestion/diagenesis;
## invertebrate cuticle/soft prey largely do not, and are rarely reported). So the extant analogue is matched to
## the fossil taphonomic filter by EXCLUDING invertebrate prey -- we compare vert-prey predation to vert-prey
## predation, not correct an unbiased reference. [JM 2026-06-03.] Set GUT_KEEPINVERT=1 to retain inverts and use
## GUT_PREYTYPE to model the invert/vert split instead. With vert-only, prey_type is constant -> dropped from the
## formula automatically below (the model reverts to the clean clade-fixed form on the matched subset).
if (!flag("GUT_KEEPINVERT")) {
  n_iv <- sum(dat$prey_type=="invert")
  dat  <- dat[dat$prey_type=="vert", ]
  cat(sprintf("VERT-ONLY (matched-filter): dropped %d invertebrate-prey rows; kept %d vertebrate-prey rows\n", n_iv, nrow(dat)))
}

## ================= 2. BALANCED, MASS- + PREY-TYPE-STRATIFIED SUBSAMPLE =================
## Replaces uniform-random-per-cell, which let single studies/species dominate (e.g. Lacertilia Scat was
## 99% one study / 2 species; Crocodylia 93% one species). Cells are now clade x obstype x PREY_TYPE: stratifying
## by prey_type is ESSENTIAL because the old mass-spread cap was prey-type-BLIND -- it spread points across each
## combo's COMBINED invert+vert mass range, so the sparse large-mass VERTEBRATE tail (the fossil-relevant subset)
## got preferentially thinned (Crocodylia: 1189 vert rows -> 51). With a vert cell of its own, the vert mass range
## is sampled on its own terms. Within a cell we cap per (study x species) combo (MASS-SPREAD subset) then cap the
## cell. The (1|species)+(1|pred_specimen) REs already absorb within-study pseudoreplication, so the caps can be
## generous (150/1500, was 40/500): this uses ~4x more data. NB sparse cells (Croc vert = 3 species / ~4 studies)
## are a real DATA limit no resampling can cure. QUALITY enters separately via weights=certainty (below).
set.seed(20260601)
SS_CAP <- as.integer(Sys.getenv("GUT_SS_CAP","150"))               # max rows per (study x species) within a cell
balanced <- function(ix) {                                         # ix = row indices of one clade x obstype x prey_type cell
  ## RETAIN ALL STOMACH: extant stomach contents share the fossil gut's digestion filter (small/soft prey
  ## under-represented), so Stomach is the matched analog of preservation bias (d) -- the level fossils are
  ## predicted at. It is the highest-fidelity reference and is kept in FULL (never capped). Non-stomach modes
  ## (Pellet/Scat/Follow) only inform the obstype offset, so they are capped to stop the 95k bird pellets swamping.
  if (!SMOKE && as.character(dat$obstype[ix[1]]) == "Stomach") return(ix)
  k <- unlist(lapply(split(ix, interaction(dat$study[ix], dat$species[ix], drop=TRUE)), function(g){
    if (length(g) <= SS_CAP) return(g)
    o <- g[order(dat$predmass[g])]; o[unique(round(seq(1, length(o), length.out=SS_CAP)))]  # mass-spread within combo
  }))
  if (CAP > 0L && length(k) > CAP) k <- sample(k, CAP)             # cell ceiling (pool already study/species-balanced)
  k
}
before <- nrow(dat)
keep <- unlist(lapply(split(seq_len(nrow(dat)), interaction(dat$clade,dat$obstype,dat$prey_type,drop=TRUE)), balanced))
dat <- dat[sort(keep), ]
cat(sprintf("balanced subsample: %d -> %d rows (ALL stomach retained; non-stomach <=%d per study x species, <=%d per cell)\n", before, nrow(dat), SS_CAP, CAP))
cat("clade x obstype after subsample (Stomach = matched-filter target):\n"); print(table(dat$clade, dat$obstype))
PM_BAR <- mean(dat$predmass); dat$predmass_c <- dat$predmass - PM_BAR
dat$obstype   <- relevel(factor(dat$obstype),   ref="Stomach")      # Stomach = reference (fossil gut ~ Stomach level)
dat$prey_type <- relevel(factor(dat$prey_type), ref="vert")          # vert = reference -> clade lines + fossil placement are at VERTEBRATE-prey level (all fossils ate verts: apples-to-apples)
for (v in c("clade","species","study","pred_specimen")) dat[[v]] <- factor(dat[[v]])
cat("clade x obstype after subsample:\n"); print(table(dat$clade, dat$obstype))
## meta for downstream fossil placement (x07): centering + factor levels + per-clade predmass range
saveRDS(list(PM_BAR=PM_BAR, clades=levels(dat$clade), obstypes=levels(dat$obstype),
             prey_types=levels(dat$prey_type),
             clade_predmass_range=tapply(dat$predmass, dat$clade, range)),
        file.path(ROOT,"data",paste0("gut_model_meta",SUFFIX,".rds")))

## ================= 3. MODEL (extant-only): clade as a FIXED effect =================
## 5 well-sampled clades -> no need for partial pooling. The clade RANDOM effect was SHRINKING the extreme clades
## (Lacertilia eats large prey -> pulled down; Crocodylia small prey -> pulled up), biasing the clade-level lines
## that fossil placement uses (Lac +1.5, Croc -1.0). Fixed clade lets each clade's intercept+slope fit freely.
## obstype still acts on BOTH intercept and slope (JM: size-dependent observation bias; Follow slope-shift P=1.0).
## --- LACERTILIA variant axes (env-controlled; each combo = one fit, distinguished by GUT_SUFFIX) ---
##   GUT_PREYTYPE = 0 | main | clade   add prey_type (vert/invert): none / main effect / prey_type*clade
##   GUT_OBSCLADE = 0/1                 add obstype:clade (Option A: clade-specific observation bias)
## Synthesis (see decisions_lacertilia.md): prey_type separates the bimodal lizard diet AND places vert-prey
## fossils against vert-prey predation (apples-to-apples); obstype:clade is the pure-model alternative.
PREYTYPE <- tolower(Sys.getenv("GUT_PREYTYPE","0"))
OBSCLADE <- flag("GUT_OBSCLADE")
multi_pt <- nlevels(droplevels(dat$prey_type)) > 1                  # prey_type modellable only if both levels present
if (PREYTYPE!="0" && !multi_pt) { cat("NOTE: single prey_type level (vert-only) -> ignoring GUT_PREYTYPE; clean clade-fixed model\n"); PREYTYPE <- "0" }
mean_terms <- c("predmass_c * clade", "predmass_c * obstype")
sig_terms  <- c("0 + clade", "predmass_c", "obstype")
if (PREYTYPE=="main")  { mean_terms <- c(mean_terms, "prey_type");         sig_terms <- c(sig_terms, "prey_type") }
if (PREYTYPE=="clade") { mean_terms <- c(mean_terms, "prey_type * clade");  sig_terms <- c(sig_terms, "prey_type") }
if (OBSCLADE)            mean_terms <- c(mean_terms, "obstype:clade")
mean_terms <- c(mean_terms, "(1|species)", "(1|pred_specimen)")
sigform  <- as.formula(paste("sigma ~", paste(sig_terms, collapse=" + ")))
meanform <- as.formula(paste("preyMass | weights(certainty) ~", paste(mean_terms, collapse=" + ")))
cat(sprintf("\nVARIANT: prey_type=%s obstype:clade=%s\n  mean : %s\n  sigma: %s\n",
            PREYTYPE, OBSCLADE, deparse1(meanform[[3]]), deparse1(sigform[[3]])))
bform <- bf(meanform, sigform)
ref <- levels(dat$clade)[1]; clade_dev <- paste0("clade", setdiff(levels(dat$clade), ref))   # non-ref clade intercept coefs
Prior <- c(
  prior(student_t(3, 0, 5), class="Intercept"),                  # reference-clade (Aves) intercept
  prior(normal(0.85, 0.25), class="b", coef="predmass_c"),       # reference-clade SLOPE = literature anchor (Tucker2014/Costa2008)
  prior(normal(0, 0.5),     class="b"),                          # default: obstype, predmass:obstype, predmass:clade slope-devs
  prior(normal(0, 1),       class="b", dpar="sigma"))            # log-sigma: per-clade + predmass + obstype
for (cd in clade_dev) Prior <- c(Prior, set_prior("student_t(3, 0, 5)", class="b", coef=cd))  # clade INTERCEPT devs WIDE = the fix (the default 0.5 would re-shrink them)

CH <- if (SMOKE) 1 else as.integer(Sys.getenv("GUT_CHAINS","4"))
IT <- if (SMOKE) 400 else as.integer(Sys.getenv("GUT_ITER","2500"))
WU <- if (SMOKE) 200 else as.integer(Sys.getenv("GUT_WARMUP","1000"))
NCORES <- as.integer(Sys.getenv("STAN_CORES","4"))
FIT <- file.path(ROOT,"data", paste0("fit_gutslopes", if (SMOKE) "_smoke" else "", SUFFIX, "_cfix.rds"))
if (!file.exists(FIT)) {
  cat("\n>>> fitting clade-FIXED model ...\n")
  fit <- brm(bform, dat, family=gaussian(), prior=Prior, chains=CH, cores=min(CH,NCORES),
             warmup=WU, iter=IT, control=list(adapt_delta=0.95, max_treedepth=TREEDEPTH), seed=1)
  saveRDS(fit, FIT); cat("saved (post-sampling) ", FIT, "\n")   # persist immediately -> a later interrupt can't lose the fit
  if (flag("GUT_LOO")) { fit <- tryCatch(add_criterion(fit, "loo"), error=function(e){ cat("(loo failed)\n"); fit }); saveRDS(fit, FIT); cat("added loo\n") }  # LOO is slow on 15k rows; only when comparing variants
} else { fit <- readRDS(FIT); cat("loaded ", FIT, "\n") }

## --- LOO: clade-fixed vs the previous clade-RANDOM interaction fit (does freeing clade help?) ---
prevf <- file.path(ROOT,"data", paste0("fit_gutslopes",SUFFIX,"_int.rds"))
if (file.exists(prevf) && !SMOKE) tryCatch({ clade_random <- readRDS(prevf); clade_fixed <- fit
  cat("\n=== LOO: clade_random (prev) vs clade_fixed (new) ===\n"); print(loo_compare(clade_random, clade_fixed)) },
  error=function(e) cat("LOO vs prev failed: ", conditionMessage(e), "\n"))

## ================= 4. POST-PROCESSING (clade fixed) =================
draws <- as_draws_df(fit); clades <- levels(dat$clade)
qci <- function(x) c(median(x), quantile(x, c(.05,.95)))
slope_draw <- function(cl) if (cl==ref) draws[["b_predmass_c"]] else draws[["b_predmass_c"]] + draws[[sprintf("b_predmass_c:clade%s", cl)]]
int_draw   <- function(cl) if (cl==ref) draws[["b_Intercept"]]  else draws[["b_Intercept"]]  + draws[[sprintf("b_clade%s", cl)]]
slope_tab <- t(sapply(clades, function(cl) qci(slope_draw(cl)))); colnames(slope_tab) <- c("slope","lo90","hi90")
cat("\n=== per-clade prey~pred SLOPE at Stomach (clade FIXED; median, 90% CrI) ===\n"); print(round(slope_tab,3))
int_tab <- t(sapply(clades, function(cl) qci(int_draw(cl)))); colnames(int_tab) <- c("intercept","lo90","hi90")
cat("\n=== per-clade INTERCEPT (ln prey at mean predmass, Stomach) ===\n"); print(round(int_tab,3))

rep_block <- function(nm, what){ if(!length(nm)) return(invisible())
  cat(sprintf("\n=== obstype effect on %s (vs Stomach) ===\n", what))
  for(n in nm){ d<-draws[[n]]; cat(sprintf("  %-30s %+.3f [%+.3f, %+.3f] P(>0)=%.2f\n",
    sub("^b_(sigma_)?","",n), median(d), quantile(d,.05), quantile(d,.95), mean(d>0))) } }
rep_block(grep("^b_obstype", names(draws), value=TRUE),            "INTERCEPT ln(prey)")
rep_block(grep("^b_predmass_c:obstype", names(draws), value=TRUE), "SLOPE (size-dependence)")

sig_d <- sapply(clades, function(cl) exp(draws[[sprintf("b_sigma_clade%s", cl)]]))
sig_tab <- t(apply(sig_d, 2, qci)); colnames(sig_tab) <- c("sigma","lo90","hi90")
cat("\n=== per-clade prey-size BREADTH (residual sigma) ===\n"); print(round(sig_tab,3))
sl <- draws[["b_sigma_predmass_c"]]
cat(sprintf("\nsigma~predmass: %.3f [%.3f, %.3f]; P(>0)=%.2f\n", median(sl),quantile(sl,.05),quantile(sl,.95),mean(sl>0)))
rep_block(grep("^b_sigma_obstype", names(draws), value=TRUE), "log-sigma (breadth)")
write.csv(cbind(clade=rownames(slope_tab), round(slope_tab,3)), file.path(ROOT,"data",paste0("gut_slopes_posterior",SUFFIX,".csv")), row.names=FALSE)
write.csv(cbind(clade=rownames(sig_tab),   round(sig_tab,3)),   file.path(ROOT,"data",paste0("gut_sigma_by_clade",SUFFIX,".csv")), row.names=FALSE)

## --- DID clade-fixed fix the misfit? per-clade obstype-adjusted gap (was Lac +1.49, Croc -0.95; target ~0) ---
nd0 <- dat; nd0$obstype <- factor("Stomach", levels=levels(dat$obstype))
ob_eff <- fitted(fit, newdata=dat, re_formula=NA)[,"Estimate"] - fitted(fit, newdata=nd0, re_formula=NA)[,"Estimate"]
dat$preyMass_adj <- dat$preyMass - ob_eff
## Gap is now measured on VERTEBRATE-prey rows only (the fossil-relevant comparison): does the vert-prey
## clade line match the vert-prey data? (Mixing in invert prey would create an artificial "gap" = real diet.)
vp <- dat$prey_type=="vert"
gridc <- do.call(rbind, lapply(clades, function(cl) data.frame(clade=cl,
            predmass_c=mean(dat$predmass_c[dat$clade==cl & vp]), obstype="Stomach",
            prey_type="vert", species=NA, pred_specimen=NA)))
linec <- fitted(fit, newdata=gridc, re_formula=NA, allow_new_levels=TRUE)[,"Estimate"]
cat("\n=== clade-level fit check (VERT-prey population-line gap; NB (1|species) absorbs guild structure) ===\n")
for (i in seq_along(clades)) { s <- dat$clade==clades[i] & vp
  cat(sprintf("  %-11s gap=%+.2f  (n_vert=%d)\n", clades[i], if(any(s)) mean(dat$preyMass_adj[s])-linec[i] else NA, sum(s))) }
## FOSSIL-RELEVANT diagnostic: where does the observed VERT-prey median sit in each clade's POSTERIOR-PREDICTIVE
## at the clade mean predmass (Stomach, vert, a NEW species -> integrates the species-RE spread, exactly as x07
## places fossils)? ~0.5 = predictive centred on the saurophagy data; extreme = the band a fossil sees is biased.
ppc <- tryCatch(do.call(rbind, lapply(clades, function(cl){ s <- dat$clade==cl & vp; if(!any(s)) return(NULL)
  g <- data.frame(clade=cl, predmass_c=mean(dat$predmass_c[s]), obstype="Stomach", prey_type="vert",
                  species=paste0("__new_",cl), pred_specimen=paste0("__new_",cl))
  pp <- posterior_predict(fit, newdata=g, allow_new_levels=TRUE, sample_new_levels="gaussian")
  data.frame(clade=cl, pctile=mean(pp < median(dat$preyMass[s]))) })), error=function(e) NULL)
if (!is.null(ppc)) { cat("\n=== fossil-relevant: obs vert-prey median percentile in clade predictive (target ~0.5) ===\n")
  for(i in seq_len(nrow(ppc))) cat(sprintf("  %-11s pctile=%.2f\n", ppc$clade[i], ppc$pctile[i])) }

## ================= 5. PLOT (obstype-adjusted points; clade-fixed population lines) =================
plotOK <- tryCatch({
  spmed <- aggregate(cbind(preyMass_adj, predmass) ~ species+clade, dat, median)
  xr <- range(dat$predmass)
  grid <- expand.grid(predmass_c=seq(xr[1],xr[2],length=40)-PM_BAR, clade=clades, obstype="Stomach", prey_type="vert", species=NA, pred_specimen=NA)
  fv <- fitted(fit, newdata=grid, re_formula=NA, probs=c(.05,.95), allow_new_levels=TRUE)
  rib <- cbind(grid, predmass=grid$predmass_c+PM_BAR, est=fv[,"Estimate"], lo=fv[,"Q5"], hi=fv[,"Q95"])
  fp <- read.csv(file.path(ROOT,"data","fossil_ppmr_points.csv"), stringsAsFactors=FALSE)
  fp <- fp[is.finite(fp$pred_mass_g) & is.finite(fp$prey_mass_g) & fp$in_fit %in% c(TRUE,"TRUE"), ]
  fossil <- data.frame(predmass=log(fp$pred_mass_g), preyMass=log(fp$prey_mass_g), clade=fp$pred_clade,
                       lab=sub("[_ ].*","",fp$predator), is_thero=fp$pred_clade=="Theropoda")
  p <- ggplot() +
    geom_ribbon(data=rib, aes(predmass, ymin=lo, ymax=hi, fill=clade), alpha=.16) +
    geom_line(data=rib, aes(predmass, est, color=clade), linewidth=.9) +
    geom_point(data=spmed, aes(predmass, preyMass_adj, color=clade), alpha=.35, size=1.1) +
    geom_abline(slope=1, intercept=0, linetype="dotted", color="grey55") +
    geom_point(data=fossil[!fossil$is_thero,], aes(predmass, preyMass, color=clade), shape=18, size=3.4) +
    geom_point(data=fossil[fossil$is_thero,],  aes(predmass, preyMass), shape=17, size=2.6, color="black") +
    geom_text_repel(data=fossil, aes(predmass, preyMass, label=lab), size=2.3, fontface="italic", max.overlaps=Inf, color="grey25") +
    labs(title="Extant prey~predator scaling -- clade FIXED + obstype(intercept+slope); obstype-adjusted points",
         subtitle="EXTANT-only; species-median points adjusted to Stomach-equivalent. Tri=theropod, diamond=fossil croc/snake/mammal. Dotted=isometry.",
         x="ln(predator mass, g)", y="ln(prey mass, g)  [obstype-adjusted to Stomach]") +
    theme_minimal(base_size=12) + theme(plot.subtitle=element_text(color="grey40", size=8))
  ggsave(file.path(ROOT,"figures",paste0("gut_slopes_comparison",SUFFIX,".png")), p, width=9.5, height=6.2, dpi=150)
  ggsave(file.path(ROOT,"figures",paste0("gut_slopes_comparison",SUFFIX,".pdf")), p, width=9.5, height=6.2)
  TRUE }, error=function(e){ cat("PLOT failed: ", conditionMessage(e), "\n"); FALSE })

cat("\nDONE clade-fixed. fit: ", FIT, " | plot ok: ", plotOK, "\n", sep="")
cat("NOTE: fossils are OVERLAY ONLY (not fit). Formal placement w/ mass-uncertainty propagation = x07.\n")
