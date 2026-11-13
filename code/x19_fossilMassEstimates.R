###################################################################
# x19_fossilMassEstimates.R
# COARSE mass estimates for theropod (and related) gut/diet cases that lack a
# predator and/or prey mass, so they can enter the PPMR analyses. Every value
# is an ESTIMATE with an explicit method + low/high range; nothing here is a
# measured mass. Writes data/fossil_mass_estimates.csv and prints a report.
# Companion human-readable writeup: data/theropod_gut_mass_estimates.md.
#
# Calibrated regressions (fit from local data):
#   benson  : theropod femur(mm) -> mass; theropoda_benson.csv (mass in KG)
#   campione: quadruped femur(mm) -> mass(g); Campione & Evans 2012 (n=242, R2=.93)
# Coarse scalings (documented anchors; WIDE ranges):
#   fish     : W(g) = K * TL_cm^3, K in [0.005,0.02] (Fulton condition factor ~ 1)
#   squamate : femur->SVL (SVL=femur/0.21) then mass(g) = 1.5e-5 * SVL_mm^3
#   mammal_mz: Mesozoic mammal mass from jaw size / published taxon estimates
#   ornithopod: mass(kg) = 4 * (TL_m)^3 (cube-law anchored to juvenile ornithopods)
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(readxl)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())

## --- fit the two calibrated regressions --------------------------
b <- read.csv(file.path(ROOT,"data","mass_references","theropoda_benson.csv"), check.names=TRUE)
b$femur <- suppressWarnings(as.numeric(b$femur)); b$mass <- suppressWarnings(as.numeric(b$mass))
b <- b[is.finite(b$femur)&is.finite(b$mass)&b$femur>0&b$mass>0,]
fitB <- lm(log(mass)~log(femur), b)                         # mass in KG
ca <- suppressMessages(read_excel(file.path(ROOT,"data","mass_references","12915_2012_575_MOESM1_ESM.XLS")))
names(ca) <- make.names(names(ca))
cm <- suppressWarnings(as.numeric(ca$Body.Mass..g.)); cf <- suppressWarnings(as.numeric(ca$Femur.Length))
ok <- is.finite(cm)&is.finite(cf)&cm>0&cf>0; fitC <- lm(log(cm[ok])~log(cf[ok]))

benson   <- function(FL) { p<-predict(fitB,data.frame(femur=FL),interval="prediction",level=.95); exp(p)*1000 } # ->g, [fit,lo,hi]
campione <- function(FL) { fv <- coef(fitC)[1] + coef(fitC)[2]*log(FL); s <- summary(fitC)$sigma
                           c(exp(fv), exp(fv-1.96*s), exp(fv+1.96*s)) }   # ->g, [fit, lo, hi] crude PI
fish_g     <- function(TLcm) c(0.01*TLcm^3, 0.005*TLcm^3, 0.02*TLcm^3)
squamate <- function(FLmm=NA, SVLmm=NA) { svl <- if(is.finite(SVLmm)) SVLmm else FLmm/0.21
                           m<-1.5e-5*svl^3; c(m, m/2.5, m*2.5) }
ornith_kg  <- function(TLmm) { kg<-4*(TLmm/1000)^3; c(kg, kg/3, kg*3)*1000 }     # ->g
# mammal dentary(mandible) LENGTH -> body mass: power law anchored to extant small
# mammals (Sorex 9mm~6g, Mus 13mm~20g, Rattus 28mm~250g => b~3.3); WIDE x/ ÷2 range.
# Replaces the earlier jaw-eyeballed mammal_mz_coarse for cases WITH a measured dentary.
# (A proper Mesozoic-mammal jaw->mass regression would tighten this; see external_data_wanted.)
mammal_jaw <- function(dentary_mm) { m <- 0.00432*dentary_mm^3.29; c(m, m/2, m*2) }

## --- per-case estimates (measurements from fossil tables / source papers) ---
# helper: build a row. mass cols in grams. method/notes documented.
mk <- function(case, role, taxon, est, method, basis)
  data.frame(case=case, role=role, taxon=taxon, mass_g=signif(est[1],3), low_g=signif(est[2],3),
             high_g=signif(est[3],3), method=method, basis=basis, stringsAsFactors=FALSE)

E <- rbind(
  # --- PREDATORS needing a mass ---
  mk("Huadanosaurus_gut","predator","Huadanosaurus_sinensis", benson(105.69),
     "benson_femur", "femur LENGTH 105.69 mm, no circumference (Qiu 2025) -> Benson femur-length regression R2=0.97"),
  mk("Confractosuchus_gut","predator","Confractosuchus_sauroktonos", c(75000,50000,100000),
     "expert_estimate", "~50-100 kg (JM, 2026-06; supersedes coarse TL guess); no femur for Campione"),
  mk("Scipionyx_gut","predator","Scipionyx_samniticus", c(30,15,60),
     "hatchling_coarse", "perinatal compsognathid, TL ~220 mm; ~15-60 g hatchling"),
  # --- PREY: lizards in a belly ---
  mk("Coelophysis_gut","prey","Hesperosuchus_crocodylomorph", campione(71.2),
     "campione_femur", "sphenosuchian femur 71.2 mm (Nesbitt 2006); Campione quad regression"),
  mk("Compsognathus_gut","prey","Schoenesmahl_dyspepsia", squamate(FLmm=18.2),
     "squamate_femur", "lizard femur 18.2 mm -> SVL ~87 mm (Conrad 2017)"),
  mk("Anchiornis_gut","prey","Squamata_indet", squamate(FLmm=8.04),
     "squamate_femur", "tiny lizard femur 8.04 mm -> SVL ~38 mm (Zheng 2018)"),
  mk("Scipionyx_gut","prey","Squamata_indet_large", squamate(SVLmm=275*0.45),
     "squamate_TL", "lizard TL 275 mm -> SVL ~124 mm"),
  mk("Scipionyx_gut","prey","Squamata_indet_small", squamate(SVLmm=110*0.45),
     "squamate_TL", "lizard TL 110 mm -> SVL ~50 mm"),
  # --- PREY: Mesozoic mammals ---
  mk("Sinosauropteryx_gut","prey","Zhangheotherium", c(30,15,70),
     "mammal_mz_coarse", "symmetrodont, dentary 27.4 mm; small mammal ~15-70 g"),
  mk("Sinosauropteryx_gut","prey","Sinobaatar", c(12,5,30),
     "mammal_mz_coarse", "multituberculate, dentary-height 6.2 mm; ~5-30 g"),
  mk("Huadanosaurus_gut","prey","Gobiconodontidae_cf_Meemannodon", mammal_jaw(15),
     "mammal_jaw", "dentary ~15 mm (Qiu 2025); robust gobiconodontid -> upper part of range"),
  mk("Huadanosaurus_gut","prey","Eutheria_indet", mammal_jaw(15),
     "mammal_jaw", "dentary ~15 mm (Qiu 2025); gracile eutherian -> lower part of range"),
  # --- PREY: fish ---
  mk("Microraptor_fish_gut","prey","Osteichthyes_indet", fish_g(6),
     "fish_coarse", "small fish, TL ~6 cm (Xing 2013, fin-ray scale)"),
  mk("Scipionyx_gut","prey","Osteichthyes_indet_a", fish_g(4.5),
     "fish_coarse", "fish TL 45 mm"),
  mk("Scipionyx_gut","prey","Osteichthyes_indet_b", fish_g(2.5),
     "fish_coarse", "fish TL 25 mm"),
  # --- PREY: ornithischians ---
  mk("Daspletosaurus_gut","prey","Hadrosauridae_juv_small", ornith_kg(1100),
     "ornithopod_TL", "juvenile hadrosaur TL 1100 mm"),
  mk("Daspletosaurus_gut","prey","Hadrosauridae_juv_large", ornith_kg(3000),
     "ornithopod_TL", "juvenile hadrosaur TL 3000 mm"),
  mk("Repenomamus_gut","prey","Psittacosaurus_juv", c(120,50,250),
     "ornith_coarse", "juvenile Psittacosaurus HB ~140 mm (Hu 2005) ~50-250 g")
)
## --- resulting PPMR per theropod gut case (predator mass x prey estimate) ----
# predator mass (g): measured/Benson from the fossil tables, or estimated here.
predmass <- c(Coelophysis_gut=1565, Compsognathus_gut=1602, Anchiornis_gut=1648,
              Sinosauropteryx_gut=1655, Microraptor_fish_gut=764, Daspletosaurus_gut=1375938,
              Repenomamus_gut=5000, Scipionyx_gut=30, Huadanosaurus_gut=3704, Confractosuchus_gut=75000)
prey <- E[E$role=="prey",]
prey$pred_mass_g <- predmass[prey$case]
prey$ppmr_ln     <- round(log(prey$mass_g)     - log(prey$pred_mass_g), 2)
prey$ppmr_lo     <- round(log(prey$low_g)      - log(prey$pred_mass_g), 2)   # prey-mass uncertainty only
prey$ppmr_hi     <- round(log(prey$high_g)     - log(prey$pred_mass_g), 2)
write.csv(E, file.path(ROOT,"data","fossil_mass_estimates.csv"), row.names=FALSE)
write.csv(prey[,c("case","taxon","pred_mass_g","mass_g","ppmr_ln","ppmr_lo","ppmr_hi","method")],
          file.path(ROOT,"data","theropod_gut_ppmr_estimated.csv"), row.names=FALSE)
cat("\n=== EXPANDED theropod-gut PPMR (estimated; ln prey/pred) ===\n")
print(prey[order(prey$ppmr_ln), c("case","taxon","pred_mass_g","mass_g","ppmr_ln","ppmr_lo","ppmr_hi")], row.names=FALSE)
cat(sprintf("Benson: ln(mass_kg)=%.2f+%.2f*ln(FL)  | Campione: ln(mass_g)=%.2f+%.2f*ln(FL)\n",
            coef(fitB)[1],coef(fitB)[2],coef(fitC)[1],coef(fitC)[2]))
cat(sprintf("\nWrote data/fossil_mass_estimates.csv: %d estimates across %d cases\n",
            nrow(E), length(unique(E$case))))
print(E[,c("case","role","taxon","mass_g","low_g","high_g","method")], row.names=FALSE)
