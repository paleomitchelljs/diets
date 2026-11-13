###################################################################
# x22_buildFossilPoints.R
# SINGLE SOURCE OF TRUTH for fossil predator-prey PPMR points.
# Consolidates three cleanly-partitioned sources into ONE deduped,
# lm-Benson-standardized file that BOTH x07 (figures) and x20 (model)
# read -- killing the old dual-path ingestion. Applies the audited
# evidence-type corrections (see DATA_AUDIT.md / reconciliations.md).
#
# Partition (each case enters from exactly ONE source; priority A>B>C):
#   A = fossil_diet_extractions.csv : curated multi-clade pairs WITH both masses,
#       excluding evidence_type {body_size_only, cololite[herbivore]}.
#   B = theropod_gut_ppmr_estimated.csv (x19): coarse length-only estimates
#       (already lm-Benson predator masses); Repenomamus excluded (it's in A).
#   C = fossil_predprey.csv : classic theropod cases with DIRECT prey masses
#       not covered by A/B; predator mass RECOMPUTED via lm-Benson(femur).
#       Velociraptor x Protoceratops dropped here (dedup -> A's attack record).
#
# Evidence-type corrections applied: Anchiornis -> pellet; Minmi cololite excluded;
#   Velociraptor x azhdarchid flagged scavenged; Azdarchid spelling -> Azhdarchid.
# Output: data/fossil_ppmr_points.csv  (in_fit = TRUE for gut/pellet/stomach;
#   FALSE for attack/association = plotted, not fit).
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(readxl)))
args <- commandArgs(trailingOnly=FALSE); fa <- grep("^--file=", args, value=TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
num <- function(x) suppressWarnings(as.numeric(x))

## --- lm-Benson (theropod femur mm -> mass g); the project standard ---
b <- read.csv(file.path(ROOT,"data","mass_references","theropoda_benson.csv"), check.names=TRUE)
b$femur <- num(b$femur); b$mass <- num(b$mass); b <- b[is.finite(b$femur)&is.finite(b$mass)&b$femur>0&b$mass>0,]
fitB <- lm(log(mass)~log(femur), b)                                  # mass in KG
benson <- function(FL) round(exp(predict(fitB, data.frame(femur=FL)))*1000)   # -> g
# Femur (mm) for x19/Source-B theropod cases -> lm-Benson predator mass, OVERRIDING x19's
# hardcoded predmass (which carried errors: Anchiornis used the corrupt mass_sd=1648;
# Microraptor-fish used the BIRD specimen's 764 g). From fossil_predprey femurs.
# No femur (Daspletosaurus=dentary, Scipionyx=hatchling) -> keep x19's recorded mass.
femur_map <- c(Coelophysis=203, Compsognathus=71.5, Anchiornis=71, Microraptor=109,
               Sinosauropteryx=93.07, Huadanosaurus=105.69)

## --- clade mapping (raw clade string -> model clade) ---
model_clade <- function(pc) ifelse(grepl("Theropoda|Avialae|Pygostylia|Confuciusornith|ornithurom|Enantiorni",pc,ignore.case=TRUE),"Theropoda",  # STEM birds (avialans) bin with Theropoda -- they are COMPARED AGAINST crown Aves, not part of it
                            ifelse(grepl("Crocodyl",pc),"Crocodylia",
                            ifelse(grepl("Serpentes|Madtsoiidae",pc),"Serpentes",
                            ifelse(grepl("Lacert|Varan|Squamata",pc),"Lacertilia",
                            ifelse(grepl("Mammal|Triconodonta",pc),"Mammalia",
                            ifelse(grepl("Aves",pc,ignore.case=TRUE),"Aves",NA))))))                # crown Aves ONLY (stem birds caught above)
mk <- function(predator,prey,pred_clade,prey_clade,pred_mass,prey_mass,prey_lo,prey_hi,evidence,source,pred_specimen,flags="")
  data.frame(predator,pred_specimen=pred_specimen,prey,pred_clade_raw=pred_clade,prey_clade_raw=prey_clade,
             pred_clade=model_clade(pred_clade), prey_mass_g=prey_mass, prey_lo_g=prey_lo, prey_hi_g=prey_hi,
             pred_mass_g=pred_mass, evidence_type=evidence, source=source, flags=flags, stringsAsFactors=FALSE)

## ===== SOURCE A: curated extractions (complete pairs; drop body_size_only + cololite) =====
de <- read.csv(file.path(ROOT,"data","fossil_diet_extractions.csv"), check.names=TRUE)
de$pm <- num(de$pred_mass_g); de$qm <- num(de$prey_mass_g)
A <- subset(de, is.finite(pm) & is.finite(qm) & !evidence_type %in% c("body_size_only","cololite"))
Arows <- mk(A$name_pred, A$name_prey, A$clade_pred, A$clade_prey, A$pm, A$qm,
            num(A$prey_mass_low_g), num(A$prey_mass_high_g), A$evidence_type, A$paper, A$specimen_pred)

## ===== SOURCE B: x19 coarse estimates (lm-Benson predators); drop Repenomamus (in A) =====
te <- read.csv(file.path(ROOT,"data","theropod_gut_ppmr_estimated.csv"))
te <- te[!grepl("Repenomamus", te$case),]
gen <- sub("_.*","",te$case)
ev  <- ifelse(grepl("Anchiornis",gen),"pellet","gut_content")          # Anchiornis = PELLET (Zheng 2018)
fem <- femur_map[gen]                                                   # femur-based mass overrides x19's buggy values
predB <- ifelse(is.na(fem), te$pred_mass_g, benson(fem))
Brows <- mk(gen, te$taxon, "Theropoda", te$taxon, predB, te$mass_g,
            NA, NA, ev, "x19_estimate; pred mass=lm-Benson(femur) where available", te$case)  # te$case groups multi-prey from ONE individual (e.g. Scipionyx_gut x4) for the specimen random effect

## ===== SOURCE C: predprey classic cases with DIRECT prey mass, not in A/B =====
pp <- read.csv(file.path(ROOT,"data","fossil_predprey.csv"), check.names=TRUE)
pp$fl <- num(pp$femur_l); pp$qm <- num(pp$mass_prey)
# keep only rows with a DIRECT prey mass; drop Velociraptor x Protoceratops (dedup -> A)
C <- subset(pp, is.finite(qm) & !(grepl("Velociraptor",name_pred) & grepl("Protoceratops",name_prey)))
# predator mass: lm-Benson(femur) where femur present, else recorded mass_pred
predmass_C <- ifelse(is.finite(C$fl), benson(C$fl), num(C$mass_pred))
prey <- ifelse(grepl("Azdarchid|Azhdarchid", C$name_prey), "Azhdarchid_indet", C$name_prey)  # spelling fix only (NOT a scavenging judgement; hunt-vs-scavenge is unobservable in BOTH extant & fossil data, so never flagged)
Crows <- mk(C$name_pred, prey, C$clade_pred, C$clade_prey, predmass_C, C$qm, NA, NA,
            "gut_content", C$source, C$specimen_pred, "")

## ===== combine, dedupe (A>B>C by predator-genus + prey), compute PPMR, in_fit =====
all <- rbind(cbind(Arows,tier=1), cbind(Brows,tier=2), cbind(Crows,tier=3))
key <- paste(tolower(sub("[_ ].*","",all$predator)), tolower(gsub("[ _]","",all$prey)))
mintier <- ave(all$tier, key, FUN=min)                                  # A>B>C priority
all <- all[all$tier == mintier, ]; all$tier <- NULL                     # keep ALL rows from the highest-priority source holding each (genus,prey): drops cross-source redundancy but PRESERVES genuine multi-prey within one source (e.g. Gorgosaurus x2 Citipes). Within-individual non-independence is handled by the (1|pred_specimen) random effect downstream, NOT by deleting data.
all$ppmr   <- round(log(all$prey_mass_g) - log(all$pred_mass_g), 2)
all$ppmr_lo <- round(log(all$prey_lo_g)  - log(all$pred_mass_g), 2)
all$ppmr_hi <- round(log(all$prey_hi_g)  - log(all$pred_mass_g), 2)
all$in_fit <- !grepl("attack|assoc", all$evidence_type)                 # gut/pellet IN fit; attack/assoc = plot only
all <- all[is.finite(all$ppmr) & !is.na(all$pred_clade), ]
all <- all[order(all$pred_clade, all$ppmr), ]

out <- all[,c("predator","pred_specimen","prey","pred_clade","pred_clade_raw","prey_clade_raw",
              "pred_mass_g","prey_mass_g","prey_lo_g","prey_hi_g","ppmr","ppmr_lo","ppmr_hi",
              "evidence_type","in_fit","source","flags")]
write.csv(out, file.path(ROOT,"data","fossil_ppmr_points.csv"), row.names=FALSE)
cat(sprintf("Wrote data/fossil_ppmr_points.csv: %d points (in_fit=%d)\n", nrow(out), sum(out$in_fit)))
cat("by clade x evidence:\n"); print(table(out$pred_clade, out$evidence_type))
cat("\n=== points ===\n")
print(out[,c("predator","prey","pred_clade","pred_mass_g","prey_mass_g","ppmr","evidence_type","in_fit")], row.names=FALSE)
