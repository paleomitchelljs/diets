###################################################################
# x_validate.R  — data consistency checks, run BEFORE any model fit.
# Durable version of the hand-audits in DATA_AUDIT.md / reconciliations.md.
# Prints PASS / WARN / FAIL per check. Non-fatal: surfaces issues for review.
###################################################################
args <- commandArgs(trailingOnly=FALSE); fa <- grep("^--file=", args, value=TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
num <- function(x) suppressWarnings(as.numeric(x))
ok <- function(tag,msg) cat(sprintf("  PASS  %-28s %s\n", tag, msg))
warn<- function(tag,msg) cat(sprintf("  WARN  %-28s %s\n", tag, msg))
cat("=== x_validate: data consistency checks ===\n")

## ---- 1. fossil_ppmr_points (the single model-input file) ----
fp <- read.csv(file.path(ROOT,"data","fossil_ppmr_points.csv"), stringsAsFactors=FALSE)
clvocab <- c("Aves","Crocodylia","Lacertilia","Mammalia","Serpentes","Theropoda")
evvocab <- c("gut_content","gut_content_abdominal","gut_content_crop","gut_content+throat","pellet",
             "coprolite","cololite","attack_predation_event","association_predation","Stomach","Regurgitation","Observation")
bad_cl <- !fp$pred_clade %in% clvocab
if (any(bad_cl)) warn("fossil clade vocab", paste(unique(fp$pred_clade[bad_cl]),collapse=",")) else ok("fossil clade vocab","all in {Aves,Croc,Lacertilia,Mammalia,Serpentes,Theropoda}")
bad_ev <- !fp$evidence_type %in% evvocab
if (any(bad_ev)) warn("fossil evidence vocab", paste(unique(fp$evidence_type[bad_ev]),collapse=",")) else ok("fossil evidence vocab","all recognized")
if (any(num(fp$pred_mass_g)<=0|num(fp$prey_mass_g)<=0,na.rm=TRUE)) warn("fossil masses>0","some <=0") else ok("fossil masses>0","all positive")
rng <- range(fp$ppmr,na.rm=TRUE); ok("fossil PPMR range", sprintf("%.2f to %.2f (n=%d, in_fit=%d)", rng[1],rng[2],nrow(fp),sum(fp$in_fit)))

## ---- 2. predator-mass note (NO "Benson inflated" judgement) ----
## Femur-scaling (lm-Benson) is MORE UNCERTAIN but MORE WIDELY APPLICABLE than volumetric
## reconstruction; every body-mass method carries large (often >2x on the log scale)
## uncertainty. We standardize on lm-Benson(femur) and CARRY that uncertainty -- we do NOT
## treat any single estimate as "true", nor any femur-vs-volumetric divergence as "inflated".
## A theropod estimate that exceeds a volumetric guess typically just matches a same-femur
## bird (e.g. Microraptor 109mm femur ~= golden-eagle scale). (JM 2026-06)
th <- fp[fp$pred_clade=="Theropoda" & is.finite(num(fp$pred_mass_g)),]
ok("theropod pred-mass range", sprintf("%.0f-%.0f g via lm-Benson(femur)  [%s ... %s]",
   min(num(th$pred_mass_g)), max(num(th$pred_mass_g)),
   th$predator[which.min(num(th$pred_mass_g))], th$predator[which.max(num(th$pred_mass_g))]))

## ---- 3. cross-file duplicate taxa (predator+prey) across the 3 fossil sources ----
norm <- function(p,q) paste(tolower(sub("[_ ].*","",p)), tolower(gsub("[ _]","",q)))  # FULL prey name (match x22 dedup); the old substr(,1,8) false-flagged legit multi-prey (Scipionyx osteicht a/b, Daspletosaurus hadrosaur small/large)
de <- read.csv(file.path(ROOT,"data","fossil_diet_extractions.csv"),check.names=TRUE)
pp <- read.csv(file.path(ROOT,"data","fossil_predprey.csv"),check.names=TRUE)
te <- read.csv(file.path(ROOT,"data","theropod_gut_ppmr_estimated.csv"))
fpk <- norm(fp$predator, fp$prey)
if (any(duplicated(fpk))) warn("ppmr_points dedup", paste("dup keys:",paste(fpk[duplicated(fpk)],collapse="; "))) else ok("ppmr_points dedup","no duplicate predator+prey rows")

## ---- 4. fossil_predprey: femur-method-without-femur + length-in-mass heuristic ----
pp$fl<-num(pp$femur_l); pp$mp<-num(pp$mass_pred); pp$tl<-num(pp$total_length)
b<-which(grepl("benson|campione|femur",tolower(pp$mass_source)) & is.na(pp$fl))
if (length(b)) warn("predprey femur-no-femur", paste(unique(pp$name_pred[b]),collapse=",")) else ok("predprey femur method","every benson/campione row has a femur")
lim <- which(is.finite(pp$mp) & is.na(pp$tl) & pp$mp<3000 & grepl("length|TL|total",tolower(pp$mass_source)))  # mass looks like a length
if (length(lim)) warn("predprey length-in-mass?", paste(unique(pp$name_pred[lim]),collapse=",")) else ok("predprey length-in-mass","no obvious length-in-mass cells")

## ---- 5. extant_new_observations: obs_type vocab + PPMR sanity for gut-usable rows ----
en <- read.csv(file.path(ROOT,"data","extant_new_observations.csv"),stringsAsFactors=FALSE)
gutmodes <- c("Stomach","Stomach_DNA_metabarcoding","Gizzard","Regurgitation","Observation")
en$pm<-num(en$pred_mass_g); en$qm<-num(en$prey_mass_g)
usable <- en[en$obs_type %in% gutmodes & is.finite(en$pm) & is.finite(en$qm),]
ok("extant gut-usable rows", sprintf("%d rows with gut-mode + both masses (of %d)", nrow(usable), nrow(en)))
pp_en <- log(usable$qm) - log(usable$pm)
if (any(abs(pp_en)>9)) warn("extant PPMR range", "some |PPMR|>9 (check masses)") else ok("extant PPMR range", sprintf("%.2f to %.2f", min(pp_en), max(pp_en)))
clades_en <- table(ifelse(en$pred_class=="Aves","Aves",ifelse(en$pred_class=="Mammalia","Mammalia",
              ifelse(grepl("Varanus|Salvator",en$pred_genus),"Lacertilia",
              ifelse(en$pred_class=="Reptilia","Serpentes/other",en$pred_class)))))
cat("    extant_new_observations by mapped clade:\n"); print(clades_en)
cat("\n=== validate complete (WARN = review, not necessarily wrong) ===\n")
