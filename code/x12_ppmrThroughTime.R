###################################################################
# x12_ppmrThroughTime.R
# BARE-MINIMUM visual check: extant clade gut-PPMR distributions (as bands)
# + individual FOSSIL PPMR points plotted against geologic age, colored by
# clade. Lets you eyeball whether extinct gut/diet ratios sit within the
# living envelope and whether anything drifts through time.
#
# NOTE: fossil ages are assigned here via a LOOKUP (formation/stage), not yet
# a column in the fossil tables -> migrate age_ma_{mid,min,max} into
# fossil_predprey.csv / fossil_diet_extractions.csv as a follow-up.
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)

## --- EXTANT gut reference (Squamata split into Serpentes / Lacertilia) ---
dat <- read.csv(file.path(ROOT, "data", "longform.csv"))
dat$clade <- ifelse(dat$clade != "Squamata", dat$clade,
                    ifelse(dat$subclade == "Serpentes", "Serpentes", "Lacertilia"))
gut <- subset(dat, obstype == "Stomach")
ext <- do.call(rbind, lapply(sort(unique(gut$clade)), function(cl) {
  v <- gut$ppreymass[gut$clade == cl]
  data.frame(clade = cl, q25 = quantile(v,.25), med = median(v), q75 = quantile(v,.75))
}))
# Replace the thin 7-heron longform Aves sample with the real bird-gut reference
# (Avian Diet DB, vertebrate prey, AVONET-massed; x15) when available.
adb <- file.path(ROOT, "data", "structured", "aviandiet_gut_ppmr.csv")
if (file.exists(adb)) {
  b <- read.csv(adb)$ppmr_ln; q <- quantile(b, c(.25,.5,.75))
  ai <- which(ext$clade == "Aves")
  ext$clade[ai] <- "Aves (AvianDiet gut)"
  ext$q25[ai] <- q[1]; ext$med[ai] <- q[2]; ext$q75[ai] <- q[3]
}
# spread near-coincident labels (Serpentes~Mammalia, Lacertilia~Crocodylia) for legibility
ext <- ext[order(ext$med), ]; ext$lab_y <- ext$med
for (i in 2:nrow(ext)) if (ext$lab_y[i] - ext$lab_y[i-1] < 0.55) ext$lab_y[i] <- ext$lab_y[i-1] + 0.55

## --- FOSSIL points: PPMR where masses exist, + age lookup + colour clade ---
age <- function(tx) switch(tx,
  Coelophysis_bauri        = c(212,205,221),  Compsognathus_longipes = c(150,148,152),
  Anchiornis               = c(160,158,162),  Microraptor_gui        = c(120,119,125),
  Sinosauropteryx_prima    = c(125,124,130),  Sinosauropteryx_primus = c(125,124,130),
  Sinocalliopteryx_gigas   = c(124,122,126),  Daspletosaurus         = c(76,75,77),
  Velociraptor_mongoliensis= c(75,71,75),     Scipionyx_samniticus   = c(113,110,115),
  Gorgosaurus_libratus_juvenile = c(76,75,77),
  Confractosuchus_sauroktonos   = c(95,92.5,104), Sanajeh_indicus     = c(68,66,70),
  c(NA,NA,NA))
clade_col <- function(cp) ifelse(grepl("Theropoda",cp),"Theropoda",
                          ifelse(grepl("Croc",cp),"Crocodyliformes",
                          ifelse(grepl("Serpentes",cp),"Serpentes (fossil)",
                          ifelse(grepl("Mammalia",cp),"Mammalia (fossil)","other"))))

fp <- read.csv(file.path(ROOT,"data","fossil_predprey.csv"), check.names=TRUE)
fp$mp <- suppressWarnings(as.numeric(fp$mass_pred)); fp$mq <- suppressWarnings(as.numeric(fp$mass_prey))
f1 <- subset(fp, is.finite(mp)&is.finite(mq))
f1 <- data.frame(pred=f1$name_pred, cp=f1$clade_pred, ppmr=log(f1$mq)-log(f1$mp),
                 ppmr_lo=NA, ppmr_hi=NA, stringsAsFactors=FALSE)
fd <- read.csv(file.path(ROOT,"data","fossil_diet_extractions.csv"), check.names=TRUE)
# Gorgosaurus (both masses)
g <- subset(fd, is.finite(suppressWarnings(as.numeric(pred_mass_g))) & is.finite(suppressWarnings(as.numeric(prey_mass_g))))
f2 <- data.frame(pred=g$name_pred, cp=g$clade_pred,
                 ppmr=log(as.numeric(g$prey_mass_g))-log(as.numeric(g$pred_mass_g)),
                 ppmr_lo=NA, ppmr_hi=NA, stringsAsFactors=FALSE)
# Confractosuchus: prey 1.0-1.7kg, predator PENDING -> envelope 25-80 kg
conf <- data.frame(pred="Confractosuchus_sauroktonos", cp="Crocodyliformes_Neosuchia",
  ppmr   = log(1350) - log(45000),
  ppmr_lo= log(1000) - log(80000),
  ppmr_hi= log(1700) - log(25000), stringsAsFactors=FALSE)
foss <- rbind(f1, f2, conf)
ag <- t(sapply(foss$pred, age)); foss$age_mid<-ag[,1]; foss$age_min<-ag[,2]; foss$age_max<-ag[,3]
foss$clade <- clade_col(foss$cp)
foss <- foss[is.finite(foss$age_mid) & is.finite(foss$ppmr), ]
cat("fossil PPMR points plotted (n=", nrow(foss), "):\n", sep=""); print(foss[,c("pred","clade","age_mid","ppmr")], row.names=FALSE)

## --- plot ---
xr <- c(220, -8)   # reversed time axis (older left); small negative pad for 'Recent'
pal <- c("Theropoda"="#1b9e77","Crocodyliformes"="#d95f02","Serpentes (fossil)"="#7570b3","Mammalia (fossil)"="#e7298a")
p <- ggplot() +
  # extant clade IQR bands spanning the time axis (grey, labelled at the Recent end)
  geom_rect(data=ext, aes(xmin=xr[1], xmax=xr[2], ymin=q25, ymax=q75), fill="grey80", alpha=.45) +
  geom_segment(data=ext, aes(x=xr[1], xend=xr[2], y=med, yend=med), color="grey55", linetype="dotted") +
  geom_text(data=ext, aes(x=218, y=lab_y, label=clade), hjust=0, vjust=-0.3, size=3, color="grey35") +
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  # fossils
  geom_errorbarh(data=foss, aes(y=ppmr, xmin=age_min, xmax=age_max, color=clade), height=.18, alpha=.7) +
  geom_errorbar(data=foss[is.finite(foss$ppmr_lo),], aes(x=age_mid, ymin=ppmr_lo, ymax=ppmr_hi, color=clade), width=3, alpha=.7) +
  geom_point(data=foss, aes(age_mid, ppmr, color=clade), size=3.2) +
  scale_x_reverse(limits=xr, breaks=seq(0,200,50)) +
  scale_color_manual(values=pal, name="fossil clade") +
  labs(title="Predator-prey mass ratio through time: fossils vs extant gut envelopes",
       subtitle="Grey bands = extant clade gut-PPMR IQR (Serpentes/Mammalia/Aves high; Croc/Lacertilia low). Confractosuchus bar = predator-mass uncertainty.",
       x="Geologic age (Ma; Recent at right)", y="log(prey mass / predator mass)") +
  coord_cartesian(xlim=c(220,-40)) +
  theme_minimal(base_size=12) + theme(plot.subtitle=element_text(color="grey40", size=8.5))
ggsave(file.path(ROOT,"figures","ppmr_through_time.png"), p, width=10, height=5.6, dpi=150)
ggsave(file.path(ROOT,"figures","ppmr_through_time.pdf"), p, width=10, height=5.6)
cat("\nwrote figures/ppmr_through_time.{png,pdf}\n")
