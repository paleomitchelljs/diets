###################################################################
# x07_fossilPlacement.R  — place fossils against the EXTANT reference
#
# Uses the converged clade-fixed extant model (data/fit_gutslopes_cfix.rds) to ask, for each fossil:
#   given a predator of THIS mass, where does its observed prey mass fall in the posterior-PREDICTIVE
#   distribution an EXTANT clade of that size would produce (at the Stomach/gut obstype = the fossil analog)?
# Mass uncertainty is propagated: the percentile is also evaluated at the fossil's prey lo/hi bracket.
# Fossils are NOT in the fit -- this is out-of-sample comparison, not contamination.
#
# Outputs:
#   data/fossil_placement.csv            per fossil x extant clade: predictive med/90%PI, percentile(+bracket), in-range
#   figures/fig_fossil_placement.{png,pdf}   extant predictive bands + fossils w/ prey-mass error bars
#   figures/fig_ppmr_vs_size.{png,pdf}       ln-PPMR vs predator size; macropredator band; fossils
#   figures/fig_fossil_percentile.{png,pdf}  forest: each fossil's percentile vs its reference clade (+ mass bracket)
#   figures/fig_model_facets.{png,pdf}       clade slopes / breadth / obstype process effects
###################################################################
suppressWarnings(suppressPackageStartupMessages({ library(brms); library(ggplot2); library(ggrepel) }))
args <- commandArgs(trailingOnly=FALSE); fa <- grep("^--file=", args, value=TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
SUFFIX <- Sys.getenv("GUT_SUFFIX","")
dir.create(file.path(ROOT,"figures"), showWarnings=FALSE)
fig <- function(name, p, w=9.5, h=6.2){ ggsave(file.path(ROOT,"figures",paste0(name,SUFFIX,".png")),p,width=w,height=h,dpi=150)
                                        ggsave(file.path(ROOT,"figures",paste0(name,SUFFIX,".pdf")),p,width=w,height=h) }
## consistent palette (ColorBrewer Paired; Theropoda = purple, extending the set for the 6th group)
CLPAL <- c(Aves="#1f78b4", Crocodylia="#33a02c", Lacertilia="#b2df8a", Mammalia="#e31a1c", Serpentes="#ff7f00", Theropoda="#6a3d9a")
OBPAL <- c(Stomach="#1f78b4", Follow="#fdbf6f", Pellet="#e31a1c", Scat="#33a02c")

fit  <- readRDS(file.path(ROOT,"data", paste0("fit_gutslopes",SUFFIX,"_cfix.rds")))
meta <- readRDS(file.path(ROOT,"data", paste0("gut_model_meta",SUFFIX,".rds")))
PM_BAR <- meta$PM_BAR; clades <- meta$clades; rng <- meta$clade_predmass_range
cat(sprintf("loaded clade-fixed fit; PM_BAR=%.3f; clades=%s\n", PM_BAR, paste(clades,collapse=",")))

## ---- fossils ----
fp <- read.csv(file.path(ROOT,"data","fossil_ppmr_points.csv"), stringsAsFactors=FALSE)
fp$lpred <- log(fp$pred_mass_g); fp$lprey <- log(fp$prey_mass_g); fp$predmass_c <- fp$lpred - PM_BAR
fp$lprey_lo <- ifelse(is.finite(fp$prey_lo_g) & fp$prey_lo_g>0, log(fp$prey_lo_g), NA)
fp$lprey_hi <- ifelse(is.finite(fp$prey_hi_g) & fp$prey_hi_g>0, log(fp$prey_hi_g), NA)
fp$is_thero <- fp$pred_clade=="Theropoda"
fos <- fp; fos$fid <- seq_len(nrow(fos))                                    # place ALL fossils (gut + predation events)
fos$tier <- ifelse(grepl("attack|assoc", fos$evidence_type), "event", "gut")  # gut/pellet DIET vs predation-event tableaux (Repenomamus attack, Veloci x Proto, Sanajeh)
cat(sprintf("placing %d fossils (%d gut/diet, %d predation-event; %d theropod) against %d extant clades\n",
            nrow(fos), sum(fos$tier=="gut"), sum(fos$tier=="event"), sum(fos$is_thero), length(clades)))

## ============ PLACEMENT: each fossil x each extant clade, at obstype=Stomach ============
## prey_type="vert": every fossil has VERTEBRATE prey (theropod gut = mammal/lizard/bird; Repenomamus = dinosaur;
## etc.), so place them against the VERTEBRATE-prey predictive = apples-to-apples (harmless extra column if the
## loaded fit has no prey_type term, e.g. the V0/obstype:clade variants).
G <- do.call(rbind, lapply(fos$fid, function(i)
  data.frame(fid=i, predator=fos$predator[i], pred_clade=fos$pred_clade[i], is_thero=fos$is_thero[i],
             lpred=fos$lpred[i], predmass_c=fos$predmass_c[i], ref_clade=clades, obstype="Stomach",
             prey_type="vert", species=NA, pred_specimen=NA, stringsAsFactors=FALSE)))
names(G)[names(G)=="ref_clade"] <- "clade"                                  # newdata needs column 'clade'
PP <- posterior_predict(fit, newdata=G, allow_new_levels=TRUE, sample_new_levels="gaussian")
pctl <- function(col, x) if (is.na(x)) NA else mean(PP[,col] < x)
G$pred_med <- apply(PP,2,median); G$pred_lo90 <- apply(PP,2,quantile,.05); G$pred_hi90 <- apply(PP,2,quantile,.95)
G$obs_lprey <- fos$lprey[G$fid]; G$obs_lo <- fos$lprey_lo[G$fid]; G$obs_hi <- fos$lprey_hi[G$fid]
G$pctile      <- sapply(seq_len(nrow(G)), function(j) pctl(j, G$obs_lprey[j]))
G$pctile_preylo <- sapply(seq_len(nrow(G)), function(j) pctl(j, G$obs_lo[j]))
G$pctile_preyhi <- sapply(seq_len(nrow(G)), function(j) pctl(j, G$obs_hi[j]))
G$in_range <- mapply(function(cl,lp){ r<-rng[[as.character(cl)]]; lp>=r[1] & lp<=r[2] }, G$clade, G$lpred)
G$tier <- fos$tier[G$fid]; G$prey <- fos$prey[G$fid]
out <- G[,c("predator","prey","pred_clade","is_thero","tier","lpred","clade","obs_lprey","pred_med","pred_lo90","pred_hi90",
            "pctile","pctile_preylo","pctile_preyhi","in_range")]
names(out)[names(out)=="clade"] <- "ref_clade"
write.csv(round_df<-out, file.path(ROOT,"data",paste0("fossil_placement",SUFFIX,".csv")), row.names=FALSE)
cat("wrote data/fossil_placement.csv\n")

## per-fossil reference: theropods vs Aves (closest extant relative); others vs own clade
fos$ref_clade <- ifelse(fos$is_thero, "Aves", fos$pred_clade)
key <- paste(G$fid, G$clade); fos$pctile_ref <- G$pctile[match(paste(fos$fid, fos$ref_clade), key)]
fos$pctile_ref_lo <- G$pctile_preylo[match(paste(fos$fid,fos$ref_clade),key)]
fos$pctile_ref_hi <- G$pctile_preyhi[match(paste(fos$fid,fos$ref_clade),key)]
fos$inrange_ref   <- G$in_range[match(paste(fos$fid,fos$ref_clade),key)]
sel <- fos$is_thero & fos$inrange_ref & fos$tier=="gut"
cat(sprintf("\n=== THEROPOD GUT records vs same-size extant BIRDS (Aves): median percentile = %.2f (n_in-range=%d)\n",
            median(fos$pctile_ref[sel], na.rm=TRUE), sum(sel)))
cat("   (>0.5 = theropods ate LARGER prey than same-size birds; <0.5 = smaller. Predation-EVENT tableaux excluded here.)\n")

## ============ FIG 1: placement -- extant predictive bands + fossils with prey error bars ============
band <- do.call(rbind, lapply(clades, function(cl){ r<-rng[[cl]]; xs<-seq(r[1],r[2],length=30)
  data.frame(clade=cl, predmass=xs, predmass_c=xs-PM_BAR, obstype="Stomach", prey_type="vert", species=NA, pred_specimen=NA) }))
PB <- posterior_predict(fit, newdata=band, allow_new_levels=TRUE, sample_new_levels="gaussian")
band$med<-apply(PB,2,median); band$lo<-apply(PB,2,quantile,.05); band$hi<-apply(PB,2,quantile,.95)
fos$lab <- sub("[_ ].*","",fos$predator)
tryCatch({ p1 <- ggplot() +
  geom_ribbon(data=band, aes(predmass, ymin=lo, ymax=hi, fill=clade), alpha=.14) +
  geom_line(data=band, aes(predmass, med, color=clade), linewidth=.8) +
  geom_abline(slope=1, intercept=0, linetype="dotted", color="grey55") +
  geom_linerange(data=fos[!is.na(fos$lprey_lo),], aes(x=lpred, ymin=lprey_lo, ymax=lprey_hi), color="grey30", linewidth=.4) +
  geom_point(data=fos[fos$tier=="gut" & !fos$is_thero,], aes(lpred, lprey, color=pred_clade), shape=18, size=3.6) +
  geom_point(data=fos[fos$tier=="gut" &  fos$is_thero,], aes(lpred, lprey), shape=17, size=2.6, color="black") +
  geom_point(data=fos[fos$tier=="event" & !fos$is_thero,], aes(lpred, lprey, color=pred_clade), shape=8, size=3, stroke=1.1) +
  geom_point(data=fos[fos$tier=="event" &  fos$is_thero,], aes(lpred, lprey), shape=8, size=3, stroke=1.1, color="black") +
  geom_text_repel(data=fos, aes(lpred, lprey, label=lab), size=2.3, fontface="italic", max.overlaps=Inf, color="grey25") +
  scale_color_manual(values=CLPAL, aesthetics=c("colour","fill")) +
  labs(title="Fossil placement vs extant prey~predator reference (90% posterior-PREDICTIVE bands, Stomach level)",
       subtitle="Bands=where same-size extant predators' prey falls. Tri=theropod gut, diamond=fossil croc/snake/mammal gut, STAR=predation-event tableau (prey>predator). Grey bars=prey-mass uncertainty. Dotted=isometry.",
       x="ln(predator mass, g)", y="ln(prey mass, g)") +
  theme_minimal(base_size=12) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_fossil_placement", p1); cat("wrote fig_fossil_placement\n") }, error=function(e) cat("fig1 failed:", conditionMessage(e), "\n"))

## ============ FIG 2: ln-PPMR vs predator size + macropredator band ============
band$ppmr<-band$med-band$predmass; band$ppmr_lo<-band$lo-band$predmass; band$ppmr_hi<-band$hi-band$predmass
fos$ppmr <- fos$lprey-fos$lpred; fos$ppmr_lo <- fos$lprey_lo-fos$lpred; fos$ppmr_hi <- fos$lprey_hi-fos$lpred
tryCatch({ p2 <- ggplot() +
  geom_ribbon(data=band, aes(predmass, ymin=ppmr_lo, ymax=ppmr_hi, fill=clade), alpha=.14) +
  geom_line(data=band, aes(predmass, ppmr, color=clade), linewidth=.8) +
  geom_hline(yintercept=0, linetype="dotted", color="grey55") +
  geom_linerange(data=fos[!is.na(fos$ppmr_lo),], aes(x=lpred, ymin=ppmr_lo, ymax=ppmr_hi), color="grey30", linewidth=.4) +
  geom_point(data=fos[fos$tier=="gut" & !fos$is_thero,], aes(lpred, ppmr, color=pred_clade), shape=18, size=3.6) +
  geom_point(data=fos[fos$tier=="gut" &  fos$is_thero,], aes(lpred, ppmr), shape=17, size=2.6, color="black") +
  geom_point(data=fos[fos$tier=="event" & !fos$is_thero,], aes(lpred, ppmr, color=pred_clade), shape=8, size=3, stroke=1.1) +
  geom_point(data=fos[fos$tier=="event" &  fos$is_thero,], aes(lpred, ppmr), shape=8, size=3, stroke=1.1, color="black") +
  geom_text_repel(data=fos, aes(lpred, ppmr, label=lab), size=2.3, fontface="italic", max.overlaps=Inf, color="grey25") +
  scale_color_manual(values=CLPAL, aesthetics=c("colour","fill")) +
  labs(title="Predator-prey mass ratio (ln-PPMR) vs predator size", subtitle="PPMR = ln(prey)-ln(pred), DERIVED for display. Dotted=isometry (PPMR 0). Bands = extant 90% predictive.",
       x="ln(predator mass, g)", y="ln-PPMR = ln(prey) - ln(predator)") +
  theme_minimal(base_size=12) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_ppmr_vs_size", p2); cat("wrote fig_ppmr_vs_size\n") }, error=function(e) cat("fig2 failed:", conditionMessage(e), "\n"))

## ============ FIG 3: per-fossil percentile vs reference (forest, with mass bracket) ============
tryCatch({ ff <- fos[order(fos$pctile_ref),]
  ff$flab <- make.unique(paste0(sub("[_ ].*","",ff$predator), ": ", substr(gsub("_"," ",ff$prey),1,16)))  # unique: multiple prey per predator
  ff$flab <- factor(ff$flab, levels=ff$flab)
  ff$brk_lo <- pmin(ff$pctile_ref_lo, ff$pctile_ref_hi, na.rm=FALSE); ff$brk_hi <- pmax(ff$pctile_ref_lo, ff$pctile_ref_hi, na.rm=FALSE)
  p3 <- ggplot(ff, aes(pctile_ref, flab, color=pred_clade)) +
    annotate("rect", xmin=0.05, xmax=0.95, ymin=-Inf, ymax=Inf, fill="grey90") +
    geom_vline(xintercept=c(.05,.5,.95), linetype=c("dashed","solid","dashed"), color="grey60") +
    geom_errorbarh(aes(xmin=brk_lo, xmax=brk_hi), height=0, linewidth=.5, na.rm=TRUE) +
    geom_point(aes(shape=tier), size=2.8) + scale_shape_manual(values=c(gut=16, event=8), name="evidence") +
    scale_color_manual(values=CLPAL, name="ref/clade") +
    labs(title="Where each fossil's prey falls in the same-size extant predictive distribution",
         subtitle="Percentile vs reference clade (theropods vs Aves; others vs own clade). 0.5=typical; >0.95 or <0.05=extreme. STAR=predation-event tableau. Bar=prey-mass uncertainty bracket.",
         x="predictive percentile (fraction of extant predators eating SMALLER prey)", y=NULL, color="ref/clade") +
    theme_minimal(base_size=11) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_fossil_percentile", p3, h=7); cat("wrote fig_fossil_percentile\n") }, error=function(e) cat("fig3 failed:", conditionMessage(e), "\n"))

## ============ FIG 4: model facets -- clade slopes, breadth, obstype process ============
tryCatch({ dr <- as_draws_df(fit); ref<-clades[1]; qci<-function(x)c(median(x),quantile(x,c(.05,.95)))
  sld <- function(cl) if(cl==ref) dr[["b_predmass_c"]] else dr[["b_predmass_c"]]+dr[[sprintf("b_predmass_c:clade%s",cl)]]
  icd <- function(cl) if(cl==ref) dr[["b_Intercept"]]  else dr[["b_Intercept"]] +dr[[sprintf("b_clade%s",cl)]]
  lsg <- function(cl) dr[[sprintf("b_sigma_clade%s",cl)]]
  SL <- sapply(clades, sld); IC <- sapply(clades, icd); LS <- sapply(clades, lsg)   # ndraws x nclade
  mS <- rowMeans(SL); mI <- rowMeans(IC); mL <- rowMeans(LS)                         # clade-averaged baselines
  A <- do.call(rbind, lapply(clades, function(cl){ s<-qci(SL[,cl]); g<-qci(exp(LS[,cl]))
    rbind(data.frame(panel="clade slope (raw)", grp=cl, m=s[1], lo=s[2], hi=s[3]),
          data.frame(panel="clade breadth (raw sigma)", grp=cl, m=g[1], lo=g[2], hi=g[3])) }))
  obs <- c("Stomach","Pellet","Scat","Follow"); shf <- function(pre,o) if(o=="Stomach") 0 else dr[[paste0(pre,o)]]
  OB <- do.call(rbind, lapply(obs, function(o){                                      # RAW per obstype (incl Stomach), clade-averaged
    sr<-qci(mS + shf("b_predmass_c:obstype",o)); ir<-qci(mI + shf("b_obstype",o)); br<-qci(exp(mL + shf("b_sigma_obstype",o)))
    rbind(data.frame(panel="obstype slope (raw)", grp=o, m=sr[1],lo=sr[2],hi=sr[3]),
          data.frame(panel="obstype intercept (raw ln prey)", grp=o, m=ir[1],lo=ir[2],hi=ir[3]),
          data.frame(panel="obstype breadth (raw sigma)", grp=o, m=br[1],lo=br[2],hi=br[3])) }))
  OB$grp <- factor(OB$grp, levels=rev(obs))                                          # Stomach (reference) at top
  D <- rbind(A, OB); pl <- c("clade slope (raw)","clade breadth (raw sigma)","obstype slope (raw)","obstype intercept (raw ln prey)","obstype breadth (raw sigma)")
  D$panel <- factor(D$panel, levels=pl)
  vl <- data.frame(panel=factor(pl,levels=pl), x=c(1,NA,1,NA,NA))                     # isometry=1 on the two SLOPE panels
  p4 <- ggplot(D, aes(m, grp)) + geom_vline(data=vl, aes(xintercept=x), linetype="dotted", color="grey55", na.rm=TRUE) +
    geom_errorbarh(aes(xmin=lo, xmax=hi), height=.2) + geom_point(size=2) +
    facet_wrap(~panel, scales="free", ncol=3) +
    labs(title="Model facets: clade scaling & breadth + observation-process (obstype) effects",
         subtitle="RAW estimates per category (not deltas). Clade values at Stomach; obstype values clade-averaged. Dotted=isometry (slope 1). Stomach=obstype reference.",
         x=NULL, y=NULL) + theme_minimal(base_size=10) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_model_facets", p4, w=11, h=6); cat("wrote fig_model_facets\n") }, error=function(e) cat("fig4 failed:", conditionMessage(e), "\n"))

## ============ FIG 5: per-clade PANELS showing ALL raw extant rows (not species medians) ============
tryCatch({ D0 <- fit$data; D0$predmass <- D0$predmass_c + PM_BAR
  pcl <- ggplot() +
    geom_point(data=D0, aes(predmass, preyMass, color=obstype), alpha=.15, size=.5) +
    geom_ribbon(data=band, aes(predmass, ymin=lo, ymax=hi), fill="grey40", alpha=.15) +
    geom_line(data=band, aes(predmass, med), color="black", linewidth=.7) +
    geom_abline(slope=1, intercept=0, linetype="dotted", color="grey55") +
    facet_wrap(~clade, ncol=3) + scale_color_manual(values=OBPAL, name="obstype") +
    labs(title="Extant prey~predator data BY CLADE -- all subsampled rows, colored by obstype",
         subtitle=sprintf("All %d extant observations (not species medians). Grey band=90%% predictive at Stomach. Dotted=isometry.", nrow(D0)),
         x="ln(predator mass, g)", y="ln(prey mass, g)") +
    theme_minimal(base_size=11) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_clade_panels", pcl, w=11, h=7); cat("wrote fig_clade_panels\n") }, error=function(e) cat("fig_clade_panels failed:", conditionMessage(e), "\n"))

## ============ FIG 6: ln-PPMR through TIME, with extant clade violins at present ============
age_ma <- c(Coelophysis=212,Compsognathus=150,Anchiornis=160,Microraptor=120,Sinosauropteryx=125,
            Sinocalliopteryx=124,Daspletosaurus=76,Gorgosaurus=76,Scipionyx=113,Huadanosaurus=125,
            Velociraptor=75,Confuciusornis=120,Piscivoravis=120,Repenomamus=125,Aplestosuchus=85,
            Confractosuchus=95,Sanajeh=68)
fos$age <- age_ma[sub("[_ ].*","",fos$predator)]
tryCatch({ ft <- fos[is.finite(fos$age),]
  D0 <- fit$data; D0$predmass <- D0$predmass_c+PM_BAR; D0$ppmr <- D0$preyMass-D0$predmass
  ext <- D0[D0$obstype=="Stomach", c("clade","ppmr")]; ext$clade <- as.character(ext$clade)
  thp <- data.frame(clade="Theropoda", ppmr=ft$ppmr[ft$is_thero & ft$tier=="gut"])    # OVERALL theropod (fossil) gut PPMR
  vio <- rbind(ext, thp); grp6 <- c(clades,"Theropoda"); slot <- setNames(seq(-8,-52,length=length(grp6)), grp6); vio$x <- slot[vio$clade]
  pt <- ggplot() +
    geom_violin(data=vio, aes(x=x, y=ppmr, fill=clade, group=clade), color=NA, alpha=.55, width=7) +
    geom_vline(xintercept=0, linetype="dashed", color="grey60") +
    annotate("text", x=-30, y=Inf, label="present: extant Stomach PPMR + OVERALL Theropoda (fossil)", vjust=1.4, size=2.7, color="grey40") +
    geom_hline(yintercept=0, linetype="dotted", color="grey55") +
    geom_linerange(data=ft[!is.na(ft$ppmr_lo),], aes(x=age, ymin=ppmr_lo, ymax=ppmr_hi), color="grey40", linewidth=.4) +
    geom_point(data=ft, aes(age, ppmr, color=pred_clade, shape=tier), size=3) + scale_shape_manual(values=c(gut=17, event=8), name="evidence") +
    geom_text_repel(data=ft, aes(age, ppmr, label=lab), size=2.3, fontface="italic", max.overlaps=Inf, color="grey25") +
    scale_color_manual(values=CLPAL, aesthetics=c("colour","fill")) +
    scale_x_reverse(breaks=seq(0,200,50)) +
    labs(title="Predator-prey mass ratio (ln-PPMR) through time, vs clade distributions",
         subtitle="Fossils at ages (older left); violins at present = extant per-clade Stomach PPMR + the OVERALL Theropoda fossil PPMR. Star=predation event. Dotted=isometry.",
         x="age (Ma)", y="ln-PPMR = ln(prey) - ln(predator)") +
    theme_minimal(base_size=12) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_ppmr_vs_time", pt); cat("wrote fig_ppmr_vs_time\n") }, error=function(e) cat("fig_ppmr_vs_time failed:", conditionMessage(e), "\n"))

## ============ BEST extant-clade MATCH per theropod gut record (in-range clade closest to 'typical', pctile~0.5) ============
## Among clades whose size-range BRACKETS this theropod, pick the one whose predictive percentile is nearest 0.5
## (most 'typical'). Theropods larger than EVERY extant clade fall back to the nearest clade (flagged extrapolated)
## so the table is never silently empty.
gt_all <- G[G$is_thero & G$tier=="gut", ]
bm <- do.call(rbind, lapply(unique(gt_all$fid), function(i){ s <- gt_all[gt_all$fid==i, ]; ir <- s[s$in_range, ]
  if (nrow(ir)) { b <- ir[which.min(abs(ir$pctile-0.5)), ]; extrap <- FALSE }
  else { d <- sapply(seq_len(nrow(s)), function(k){ r<-rng[[as.character(s$clade[k])]]; min(abs(s$lpred[k]-r)) })
         b <- s[which.min(d), ]; extrap <- TRUE }
  data.frame(theropod=b$predator, prey=fos$prey[i], pred_mass_g=round(exp(fos$lpred[i])),
             best_match_clade=b$clade, pctile_at_match=round(b$pctile,2), extrapolated=extrap) }))
bm <- bm[order(bm$pred_mass_g),]
cat("\n=== BEST extant-clade match per theropod gut record (by prey-size; in-range, percentile nearest 0.5) ===\n"); print(bm, row.names=FALSE)
cat("   match tally: "); print(table(bm$best_match_clade))
write.csv(bm, file.path(ROOT,"data",paste0("theropod_best_match",SUFFIX,".csv")), row.names=FALSE)

## ============ SIMILARITY: overall Theropoda PPMR distribution vs each extant clade ============
tryCatch({
  D0 <- fit$data; D0$predmass <- D0$predmass_c+PM_BAR; D0$ppmr <- D0$preyMass-D0$predmass
  thp <- (fos$lprey - fos$lpred)[fos$is_thero & fos$tier=="gut"]                          # overall theropod gut PPMR
  ovc <- function(a,b){ r<-range(c(a,b)); da<-density(a,from=r[1],to=r[2],n=512)$y; db<-density(b,from=r[1],to=r[2],n=512)$y; sum(pmin(da/sum(da), db/sum(db))) }
  sim <- do.call(rbind, lapply(clades, function(cl){
    ep <- D0$ppmr[D0$obstype=="Stomach" & D0$clade==cl]                                   # extant clade Stomach PPMR
    pc <- G$pctile[G$is_thero & G$tier=="gut" & G$in_range & G$clade==cl]                 # SIZE-MATCHED theropod percentiles vs this clade
    data.frame(clade=cl, overlap_PPMR=round(ovc(thp,ep),3),
               sizematched_med_pctile=round(median(pc),2),
               sizematched_KS_D=round(as.numeric(suppressWarnings(ks.test(pc,"punif"))$statistic),3), n_sizematched=length(pc)) }))
  sim <- sim[order(-sim$overlap_PPMR),]
  cat("\n=== SIMILARITY of overall THEROPOD PPMR distribution to each extant clade ===\n")
  cat("   overlap_PPMR     : raw PPMR-distribution overlap (1=identical; ignores predator size)\n")
  cat("   sizematched_*    : theropod percentiles vs that clade's SIZE-MATCHED predictive (med~0.5 & low KS_D = most similar)\n")
  print(sim, row.names=FALSE); write.csv(sim, file.path(ROOT,"data",paste0("theropod_clade_similarity",SUFFIX,".csv")), row.names=FALSE)
  simL <- rbind(data.frame(clade=sim$clade, metric="raw PPMR overlap (size-IGNORING)", value=sim$overlap_PPMR),
                data.frame(clade=sim$clade, metric="size-MATCHED similarity (1 - KS D)", value=round(1-sim$sizematched_KS_D,3)))
  ord <- sim$clade[order(1-sim$sizematched_KS_D)]; simL$clade <- factor(simL$clade, levels=ord)
  ps <- ggplot(simL, aes(clade, value, fill=clade)) +
    geom_col(width=.6) + geom_text(aes(label=sprintf("%.2f", value)), hjust=-.25, size=2.8) +
    scale_fill_manual(values=CLPAL, guide="none") + coord_flip(clip="off") + ylim(0, 1) + facet_wrap(~metric) +
    labs(title="How similar is the overall Theropoda PPMR distribution to each extant clade?",
         subtitle="LEFT: raw PPMR-distribution overlap (ignores predator size) -> Mammalia. RIGHT: size-matched -> Lacertilia. The flip IS the body-size confound.",
         x=NULL, y="similarity (1 = identical)") +
    theme_minimal(base_size=11) + theme(plot.subtitle=element_text(color="grey40", size=8))
  fig("fig_theropod_similarity", ps, w=10, h=4.2); cat("wrote fig_theropod_similarity\n")
}, error=function(e) cat("similarity failed:", conditionMessage(e), "\n"))

cat("\nx07 DONE. tables: fossil_placement.csv, theropod_best_match.csv, theropod_clade_similarity.csv\n")
cat("  figs: fig_fossil_placement, fig_ppmr_vs_size, fig_fossil_percentile, fig_model_facets, fig_clade_panels, fig_ppmr_vs_time, fig_theropod_similarity\n")
