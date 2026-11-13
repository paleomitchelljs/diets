#!/bin/bash
###################################################################
# run_vertonly.sh — the VERTEBRATE-ONLY matched-filter reference model.
#   * vert prey only (matched to fossil gut taphonomy; invert excluded)
#   * obstype modelled, fossils predicted at obstype=Stomach (matched analog of preservation bias d)
#   * ALL stomach data retained (highest-fidelity match); non-stomach modes capped (inform the offset only)
# Default: fit the MAIN model + place fossils (x07). Set GUT_FULL=1 to also fit the obstype:clade variant
# + LOO-compare (slower, exposes the run to laptop-sleep interruption for longer).
# Launch:  nohup caffeinate -dimsu bash code/run_vertonly.sh >/dev/null 2>&1 &
#   (NB closing the lid still sleeps the Mac on battery -> keep lid OPEN or on AC; the fit is saved right after
#    sampling, so a sleep only PAUSES/resumes -- it will not lose a completed fit.)
###################################################################
cd "$(dirname "$0")/.." || exit 1
ts=$(date +%Y%m%d_%H%M%S); mkdir -p logs; LOG="logs/vertonly_${ts}.log"
exec > >(tee -a "$LOG") 2>&1
export STAN_CORES=4 GUT_CHAINS=4 GUT_ITER=2500 GUT_WARMUP=1000
echo "================ VERT-ONLY matched-filter run ${ts} ================"; date

echo; echo "------------------ MAIN: vert-only, obstype modelled, predict@Stomach ------------------"; date
GUT_SUFFIX="" GUT_OBSCLADE=0 GUT_LOO="${GUT_FULL:-0}" Rscript code/x20_gutSlopesStan.R || echo "!!! MAIN FAILED !!!"

if [ "${GUT_FULL:-0}" = "1" ]; then
  echo; echo "------------------ VARIANT: + obstype:clade ------------------"; date
  GUT_SUFFIX=_oc GUT_OBSCLADE=1 GUT_LOO=1 Rscript code/x20_gutSlopesStan.R || echo "!!! _oc FAILED !!!"
  echo; echo "------------------ LOO compare (does obstype:clade help?) ------------------"
  Rscript -e '
  suppressWarnings(suppressPackageStartupMessages(library(brms)))
  f<-c(main="data/fit_gutslopes_cfix.rds", "obstype:clade"="data/fit_gutslopes_oc_cfix.rds")
  L<-list(); for(n in names(f)) if(file.exists(f[[n]])){ x<-readRDS(f[[n]]); if("loo"%in%names(x$criteria)) L[[n]]<-x$criteria$loo }
  if(length(L)>=2){ print(loo_compare(L)); write.csv(data.frame(variant=rownames(loo_compare(L)),loo_compare(L)),"data/vertonly_loo_compare.csv",row.names=FALSE) } else cat("(loo compare unavailable)\n")
  ' || echo "!!! LOO compare FAILED !!!"
fi

echo; echo "------------------ FOSSIL PLACEMENT (x07) on main vert-only fit ------------------"; date
Rscript code/x07_fossilPlacement.R || echo "!!! x07 FAILED !!!"

echo; echo "================ DONE ================"; date; echo "log: ${LOG}"
