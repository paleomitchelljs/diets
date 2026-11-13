#!/bin/bash
###################################################################
# run_lacertilia_overnight.sh — fit the 4 Lacertilia model variants, compare, promote, place fossils.
# Launch detached:  nohup caffeinate -i bash code/run_lacertilia_overnight.sh >/dev/null 2>&1 &
# Each variant fit skips if its .rds already exists (re-runnable / resumable).
###################################################################
cd "$(dirname "$0")/.." || exit 1
ts=$(date +%Y%m%d_%H%M%S)
mkdir -p logs
LOG="logs/lacertilia_overnight_${ts}.log"
exec > >(tee -a "$LOG") 2>&1

export STAN_CORES=4 GUT_CHAINS=4 GUT_ITER=2500 GUT_WARMUP=1000   # full sampling; GUT_SUBSAMPLE defaults to 500/cell
echo "================ Lacertilia variant run ${ts} ================"
echo "host=$(hostname) Rscript=$(which Rscript)"; date

run () {  # $1 suffix  $2 GUT_PREYTYPE  $3 GUT_OBSCLADE  $4 label
  echo; echo "------------------ VARIANT: $4  (suffix=$1 prey_type=$2 obstype:clade=$3) ------------------"; date
  GUT_SUFFIX="$1" GUT_PREYTYPE="$2" GUT_OBSCLADE="$3" Rscript code/x20_gutSlopesStan.R \
    || echo "!!! variant $4 FAILED (continuing) !!!"
}

run _v0   0     0  "V0 baseline (plant-stripped)"
run _vpt  main  0  "prey_type main effect"
run _vptx clade 0  "prey_type x clade  [synthesis]"
run _voc  0     1  "obstype x clade  [Option A]"

echo; echo "================ COMPARE (LOO + slopes) ================"; date
Rscript code/x22_compareVariants.R || echo "!!! x22 compare FAILED !!!"

echo; echo "================ PROMOTE synthesis (prey_type*clade) -> default for x07 ================"
if [ -f data/fit_gutslopes_vptx_cfix.rds ]; then
  cp data/fit_gutslopes_vptx_cfix.rds data/fit_gutslopes_cfix.rds
  cp data/gut_model_meta_vptx.rds      data/gut_model_meta.rds
  echo "promoted _vptx -> default (fit_gutslopes_cfix.rds + gut_model_meta.rds)"
  echo "================ FOSSIL PLACEMENT (x07) on synthesis fit ================"; date
  Rscript code/x07_fossilPlacement.R || echo "!!! x07 FAILED !!!"
else
  echo "!!! synthesis fit missing; x07 NOT run !!!"
fi

echo; echo "================ DONE ================"; date
echo "log: ${LOG}"
