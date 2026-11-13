#!/bin/zsh
# Steps 1-3 of the talk plan on one terrestrial fit (default fit_free):
# predictive check, fossil similarity test (+ sensitivities), tree prediction, calibration.
cd "$(dirname $0)/../.."
FIT=${FIT:-results/6_work/terr/fit_free}; W=results/6_work/terr
export FIT_DIR=$FIT WORK=$W
ROWS=$W/analysis_rows.csv Rscript code/db_analysis/b07_ppc.R
Rscript code/db_analysis/b10_tree_prediction.R
Rscript code/db_analysis/b08_fossil_test.R
for s in no_combat no_juv_pred no_bracket stomach_only; do SENS=$s NDRAW=200 NINNER=100 Rscript code/db_analysis/b08_fossil_test.R | tail -40; done
UNIT=Theropoda Rscript code/db_analysis/b09_calibration.R
UNIT=Avialae PART=a Rscript code/db_analysis/b09_calibration.R
