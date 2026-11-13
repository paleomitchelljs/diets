#!/bin/zsh
# Rerun steps 2-3 with the dated composite tree (a02) on the terrestrial free fit.
cd "$(dirname $0)/../.."
export FIT_DIR=results/6_work/terr/fit_free WORK=results/6_work/terr
Rscript code/db_analysis/a02_theropod_tree.R
DROP= OUT=db/tree/theropod/sens_withEshano Rscript code/db_analysis/a02_theropod_tree.R > /dev/null
TIP=fad OUT=db/tree/theropod/sens_fad Rscript code/db_analysis/a02_theropod_tree.R > /dev/null
Rscript code/db_analysis/b10_tree_prediction.R
Rscript code/db_analysis/b08_fossil_test.R
for s in no_combat no_juv_pred no_bracket stomach_only; do SENS=$s NDRAW=200 NINNER=100 Rscript code/db_analysis/b08_fossil_test.R > /dev/null; done
UNIT=Theropoda Rscript code/db_analysis/b09_calibration.R
UNIT=Avialae PART=a Rscript code/db_analysis/b09_calibration.R
