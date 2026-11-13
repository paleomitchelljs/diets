#!/bin/zsh
# Run all five b06 pooling-rule fits on the terrestrial table, two at a time.
cd "$(dirname $0)/../.."
ts=$(date +%Y%m%d_%H%M%S)
printf "%s\n" free lambda0 lambda1 unpooled pooled | \
  xargs -P 2 -I{} zsh -c "RULE={} WORK=results/6_work/terr Rscript code/db_analysis/b06_fit_rules.R > logs/b06_{}_${ts}.log 2>&1; tail -2 logs/b06_{}_${ts}.log"
