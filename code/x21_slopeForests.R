###################################################################
# x21_slopeForests.R
# Two forest plots that summarize the x20 Bayesian gut-slopes fit, as clean
# companions to the ribbon scatter (gut_slopes_comparison*):
#   (1) slope_forest*       per-clade scaling slope b (median + 90% CrI),
#                           theropods included + highlighted; refs at b=0
#                           (prey size independent of predator = boundary null)
#                           and b=1 (isometry, prey scales 1:1 with predator).
#   (2) theropod_contrast_forest*   Theropoda - each extant clade (Δslope,
#                           median + 90% CrI) with P(Δ>0); ref at 0 (= no diff).
#
# Reads ONLY the CSVs x20 writes (so it's fast + needs no fit object):
#   data/gut_slopes_posterior{SUFFIX}.csv     (clade, slope, lo90, hi90)
#   data/theropod_vs_clade_contrasts{SUFFIX}.csv (vs, d_slope, d_slope_lo/hi, P_slope_gt, ...)
# Pick the fit with env GUT_SUFFIX:  ""=full/canonical, "_v2"=converged subsample,
#   "_lite"=the non-converged preview (flagged on the plot).
# Usage:  GUT_SUFFIX=_v2 Rscript code/x21_slopeForests.R
###################################################################
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
dir.create(file.path(ROOT, "figures"), showWarnings = FALSE)
SUFFIX <- Sys.getenv("GUT_SUFFIX", "")
note <- if (SUFFIX == "_lite") "  PREVIEW - non-converged fit (R-hat 1.39); shape only, do not cite." else ""
fit_lab <- if (!nzchar(SUFFIX)) "full 19k rows (NOCOR, nu=4)" else switch(SUFFIX,
                  "_lite"="lite (subsample, non-converged)", "_v2"="v2 (subsample, NOCOR)",
                  "_v3"="v3 (subsample, NOCOR, nu=4; R-hat 1.026)", paste0(SUFFIX," fit"))

slope_f <- file.path(ROOT, "data", paste0("gut_slopes_posterior", SUFFIX, ".csv"))
contr_f <- file.path(ROOT, "data", paste0("theropod_vs_clade_contrasts", SUFFIX, ".csv"))
if (!file.exists(slope_f)) stop("missing ", slope_f, " — run x20 with this GUT_SUFFIX first.")

THERO <- "Theropoda"
pal <- c(Aves="#1b9e77", Crocodylia="#7570b3", Lacertilia="#66a61e",
         Mammalia="#e6ab02", Serpentes="#a6761d", Theropoda="#d95f02")

## ---- (1) per-clade slope forest ----
s <- read.csv(slope_f)
s$clade <- factor(s$clade, levels = s$clade[order(s$slope)])   # ascending slope
s$is_th <- s$clade == THERO
p1 <- ggplot(s, aes(slope, clade, color = clade)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "grey55") +
  annotate("text", x = 0, y = 0.55, label = "b=0: prey size indep. of predator", hjust = 0, vjust = 1, size = 2.5, color = "grey45") +
  annotate("text", x = 1, y = 0.55, label = "b=1: isometry", hjust = 0.5, vjust = 1, size = 2.5, color = "grey45") +
  geom_errorbarh(aes(xmin = lo90, xmax = hi90), height = 0, linewidth = 0.8) +
  geom_point(aes(size = is_th), shape = 16) +
  geom_text(aes(label = sprintf("%.2f [%.2f, %.2f]", slope, lo90, hi90)),
            hjust = 0, nudge_y = 0.22, size = 2.7, color = "grey25") +
  scale_color_manual(values = pal, guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 4.2), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.7))) +
  labs(title = "Prey-predator mass-scaling slope b, per clade (90% CrI)",
       subtitle = paste0("ln(prey) ~ ln(predator); Theropoda highlighted. Fit: ", fit_lab, ".", note),
       x = "scaling slope b", y = NULL) +
  theme_minimal(base_size = 12) + theme(plot.subtitle = element_text(color = "grey40", size = 8))
ggsave(file.path(ROOT, "figures", paste0("slope_forest", SUFFIX, ".png")), p1, width = 8.5, height = 4.6, dpi = 150)
ggsave(file.path(ROOT, "figures", paste0("slope_forest", SUFFIX, ".pdf")), p1, width = 8.5, height = 4.6)
cat("wrote figures/slope_forest", SUFFIX, ".{png,pdf}\n", sep = "")

## ---- (2) Theropoda-vs-clade contrast forest ----
if (file.exists(contr_f)) {
  c0 <- read.csv(contr_f)
  c0$vs <- factor(c0$vs, levels = c0$vs[order(c0$d_slope)])
  c0$sig <- ifelse(c0$P_slope_gt >= 0.95 | c0$P_slope_gt <= 0.05, "strong", "weak")
  p2 <- ggplot(c0, aes(d_slope, vs)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
    annotate("text", x = 0, y = 0.5, label = "d=0: theropods = clade", hjust = 0.5, vjust = 0, size = 2.5, color = "grey45") +
    geom_errorbarh(aes(xmin = d_slope_lo, xmax = d_slope_hi, color = sig), height = 0, linewidth = 0.9) +
    geom_point(aes(color = sig), size = 3) +
    geom_text(aes(label = sprintf("d=%.2f  P(d>0)=%.2f", d_slope, P_slope_gt)),
              hjust = 0, nudge_y = 0.24, size = 2.7, color = "grey25") +
    scale_color_manual(values = c(strong = "#b2182b", weak = "grey55"), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.30))) +
    scale_y_discrete(expand = expansion(add = c(0.8, 0.7))) +
    labs(title = "Theropoda vs each extant clade: difference in scaling slope (90% CrI)",
         subtitle = paste0("Positive = theropods scale prey size up with body size MORE steeply. Red = P(d>0)>=0.95 or <=0.05. Fit: ", fit_lab, ".", note),
         x = "d slope  (Theropoda - clade)", y = NULL) +
    theme_minimal(base_size = 12) + theme(plot.subtitle = element_text(color = "grey40", size = 8))
  ggsave(file.path(ROOT, "figures", paste0("theropod_contrast_forest", SUFFIX, ".png")), p2, width = 8.8, height = 4.2, dpi = 150)
  ggsave(file.path(ROOT, "figures", paste0("theropod_contrast_forest", SUFFIX, ".pdf")), p2, width = 8.8, height = 4.2)
  cat("wrote figures/theropod_contrast_forest", SUFFIX, ".{png,pdf}\n", sep = "")
} else cat("(no contrasts CSV for SUFFIX='", SUFFIX, "' — skipped contrast forest)\n", sep = "")
