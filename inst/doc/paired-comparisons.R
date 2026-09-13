## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)
source("precomputed.R")

## ----library------------------------------------------------------------------
library(rasch)

## ----fit----------------------------------------------------------------------
d <- simulate_btl(
  n_objects = 8,
  n_judges = 48,
  reps_per_pair = 84,
  erratic_judges = 2 / 48,
  dependence = list(exposure = 0.7, carry_over = 0),
  seed = 2
)
truth <- attr(d, "truth")
judge_number <- as.integer(sub("^J", "", d$judge))
d$panel <- factor(ifelse(judge_number %% 2L,
                         "panel A", "panel B"))
d$experience <- factor(ifelse(judge_number <= 24,
                              "experienced", "novice"))

fit <- btl(d, object_a = "object_a", object_b = "object_b",
           winner = "winner", judge = "judge", order = "order")
fit
fit$objects

## ----judges-------------------------------------------------------------------
judge_order <- order(abs(fit$judges$fit_resid), decreasing = TRUE)
head(fit$judges[judge_order, ], 6)
erratic_fit <- fit$judges[match(truth$erratic, fit$judges$judge), ]
erratic_judge <- erratic_fit$judge[
  which.max(abs(erratic_fit$fit_resid))
]
judge_surprise(fit, erratic_judge)
btl_information(fit)

## ----btl-fit-bootstrap, eval = FALSE------------------------------------------
# boot <- fit_bootstrap(fit, B = 999, seed = 2026)
# boot$total
# head(boot$pairs[order(boot$pairs$chisq_p_boot_adj), c(
#   "object_a", "object_b", "chisq", "chisq_p_boot_adj"
# )])
# head(boot$objects[order(boot$objects$fit_resid_p_boot_adj), c(
#   "object", "fit_resid", "fit_resid_p_boot_adj"
# )])
# head(boot$judges[order(boot$judges$fit_resid_p_boot_adj), c(
#   "judge", "fit_resid", "fit_resid_p_boot_adj"
# )])

## ----structure----------------------------------------------------------------
tr <- btl_transitivity(fit)
tr

dim_data <- simulate_btl(
  n_objects = 8, n_judges = 48, reps_per_pair = 84,
  second_attribute = list(rho = 0.3), seed = 47
)
dim_fit <- btl(dim_data, object_a = "object_a", object_b = "object_b",
               winner = "winner", judge = "judge")

## ----dimensions, eval = recompute---------------------------------------------
# dimensions <- btl_dimensionality(dim_fit, reps = 20, seed = 2026,
#                                 independent_comparisons = TRUE)
# dimensions

## ----dimensions-precomputed, echo = FALSE-------------------------------------
if (!recompute) {
  dimensions <- vignette_result("paired-comparisons")$dimensions
  dimensions
}

## ----plot-transitivity, fig.alt = "Per-judge consistency of the paired comparisons."----
plot_btl_transitivity(tr)

## ----plot-scree, fig.alt = "Residual bimension strengths against the simulated noise reference."----
plot_btl_scree(dimensions)

## ----btl-dif-example----------------------------------------------------------
bd <- btl_dif(fit, d[c("panel", "experience")])
bd_order <- order(bd$summary$p_uniform_adj)
head(bd$summary[bd_order, c(
  "object", "term", "F_uniform", "p_uniform_adj", "eta2_uniform",
  "min_judges", "min_effective_judges"
)], 6)

## ----btl-dif-bootstrap, eval = FALSE------------------------------------------
# bd_boot <- dif_bootstrap(fit, bd, B = 999, seed = 2026)
# bd_boot$summary

## ----equating-example, eval = FALSE-------------------------------------------
# eq <- btl_equate(current_panel, reference_panel, independent = TRUE)
# eq$table
# eq$equated                 # reference panel on the current panel's origin

## ----efrm---------------------------------------------------------------------
de <- simulate_btl_efrm(
  n_objects_per_set = 5, n_sets = 2,
  n_judges_per_panel = 6, n_panels = 2,
  reps_within = 15, reps_cross = 15,
  set_units = c(1, 1.3), set_origins = c(0, 0.6), seed = 9
)
ef <- btl_efrm(
  de, "object_a", "object_b", winner = "winner", judge = "judge",
  panels = "panel", object_sets = attr(de, "truth")$object_sets,
  se_method = "conditional"
)
ef$phi_table
ef$alpha_table
ef$kappa_table

## ----app-cj-data, echo = FALSE, out.width = "100%", fig.alt = "The Data panel with the comparative judgement roles assigned: the two object columns, the observed preference, and the judge column."----
knitr::include_graphics("figures/app-cj-data.png")

## ----app-cj-summary, echo = FALSE, out.width = "100%", fig.alt = "The Summary panel for a comparative judgement fit: the design counts, object separation and pairwise fit tiles, and the test-of-fit table."----
knitr::include_graphics("figures/app-cj-summary.png")

## ----app-cj-items, echo = FALSE, out.width = "100%", fig.alt = "The object panel, showing the caterpillar plot of object locations with confidence intervals."----
knitr::include_graphics("figures/app-cj-items.png")

## ----app-cj-judges, echo = FALSE, out.width = "100%", fig.alt = "The judge panel, showing judge fit and consistency against the common object scale."----
knitr::include_graphics("figures/app-cj-judges.png")

## ----app-cj-dif, echo = FALSE, out.width = "100%", fig.alt = "The DIF panel for a comparative judgement fit, showing the omnibus test across judge groups with the judge counts per level."----
knitr::include_graphics("figures/app-cj-dif.png")

