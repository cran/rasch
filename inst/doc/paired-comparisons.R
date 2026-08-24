## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----fit----------------------------------------------------------------------
d <- simulate_btl(n_objects = 7, n_judges = 12,
                  reps_per_pair = 20, seed = 5)
fit <- btl(d, object_a = "object_a", object_b = "object_b",
           winner = "winner", judge = "judge")
fit
fit$objects

## ----judges-------------------------------------------------------------------
fit$judges
judge_surprise(fit, "J1")
btl_information(fit)

## ----structure----------------------------------------------------------------
tr <- btl_transitivity(fit)
tr
dimensions <- btl_dimensionality(fit, reps = 20)
dimensions

## ----plot-transitivity, fig.alt = "Per-judge consistency of the paired comparisons."----
plot_btl_transitivity(tr)

## ----plot-scree, fig.alt = "Residual bimension strengths against the simulated noise reference."----
plot_btl_scree(dimensions)

## ----btl-dif-example, eval = FALSE--------------------------------------------
# judge_group <- setNames(panel_data$discipline, panel_data$judge)
# bd <- btl_dif(fit, judge_group)
# bd$summary
# bd$sizes

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

