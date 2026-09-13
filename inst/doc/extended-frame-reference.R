## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)
source("precomputed.R")

## ----library------------------------------------------------------------------
library(rasch)

## ----simulate-----------------------------------------------------------------
d <- simulate_efrm(
  n_per_group = 150,
  items_per_set = 8,
  set_unit_ratio = 1.30,
  group_unit_ratio = 1.10,
  seed = 25
)
truth <- attr(d, "truth")
d$site <- rep(c("A", "B"), length.out = nrow(d))
d

## ----fit, eval = recompute----------------------------------------------------
# fit <- rasch_efrm(
#   d,
#   item_sets = truth$item_sets,
#   groups = "group",
#   id = "id",
#   factors = "site",
#   boot_reps = 50,
#   workers = 1,
#   seed = 25
# )
# fit

## ----fit-precomputed, echo = FALSE--------------------------------------------
if (!recompute) {
  fit <- vignette_result("extended-frame-reference")$fit
  fit
}

## ----tables-------------------------------------------------------------------
fit$phi_table       # person-group units
fit$alpha_table     # item-set unit ratios (reference unit / set unit)
fit$set_table       # linked set locations
fit$frames          # complete frame units
fit$linking         # set-linking design

## ----dif-bootstrap, eval = FALSE----------------------------------------------
# dif <- dif_anova(fit)
# dif_bootstrap(fit, dif, B = 999, seed = 2026)$summary

## ----crossed, eval = FALSE----------------------------------------------------
# fit_crossed <- rasch_efrm(
#   data,
#   item_sets = item_sets,
#   groups = c("language", "cohort"),
#   id = "id"
# )
# fit_crossed$phi_factorial
# fit_crossed$phi_factorial_tests

## ----comparison---------------------------------------------------------------
fit$efrm_vs_rasch$unit_omnibus
fit$efrm_vs_rasch$unit_tests

## ----invariance, eval = FALSE-------------------------------------------------
# inv <- frame_invariance(fit, se_method = "conditional")
# inv$summary
# inv$locations
# inv$discrimination

## ----invariance-bootstrap, eval = FALSE---------------------------------------
# inv_boot <- frame_invariance(
#   fit,
#   se_method = "bootstrap",
#   boot_reps = 300,
#   seed = 1
# )

## ----repair, eval = FALSE-----------------------------------------------------
# resolved <- resolve_frames(fit, "S1I02")
# removed <- drop_items(fit, "S1I02")

