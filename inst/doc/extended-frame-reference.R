## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

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

## ----fit----------------------------------------------------------------------
fit <- rasch_efrm(
  d,
  item_sets = truth$item_sets,
  groups = "group",
  id = "id",
  boot_reps = 30,
  workers = 1
)
fit

## ----tables-------------------------------------------------------------------
fit$phi_table       # person-group units
fit$alpha_table     # item-set units
fit$set_table       # linked set locations
fit$frames          # complete frame units
fit$linking         # set-linking design

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

