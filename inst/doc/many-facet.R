## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----fit----------------------------------------------------------------------
d <- simulate_mfrm(n_persons = 60, n_items = 4, n_raters = 5,
                   rater_severity_sd = 0.7, seed = 8)
fit <- rasch_mfrm(d, person = "person", item = "item", score = "score",
                  facets = "rater")
fit

## ----tables-------------------------------------------------------------------
fit$item_effects
fit$facet_effects$rater
head(fit$item_thresholds)

## ----facets, fig.alt = "Rater severity estimates with confidence intervals."----
plot_facets(fit, facet = "rater")

## ----interaction--------------------------------------------------------------
fit_interaction <- rasch_mfrm(
  d, person = "person", item = "item", score = "score",
  facets = "rater", interaction = "rater"
)
head(fit_interaction$interaction_effects)
fit_interaction$interaction_test

