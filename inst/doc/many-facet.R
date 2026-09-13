## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----fit----------------------------------------------------------------------
d <- simulate_mfrm(n_persons = 60, n_items = 4, n_raters = 5,
                   rater_severity_sd = 0.7, seed = 8)
person_group <- setNames(rep(c("A", "B"), length.out = 60),
                         unique(d$person))
d$group <- person_group[d$person]
fit <- rasch_mfrm(d, person = "person", item = "item", score = "score",
                  facets = "rater", factors = "group")
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

## ----dif-bootstrap, eval = FALSE----------------------------------------------
# dif <- dif_anova(fit)
# dif_bootstrap(fit, dif, B = 999, seed = 2026)$summary

