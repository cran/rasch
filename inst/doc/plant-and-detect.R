## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  fig.width = 7, fig.height = 4.2, dpi = 96
)
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----whatplanted--------------------------------------------------------------
d <- simulate_rasch(n_persons = 400, n_items = 10, seed = 101)
d                                   # the print method reports the plant
names(attr(d, "truth"))            # the truth travels with the data

## ----baseline-----------------------------------------------------------------
fit <- rasch(d, id = "id")
rec <- sim_recovery(fit, d)
rec

## ----baseline-plot, fig.alt = "Recovery scatter plots of planted against recovered item difficulty and person ability."----
plot_recovery(rec)

## ----discrim------------------------------------------------------------------
disc <- rep(1, 10); disc[5] <- 2.5; disc[6] <- 0.4
d2  <- simulate_rasch(400, 10, discrimination = disc, seed = 21)
fit2 <- rasch(d2, id = "id")
fit2$items[, c("item", "location", "infit_ms", "outfit_ms")]

## ----dif----------------------------------------------------------------------
d3   <- simulate_rasch(500, 10, dif = list(items = "I06", uniform = 1),
                       n_groups = 2, seed = 303)
fit3 <- rasch(d3, id = "id", factors = "group")
da   <- dif_anova(fit3)
da$summary[, c("item", "term", "F_uniform", "p_uniform_adj", "uniform_DIF")]

## ----difsize------------------------------------------------------------------
dif_size(fit3, "I06", by = "group")

## ----dependence---------------------------------------------------------------
d4   <- simulate_rasch(500, 10,
                       dependence = list(pairs = list(c("I04", "I05")),
                                         strength = 1.8), seed = 41)
fit4 <- rasch(d4, id = "id")
rc   <- residual_correlations(fit4)
rc$average                          # near -1/(L-1) under independence
head(rc$pairs, 3)                   # the planted pair leads the table
rc$flagged

## ----depmag-------------------------------------------------------------------
dependence_magnitude(fit4, dependent = "I05", independent = "I04")

## ----btl----------------------------------------------------------------------
b  <- simulate_btl(n_objects = 7, n_judges = 8, reps_per_pair = 30,
                   erratic_judges = 0.25, seed = 61)
bt <- btl(b, "object_a", "object_b", winner = "winner", judge = "judge")
bt$judges[order(-bt$judges$fit_resid), ]

## ----transitivity-------------------------------------------------------------
tr <- btl_transitivity(bt)
tr$summary[, c("n_objects", "n_triples", "n_circular",
               "circular_rate", "consistency")]
tr$judges[, c("judge", "n_triples", "circular_rate", "consistency")]

## ----power--------------------------------------------------------------------
batch <- sim_replicate(simulate_rasch, 10, n_persons = 400, n_items = 8,
                       dif = list(items = "I04", uniform = 0.8),
                       n_groups = 2, seed = 700)
flagged <- vapply(batch, function(dd) {
  s <- dif_anova(rasch(dd, id = "id", factors = "group"))$summary
  isTRUE(s$uniform_DIF[s$item == "I04"])
}, logical(1))
mean(flagged)                       # proportion of runs that flagged I04

