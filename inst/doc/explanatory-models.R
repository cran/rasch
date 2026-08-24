## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(rasch)

## ----lltm---------------------------------------------------------------------
set.seed(1)
item_design <- data.frame(
  item = paste0("I", 1:8),
  operation = rep(c("recall", "inference"), each = 4),
  format = rep(c("selected", "constructed"), 4)
)

difficulty <- 0.8 * (item_design$operation == "inference") +
  0.4 * (item_design$format == "constructed")
theta <- rnorm(500)
X <- sapply(difficulty, function(delta)
  rbinom(length(theta), 1, plogis(theta - delta)))
colnames(X) <- item_design$item

fit <- rasch_explanatory(
  X,
  predictors = item_design,
  formula = ~ operation + format,
  level = "item"
)
fit$est$coefficients

## ----predictor-types, eval=FALSE----------------------------------------------
# item_design$demand <- as.numeric(item_design$demand)
# item_design$format <- factor(item_design$format)
# item_design$complexity <- ordered(
#   item_design$complexity,
#   levels = c("low", "moderate", "high")
# )

## ----lpcm-form, eval=FALSE----------------------------------------------------
# lpcm <- rasch_explanatory(
#   responses,
#   predictors = item_design,
#   formula = ~ operation + format + threshold + format:threshold,
#   level = "item"
# )

## ----comparison---------------------------------------------------------------
explanatory_test(fit)

## ----diagnostics--------------------------------------------------------------
departures <- explanatory_diagnostics(fit)
head(departures)

## ----relax, eval=FALSE--------------------------------------------------------
# fit <- relax_explanatory(fit, item = "I4", component = "location")

## ----cj, eval=FALSE-----------------------------------------------------------
# cj <- btl_explanatory(
#   comparisons,
#   predictors = object_design,
#   formula = ~ domain + format + domain:format,
#   object_a = "object_a",
#   object_b = "object_b",
#   winner = "winner",
#   judge = "judge"
# )
# explanatory_test(cj)
# explanatory_diagnostics(cj)

