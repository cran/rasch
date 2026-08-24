## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----fit----------------------------------------------------------------------
d <- simulate_rasch(
  n_persons = 600,
  n_items = 12,
  model = "PCM",
  n_categories = 4,
  difficulty = c(-1.5, 1.5),
  disordered = "I04",
  dependence = list(pairs = list(c("I10", "I11")), strength = 1.3),
  dif = list(items = "I08", uniform = 0.8),
  n_groups = 3,
  seed = 17
)

fit <- rasch(d, model = "PCM", id = "id", factors = "group")

## ----overall-summary----------------------------------------------------------
fit

## ----item-estimates-----------------------------------------------------------
item_order <- order(abs(fit$items$fit_resid), decreasing = TRUE)
head(fit$items[item_order, c(
  "item", "location", "se", "fit_resid", "p_adj"
)], 6)

## ----item-fit-plot, fig.alt = "Item locations plotted against item fit residuals."----
plot_item_map(fit)

## ----thresholds, fig.height = 5.2, fig.alt = "Estimated category thresholds for all items on the common logit scale."----
plot_threshold_map(fit)

## ----category-curves, fig.alt = "Category characteristic curves and observed category proportions for item I04."----
plot_ccc(fit, "I04", observed = TRUE)

## ----person-estimates---------------------------------------------------------
person_order <- order(abs(fit$person$fit_resid),
                      decreasing = TRUE, na.last = TRUE)
head(fit$person[person_order, c(
  "id", "group", "raw", "theta", "se", "fit_resid"
)], 6)

## ----person-fit-plot, fig.alt = "Person locations plotted against person fit residuals."----
plot_person_fit(fit)

## ----targeting-summary--------------------------------------------------------
fit$targeting

## ----targeting-map, fig.height = 5.2, fig.alt = "Person and item distributions with the test information curve on the common logit scale."----
plot_pimap(fit, information = TRUE)

## ----wright, fig.height = 5.2, fig.alt = "Wright map of the person distribution and item thresholds on the common logit scale."----
plot_wright(fit)

## ----wrightmap, fig.width = 8, fig.height = 5.2, fig.alt = "Wright map with one person panel per group."----
if (requireNamespace("WrightMap", quietly = TRUE)) {
  wright_map(fit, person_panels = "group")
}

## ----local-dependence---------------------------------------------------------
q3 <- residual_correlations(fit)
q3$average
head(q3$pairs[, c("item_a", "item_b", "q3", "q3_star")], 5)

## ----local-dependence-plot, fig.alt = "Heatmap of adjusted residual correlations between items."----
plot_resid_cor(fit)

## ----trait-dependence---------------------------------------------------------
dimensionality <- dimensionality_test(fit)

## ----trait-dependence-plot, fig.alt = "Loadings of items on the first residual component."----
plot_pca(fit)

## ----dif----------------------------------------------------------------------
dif <- dif_anova(fit, sizes = TRUE)
flagged_dif <- subset(dif$summary, uniform_DIF | nonuniform_DIF)
flagged_dif[, c(
  "item", "term", "F_uniform", "p_uniform_adj", "eta2_uniform",
  "F_nonuniform", "p_nonuniform_adj", "eta2_nonuniform"
)]

## ----dif-plot, fig.alt = "Observed and expected item characteristic curves for item I08 by person group."----
plot_icc(fit, "I08", group = "group")

## ----dif-follow-up------------------------------------------------------------
dif$posthoc[, c(
  "item", "contrast", "estimate", "se", "p_adj",
  "lower", "upper", "practical"
)]

