## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2)
options(digits = 4)
source("precomputed.R")

## ----library------------------------------------------------------------------
library(rasch)

## ----fit, eval = recompute----------------------------------------------------
# d <- simulate_rasch(
#   n_persons = 600,
#   n_items = 12,
#   model = "PCM",
#   n_categories = 4,
#   difficulty = c(-1.5, 1.5),
#   disordered = "I04",
#   dependence = list(pairs = list(c("I10", "I11")), strength = 1.3),
#   dif = list(items = "I08", uniform = 0.8),
#   n_groups = 3,
#   seed = 17
# )
# 
# fit <- rasch(d, model = "PCM", id = "id", factors = "group")

## ----fit-precomputed, include = FALSE-----------------------------------------
if (!recompute) {
  recorded <- vignette_result("rasch-workflow")
  d <- recorded$d
  fit <- recorded$fit
}

## ----overall-summary----------------------------------------------------------
fit

## ----item-estimates-----------------------------------------------------------
item_order <- order(abs(fit$items$fit_resid), decreasing = TRUE)
head(fit$items[item_order, c(
  "item", "location", "se", "fit_resid", "p_adj"
)], 6)

## ----item-fit-plot, fig.alt = "Item locations plotted against item fit residuals."----
plot_item_map(fit)

## ----item-fit-bootstrap, eval = FALSE-----------------------------------------
# boot <- fit_bootstrap(fit, B = 999, seed = 2026)
# head(boot$items[order(boot$items$chisq_p_boot_adj), c(
#   "item", "chisq", "chisq_p_boot_adj",
#   "fit_resid", "fit_resid_p_boot_adj"
# )], 6)

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

## ----person-fit-bootstrap, eval = FALSE---------------------------------------
# head(boot$persons[order(boot$persons$fit_resid_p_boot_adj), c(
#   "id", "raw", "theta", "fit_resid", "fit_resid_p_boot_adj"
# )], 6)

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

## ----trait-dependence, eval = recompute---------------------------------------
# dimensionality <- dimensionality_test(
#   fit,
#   items_positive = paste0("I", sprintf("%02d", 1:6)),
#   items_negative = paste0("I", sprintf("%02d", 7:12)),
#   B = 199, seed = 2026
# )

## ----trait-dependence-precomputed, include = FALSE----------------------------
if (!recompute) dimensionality <- recorded$dimensionality

## ----residual-scree, eval = recompute, fig.alt = "Residual eigenvalues against the score-conditional model-reference band."----
# scree <- plot_scree(fit, seed = 2026)

## ----residual-scree-precomputed, echo = FALSE, eval = !recompute, fig.alt = "Residual eigenvalues against the score-conditional model-reference band."----
scree <- plot_scree(fit, result = recorded$scree)

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

## ----dif-bootstrap, eval = FALSE----------------------------------------------
# dif_boot <- dif_bootstrap(fit, dif, B = 999, workers = 4, seed = 2026)
# dif_boot$summary[, c(
#   "item", "term", "p_uniform_boot_adj", "p_nonuniform_boot_adj"
# )]

## ----app-data, echo = FALSE, out.width = "100%", fig.alt = "The Data panel of the application. The sidebar assigns the person identifier, person factors and item columns; the main area previews the responses."----
knitr::include_graphics("figures/app-data.png")

## ----app-summary, echo = FALSE, out.width = "100%", fig.alt = "The Summary panel, showing the test of fit, reliability and targeting tables with the test characteristic curve."----
knitr::include_graphics("figures/app-summary.png")

## ----app-items, echo = FALSE, out.width = "100%", fig.alt = "The Items panel: the item statistics table on the left with the item having the largest absolute fit residual selected, and its item characteristic curve on the right."----
knitr::include_graphics("figures/app-items.png")

## ----app-chisq, echo = FALSE, out.width = "100%", fig.alt = "The class-interval chi-square tab, showing observed and expected means by class interval for the selected item."----
knitr::include_graphics("figures/app-items-chisq.png")

## ----app-persons, echo = FALSE, out.width = "100%", fig.alt = "The Persons panel, showing the person estimate table and the person fit summary."----
knitr::include_graphics("figures/app-persons.png")

## ----app-targeting, echo = FALSE, out.width = "100%", fig.alt = "The Targeting panel, showing the person-item map with the person distribution against the item thresholds."----
knitr::include_graphics("figures/app-targeting.png")

## ----app-local, echo = FALSE, out.width = "100%", fig.alt = "The Local dependence panel, showing the residual correlation matrix and its nominated screening threshold."----
knitr::include_graphics("figures/app-local.png")

## ----app-dif, echo = FALSE, out.width = "100%", fig.alt = "The DIF panel, showing the analysis of variance table by item and term with the flagged items and the characteristic curves by person group."----
knitr::include_graphics("figures/app-dif.png")

## ----app-rcode, echo = FALSE, out.width = "80%", fig.alt = "A results table with its R code disclosure open, showing the call that produced it."----
knitr::include_graphics("figures/app-rcode.png")

## ----app-export, echo = FALSE, out.width = "100%", fig.alt = "The Export panel, offering the tables, figures and report formats an analysis can be written out as."----
knitr::include_graphics("figures/app-export.png")

