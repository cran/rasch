## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----library------------------------------------------------------------------
library(rasch)

## ----wide-data----------------------------------------------------------------
responses <- data.frame(
  person = c("P01", "P02", "P03", "P04"),
  group = c("control", "control", "treated", "treated"),
  I1 = c(0, 1, 1, 1),
  I2 = c(0, 1, 2, 2),
  I3 = c(1, 2, 3, NA)
)
responses

## ----ordinary-fit, eval=FALSE-------------------------------------------------
# fit <- rasch(
#   responses,
#   id = "person",
#   factors = "group",
#   items = c("I1", "I2", "I3"),
#   model = "PCM"
# )

## ----person-weights, eval=FALSE-----------------------------------------------
# item_weights <- c(I1 = 2, I2 = 1, I3 = 0.5)
# weighted <- weighted_person_estimates(fit, item_weights)
# 
# set_of <- c(I1 = "core", I2 = "core", I3 = "extension")
# set_weights <- c(core = 2, extension = 1)
# weighted_person_estimates(fit, set_weights, by = "set", sets = set_of)
# 
# save_outputs(fit, "analysis", person_weights = weighted) # new or empty folder

## ----threshold-count----------------------------------------------------------
items <- c("I1", "I2", "I3")
maxima <- vapply(responses[items], max, 0, na.rm = TRUE)
data.frame(item = items, max_score = maxima, thresholds = maxima,
           row.names = NULL)
sum(maxima)          # rows a threshold-level design must have

## ----item-predictors----------------------------------------------------------
item_design <- data.frame(
  item = items,
  format = c("selected", "constructed", "constructed"),
  demand = c(0.2, 0.7, 1.1)
)
item_design

## ----explanatory-item-fit, eval=FALSE-----------------------------------------
# fit <- rasch_explanatory(
#   responses,
#   predictors = item_design,
#   formula = ~ format + demand + format:demand,
#   level = "item",
#   id = "person",
#   factors = "group",
#   items = items
# )

## ----threshold-predictors-----------------------------------------------------
threshold_design <- data.frame(
  item = rep(items, maxima),
  threshold = sequence(maxima),
  format = rep(c("selected", "constructed", "constructed"), maxima),
  demand = c(0.2,
             0.5, 0.9,
             0.4, 0.8, 1.4)
)
threshold_design
nrow(threshold_design) == sum(maxima)

## ----explanatory-threshold-fit, eval=FALSE------------------------------------
# fit <- rasch_explanatory(
#   responses,
#   predictors = threshold_design,
#   formula = ~ format + demand,
#   level = "threshold",
#   id = "person",
#   factors = "group",
#   items = items
# )

## ----mfrm-long----------------------------------------------------------------
ratings <- data.frame(
  person = c("P01", "P01", "P02", "P02"),
  item = c("Essay1", "Essay2", "Essay1", "Essay2"),
  rater = c("R1", "R2", "R2", "R1"),
  occasion = c("first", "first", "first", "first"),
  score = c(2, 3, 1, 2)
)
ratings

## ----mfrm-fit, eval=FALSE-----------------------------------------------------
# fit <- rasch_mfrm(
#   ratings,
#   person = "person",
#   item = "item",
#   score = "score",
#   facets = c("rater", "occasion")
# )

## ----mfrm-wide----------------------------------------------------------------
wide_ratings <- data.frame(
  person = c("P01", "P01", "P02", "P02"),
  rater = c("R1", "R2", "R1", "R2"),
  Essay1 = c(2, 3, 1, 2),
  Essay2 = c(3, 2, 2, 2)
)
wide_ratings

## ----mfrm-wide-fit, eval=FALSE------------------------------------------------
# fit <- rasch_mfrm(
#   wide_ratings,
#   person = "person",
#   facets = "rater",
#   items = c("Essay1", "Essay2")
# )

## ----efrm-structure-----------------------------------------------------------
frame_data <- data.frame(
  person = paste0("P", 1:6),
  group = rep(c("A", "B"), each = 3),
  S1I1 = c(0, 1, 1, 0, 1, 1),
  S1I2 = c(0, 0, 1, 0, 1, 1),
  S2I1 = c(0, 1, 1, 0, 0, 1),
  S2I2 = c(0, 1, 1, 0, 1, 1)
)
frame_data

item_sets <- list(
  set1 = c("S1I1", "S1I2"),
  set2 = c("S2I1", "S2I2")
)
item_sets

## ----efrm-fit, eval=FALSE-----------------------------------------------------
# fit <- rasch_efrm(
#   frame_data,
#   item_sets = item_sets,
#   groups = "group",
#   id = "person"
# )

## ----cj-data------------------------------------------------------------------
comparisons <- data.frame(
  object_a = c("A", "A", "B", "A"),
  object_b = c("B", "C", "C", "C"),
  winner = c("A", "C", "B", "A"),
  judge = c("J1", "J1", "J2", "J2")
)
comparisons

## ----cj-fit, eval=FALSE-------------------------------------------------------
# fit <- btl(
#   comparisons,
#   object_a = "object_a",
#   object_b = "object_b",
#   winner = "winner",
#   judge = "judge"
# )

## ----cj-explanatory-----------------------------------------------------------
object_design <- data.frame(
  object = c("A", "B", "C"),
  genre = factor(c("essay", "essay", "report")),
  length = c(800, 950, 700)
)
object_design

## ----cj-explanatory-fit, eval=FALSE-------------------------------------------
# fit <- btl_explanatory(
#   comparisons,
#   predictors = object_design,
#   formula = ~ genre + length,
#   object_a = "object_a",
#   object_b = "object_b",
#   winner = "winner",
#   judge = "judge"
# )

## ----cj-frames----------------------------------------------------------------
object_sets <- list(
  set1 = c("S1A", "S1B", "S1C"),
  set2 = c("S2A", "S2B", "S2C")
)
object_sets

# the first four rows compare within a set, the last two across sets
frame_comparisons <- data.frame(
  object_a = c("S1A", "S1B", "S2A", "S2B", "S1A", "S1C"),
  object_b = c("S1B", "S1C", "S2B", "S2C", "S2A", "S2C"),
  winner   = c("S1A", "S1C", "S2A", "S2B", "S2A", "S1C"),
  judge    = c("J1", "J1", "J2", "J2", "J3", "J3"),
  panel    = c("east", "east", "west", "west", "east", "east")
)
frame_comparisons

## ----cj-frames-fit, eval=FALSE------------------------------------------------
# fit <- btl_efrm(
#   frame_comparisons,
#   object_a = "object_a",
#   object_b = "object_b",
#   winner = "winner",
#   judge = "judge",
#   panels = "panel",
#   object_sets = object_sets
# )

## ----simulation-map-----------------------------------------------------------
d <- simulate_efrm(n_per_group = 100, items_per_set = 5, seed = 4)
names(attr(d, "truth"))

