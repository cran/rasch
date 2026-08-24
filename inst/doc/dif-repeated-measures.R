## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----library------------------------------------------------------------------
library(rasch)

## ----data---------------------------------------------------------------------
set.seed(21)
N <- 320
difficulty <- seq(-1.5, 1.5, length.out = 8)
theta <- rnorm(N)
group <- rep(c("A", "B"), each = N / 2)

make_wave <- function(occasion_shift, interaction_shift) {
  shift <- matrix(0, N, 8)
  shift[group == "B", 3] <- 1.2
  shift[, 6] <- occasion_shift
  shift[group == "B", 5] <- interaction_shift
  matrix(rbinom(N * 8, 1,
                plogis(outer(theta, difficulty, "-") - shift)), N, 8)
}

X <- rbind(make_wave(0, 0), make_wave(1.0, 2.0))
colnames(X) <- sprintf("I%02d", 1:8)
dat <- data.frame(
  pid = rep(sprintf("P%03d", seq_len(N)), 2),
  X,
  group = rep(group, 2),
  occasion = rep(c("T1", "T2"), each = N)
)

## ----analysis-----------------------------------------------------------------
fit <- rasch(dat, id = "pid", factors = c("group", "occasion"),
             items = sprintf("I%02d", 1:8))
da <- dif_anova(fit, within = "occasion", effects = "factorial", sizes = TRUE)
da$summary

## ----magnitude----------------------------------------------------------------
dif_size(fit, "I03", by = "group")
dc <- dif_contrasts(fit, items = c("I03", "I06"), within = "occasion")
dc$table
da$posthoc

