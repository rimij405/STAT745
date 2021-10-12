# R/analysis/lda.R
#
# LDA helpers.

## ---- analysis::lda::imports ----

library(MASS)
source(here::here("R/utils.R"))
source.submodule(files = UTILS$printf)

## ---- analysis::lda::exports ----

calc.priors <- function(.data, truth = (.data[,Y] == TRUE), weight = 1) {
  n_samples <- nrow(.data)
  p <- sum(truth) / n_samples
  q <- (1 - p)
  if (weight != 1) {
    p <- weight * p
    pq <- p + q
    p <- p / pq
    q <- q / pq
  }
  return(list(
    # Keep order of categories as the response.
    n_samples = n_samples,
    priors = c(q,p),
    p = p,
    q = q,
    weight = weight
  ))
}

#' Calculate cost of misclassification.
calc.total.cost <- function(mat, weight = 10) {
  # weight * p_2 + p_1
  return(weight * mat[1,2] + mat[2,1])
}

## ---- analysis::lda::exec ----

println("LDA helpers imported.", message = TRUE)
