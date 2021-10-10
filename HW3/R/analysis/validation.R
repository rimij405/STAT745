# R/analysis/validation.R
#
# Cross validation functions.

# Source the model function.
source(here::here("R/analysis/model.R"))

# Source the model function.
source(here::here("R/analysis/metrics.R"))

#' Make sequence (n = `length`) of equidistant threshold values between `from` and `to`.
#'
#' @param from Minimum threshold value.
#' @param to Maximum threshold value.
#' @param length Number of equidistant chunks to calculate sequence with.
#'
#' @return Sequence.
make.thresholds <- function(from = 0, to = 1, length = 100) {
  thresholds <- seq(from = from, to = to, length = length)
  return(thresholds)
}

#' Calculate cross validated error rates.
#'
#' @param .data Dataset.
#' @param truth Ground truth class labels.
#' @param k Number of k folds to make.
#' @param m Number of equidistant threshold cuts to make.
#' @param rounds Number of rounds in every fold.
#' @param thresholds Optional sequence of thresholds to pass. Useful if repeated thresholds.
#' @param algorithm Model to fit every round.
#' @param formula Formula to pass to the model.
#'
#' @return Error rate.
calc.cv.error.rates <- function(.data, truth, algorithm,
                                ...,  formula = Y ~ .,
                                k = 10, m = 10, rounds = 10,
                                thresholds = seq(from = 0, to = 1, length = m)) {

  # Get the sample count.
  n_samples <- nrow(.data)
  m <- length(thresholds)

  # Prepare collection of matrices.
  errors_arr <- array(0, c(m, rounds, k))
  str(errors_arr)


  # Process each round of CV.
  for (i in 1:rounds) {

    # Sample sequence 1 to k to make vector
    # of numbers with fold ids.
    fold <- sample(rep(1:k, length = n_samples))

    # Fit model on fold.
    for (j in 1:k) {

      # Select the fold.
      cond <- (fold == j)
      .fold <- .data[!cond, ]

      # Fit a model.
      .obj <- .fold %>% fit.model(
                algorithm = algorithm,
                formula = formula,
                family = "binomial"
              )

      # Calculate predictions.
      str(.data[cond,]$severity)
      p <- predict(.obj, newdata = .data[cond, ], type = "response")


      # probabilities <- predict(.obj, newdata = .data[cond,],  type = "response")

      # Get truth for the fold.
      .truth <- truth[cond, ]

      # Make predictions for thresholds.
      for(h in 1:m) {
        predictions <- (probabilities >= thresholds[h])
        error_arr[h,i,j] <- mean((predictions == FALSE) && (.truth == TRUE)) +
                            mean((predictions == TRUE) && (.truth == FALSE))

      }
    }
  }

  return(list(
    errors = errors_arr,
    thresholds = thresholds
  ))
}
