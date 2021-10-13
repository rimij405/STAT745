# R/analysis/validation.R
#
# Cross validation functions.

## ---- analysis::validation::constants ----

# Global scoped module.
VALIDATE <<- vector("list")

## ---- analysis::validation::imports ----

# Source the utility functions.
source(here::here("R/utils.R"))
source.submodule(UTILS$printf)
source.submodule(ANALYSIS$paths$model)
source.submodule(ANALYSIS$paths$metrics)

# Library()
library(dplyr)

## ---- analysis::validation::utilities ----

#' Provide the number of unique values (categories) in
#' each of the different fields.
#'
#' @param errors Dataframe with CV results.
#'
#' @return unique counts.
.n_unique <- function(errors) {
  errors %>%
    summarize(across(everything(), n_distinct)) %>%
    return()
}

## ---- analysis::validation::defines ----

#' Perform k-fold CV on a glm.
#'
#' @param .features Training data passed to the model.
#' @param labels Ground truth labels for the training data.
#' @param ... Parameters to pass to the model fitting algorithm.
#' @param k Number of folds.
#' @param rounds Number of rounds per fold.
#' @param m Threshold vector length.
#' @param from Minimum threshold value.
#' @param to Maximum threshold value.
#'
#' @return Results from the cross validation process.
.glm.cv <- function(
  # training features and training labels.
  .features, labels,
  # model fitting params. see `MODEL$fit`.
  ...,
  # k-fold cross validation settings.
  # - k total folds.
  # - rounds per fold.
  # - m is length of the thresholds to use.
  k = 5, rounds = 5, m = 5,
  from = 0.0, to = 1.0, verbose = FALSE) {

  # Count number of samples.
  n_features <- ncol(.features)
  n_samples <- nrow(.features)

  # Create threshold range.
  thresholds.v <- seq(from = from, to = to, length = m)

  #'   # Prepare collection of matrices.
  #'   # - Creates an array of k matrices (all filled with 0).
  #'   # - Each matrix is m by rounds in size.
  #'   # Default: 10 (10 x 10) matrices.
  #'   errors_arr <- array(0, dim = c(m, rounds, k))

  # Create a tbl to contain the CV results.
  errors_df <- tibble(
    round = integer(),
    k = integer(),
    threshold = double(),
    error = double()
  )

  println("===========================")
  printf("Performing %s round(s) of %s-Fold CV.", rounds, k)
  printf("# Samples: %s || Features: %s", n_samples, n_features)
  printf("# Thresholds: %s [%s, %s]", m, from, to)
  for (i in 1:rounds) {
    if (verbose) println("---------------------------")
    if (verbose) printf("Performing %s-fold CV [Round %s/%s]...", k, i, rounds)
    # Create vector of fold ids.
    fold <- sample(rep(1:k, length = n_samples))
    for (j in 1:k) {
      if (verbose) println("- - - - - - - - - - - - - -")
      if (verbose)printf("Fitting fold %s of %s [Round %s/%s]", j, k, i, rounds)
      # Select splits for this fold.
      validation_idx <- (fold == j)
      X_train <- .features[!validation_idx,]
      X_validation <- .features[validation_idx,]
      y_labels <- labels[validation_idx]
      # Fit a model.
      fit <- MODEL$fit(.data = X_train, FUN = glm, params = list(...))
      fit.predict <- list(
        probability = predict(fit, newdata = X_validation, type = "response")
      )

      # Make predictions for each threshold we are testing,
      # and add an appropriate row for the (threshold, round, k)
      if (verbose) printf("# Calculating total error rate with probability (p = %s)", j, k, i)
      for (h in 1:m) {
        # Make prediction for specific threshold (index = h).
        h.threshold <- thresholds.v[h]
        h.class <- METRICS$as$prediction(fit.predict$probability, h.threshold)
        # print(h.class)
        h.labels <- METRICS$as$label(h.class, yes = "Severe", no = "Not Severe", missing = NULL)
        h.table <- table(h.labels, y_labels)
        # print(h.table)
        h.error <- METRICS$calc$error.rate(h.table)
        if (verbose) printf("# || Using threshold %s of %s [Threshold = %0.2f] (Error Rate = %0.2f)", h, m, h.threshold, h.error)

        # Create a row and add it to the data.frame.
        errors_df %<>% add_row(
          tibble_row(
            round = i,
            k = j,
            threshold = h.threshold,
            error = h.error,
          ))
      }
      if (verbose) println("- - - - - - - - - - - - - -")
    }
    if (verbose) println("---------------------------")
  }
  printf("Completed %s round(s) of %s-Fold CV.", rounds, k)
  str(errors_df)
  println("===========================")

  # Return the dataframe.
  return(errors_df)
}

.lda.cv <- function(...) { message("Not implemented.")}

#' Count the number of k-folds saved in the errors data.frame.
#'
#' @param errors Dataframe with cross-validation results.
#'
#' @return k
.count.folds <- function(errors) {
  errors %>%
    .n_unique() %>%
    select(k) %>%
    pull %>%
    return()
}

#' Count the number of rounds saved in the errors data.frame.
#'
#' @param errors Dataframe with cross-validation results.
#'
#' @return round count.
.count.rounds <- function(errors) {
  errors %>%
    .n_unique() %>%
    select(round) %>%
    pull() %>%
    return()
}

#' Count an arbitrary field's unique values.
#'
#' @param errors Dataframe with cross-validation results.
#' @param field Field selector.
#'
#' @return field's unique value count.
.count.field <- function(errors, field) {
  df <- errors %>% .n_unique()
  return(df[[field]])
}

#' Calculate mean error rate(s).
#'
#' @param errors Dataframe with cross-validation results.
.calc.error.rate.mean <- function(errors) {
  errors %>%
    summarize(across(error, ~ mean(.x))) %>%
    return()
}

#' Calculate std dev of error rate(s).
#'
#' @param errors Dataframe with cross-validation results.
.calc.error.rate.sd <- function(errors) {
  errors %>%
    summarize(across(error, ~ sd(.x))) %>%
    return()
}

#' Calculate error rate mean, grouped by threshold.
.calc.threshold.error.rate.mean <- function(errors) {
  errors %>%
    group_by(threshold) %>%
    .calc.error.rate.mean() %>%
    return()
}

#' Calculate error rate sd, grouped by threshold.
.calc.threshold.error.rate.sd <- function(errors) {
  errors %>%
    group_by(threshold) %>%
    .calc.error.rate.sd() %>%
    return()
}

#' Calculate the confidence interval for a set of errors
#' given a data.frame with k-fold,
#'
#' @param errors Dataframe with cross-validation results.
#' @param errors_mean Mean error rates. By default, no grouping is applied.
.calc.error.rate.confint <- function(errors, errors_mean = .calc.avg.error.rates(errors)) {

  # Assumption is that the data frame
  # is already properly filtered.

  # Get the number of rounds.
  rounds <- .count.rounds(errors)

  # Get the number of folds.
  k <- .count.folds(errors)

  # Calculate the standard error.
  N <- rounds * k
  sqrt.N <- sqrt(N)

  # Get the summaries.
  errors_sd <- errors %>%
    mutate(
      sd = .calc.threshold.error.rate.sd(errors)
    )

  # Mutate the fields.





  #'
  #'   # Calculate average error rate and standard error (stdev / sqrt(N))
  #'   # - Not quite sure how to parse this line.
  #'   N <- prod(dim(errs)[2:3]) # Equiv~ ==> rounds * k
  #'   err <- apply(errs, 1, mean)
  #'   err_se <- apply(errs, 1, sd) / sqrt(N)
  #'
  #'   # Get the confidence interval for the error rate.
  #'   confinterval <- list(
  #'     low = err - 2 * err_se,
  #'     high = err + 2 * err_se
  #'   )

  # Calculate the optimal threshold.



}

.calc.error.rate.optimal <- function(errors) {

}

## ---- analysis::validation::exports ----

VALIDATE$cv <- list(
  glm = .glm.cv,
  lda = .lda.cv
)

# Add functions.
VALIDATE$count <- list(
  unique = .n_unique,
  folds = .count.folds,
  rounds = .count.rounds,
  range = .count.field
)

#' #' Calculate cross validated error rates.
#' #'
#' #' @param .data Dataset.
#' #' @param truth Ground truth class labels.
#' #' @param algorithm Model to fit every round.
#' #' @param formula Formula to pass to the model.
#' #' @param k Number of k folds to make.
#' #' @param m Number of equidistant threshold cuts to make.
#' #' @param rounds Number of rounds in every fold.
#' #' @param thresholds Optional sequence of thresholds to pass. Useful if repeated thresholds.
#' #'
#' #' @return Error rate.
#' calc.cv.error.rates <- function(.data, truth,
#'                                 algorithm = glm,
#'                                 params = list(
#'                                   formula = Y ~ .,
#'                                   family = "binomial"
#'                                 ),
#'                                 k = 10, rounds = 10, m = 10,
#'                                 thresholds = seq(from = 0, to = 1, length = m)) {
#'
#'   # printf("%s", deparse1(sys.call()))
#'   # printf("%s", ls())
#'
#'   # Get the sample count.
#'   n_samples <- nrow(.data)
#'   m <- length(thresholds)
#'   println("---------------------------")
#'   printf("Performing %s-fold CV:", k)
#'   printf("# Samples: %s", n_samples)
#'   printf("# Thresholds: %s", m)
#'   printf("# k folds: %s || # rounds: %s", k, rounds)
#'   println("---------------------------")
#'
#'   # Prepare collection of matrices.
#'   # - Creates an array of k matrices (all filled with 0).
#'   # - Each matrix is m by rounds in size.
#'   # Default: 10 (10 x 10) matrices.
#'   errors_arr <- array(0, dim = c(m, rounds, k))
#'   # str(errors_arr)
#'
#'   # Process each round of CV.
#'   for (i in 1:rounds) {
#'
#'     # Sample sequence 1 to k to make vector
#'     # of numbers with fold ids.
#'     fold <- sample(rep(1:k, length = n_samples))
#'     # print("Fold distribution: ")
#'     # str(fold)
#'
#'     # Fit model on fold.
#'     for (j in 1:k) {
#'
#'       # Select the fold.
#'       validation_rows <- (fold == j)
#'       # printf("Fold validation selected for j == %s (n = %s):", j, length(validation_rows))
#'       # str(validation_rows)
#'
#'       # Get the training and validation splits.
#'       fold_train <- .data[!validation_rows, ]
#'       # printf("Fold set (train) (n = %s):", nrow(fold_train))
#'       # str(fold_train)
#'       fold_valid <- .data[validation_rows, ]
#'       # printf("Fold set (validation) (n = %s):", nrow(fold_valid))
#'       # str(fold_valid)
#'
#'       # Get truth for the fold.
#'       fold_truth <- (.data[validation_rows, ]$severity == "Severe")
#'       # printf("Fold ground truth (n = %s):", length(fold_truth))
#'       # str(fold_truth)
#'
#'       # Fit a model.
#'       .obj <- fit.model(
#'         .data = fold_train,
#'         algorithm = algorithm,
#'         params = params
#'       )$obj
#'
#'       # Calculate predictions.
#'       p <- predict(.obj, newdata = fold_valid, type = "response")
#'       # printf("Predict probabilities with fitted model (n = %s):", length(p))
#'
#'       # Make predictions for positive class using thresholds.
#'       for(h in 1:m) {
#'         # j = Fold
#'         # h = Threshold
#'         # i = Average Total Error
#'         predictions <- (p >= thresholds[h])
#'         errors_arr[h,i,j] <- mean((predictions == FALSE) & (fold_truth == TRUE)) +
#'                             mean((predictions == TRUE) & (fold_truth == FALSE))
#'
#'       }
#'     }
#'   }
#'
#'   return(list(
#'     errors = errors_arr,
#'     thresholds = thresholds
#'   ))
#' }
#'
#' #' Plot the cross validation error rates.
#' #'
#' #' @param errors Array of 3 by 3 error matrices.
#' #' @param thresholds Range of thresholds used.
#' #' @param from Minimum threshold value.
#' #' @param to Maximum threshold value.
#' #' @param digits Rounding position.
#' #'
#' #' @return Plot.
#' plot.cv.error.rates <- function(errors, thresholds, ..., from = 0, to = 1, digits = 4) {
#'
#'   # Get the m, rounds, and k from the dimensions.
#'   errors_dim <- dim(errors)
#'   # m <- errors_dim[1]
#'   rounds <- errors_dim[2]
#'   k <- errors_dim[3]
#'
#'   # Calculate threshold limit.
#'   threshold_limit <- ((thresholds >= from) & (thresholds <= to))
#'
#'   # Select thresholds and error matrices.
#'   errs <- errors[threshold_limit, , ]
#'   threshs <- thresholds[threshold_limit]
#'
#'   # Calculate average error rate and standard error (stdev / sqrt(N))
#'   # - Not quite sure how to parse this line.
#'   N <- prod(dim(errs)[2:3]) # Equiv~ ==> rounds * k
#'   err <- apply(errs, 1, mean)
#'   err_se <- apply(errs, 1, sd) / sqrt(N)
#'
#'   # Get the confidence interval for the error rate.
#'   confinterval <- list(
#'     low = err - 2 * err_se,
#'     high = err + 2 * err_se
#'   )
#'
#'   # Matrix of error rate and CI.
#'   errs_mat <- cbind(err, confinterval$low, confinterval$high)
#'
#'   # Calculate the optimal threshold.
#'   optimal_error <- min(err)
#'   optimal_index <- match(optimal_error, err)
#'   optimal_threshold <- threshs[optimal_index]
#'
#'   # Calculate rounded values.
#'   rounded_error <- round(optimal_error, digits = digits)
#'   rounded_threshold <- round(optimal_threshold, digits = digits)
#'
#'   # Form the plot data matrix.
#'   .data <- dplyr::as_tibble(cbind(errs_mat, threshs))
#'   names(.data) <- c("total.err", "ci.low", "ci.high", "thresholds")
#'   str(.data)
#'
#'   # Pivot the data for plotting.
#'   data <- .data %>%
#'     tidyr::pivot_longer(
#'       cols = !thresholds,
#'       names_to = "category",
#'       values_to = "error"
#'     )
#'   data$linetype <- (data$category == "total.err")
#'   str(data)
#'
#'   # Get the graph title.
#'   p_title <- sprintf("Error rates with %s-fold CV (rounds = %s)", k, rounds)
#'
#'   # Get the graph axis labels.
#'   p_xlab <- sprintf("Thresholds (optimal at %s)", rounded_threshold)
#'   p_ylab <- sprintf("Error from Misclassification (optimal = %s)", rounded_error)
#'
#'   # Get the annotation.
#'   annotation <- data.frame(
#'     x = c(optimal_threshold + 0.05),
#'     y = c(optimal_error - 0.025),
#'     label = c(sprintf("Error = %s, Threshold = %s", rounded_error, rounded_threshold))
#'   )
#'
#'   # Draw a scatterplot matrix.
#'   p <- ggplot(data = data, mapping = aes(x = thresholds, y = error)) +
#'     # Add points grouped by error type.
#'     geom_point(mapping = aes(color = category), size = 1, alpha = 0.25) +
#'     geom_line(mapping = aes(color = category, linetype = linetype), size = 1, alpha = 1) +
#'     # Add reference lines.
#'     geom_hline(yintercept = optimal_error, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
#'     geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
#'     # Add annotation.
#'     geom_label(data = annotation, mapping = aes( x = x, y = y, label = label),
#'                color = "black", alpha = 0.5,
#'                size = 3, angle = 45, fontface = "bold") +
#'     # Label the graph.
#'     labs(title = p_title) +
#'     scale_x_continuous(p_xlab,
#'                        breaks = seq(0.0, 1.0, 0.2)) +
#'     scale_y_continuous(p_ylab,
#'                        breaks = seq(0.0, 1.0, 0.05)) +
#'     scale_color_manual("Measurement",
#'                        values = c("black", "red", "green"),
#'                        breaks = c("total.err", "ci.low", "ci.high"),
#'                        labels = c("Total Error", "Conf. Lower Bound", "Conf. Upper Bound")) +
#'     scale_linetype_manual(values = c(3, 1),
#'                           labels = c("Confidence Interval", "Total Error")) +
#'     # scale_linetype_discrete("",
#'     #                        breaks = c(FALSE, TRUE),
#'     #                        labels = c("False Neg/Pos", "Total Error")) +
#'     # Change the legend.
#'     theme(
#'       legend.position = "bottom",
#'       legend.direction = "horizontal",
#'       legend.title = element_blank(),
#'       legend.text = element_text(size = 10),
#'       legend.background = element_blank(),
#'       legend.key = element_blank()
#'     )
#'
#'   # Return the optimal index.
#'   return(list(
#'     plot = p,
#'     optimal_threshold = optimal_threshold
#'   ))
#' }
#'
#' #' Calculate confusion table for a CV.
#' #'
#' #' @param .data Dataset.
#' #' @param truth Ground truth class labels.
#' #' @param algorithm Model to fit every round.
#' #' @param formula Formula to pass to the model.
#' #' @param k Number of k folds to make.
#' #' @param rounds Number of rounds in every fold.
#' #' @param threshold Threshold to use for the table.
#' #'
#' #' @return Error rate.
#' make.cv.confusion.mat <- function(.data, truth,
#'                                   algorithm = glm,
#'                                   params = list(
#'                                     formula = Y ~ .,
#'                                     family = "binomial"
#'                                   ),
#'                                   k = 10, rounds = 10,
#'                                   threshold = 0.5) {
#'
#'   # Get the sample count.
#'   n_samples <- nrow(.data)
#'   println("---------------------------")
#'   printf("Calculating %s-fold CV table:", k)
#'   printf("# Samples: %s", n_samples)
#'   printf("# Threshold: %s", round(threshold))
#'   printf("# k folds: %s || # rounds: %s", k, rounds)
#'   println("---------------------------")
#'
#'   # Prepare collection of matrices.
#'   # - (2 by 2) matrices for each round, for each fold in k-folds.
#'   clf_tbl_arr <- array(0, dim = c(2, 2, rounds, k))
#'
#'   # Process each round of CV.
#'   for (i in 1:rounds) {
#'
#'     # Sample sequence 1 to k to make vector
#'     # of numbers with fold ids.
#'     fold <- sample(rep(1:k, length = n_samples))
#'
#'     # Fit model on fold.
#'     for (j in 1:k) {
#'
#'       # Select the fold.
#'       validation_rows <- (fold == j)
#'
#'       # Get the training and validation splits.
#'       fold_train <- .data[!validation_rows, ]
#'       fold_valid <- .data[validation_rows, ]
#'
#'       # Get truth for the fold.
#'       fold_truth <- (.data[validation_rows, ]$severity == "Severe")
#'
#'       # Fit a model.
#'       .obj <- fit.model(
#'         .data = fold_train,
#'         algorithm = algorithm,
#'         params = params
#'       )$obj
#'
#'       # Calculate predictions.
#'       p <- predict(.obj, newdata = fold_valid, type = "response")
#'
#'       # Only single threshold provided for this table.
#'       predictions <- (p >= threshold)
#'       clf_tbl_arr[,,i,j] <- table(predictions, fold_truth)
#'     }
#'   }
#'
#'   # Array of clf_tbl's for every round in each fold in k-folds.
#'   return(clf_tbl_arr)
#' }
#'
#' #' Get the mean and std. err from array of confusion matrices.
#' summarize.cv.confusion.mat <- function(.arr) {
#'   # Get the array summary
#'   clf_tbl_arr <- apply(.arr, 1:3, sum)
#'
#'   # Calculate errors.
#'   N <- dim(clf_tbl_arr)[3]
#'   table_mean <- apply(clf_tbl_arr, 1:2, mean)
#'   table_se <- apply(clf_tbl_arr, 1:2, sd) / sqrt(N)
#'
#'   # Return mean and stdev err.
#'   return(list(
#'     mean = table_mean,
#'     sterr = table_se
#'   ))
#' }
