# R/analysis/metrics.R
#
# Functions related to metrics.

#' Calculate the probability of the provided condition.
#'
#' @param .data Dataset to calculate class probabilities.
#' @param ... Conditional used  to filter the dataset.
#'
#' @return Prior probability of class in the dataset.
calc.probability <- function(.data, ...) {
  n_match <- nrow(.data %>% filter(...))
  # n_match <- .data %>% dplyr::filter(condition)
  # n_match <- sum(n_match)
  n_samples <- nrow(.data)
  return(n_match / n_samples)
}

#' Select model predictions above a certain threshold.
#'
#' @param .obj Fitted model object.
#' @param threshold Probability threshold.
#'
#' @return Logical vector indicating classes above threshold.
calc.predictions <- function(.obj, threshold = 0.5, ...) {
  predictions <- .obj %>% predict(...)
  return(predictions >= threshold)
}

#' Convert vector to label vector.
#'
#' @param vec Logical vector.
as.labels <- function(vec, ..., true = "Y", false = "N") {
  return(dplyr::if_else(vec, true = true, false = false, ...))
}

#' Create confusion matrix/table.
#'
#' @param predictions Predicted class labels.
#' @param truth Ground truth class labels.
#'
#' @return 2 by 2 confusion matrix.
make.confusion.mat <- function(predictions, truth, ..., true = TRUE, false = FALSE) {
  truth <- as.labels(truth, true = true, false = false)
  predictions <- as.labels(predictions, true = true, false = false)
  return(table(predictions, truth))
}

#' Calculate the error rate.
#'
#' @param mat Confusion table.
#'
#' @return Double representing the misclassification error rate.
calc.error.rate <- function(mat) {
  # assumes mat is a 2 by 2 table.
  if (ncol(mat) != 2 || nrow(mat) != 2) {
    err <- sprintf(
      "Expected %s by %s array but received %s by %s dimensions.",
      2, 2,
      ncol(mat), nrow(mat)
    )
    err <- sprintf("Undefined error rate for malformed matrix. %s", err)
    stop(err)
    return(NULL)
  }

  # Error rate = 1 - correct classification rate.
  1 - sum(diag(mat)) / sum(mat)
}

#' Find the optimal error rate, minimizing it using a threshold.
#'
#' @param probabilities Predicted probabilities for the positive class.
#' @param truth Ground truth class labels.
#'
#' @return List containing the optimal error rate and optimal threshold.
calc.optimal.error.rates <- function(probabilities, truth) {

  # Threshold vector.
  thresholds <- sort(probabilities[truth])
  n_thresholds <- length(thresholds)
  n_cols <- 3

  # Prepare the error matrix.
  err_mat <- matrix(0, n_thresholds, n_cols)

  # Calculate error rates.
  for (i in 1:n_thresholds) {
    predictions <- (probabilities >= thresholds[i])
    clf_table <- table(predictions, truth)
    err_mat[i, 1] <- clf_table[1, 2] # FN
    err_mat[i, 2] <- clf_table[2, 1] # FP
  }

  # Calculate the total error rates for each threshold value (row).
  err_mat[, 3] <- apply(err_mat[,1:2], 1, sum)

  # Scale error rates by total count.
  errs <- err_mat / length(truth)

  # Return list.
  return(list(
    errors = errs,
    thresholds = thresholds,
    probabilities = probabilities,
    truth = truth
  ))
}

#' Plot the optimal error rates.
#'
#' @param errors Matrix 145 by 3 containing optimal FN, FP, and total errors.
#' @param thresholds Vector 145 by 1 containing thresholds.
#' @param limit Limit on the error rates.
#' @param digits Rounding position.
#'
#' @return Plot.
plot.optimal.error.rates <- function(errors, thresholds, ..., limit = 1.0, digits = 4) {

  # Conditional.
  total_errors_limit <- (errors[, 3] < limit)

  # Select errors within the valid total error limits.
  errs <- errors[total_errors_limit, ]
  threshs <- thresholds[total_errors_limit]

  # Find the minimum total error.
  optimal_error <- min(errs[, 3])

  # Find the index of the optimal error.
  optimal_index <- match(optimal_error, errs[, 3])

  # Find the optimal threshold.
  optimal_threshold <- threshs[optimal_index]

  # Round the optimal error and threshold.
  rounded_error <- round(optimal_error, digits = digits)
  rounded_threshold <- round(optimal_threshold, digits = digits)

  # Bind thresholds.
  .data <- dplyr::as_tibble(cbind(errs, threshs))
  names(.data) <- c("false.neg", "false.pos", "total.err", "thresholds")
  str(.data)

  # Pivot the data for plotting.
  data <- .data %>%
    tidyr::pivot_longer(
      cols = !thresholds,
      names_to = "error_type",
      values_to = "error"
    )
  data$linetype <- (data$error_type == "total.err")
  str(data)

  # Get the graph title.
  p_title <- sprintf("Optimum error rate is %s at threshold = %s", rounded_error, rounded_threshold)

  # Get the graph axis labels.
  p_xlab <- "Thresholds"
  p_ylab <- "Fraction Misclassified"

  # Get the annotation.
  annotation <- data.frame(
    x = c(optimal_threshold + 0.05),
    y = c(optimal_error - 0.025),
    label = c(sprintf("Error = %s, Threshold = %s", rounded_error, rounded_threshold))
  )

  # Draw a scatterplot matrix.
  p <- ggplot(data = data, mapping = aes(x = thresholds, y = error)) +
    # Add points grouped by error type.
    geom_point(mapping = aes(color = error_type), size = 1, alpha = 0.25) +
    geom_line(mapping = aes(color = error_type, linetype = linetype), size = 1, alpha = 1) +
    # Add reference lines.
    geom_hline(yintercept = optimal_error, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
    geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
    # Add annotation.
    geom_label(data = annotation, mapping = aes( x = x, y = y, label = label),
               color = "black", alpha = 0.5,
               size = 3, angle = 45, fontface = "bold") +
    # Label the graph.
    labs(title = p_title) +
    scale_x_continuous(p_xlab,
                       breaks = seq(0.2, 0.8, 0.2)) +
    scale_y_continuous(p_ylab,
                       breaks = seq(0.0, 0.6, 0.1)) +
    scale_color_manual("Error Types",
                        values = c("red", "blue", "black"),
                        breaks = c("false.neg", "false.pos", "total.err"),
                        labels = c("Severe Misclassification", "Not Severe Misclassification", "Total Error")) +
    scale_linetype_manual(values = c(3, 1),
                          labels = c("Misclassification Error", "Total Error")) +
    # scale_linetype_discrete("",
    #                        breaks = c(FALSE, TRUE),
    #                        labels = c("False Neg/Pos", "Total Error")) +
    # Change the legend.
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.background = element_blank(),
      legend.key = element_blank()
    )

  # Return the optimal index.
  return(p)
}

#' Calculate the optimal classification.
#'
#' @param .results Results from the optimal error rate calculations.
#' @param limit Error limit.
#' @param true Label to give the positive class.
#' @param false Label to give the negative class.
#'
#' @return Classfication table.
make.optimal.confusion.mat <- function(.results, ..., limit = 1.0, true = TRUE, false = FALSE) {

  # Get the errors and thresholds.
  # @param probabilities Predicted probabilities for the positive class.
  # @param truth Ground truth class labels.
  # .results <- calc.optimal.error.rates(probabilities, truth)

  # Get the results values.
  errors <- .results$errors
  thresholds <- .results$thresholds
  probabilities <- .results$probabilities
  truth <- .results$truth

  # Conditional.
  total_errors_limit <- (errors[, 3] < limit)

  # Select errors within the valid total error limits.
  errors <- errors[total_errors_limit, ]
  thresholds <- thresholds[total_errors_limit]

  # Find the minimum total error.
  optimal_error <- min(errors[, 3])

  # Find the index of the optimal error.
  optimal_index <- match(optimal_error, errors[, 3])

  # Find the optimal threshold.
  optimal_threshold <- thresholds[optimal_index]

  # Find the optimal prediction.
  optimal_predictions <- (probabilities >= optimal_threshold)

  # Get the optimal misclassification table:
  truth <- as.labels(truth, true = true, false = false)
  optimal_predictions <- as.labels(optimal_predictions, true = true, false = false)
  optimal_clf_table <- table(optimal_predictions, truth)
  return(optimal_clf_table)
}

