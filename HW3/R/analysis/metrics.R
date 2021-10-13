# R/analysis/metrics.R
#
# Functions related to metrics.

## ---- analysis::metrics::constants ----

# Create a METRICS pointer in the global scope.
METRICS <<- vector("list", 0L)

# Default settings for the metrics.R script.
.METRICS <- list(
  offset = list(
    x = 0.05,
    y = 0.25
  )
)

## ---- analysis::metrics::imports ----

library(dplyr)

## ---- analysis::metrics::utilities ----

# table(predictions, truth):
#          truth -->
#          N   P
# pred N  TN  FN
#  |   P  FP  TP
#  V

#' Total frequency.
.total <- function(mat) { return(sum(mat)) }

#' True Negative.
.tn <- function(mat) { return(mat[1, 1]) }

#' False Negative.
.fn <- function(mat) { return(mat[1, 2]) }

#' False Positive.
.fp <- function(mat) { return(mat[2, 1]) }

#' True Positive.
.tp <- function(mat) { return(mat[2, 2]) }

#' Conditional negatives from the sample.
#' NEG = TN + FP
.cond.neg <- function(mat) { return(.tn(mat) + .fp(mat)) }

#' Conditional positives from the sample.
#' POS = TP + FN
.cond.pos <- function(mat) { return(.fn(mat) + .tp(mat)) }

#' Negative class classifications.
#' PRED. NEG = TN + FN
.clf.neg <- function(mat) { return(.tn(mat) + .fn(mat)) }

#' Positive class classifications.
#' PRED. POS = TP + FP
.clf.pos <- function(mat) { return(.tp(mat) + .fp(mat))}

#' Correct classifications.
#' CORRECT = TN + TP
.correct <- function(mat) { return(.tn(mat) + .tp(mat)) }

#' Incorrect classifications.
#' INCORRECT = FN + FP
.incorrect <- function(mat) { return(.fn(mat) + .fp(mat)) }

#' Positive predictive value.
#' PPV = PRECISION
#' = (TP) / (PRED. POS)
#' = (TP) / (TP + FP)
#' = 1 - FDR
.ppv <- function(mat) { return(.tp(mat) / .clf.pos(mat)) }
.precision <- .ppv

#' Negative predictive value.
#' NPV = (TN) / (PRED. NEG)
#' = (TN) / (TN + FN)
#' = 1 - FOR
.npv <- function(mat) { return(.tn(mat) / .clf.neg(mat))}

#' True positive rate.
#' TPR = RECALL = SENSITIVITY
#' = (TP) / (POS)
#' = (TP) / (TP + FN)
#' = 1 - FNR
.tpr <- function(mat) { return(.tp(mat) / .cond.pos(mat)) }
.recall <- .tpr
.sensitivity <- .tpr

#' True negative rate.
#' TNR = SPECIFICITY = SELECTIVITY
#' = (TN) / (NEG)
#' = (TN) / (TN + FP)
#' = 1 - FPR
.tnr <- function(mat) { return(.tn(mat) / .cond.neg(mat)) }
.specificity <- .tnr
.selectivity <- .tnr

#' False negative rate.
#' FNR = (FN) / (POS)
#' = (FN) / (TP + FN)
#' = 1 - TPR
#' = 1 - RECALL
#' = 1 - SENSITIVITY
.fnr <- function(mat) { return(1 - .tpr(mat)) }
.miss.rate <- .fnr

#' False positive rate.
#' FPR = (FP) / (NEG)
#' = (FP) / (FP + TN)
#' = 1 - TNR
#' = 1 - SPECIFICITY
#' = 1 - SELECTIVITY
.fpr <- function(mat) { return(1 - .tnr(mat)) }

#' False discovery rate.
#' FDR = (FP) / (PRED. POS)
#' = (FP) / (FP + TP)
#' = 1 - PPV
.fdr <- function(mat) { return(1 - .ppv(mat)) }

#' False omission rate.
#' FOR = (FN) / (FN + TN)
#' = (FN) / (PRED. NEG)
#' = 1 - NPV
.for <- function(mat) { return(1 - .npv(mat)) }
.omission.rate <- .for

#' Accuracy.
#' ACC = (CORRECT) / (TOTAL)
#' = (TP + TN) / (POS + NEG)
#' = (TP + TN) / (TP + TN + FP + FN)
#' = 1 - ERR
.acc <- function(mat) { return(.correct(mat) / .total(mat)) }
.accuracy <- .acc

#' Error rate.
#' ERR = (INCORRECT) / (TOTAL)
#' = (FP + FN) / (POS + NEG)
#' = (FP + FN) / (TP + TN + FP + FN)
#' = 1 - ACC
.err <- function(mat) { return(1 - .acc(mat))}
.error.rate <- .err

## ---- analysis::metrics::defines ----

#' Calculate the counts for the labels in the provided vector.
#'
#' @param .data Dataset to select response from.
#' @param Y Response vector.
#'
#' @return Dataset containing counts.
.count.labels <- function(.data, Y = "Y") {
  return(.data %>% count(.data[[Y]]))
}

#' Calculate the priors for the labels in the provided vector.
#'
#' @param .data Dataset to select response from.
#' @param Y Response vector.
#'
#' @return Dataset containing prior probabilities.
.calc.priors <- function(.data, Y = "Y") {
  n_samples <- nrow(.data)
  counts <- .count.labels(.data, Y = Y)
  priors <- counts %>% mutate(priors = n / n_samples)
  return(priors)
}

#' Create a set of predictions, based on an input probability and threshold values.
#'
#' @param .probability Vector of numeric probabilities [0...1]
#' @param threshold [0.5] Numeric threshold that determines the class label.
.as.prediction <- function(.probability, threshold = 0.5) {
  return((.probability >= threshold))
}

#' Recode a logical vector into a new set of values using `yes`, `no`, and `missing` appropriately.
#'
#' @param .logi Vector to relabel.
#' @param yes [TRUE] Label to assign when logical vector is TRUE.
#' @param no [FALSE] Label to assign when logical vector is FALSE.
#' @param missing [NULL] Label to assign when logical vector value is missing.
#'
#' @return Vector.
.as.label <- function(.logi, yes = TRUE, no = FALSE, missing = NULL) {
  labels <- if_else(.logi, true = yes, false = no, missing = missing)
  return(factor(labels, levels = c(no, yes)))
}

#' Recode a vector into a series of logicals based on `true`, `false`, and `missing`.
#'
#' @param .vec Vector to relabel.
#' @param match ["positive"] Logical to assign when vector element matches value.
#' @param missing [NULL] Value to assign when vector element is missing.
.as.logical <- function(.vec, match = "positive", missing = NULL) {
  return(if_else(.vec == match, TRUE, FALSE, missing = missing))
}


#'
#' #' Find the optimal error rate, minimizing it using a threshold.
#' #'
#' #' @param probabilities Predicted probabilities for the positive class.
#' #' @param truth Ground truth class labels.
#' #'
#' #' @return List containing the optimal error rate and optimal threshold.
#' calc.optimal.error.rates <- function(probabilities, truth) {
#'
#'   # Threshold vector.
#'   thresholds <- sort(probabilities[truth])
#'   n_thresholds <- length(thresholds)
#'   n_cols <- 3
#'
#'   # Prepare the error matrix.
#'   err_mat <- matrix(0, n_thresholds, n_cols)
#'
#'   # Calculate error rates.
#'   for (i in 1:n_thresholds) {
#'     predictions <- (probabilities >= thresholds[i])
#'     clf_table <- table(predictions, truth)
#'     err_mat[i, 1] <- clf_table[1, 2] # FN
#'     err_mat[i, 2] <- clf_table[2, 1] # FP
#'   }
#'
#'   # Calculate the total error rates for each threshold value (row).
#'   err_mat[, 3] <- apply(err_mat[,1:2], 1, sum)
#'
#'   # Scale error rates by total count.
#'   errs <- err_mat / length(truth)
#'
#'   # Return list.
#'   return(list(
#'     errors = errs,
#'     thresholds = thresholds,
#'     probabilities = probabilities,
#'     truth = truth
#'   ))
#' }
#'
#' #' Plot the optimal error rates.
#' #'
#' #' @param errors Matrix 145 by 3 containing optimal FN, FP, and total errors.
#' #' @param thresholds Vector 145 by 1 containing thresholds.
#' #' @param limit Limit on the error rates.
#' #' @param digits Rounding position.
#' #'
#' #' @return Plot.
#' plot.optimal.error.rates <- function(errors, thresholds, ..., limit = 1.0, digits = 4) {
#'
#'   # Conditional.
#'   total_errors_limit <- (errors[, 3] < limit)
#'
#'   # Select errors within the valid total error limits.
#'   errs <- errors[total_errors_limit, ]
#'   threshs <- thresholds[total_errors_limit]
#'
#'   # Find the minimum total error.
#'   optimal_error <- min(errs[, 3])
#'
#'   # Find the index of the optimal error.
#'   optimal_index <- match(optimal_error, errs[, 3])
#'
#'   # Find the optimal threshold.
#'   optimal_threshold <- threshs[optimal_index]
#'
#'   # Round the optimal error and threshold.
#'   rounded_error <- round(optimal_error, digits = digits)
#'   rounded_threshold <- round(optimal_threshold, digits = digits)
#'
#'   # Bind thresholds.
#'   .data <- dplyr::as_tibble(cbind(errs, threshs))
#'   names(.data) <- c("false.neg", "false.pos", "total.err", "thresholds")
#'   # str(.data)
#'
#'   # Pivot the data for plotting.
#'   data <- .data %>%
#'     tidyr::pivot_longer(
#'       cols = !thresholds,
#'       names_to = "error_type",
#'       values_to = "error"
#'     )
#'   data$linetype <- (data$error_type == "total.err")
#'   # str(data)
#'
#'   # Get the graph title.
#'   p_title <- sprintf("Optimum error rate is %s at threshold = %s", rounded_error, rounded_threshold)
#'
#'   # Get the graph axis labels.
#'   p_xlab <- "Thresholds"
#'   p_ylab <- "Fraction Misclassified"
#'
#'   # Get the annotation.
#'   annotation <- data.frame(
#'     x = c(optimal_threshold + METRICS$offset$x),
#'     y = c(optimal_error - METRICS$offset$y),
#'     label = c(sprintf("Error = %s, Threshold = %s", rounded_error, rounded_threshold))
#'   )
#'
#'   # Draw a scatterplot matrix.
#'   p <- ggplot(data = data, mapping = aes(x = thresholds, y = error)) +
#'     # Add points grouped by error type.
#'     geom_point(mapping = aes(color = error_type), size = 1, alpha = 0.25) +
#'     geom_line(mapping = aes(color = error_type, linetype = linetype), size = 1, alpha = 1) +
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
#'                        breaks = seq(0.2, 0.8, 0.2)) +
#'     scale_y_continuous(p_ylab,
#'                        breaks = seq(0.0, 0.6, 0.1)) +
#'     scale_color_manual("Error Types",
#'                         values = c("red", "blue", "black"),
#'                         breaks = c("false.neg", "false.pos", "total.err"),
#'                         labels = c("Severe Misclassification", "Not Severe Misclassification", "Total Error")) +
#'     scale_linetype_manual(values = c(3, 1),
#'                           labels = c("Misclassification Error", "Total Error")) +
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
#'   return(p)
#' }
#'
#' #' Calculate the optimal classification.
#' #'
#' #' @param .results Results from the optimal error rate calculations.
#' #' @param limit Error limit.
#' #' @param true Label to give the positive class.
#' #' @param false Label to give the negative class.
#' #'
#' #' @return Classfication table.
#' make.optimal.confusion.mat <- function(.results, ..., limit = 1.0, true = TRUE, false = FALSE) {
#'
#'   # Get the errors and thresholds.
#'   # @param probabilities Predicted probabilities for the positive class.
#'   # @param truth Ground truth class labels.
#'   # .results <- calc.optimal.error.rates(probabilities, truth)
#'
#'   # Get the results values.
#'   errors <- .results$errors
#'   thresholds <- .results$thresholds
#'   probabilities <- .results$probabilities
#'   truth <- .results$truth
#'
#'   # Conditional.
#'   total_errors_limit <- (errors[, 3] < limit)
#'
#'   # Select errors within the valid total error limits.
#'   errors <- errors[total_errors_limit, ]
#'   thresholds <- thresholds[total_errors_limit]
#'
#'   # Find the minimum total error.
#'   optimal_error <- min(errors[, 3])
#'
#'   # Find the index of the optimal error.
#'   optimal_index <- match(optimal_error, errors[, 3])
#'
#'   # Find the optimal threshold.
#'   optimal_threshold <- thresholds[optimal_index]
#'
#'   # Find the optimal prediction.
#'   optimal_predictions <- (probabilities >= optimal_threshold)
#'
#'   # Get the optimal misclassification table:
#'   truth <- as.labels(truth, true = true, false = false)
#'   optimal_predictions <- as.labels(optimal_predictions, true = true, false = false)
#'   optimal_clf_table <- table(optimal_predictions, truth)
#'   return(optimal_clf_table)
#' }
#'

## ---- analysis::metrics::exports ----

METRICS$as <- list(
  prediction = .as.prediction,
  label = .as.label,
  logical = .as.logical
)

METRICS$calc <- list(
  # prior probabilities.
  priors = .calc.priors,
  labels = .count.labels,

  # matrix cells.
  total = .total,
  tn = .tn,
  fn = .fn,
  fp = .fp,
  tp = .tp,

  # conditionals
  cond = list(
    neg = .cond.neg,
    pos = .cond.pos
  ),

  # predictions
  pred = list(
    neg = .clf.neg,
    pos = .clf.pos
  ),

  # counts
  correct = .correct,
  incorrect = .incorrect,

  # metric short names
  ppv = .ppv,
  npv = .npv,
  tpr = .tpr,
  tnr = .tnr,
  fnr = .fnr,
  fpr = .fpr,
  fdr = .fdr,
  f_r = .for,
  acc = .acc,
  err = .err,

  # metric full names
  precision = .precision,
  recall = .recall,
  sensitivity = .sensitivity,
  specificity = .specificity,
  selectivity = .selectivity,
  omission.rate = .omission.rate,
  miss.rate = .miss.rate,
  error.rate = .error.rate
)
