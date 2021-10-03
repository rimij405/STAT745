# utils-confmat.R
#
# Utility functions used for classifier analysis.

## ---- confusion-matrix ----

# Define helper functions used for metric calculations

# Confusion matrix fields:
# mat[1,1] = TP
# mat[1,2] = FP
# mat[2,1] = FN
# mat[2,2] = TN

#' Map probability vector to positive and negative values if row meets
#' or exceeds a particular threshold.
#'
#' @param y_pred Probabilities to map.
#' @param pos Positive class label.
#' @param neg Negative class label.
#'#'
#' @return vector of mapped values.
#'
map.where <- function(condition, ..., pos = TRUE, neg = FALSE) {
  return(ifelse(condition, pos, neg))
}

#' Given a confusion matrix, get the true positive.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return value representing true positive.
TP <- function(mat) {
  return(mat[1,1])
}

#' Given a confusion matrix, get the false positive (type I error).
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return value representing false positive.
FP <- function(mat) {
  return(mat[1,2])
}

#' Given a confusion matrix, get the false negative (type II error).
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return value representing false negative
FN <- function(mat) {
  return(mat[2,1])
}

#' Given a confusion matrix, get the true negative.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return value representing true negative.
TN <- function(mat) {
  return(mat[2,2])
}

#' Given a confusion matrix, count the positive classifications.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return TP + FN
calc.POS <- function(mat) {
  return(mat[1,1] + mat[2,1])
}

#' Given a confusion matrix, count the negative classifications.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return FP + TN
calc.NEG <- function(mat) {
  return(mat[1,2] + mat[2,2])
}

#' Given a confusion matrix, count the total classifications.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return TP + TN + FN + FP = P + N = TOTAL
calc.TOTAL <- function(mat) {
  return(sum(mat))
}

#' Create a confusion matrix.
#'
#' @param y_pred Predicted probabilities for the positive class.
#' @param y_truth Vector containing the ground truth of positive class.
#' @param p.threshold (optional) Threshold to determine label.
#'
#' @return 2x2 confusion matrix.
as.Confusion.mat <- function(y_pred, y_truth, ..., p.threshold = 0.5) {
  y_predictions <- map.where((y_pred >= p.threshold), pos = TRUE, neg = FALSE)
  mat <- table(y_predictions, y_truth)
  colnames(mat) <- c("TRUE", "FALSE")
  rownames(mat) <- c("TRUE", "FALSE")
  return(mat)
}

#' Create a confusion matrix explicitly.
#'
#' @param tp true positive
#' @param fp false positive (Type I error)
#' @param fn false negative (Type II error)
#' @param tn true negative
#'
#' @return 2x2 confusion matrix.
make.Confusion.mat <- function(..., tp, fp, fn, tn) {
  mat <- rbind(c(tp, fp), c(fn, tn))
  colnames(mat) <- c("TRUE", "FALSE")
  rownames(mat) <- c("TRUE", "FALSE")
  return(mat)
}

#' Create a Classification table.
#'
#' @param y_pred Predicted probabilities for the positive class.
#' @param y_truth Vector containing the ground truth of positive class.
#' @param p.threshold (optional) Threshold to determine label.
#'
#' @return list containing a confusion matrix and quick accessors.
as.Classif.table <- function(y_pred, y_truth, ..., p.threshold = 0.5) {
  # Get the confusion matrix.
  mat <- as.Confusion.mat(y_pred, y_truth, p.threshold = p.threshold)
  response <- list(
    "tp" = TP(mat),
    "fp" = FP(mat),
    "fn" = FN(mat),
    "tn" = TN(mat),
    "p" = calc.POS(mat),
    "n" = calc.NEG(mat),
    "total" = calc.TOTAL(mat),
    "table" = mat
  )
  return(response)
}

# METRIC CALCULATIONS #

#' Calculate the false positive rate.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (FP) / (FP + TN)
calc.FPR <- function(mat) {
  # FPR (Type I Error)
  # Proportion of all negative observations
  # incorrectly classified as positive.
  return((mat[1,2]) / (mat[1,2] + mat[2,2]))
}

#' Calculate the false negative rate.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (FN) / (TP + FN)
calc.FNR <- function(mat) {
  # FNR (Type II Error)
  # Proportion of all positive observations
  # incorrectly classified as negative.
  return((mat[2,1]) / (mat[1,1] + mat[2,1]))
}

#' Calculate the true negative rate.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TN) / (TN + FP)
calc.TNR <- function(mat) {
  # TNR (Specificity)
  # Proportion of all negative observations
  # correctly classified as negative.
  return((mat[2,2]) / (mat[2,2] + mat[1,2]))
}

#' Calculate the true positive rate.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TP) / (TP + FN)
calc.TPR <- function(mat) {
  # TPR (Sensitivity) (Recall)
  # Proportion of all positive observations
  # correctly classified as positive.
  return((mat[1,1]) / (mat[1,1] + mat[2,1]))
}

#' Calculate the negative predictive value.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TN) / (TN + FN)
calc.NPV <- function(mat) {
  # NPV (Negative Class Precision)
  # Proportion of all negative classifications
  # correctly classified as negative.
  return((mat[2,2]) / (mat[2,2] + mat[2,1]))
}

#' Calculate the positive predictive value
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TP) / (TP + FP)
calc.PPV <- function(mat) {
  # PPV (Positive Class Precision)
  # Proportion of all positive classifications
  # correctly classified as positive.
  return()
}

#' Calculate the false discovery rate.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (FP) / (TP + FP)
calc.FDR <- function(mat) {
  # FDR (1 - Precision)
  # Proportion of all positive classifications
  # incorrectly classified as positive.
  return((mat[1,2]) / (mat[1,1] + mat[1,2]))
}

#' Calculate the error rate of the model.
#'
#' @param mat 2x2 confusion matrix.
#'
#' @return (FP + FN) / (TP + TN + FN + FP)
calc.ERR <- function(mat) {
  # ERR (1 - Accuracy)
  # Proportion of all classifications
  # incorrectly classified.
  return((mat[1,2] + mat[2,1]) / (sum(mat)))
}

#' Calculate the accuracy of a model.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TP + TN) / (TP + FP + FN + TN)
calc.ACC <- function(mat) {
  # ACC (Accuracy)
  # Proportion of all classifications
  # correctly classified.
  return((mat[1,1] + mat[2,2]) / sum(mat))
}

#' Calculate the precision of a model.
#'
#' @param mat 2x2 confusion matrix
#' @seealso calc.PPV
#'
#' @return (TP) / (TP + FP)
calc.Precision <- function(mat) {
  # Equivalent to positive predictive value.
  return(calc.PPV(mat))
}

#' Calculate the recall of a model.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (TP) / (TP + FN)
calc.Recall <- function(mat) {
  # Equivalent to true positive rate.
  return(calc.TPR(mat))
}

#' Calculate the F-Score of a model.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return (1 + beta^2) x ((precision x recall) / (beta^2 x precision) + recall)
calc.FScore <- function(mat, ..., beta = 1.0) {
  # F_beta (F-Score)
  # Harmonic average between precision and recall.
  # The higher the beta chosen, the more important
  # recall is over precision.
  p <- calc.Precision(mat)
  r <- calc.Recall(mat)
  scale_f <- (1 + beta * beta)
  num_f <- p * r
  den_f <- (beta * beta * p) + r

  # F1 score treats recall and precision equally.
  # F2 score treats recall as twice as important.
  return((scale_f) * (num_f / den_f))
}

#' Calculate the F1 score.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return ((2 x Recall x Precision) / (Recall + Precision))
calc.F1 <- function(mat) {
  # F1 score treats recall and precision equally.
  return(calc.FScore(mat, beta = 1.0))
}

#' Calculate the F2 score.
#'
#' @param mat 2x2 confusion matrix
#'
#' @return ((2 x Recall x Precision) / (Recall + Precision))
calc.F2 <- function(mat) {
  # F2 score treats recall as twice as important.
  return(calc.FScore(mat, beta = 2.0))
}

#' Calculate the Cohen Kappa metric
#'
#' @param fit.mat 2x2 confusion matrix for fit model.
#' @param random.mat 2x2 confusion matrix for random classifier.
#'
#' @return k = (ACC(fit) - ACC(random)) / (1 - ACC(random))
calc.CohenKappaScore <- function(fit.mat, random.mat) {
  p0 <- calc.ACC(fit.mat)
  pE <- calc.ACC(random.mat)
  num_k <- p0 - pE
  den_k <- 1 - pE
  return(num_k / den_k)
}

## ---- classifier-utils ----

#' Calculate all error rates for a set of thresholds.
#'
#' @param y_pred Predicted probability for the positive class.
#' @param y_label Ground truth labels with positive and negative classes.
#' @param pos (optional) Value assigned to the positive class.
#'
#' @return list with $FalsePos.rate, $FalseNeg.rate, $TotalErr.rate, $P.Thresholds
calc.ERR.rates <- function(y_pred, y_label, ..., pos = 1, neg = 0) {
  # Get a logical vector for ground truth.
  y_truth <- map.where(y_label == pos)

  # Sort only the probabilities for the positive class.
  p.thresholds <- sort(y_pred[y_truth])

  # Form matrix with k rows and 3 columns.
  k <- length(p.thresholds)
  Error.mat <- matrix(0, k, 4)

  # For each i in sequential range 1 to k.
  for(i in 1:k) {
    # Get the classification table at this p.threshold.
    Classif.table <- as.Confusion.mat(y_pred, y_truth, p.threshold = p.thresholds[i])

    # Calculate the positive misclassification rate.
    Error.mat[i, 1] <- calc.FPR(Classif.table)

    # Calculate the negative misclassification rate.
    Error.mat[i, 2] <- calc.FNR(Classif.table)

    # Calculate the total misclassification rate.
    Error.mat[i, 3] <- calc.ERR(Classif.table)

    # Save the thresholds.
    Error.mat[i, 4] <- p.thresholds[i]
  }

  # Output the error rates
  return(list(
    FalsePos.rate = Error.mat[,1],
    FalseNeg.rate = Error.mat[,2],
    TotalErr.rate = Error.mat[,3],
    P.Thresholds = Error.mat[,4]
  ))
}

#' Plot all the error rates by the generated probability thresholds.
#'
#' @param FPRs Vector of false positive rates.
#' @param FNRs Vector of false negative rates.
#' @param ERRs Vector of total error rates.
#' @param P.Thresholds Vector of probability thresholds used to calculate err.
#' @param limit Boundaries on the error rates.
plot.ERRvsThreshold <- function(FPRs, FNRs, ERRs, P.Thresholds, ..., limit = c(0.0, 1.0)) {

  # FPRs = liver.fit.ERR.rates$FalsePos.rate
  # FNRs = liver.fit.ERR.rates$FalseNeg.rate
  # ERRs = liver.fit.ERR.rates$TotalErr.rate
  # P.Thresholds = liver.fit.ERR.rates$P.Thresholds
  # limit = c(0.0, 1.0)

  # Prepare matrix.
  ERR.mat <- matrix(0, length(P.Thresholds), 4)
  ERR.mat[,1] = FPRs
  ERR.mat[,2] = FNRs
  ERR.mat[,3] = ERRs
  ERR.mat[,4] = P.Thresholds

  # Prepare boundaries.
  min.limit <- limit[1]
  max.limit <- limit[2]
  condition <- (ERR.mat[,3] > min.limit & ERR.mat[,3] < max.limit)

  # Filter based on boundary condition.
  ERR.rates <- ERR.mat[condition,]
  Thresholds <- P.Thresholds[condition]

  # Get the minimum error rate.
  min.ERR.rate <- min(ERR.rates[,3])
  ind <- match(min.ERR.rate, ERR.rates[,3])
  min.Threshold = Thresholds[ind]

  # Convert to dataframe.
  ERR.df <- as_tibble(as.data.frame(ERR.rates))
  names(ERR.df) <- c("FPRs", "FNRs", "ERRs", "P.Threshold")
  ERR.melted <- reshape2::melt(ERR.df,
                               id.vars = c("P.Threshold"),
                               variable.name = "metric.variable",
                               value.name = "metric.value")

  # Round the minimums.
  min.ERR.rate.rounded <- round(min.ERR.rate, 4)
  min.Threshold.rounded <- round(min.Threshold, 3)

  # Labels for the plot.
  plot.title = sprintf("Optimum error rate is %s at Threshold = %s",
                       min.ERR.rate.rounded, min.Threshold.rounded)
  plot.tooltip = sprintf("ERR = (%s), Threshold = (%s)",
                         min.ERR.rate.rounded, min.Threshold.rounded)
  plot.x_label = "Probability Threshold"
  plot.y_label = "% Misclassified Error"

  p <- ggplot(data = ERR.melted, mapping = aes(x = P.Threshold,
                                               y = metric.value,
                                               colour = metric.variable)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = min.ERR.rate) +
    geom_vline(xintercept = Thresholds[ind]) +
    annotate("text",
             x = 0.05 + min.Threshold.rounded,
             y = 0.05 + min.ERR.rate.rounded,
             label=plot.tooltip) +
    ggtitle(plot.title) +
    labs(fill = "Legend: Metrics") +
    xlab(plot.x_label) +
    ylab(plot.y_label)

  return(p)
}
