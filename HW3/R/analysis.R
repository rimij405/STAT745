# R/analysis.R
#
# Analysis functions for the liver dataset.

## ---- analysis::constants ----

ANALYSIS <- list(
  setup = here::here("R/analysis/setup.R"),
  eda = here::here("R/analysis/eda.R"),
  model = here::here("R/analysis/model.R"),
  metrics = here::here("R/analysis/metrics.R"),
  validation = here::here("R/analysis/validation.R"),
  lda = here::here("R/analysis/lda.R")
)

## ---- analysis::imports ----

# Source the utility function.
source(here::here("R/packages.R"))
source(here::here("R/utils.R"))
source.submodule(files = ANALYSIS)

## ---- def-analysis-exec ----

#' Run cross-validation analysis.
#'
#' @param .data Training dataset that will be folded.
#' @param truth Ground truth labels..
#' @param algorithm Model to use.
#' @param ... Algorithm parameters.
#' @param formula Patsy formula to fit model with.
#' @param k Number of folds in k-fold CV.
#' @param rounds Number of rounds.
#' @param m Number of thresholds to use.
#' @param pos Name of the positive class label.
#' @param neg Name of the negative class label.
#'
#' @return Results from analysis.
analysis.cv <- function(.data,
                        truth = .data[,Y],
                        algorithm = glm,
                        params = list(
                          formula = Y ~ .,
                          family = "binomial"
                        ),
                        k = 10, rounds = 100, m = 100) {

  # Find the cross validation rates.
  baseline_cv <- calc.cv.error.rates(
    .data, truth,
    algorithm = algorithm,
    params = params,
    k = k, rounds = rounds, m = m,
  )

  # Plot the CV error rates.
  plot_cv <- plot.cv.error.rates(
    baseline_cv$errors,
    baseline_cv$thresholds
  )

  # Find the cross validation error table.
  clf_cv <- make.cv.confusion.mat(
    .data, truth,
    algorithm = algorithm,
    params = params,
    k = k, rounds = rounds,
    threshold = plot_cv$optimal_threshold
  )

  # Summarize the confusion matrices.
  clf_cv_summary <- summarize.cv.confusion.mat(clf_cv)

  # Return elements.
  return(list(
    results = baseline_cv,
    plot = plot_cv,
    optimal_threshold = plot_cv$optimal_threshold,
    confusion_mat = clf_cv,
    confusion_mat_summary = clf_cv_summary
  ))
}

#' Run model analysis.
#'
#' @param .data Dataset to train model with.
#' @param truth Ground truth.
#' @param algorithm Model to train.
#' @param ... Algorithm parameters.
#' @param formula Patsy formula for model.
#' @param pos Positive class label.
#' @param neg Negative class label.
#'
#' @return Model summary and results.
analysis.clf <- function(.data,
                         truth = .data[,Y],
                         algorithm = glm,
                         params = list(
                           formula = Y ~ .,
                           family = "binomial"
                         ),
                         call = NULL,
                         pos = TRUE, neg = FALSE) {

  # Fit and summarize a model.
  baseline <- fit.model(
    quote(.data),
    algorithm = algorithm,
    params = params)
  clf <- baseline$obj
  clf_summary <- summarize.model(clf)

  # Calculate table.
  p <- calc.probability(.data, truth)
  predictions <- calc.predictions(clf, threshold = p, type = "response")
  clf_table <- make.confusion.mat(predictions, truth, true = pos, false = neg)

  # Calculate the error rate.
  clf_error <- calc.error.rate(clf_table)

  # Find the optimal error rate.
  clf_optimal_error <- calc.optimal.error.rates(clf$fitted.values, truth)

  # Get the optimal table.
  clf_optimal_table <- make.optimal.confusion.mat(clf_optimal_error, true = pos, false = neg)

  # Plot the optimal error rates.
  clf_plot_errors <- plot.optimal.error.rates(
    clf_optimal_error$errors,
    clf_optimal_error$thresholds
  )

  # Return summaries and plot.
  return(list(
    model = baseline,
    clf = clf,
    summary = clf_summary,
    probabilities = p,
    predictions = predictions,
    error = clf_error,
    table = clf_table,
    optimal_errors = clf_optimal_error,
    optimal_table = clf_optimal_table,
    plot = clf_plot_errors
  ))
}

#' Run EDA analysis.
#'
#' @param .data Dataset.
#' @param truth Ground truth.
#' @param pos Positive class label.
#' @param neg Negative class label.
#'
#' @return Results of EDA.
analysis.eda <- function(.data, truth = .data[,Y], pos = TRUE, neg = FALSE) {

  # Get the shape.
  shape <- .data %>% make.shape()

  # Summarize the features.
  features <- .data %>% summarize.features()

  # Summarize the response.
  response <- .data %>% summarize.response()

  # Count the responses.
  n_labels = list(
    pos = length(truth[truth == pos]),
    neg = length(truth[truth == neg]),
    total = length(truth)
  )
  p_labels <- list(
    pos = n_labels$pos / n_labels$total,
    neg = n_labels$neg / n_labels$total
  )

  # Get the correlation of features.
  corr_features <- corr.features(.data)

  # Return results.
  return(list(
    shape = shape,
    corr = corr_features,
    X.summary = features,
    y.summary = response,
    n_labels = n_labels,
    p_labels = p_labels
  ))

}

## ---- def-analysis-liver ----

#' Analyize the liver dataset.
analysis.liver <- function(..., log = TRUE, show_figures = TRUE) {
  # If logging is on, setup the file.
  if (log) {
    clean.log()
    output_file <- setup.log()
    sink(file = output_file, append = TRUE, split = TRUE)
  }

  # Start logging.
  start.log()

  # Close the log file.
  on.exit(sink(NULL))

  # ---- analysis-liver-setup ----

  # Prepare for the EDA.
  print.section("{1} Setup")
  setup.analysis(target = "liver", cache = FALSE)

  # Recode the response.
  printf("Recoding the response in the dataset...")
  liver <- liver %>% make.response()
  print(summary(liver))

  # Extract the label fields.
  print("Extracting positive class labels...")
  truth <- (liver$severity == "Severe")
  print(summary(truth))

  # ---- analysis-liver-EDA ----

  # Perform EDA.
  print.section("{2} EDA")
  liver.info <- analysis.eda(liver, truth)
  do.call(printf, c("Shape of data: (%s, %s)", liver.info$shape))
  print("Summary of features: "); print(liver.info$X.summary)
  print("Summary of response: "); print(liver.info$y.summary)
  print("Label counts: "); print(liver.info$n_labels)
  print("Class prior probabilities: "); print(liver.info$p_labels)

  # Plot correlation matrix.
  liver.corrplot <- corr.plot(liver,
    output_name = "HW3_corrplot.png",
    sig.level = 0.05, insig = "blank",
    title = "Correlation Plot",
    mar = c(1,1,2,1),
    device = png, device_opts = list(
      units = "in",
      width = 8,
      height = 8,
      res = 600
    )
  )
  print("Feature correlation matrix: "); print(liver.corrplot$corr)
  if (show_figures == TRUE) {
    # Show plot for correlation matrix.
    print("RStudio... Check View pane for plot.");
    corrplot::corrplot(liver.corrplot$corr)
  }

  # ---- analysis-liver-baseline ----

  # Fit baseline model.
  print.section("{3} Baseline Model Fitting")
  liver.baseline <- analysis.clf(
    liver, truth,
    params = list(
      formula = severity ~ .,
      family = "binomial"
    ),
    pos = "Severe", neg = "Not Severe")
  print("Summary of baseline glm: "); print(liver.baseline$summary)
  printf("Baseline error rate: %s", liver.baseline$error)
  print("Baseline Classification Table: "); print(liver.baseline$table)
  if (show_figures == TRUE) {
    # Show plot for baseline error plot.
    print(liver.baseline$plot)
  }
  print("Summary of optimal error rates: "); print(summary(liver.baseline$optimal_errors))
  print("Classification Table in optimal case: "); print(liver.baseline$optimal_table)

  # ---- analysis-liver-10cv ----

    # 10-fold CV analysis.
  print.section("{4} 10-fold CV")
  liver.10fold.cv <- analysis.cv(
     liver, truth,
     params = list(
       formula = severity ~ .,
       family = "binomial"
     ),
     k = 10, rounds = 100, m = 100
  )
  print(summary(liver.10fold.cv))
  if (show_figures == TRUE) {
    # Show plot for baseline error plot.
    print(liver.10fold.cv$plot)
  }

  # ---- analysis-liver-3cv ----

  # CV analysis.
  print.section("{5} 3-fold CV")
  liver.3fold.cv <- analysis.cv(
    liver, truth,
    params = list(
      formula = severity ~ .,
      family = "binomial"
    ),
    k = 3, rounds = 100, m = 100
  )
  print(summary(liver.3fold.cv))
  if (show_figures == TRUE) {
    # Show plot for baseline error plot.
    print(liver.3fold.cv$plot)
  }

  # End logging.
  end.log()
}
