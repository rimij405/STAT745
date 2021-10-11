# R/analysis.R
#
# Perform EDA on the Liver dataset.

# Source the utility functions.
source(here::here("R/packages.R"))

## ---- analysis-setup ----

# Source the setup script.
source(here::here("R/analysis/setup.R"))

## ---- analysis-EDA ----

# Source the EDA functions.
source(here::here("R/analysis/eda.R"))

## ---- analysis-model ----

# Source the model function.
source(here::here("R/analysis/model.R"))

## ---- analysis-metrics ----

# Source the model function.
source(here::here("R/analysis/metrics.R"))

## ---- analysis-validation ----

# Source the model function.
source(here::here("R/analysis/validation.R"))

## ---- exec-analysis ----

liver.analysis <- function(..., log = TRUE) {
  # If logging is on, setup the file.
  if (log) {
    output_file <- setup.log()
    sink(file = output_file, append = TRUE, split = TRUE)
  }

  # Start logging.
  start.log()

  # Close the log file.
  on.exit(sink(NULL))

  # Prepare for the EDA.
  setup.analysis(target = "liver", cache = FALSE)
  liver <- liver %>% make.response()
  liver_shape <- liver %>% make.shape()
  str(liver)
  print(liver_shape)

  # Describe the features.
  liver_features <- liver %>% summarize.features()
  print(liver_features)

  # Describe the response.
  liver_response <- liver %>% summarize.response()
  print(liver_response)

  # Plot the features.
  # liver_corr <- liver %>% corr.features()
  liver_corrplot <- liver %>%
    corr.plot("HW3_corrplot.png",
              device = png,
              device_opts = list(
                units = "in",
                width = 8,
                height = 8,
                res = 300
              ),
              sig.level = 0.05,
              insig = "blank",
              title = "Correlation Plot",
              mar = c(1,1,2,1))
  print(liver_corrplot$corr)

  ## ---- exec-classifier ----

  # Fit a logistic regression model.
  liver_mod <- liver %>%
    fit.model(algorithm = glm, formula = severity ~ ., family = "binomial")
  liver_fit <- liver_mod$obj
  liver_fit$call <- liver_mod$expr
  print(liver_fit)

  # Get the summary.
  liver_summary <- liver_fit %>% summarize.model()
  print(liver_summary)

  # Calculate classification table.
  # print(liver$severity)
  p <- liver %>% calc.probability(severity == "Severe")
  predictions <- liver_fit %>% calc.predictions(threshold = p, type = "response")
  truth <- (liver$severity == "Severe")
  liver_table <- make.confusion.mat(predictions, truth, true = "Severe", false = "Not Severe")
  print(liver_table)

  # Calculate the error rate.
  liver_error <- calc.error.rate(liver_table)
  print(liver_error)

  # Find the optimal error rates.
  liver_optimal_errors <- calc.optimal.error.rates(liver_fit$fitted.values, truth)
  str(liver_optimal_errors)

  # Plot the optimal error rates.
  liver_optimal_plot <- plot.optimal.error.rates(
    liver_optimal_errors$errors,
    liver_optimal_errors$thresholds
  )
  print(liver_optimal_plot)
  # Show the plot.

  # Find the optimal error table.
  liver_optimal_table <- make.optimal.confusion.mat(liver_optimal_errors, true = "Severe", false = "Not Severe")
  print(liver_optimal_table)

  # Find the cross validation rates.
  liver_10cv_errors <- calc.cv.error.rates(
    liver, truth,
    glm, formula = severity ~ ., family = "binomial",
    k = 10, rounds = 100, m = 100)
  print(liver_10cv_errors)

  # Plot the cross validation error rates.
  liver_10cv_plot <- plot.cv.error.rates(
    liver_10cv_errors$errors,
    liver_10cv_errors$thresholds
  )
  print(liver_10cv_plot$plot)

  # Find the cross validation error table.
  liver_10cv_table <- make.cv.confusion.mat(
    liver, truth,
    glm, formula = severity ~ ., family = "binomial",
    k = 10, rounds = 10,
    threshold = liver_10cv_plot$optimal_threshold)

  # Summarize the matrices of tables:
  liver_10cv_table_summary <- liver_10cv_table %>% summarize.cv.confusion.mat()
  print(liver_10cv_table_summary)

  # End logging.
  end.log()
}

# Run analysis.
liver.analysis()
