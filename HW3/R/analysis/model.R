# R/analysis/model.R
#
# Model related functions.

## ---- analysis::model::constants ----

# Create a MODEL pointer in the global scope.
MODEL <<- vector("list", 0L)

# Default settings for the model script.
.MODEL <- list(
  formula = Y ~ .,
  family = "binomial"
)

## ---- analysis::model::defines ----

#' Fit an arbitrary statistical learning model.
#'
#' @param .data Training dataset.
#' @param FUN algorithm to execute.
#' @param ... Model parameters to pass into the algorithm.
#' @param params Model parameters to pass into the algorithm. Only active when `...` is not provided.
#'
#' @return Model object.
#' @export
.fit.model <- function(.data, FUN = glm, ..., params = NULL, silent = FALSE) {

  # Get the ... arguments.
  arguments <- list(...)
  if ((length(arguments) > 0) && (exists("params") && !is.null(params) && length(params) > 0)) {
    if (!silent) {
      warning("Conflicting model arguments provided.")
      warning("The `params` list is ignored when `...` arguments are passed directly.")
      warning("Please choose one method of specifying model arguments.")
    }
  }

  if (length(arguments) > 0) {
    model_params <- list(...)
  } else if (exists("params") && (!is.null(params)) && (length(params) > 0)) {
    model_params <- params
  } else {
    if (!silent) {
      message("No model parameters specified. Using default model arguments.")
      message(sprintf("Default parameters: %s", deparse(.MODEL)))
    }
    model_params <- .MODEL
  }

  if (exists("data", where = model_params) && !silent) {
    warning("Conflicting training data source provided.")
    warning("Data passed into the `params` list are ignored.")
  }

  # Add the data argument.
  model_params <- modifyList(model_params, list(data = .data))

  # Execute the model.
  model_obj <- do.call(deparse(substitute(FUN)), args = model_params, quote = FALSE)
  return(model_obj)
}

#' Summarize a model.
#'
#' @param .obj Model to summarize.
#' @param show [TRUE] Display summary immediately?
#'
#' @return summary of model object.
.summarize.model <- function(.obj, show = TRUE) {
  .summary <- summary(.obj)
  if (show) {
    writeLines("-----------------------------------")
    cat(deparse(.obj$call), sep = "\n")
    cat(deparse(.obj$terms), sep = "\n")
    writeLines("-----------------------------------")
  }
  return(.summary)
}

## ---- analysis::model::exports ----

MODEL$fit <- .fit.model
MODEL$summarize <- .summarize.model
