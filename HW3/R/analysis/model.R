# R/analysis/model.R
#
# Model related functions.

## ---- analysis::model::constants ----

MODEL <- list(
  . = glm,
  params = list(
    formula = Y ~ .,
    family = "binomial"
  )
)

## ---- analysis::model::exports ----

#' Fit a model.
#'
#' @param .data Dataset.
#' @param algorithm Model fitting function.
#' @param formula Formula used to fit the model.
#' @param ... Additional model parameters.
#'
#' @return Fit model object.
fit.model <- function(.data,
                      algorithm = MODEL$.,
                      params = list(), ...) {

  # Get summary and replace the expanded $call.
  model_expr <- sprintf("%s", deparse1(sys.call()))
  model_expr <- gsub("\"", "'", model_expr)
  model_expr <- gsub("", "", model_expr)
  # print(model_expr)

  # Prepare algorithm arguments.
  # - data is always ovewritten to .data
  # - params is of least priority.
  # - ... parameters overwrite anything in params.
  model_args <- list( ) # Empty list to begin.
  model_args <- modifyList(model_args, params, keep.null = TRUE) # Allows us to mark params as NULL.
  model_args <- modifyList(model_args, list(...), keep.null = TRUE) # Allows us to mark params as NULL.
  model_args <- modifyList(model_args, list(data = .data)) # Ensures our data param is .data always.
  model_obj <- do.call(algorithm, args = model_args)

  # model_obj$call <- model_expr
  return(list(
    obj = model_obj,
    expr = model_expr
  ))

}

#' Summarize a fit model.
#'
#' @param .obj A fitted model object.
#'
#' @return Fit model results.
summarize.model <- function(.obj) {

  model_summary <- summary(.obj)
  return(model_summary)

}
