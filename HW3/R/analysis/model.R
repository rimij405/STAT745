# R/analysis/model.R
#
# Model related functions.

#' Fit a model.
#'
#' @param .data Dataset.
#' @param algorithm Model fitting function.
#' @param formula Formula used to fit the model.
#' @param ... Additional model parameters.
#'
#' @return Fit model object.
fit.model <- function(.data, algorithm, formula = Y ~ ., ...) {

  # print("Fitting model on the dataset...")
  model_opts <- list(formula = formula, data = .data, ...)
  model_obj <- do.call(algorithm, args = model_opts)
  # Equivalent: model_obj <- rlang::exec(quote(algorithm), !!!model_opts)

  # Get summary and replace the expanded $call.
  model_expr <- sprintf("%s", deparse1(sys.call()))
  model_expr <- gsub("\"", "'", model_expr)
  model_expr <- gsub("", "", model_expr)
  # print(model_expr)
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
