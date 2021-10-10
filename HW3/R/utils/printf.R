# R/utils/printf.R
#
# Utility function.

#' Print a formatted string.
#'
#' @param fmt String containing formatted template.
#'
#' @return NULL
printf <- function(template, ...) {
  fmtted <- sprintf(template, ...)
  print(fmtted)
}

#' Print and return an object's classes.
#'
#' @param obj Object to find classes for.
#'
#' @return Formatted String containing the object classes.
printf.class <- function(obj, ..., collapse = "/") {
  fmtted <- paste(class(obj), collapse = collapse)
  print(fmtted)
  return(fmtted)
}
