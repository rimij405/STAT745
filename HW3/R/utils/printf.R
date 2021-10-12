# R/utils/printf.R
#
# Utility functions related to printing and output.

## ---- printf::constants ----

DEFAULT_BORDER = "========================"

## ---- printf::functions ----

#' Write single line to the provided output.
#'
#' @param text Message to print.
#' @param ... Additional options to pass to `writeLines` or `message`.
#' @param message TRUE or FALSE. If TRUE, use the `message` function instead of `writeLines`.
#' @param dry Skip printing?
println <- function(text, ..., con = stdout(), sep = "\n", useBytes = FALSE, message = FALSE, dry = FALSE, domain = NULL, appendLF = TRUE) {
  if (!dry) {
    arguments <- list(...)
    content <- c(unlist(text), unlist(arguments, use.names = TRUE), list())
    content <- paste0(content, collapse = sep)
    if (!message) {
      writeLines(text = content, con = con, sep = sep, useBytes = useBytes)
    } else {
      message(content, domain = domain, appendLF = appendLF)
    }
  }
}

#' Write a list, using unllist.
#'
#' @param .target List to unlist.
#' @param ... Additional params to pass to `println`.
#' @param message TRUE or FALSE. If TRUE, use the `message` function instead of `writeLines`.
#' @param dry Skip printing?
println.list <- function(.target, ...) {
  println(unlist(.target, use.names = TRUE), ...)
}

#' Alias for `println.list`
printl <- println.list

#' Write formatted line to output connection.
#'
#' @param .template Template String to format arguments into.
#' @param ... Additional options to pass to `sprintf`.
#' @param message TRUE or FALSE. If TRUE, use the `message` function instead of `writeLines`.
#' @param dry Skip printing?
println.formatted <- function(.template, ..., con = stdout(), sep = "\n", useBytes = FALSE, message = FALSE, dry = FALSE, domain = NULL, appendLF = TRUE) {
  formatted <- sprintf(.template, ...)
  println(text = formatted, con = con, sep = sep, useBytes = useBytes, message = message, appendLF = appendLF)
}

#' Alias for `println.formatted()`
printf <- println.formatted

#' Write classes of input object.
#'
#' @param .obj Object to print classes for.
#' @param ... Additional objects to pass to `println`.
#' @param collapse Character to use when combining multiple classes.
#' @param message TRUE or FALSE. If TRUE, use the `message` function instead of `writeLines`.
#' @param dry Skip printing?
println.class <- function(.obj, ..., collapse = "/", message = FALSE, dry = FALSE) {
  formatted <- paste(class(.obj), collapse = collapse)
  println(formatted, ..., message = message, dry = dry)
}

#' Alias for `println.class`
printc <- println.class

#' Write message between a set of borders.
#'
#' @param ... Message contents to pass to `println`.
#' @param border Border to print.
#' @param sep Separator to use between 'lines'.
#' @param template (Optional) template String to use.
#' @param message TRUE or FALSE. If TRUE, use the `message` function instead of `writeLines`.
#' @param dry Skip printing?
println.section <- function(..., border = DEFAULT_BORDER, sep = "\n", message = FALSE, dry = FALSE) {
  println(border, sep = sep, message = message, dry = dry)
  println(..., sep = sep, message = message, dry = dry)
  println(border, sep = sep, message = message, dry = dry)
}

#' Alias for `println.section`
header <- println.section

#' Write message with formatted text.
#'
#' @param .template Template String.
#' @param ... Arguments to pass to message.
println.message <- function(...) {
  println(..., message = TRUE)
}

#' Alias for message(sprintf(fmt, ...))
messagef <- function(.template, ..., domain = NULL, appendLF = TRUE) {
  formatted <- sprintf(.template, ...)
  message(formatted, domain = domain, appendLF = appendLF)
}
