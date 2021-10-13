# R/packages.R
#
# Import necessary packages.

## ---- packages::constants ----

REQUIREMENTS = list(
  . = "requirements.txt"
)

## ---- packages::exports ----

#' Write dependencies out from renv.
#'
#' @param file File to write dependencies to.
#'
#' @export
write.deps <- function(file = REQUIREMENTS$.) {
  file_path <- here::here(file)
  deps <- renv::dependencies()[,"Package"]
  deps <- as.vector(deps)
  deps <- unique(deps)
  deps <- sort(deps, decreasing = FALSE)
  if (length(deps) > 0) {
    message(sprintf("Writing project dependencies to '%s': ", basename(file_path)))
    conn <- file(file_path, blocking = TRUE)
    on.exit(close(conn))
    writeLines(deps, conn)
  }
  return(deps)
}

#' Read dependencies from a requirements.txt file.
#'
#' @param file File to read dependencies from.
#'
#' @export
read.deps <- function(file = REQUIREMENTS$.) {
  file_path <- here::here(file)
  deps <- NULL

  # Load the dependencies if requirements.txt exists.
  if (file.exists(file_path)) {
    conn <- file(file_path, open = "r", blocking = FALSE)
    on.exit(close(conn))
    deps <- as.vector(readLines(conn, skipNul = TRUE))
  }

  # Parse dependencies.
  if (!is.na(deps) && length(deps) > 0) {
    pkgs <- deps[!(deps %in% installed.packages()[, "Package"])]
    return(pkgs)
  }
  return(NULL)
}

#' Install required packages.
#'
#' @param file File to read dependencies from.
#'
#' @export
install.deps <- function(file = REQUIREMENTS$.) {
  if (renv::status()$synchronized == FALSE) {
    message("Installing dependencies...")
    pkgs <- read.deps(file)
    if (length(pkgs) > 0) {
      message("Installing packages...")
      install.packages(pkgs, dep = TRUE)
    }
    renv::restore()
  }
}

#' Require these packages to be on the system.
#' @export
require.deps <- function() {
  require(knitr)
  require(rmarkdown)
  require(markdown)
  require(mime)
}

#' Require and attach these packages to the project namespace.
#' @export
attach.deps <- function() {
  library(MASS)
  library(magrittr)
  library(foreach)
  library(ggplot2)
  library(forcats)
  library(tibble)
  library(tidyr)
  library(dplyr)
}

## ---- packages::exec ----

# Install the dependencies.
install.deps()
require.deps()
attach.deps()
