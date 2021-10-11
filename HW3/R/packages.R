# R/packages.R
#
# Adds necessary packages to the source.

# Source the utility functions.
# source(here::here("R/utils.R"))
# source.utils()

## ---- def-packages ----

#' Require these packages to be on the system.
require.deps <- function() {
  require(knitr)
  require(rmarkdown)
  require(markdown)
  require(mime)
}

#' Require and attach these packages to the project namespace.
attach.deps <- function() {
  library(magrittr)
  library(foreach)
  library(ggplot2)
  library(forcats)
  library(tidyr)
  library(dplyr)

}
