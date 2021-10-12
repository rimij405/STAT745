# R/build.R
#
# Common functions for building / rendering.

## ---- build::constants ----

BUILDS <- list(
  dataset = here::here("R/build/dataset.R"),
  report = here::here("R/build/report.R")
)

## ---- build::imports ----

# See: source.submodule()
source(here::here("R/utils.R"))
source.submodule(files = BUILDS)
