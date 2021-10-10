# packages.R
#
# Adds necessary packages to the source.

## ---- install-deps ----

# Install dependencies.
if (renv::status()$synchronized == FALSE) {
  print("Installing dependencies...")
  renv::restore()
}

## ---- attach-deps ----

# Add packages to namespace.
library(magrittr)
library(foreach)
library(ggplot2)
