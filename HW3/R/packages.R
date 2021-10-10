# R/packages.R
#
# Adds necessary packages to the source.

# Source the utility functions.
source(here::here("R/utils.R"))

## ---- install-deps ----

write.deps()
install.deps()

## ---- attach-deps ----

# Add packages to namespace.
library(magrittr)
library(foreach)
library(ggplot2)
library(forcats)
library(dplyr)
