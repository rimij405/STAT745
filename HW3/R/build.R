# R/build.R
#
# Common functions for building / rendering.

# Source the utility functions.
source(here::here("R/packages.R"))
require.deps()
attach.deps()

# Source the build functions for the dataset.
source(here::here("R/build/dataset.R"))

# Source the build functions for the report.
source(here::here("R/build/report.R"))

## ---- def-build ----

build.project <- function(
  opts_dataset = list(),
  opts_report = list()
) {
  do.call(make.dataset, args = opts_dataset)
  do.call(make.report, args = opts_report)
}
