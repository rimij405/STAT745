# R/analysis/setup.R
#
# Setup the analysis.

## ---- analysis::setup::constants ----

.DEFAULT_TARGET = "Liver"

## ---- analysis::setup::imports ----

# Source the build functions.
source(here::here("R/utils.R"))
source.submodule(file = UTILS$printf)
source(here::here("R/build.R"))

## ---- analysis::setup::exports ----

#' Setup the analysis.
#'
#' @param target Named object to create that will contain data.
#' @param cache Should we skip making the dataset if it already exists?
#'
#' @return Reference to the loaded data.
setup.analysis <- function(target = .DEFAULT_TARGET, cache = TRUE) {
  # Setup the analysis.
  messagef("Use cached dataset? %s", cache)
  if (!cache) { clean.dataset(target = target) }
  make.dataset(target = target, cache = cache)
  df <- get(target)
  messagef("Use '%s' to access underlying tbl_df.", target)
  messagef("Dataset of type: '%s'.", paste(class(df), collapse = "/"))
  return(df)
}

## ---- analysis::setup::helpers ----

setup.log <- function(file = "HW3_analysis.log", ..., output_dir = "log") {
  # Create directory.
  output_path <- here::here(output_dir)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Get the absolute path.
  output_file <- file.path(output_path, file)
  return(output_file)
}

start.log <- function() {
  println.section(sprintf("LOGGING FROM [%s] %s\n", Sys.time(), Sys.Date()))
}

write.log <- function(message) {
  cat(message, sep = "\n")
}

end.log <- function() {
  cat("\nEND OF LOGGING...\n\n")
}

clean.log <- function(file = "HW3_analysis.log", ..., output_dir = "log") {
  log_path <- setup.log(file = file, output_dir = output_dir)
  unlink(log_path)
}
