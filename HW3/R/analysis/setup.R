# R/analysis/setup.R
#
# Setup the analysis.

# Source the build functions.
source(here::here("R/build.R"))

## ---- def-analysis-setup ----

#' Setup the analysis.
#'
#' @param target Named object to create that will contain data.
#' @param cache Should we skip making the dataset if it already exists?
#'
#' @return Reference to the loaded data.
setup.analysis <- function(target = "liver", cache = TRUE) {
  # Setup the analysis.
  printf("Use cached dataset? %s", cache)
  if (!cache) { clean.dataset(target = target) }
  make.dataset(target = target, cache = cache)
  df <- get(target)
  printf("Use '%s' to access underlying tbl_df.", target)
  printf("Dataset of type: '%s'.", paste(class(df), collapse = "/"))
  return(df)
}

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
  cat("\n=====================================================\n")
  cat(sprintf("LOGGING FROM [%s] %s\n", Sys.time(), Sys.Date()))
  cat(  "=====================================================\n\n")
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
