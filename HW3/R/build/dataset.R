# R/build/dataset.R
#
# Contains make/clean functions for making
# and cleaning the dataset cache files.

# Import the utilities.
source(here::here("R/utils.R"))

#' Make the dataset.
#'
#' @param target Adds a variable to the global environment
#' with this name.
#' @param cache If FALSE, will force rebuild of dataset.
#'
#' @return Data.frame containing the target dataset.
make.dataset <- function(target = "Liver.rds", ..., cache = TRUE, clean = FALSE) {
  if (!exists(target) || !cache) {
    # Functions for creating dataset are provided by the
    # `data-raw/liver.R` script.
    if (clean) { clean.dataset(target = target, dry = FALSE) }
    source(here::here("data-raw/liver.R"))
    printf("Loading '%s'...", target)
  } else {
    printf("'%s' exists.", target)
  }
  assign(target, get.liver(name = target), envir = .GlobalEnv)
  target_df <- get(target)
  printf("Registered target dataset to global environment. Access using '%s'.", target)
  return(target_df)
}

#' Clean the dataset.
#'
#' @param target Target file to clean.
#' @param dry Dry run?
clean.dataset <- function(target = "Liver.rds", ..., dry = FALSE) {

  # Generic remove function that skips the inner function when dry.
  remove.x <- function(x, FUN = rm, ..., dry = FALSE) {
    if (is.na(x) || x == '') { print("Nothing to remove.") }
    if (!dry) {
      do.call(FUN, list(x))
    } else {
      print("Skipping removal during DRY run...")
    }
  }

  # Remove a single file.
  remove.file <- function(file) {
    path <- here::here(file)
    if (!is.na(path) && path != '' && file.exists(path)) {
      printf("Found %s (%s). Removing dataset file...", basename(file), path)
      unlink(path)
    } else {
      printf("No file '%s' exists at the specified location (%s).", basename(file), path)
    }
  }

  # Clean files specified by vector of relative file names.
  clean.files <- function(files, ..., dry = FALSE) {
    if (is.na(files) || length(files) == 0) { print("No files to remove.") }
    for (i in 1:length(files)) {
      remove.x(files[i], remove.file, dry = dry)
    }
  }

  # Remove a single named object.
  remove.var <- function(x) {
    if (exists(x)) {
      printf("Removing '%s'...", x)
      rm(list = x, envir = .GlobalEnv)
    } else {
      printf("No object '%s' in the global environment.", x)
    }
  }

  # Clean named objects in vector.
  clean.vars <- function(vars, ..., dry = FALSE) {
    if (is.na(vars) || length(vars) == 0) { print("No variables to remove.") }
    for (i in 1:length(vars)) {
      remove.x(vars[i], remove.var, dry = dry)
    }
  }

  .FILES <- c(
    "data/Liver.rds",
    "data-raw/Liver.csv"
  )

  .VARS <- c(
    target,
    "liver.filepaths",
    "liver.colnames",
    "liver.coltypes",
    "import.liver",
    "read.liver",
    "cache.liver",
    "get.liver"
  )

  # Remove the files.
  clean.files(files = .FILES, dry = dry)

  # Remove the variables.
  clean.vars(vars = .VARS, dry = dry)

  # Clean complete.
  print("Clean completed. Run `make.dataset()` to rebuild dataset.")
}
