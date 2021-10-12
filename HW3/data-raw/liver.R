# liver.R

# Reference(s):
# https://towardsdatascience.com/put-your-data-analysis-in-an-r-package-even-if-you-dont-publish-it-64f2bb8fd791
# https://github.com/hadley/babynames/blob/master/data-raw/applicants.R

## ---- liver::constants ----

.DEFAULT_NAME <- "Liver"

# Construct list with filepaths.
LIVER.FILEPATHS <- list(
  source = here::here("data-raw/Liver.txt"),
  csv = here::here("data-raw/Liver.csv"),
  rds = here::here("data/Liver.rds")
)

# Constructs list with dataset column names.
LIVER.COLNAMES <- c(
  paste0("blood.",1:5),
  "drinks",
  "severity"
)

# Construct list of column types.
LIVER.COLTYPES <- readr::cols(
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  readr::col_double(),
  readr::col_factor(levels = c("1", "2"), ordered = FALSE)
)

## ---- liver::imports ----

# Get the printing utilities.
source(here::here("R/utils.R"))
source.submodule(files = UTILS$printf)

## ---- liver::exports ----

# Read the source `txt` file from the
# `data-raw/` directory by default. Can
# supply a different path, if necessary.
read.liver <- function(path = LIVER.FILEPATHS$source) {
  message("Parsing dataset from local file...")
  df <- NULL
  if (identical(path, LIVER.FILEPATHS$rds)) {
    message("Reading dataset from compressed cache file...")
    df <- readr::read_rds(file = path)
  } else if (identical(path, LIVER.FILEPATHS$csv)) {
    message("Reading dataset from intermediate *.csv file...")
    df <- readr::read_csv(
      file = path,
      col_names = TRUE,
      col_types = LIVER.COLTYPES,
    )
  } else {
    message("Reading dataset from source text file...")
    df <- readr::read_csv(
      file = path,
      col_names = LIVER.COLNAMES,
      col_types = LIVER.COLTYPES,
    )
  }
  message("Done.")
  return(dplyr::as_tibble(df))
}

# Save the raw data to the proper location.
cache.liver <- function(path = LIVER.FILEPATHS$rds, data = read.liver()) {
  message("Updating data cache...")
  df <- dplyr::as_tibble(data)
  if (identical(path, LIVER.FILEPATHS$rds)) {
    # Save the liver data to the `data-raw/` directory.
    # Compression because, why not?
    readr::write_rds(df, file = path, compress = 'gz')
    message("Save successful.")
  } else if (identical(path, LIVER.FILEPATHS$csv)) {
    # Write *.csv file for human-readable intermediate
    # data format that isn't a text file.
    # append = FALSE to overwrite file.
    readr::write_csv(df, file = path, append = FALSE)
    message("Save successful.")
  } else {
    # Write *.csv file for human-readable intermediate
    # data format that isn't a text file.
    # append = FALSE to overwrite file.
    readr::write_csv(df, file = path, append = FALSE)
    message("Save successful.")
    warning("Non-normal path used. File cached as *.csv file.")
  }
}

# Import the liver data object into memory.
import.liver <- function(path = LIVER.FILEPATHS$rds) {
  message("Importing dataset...")
  if (!file.exists(path)) {
    messagef("No dataset exists at path. Attempting to import from source...")
    # If cached file doesn't exist,
    # import raw data and cache it.
    cache.liver(path, data = read.liver(LIVER.FILEPATHS$source))
  }
  df <- read.liver(path)
  message("Dataset imported.")
  return(df)
}

## ---- liver::exec ----

# tbl is easier to work with than data.frame
load.liver <- function(name = .DEFAULT_NAME, ...) {
  assign(name, import.liver(...), envir = .GlobalEnv)
  return(get(name, envir = .GlobalEnv))
}
