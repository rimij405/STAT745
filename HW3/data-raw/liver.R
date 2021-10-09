# liver.R

# Reference(s):
# https://towardsdatascience.com/put-your-data-analysis-in-an-r-package-even-if-you-dont-publish-it-64f2bb8fd791
# https://github.com/hadley/babynames/blob/master/data-raw/applicants.R

## ---- get-liver-filepath ----

# Define function for getting the filepaths.
liver.filepaths = function() {
  return(list(
    source = here::here("data-raw/Liver.txt"),
    csv = here::here("data-raw/Liver.csv"),
    rds = here::here("data/Liver.rds")
  ))
}

## ---- get-liver-colnames ----

# Constructs list with dataset column names.
liver.colnames <- function() {
  return(c(
    paste0("blood.",1:5),
    "drinks",
    "severity"
  ))
}

## ---- get-liver-coltypes ----

# Construct list of column types.
liver.coltypes <- function() {
  return(readr::cols(
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_double(),
    readr::col_factor(levels = c("1", "2"), ordered = FALSE)
  ))
}

## ---- read-liver-text ----

# Read the source `txt` file from the
# `data-raw/` directory by default. Can
# supply a different path, if necessary.
read.liver <- function(path = liver.filepaths()$source) {
  print("Parsing dataset from local file...")
  df <- NULL
  if(identical(path, liver.filepaths()$rds)) {
    print("Reading dataset from compressed cache file...")
    df <- readr::read_rds(file = path)
  } else if(identical(path, liver.filepaths()$csv)) {
    print("Reading dataset from intermediate *.csv file...")
    df <- readr::read_csv(
      file = path,
      col_names = TRUE,
      col_types = liver.coltypes(),
    )
  } else {
    print("Reading dataset from source text file...")
    df <- readr::read_csv(
      file = path,
      col_names = liver.colnames(),
      col_types = liver.coltypes(),
    )
  }
  print("Done.")
  return(dplyr::as_tibble(df))
}

## ---- cache-liver-data ----

# Save the raw data to the proper location.
cache.liver <- function(path = liver.filepaths()$rds, data = read.liver()) {
  print("Updating data cache...")
  df <- dplyr::as_tibble(data)
  if(identical(path, liver.filepaths()$rds)) {
    # Save the liver data to the `data-raw/` directory.
    # Compression because, why not?
    readr::write_rds(df, file = path, compress = 'gz')
    print("Save successful.")
  } else if(identical(path, liver.filepaths()$csv)) {
    # Write *.csv file for human-readable intermediate
    # data format that isn't a text file.
    # append = FALSE to overwrite file.
    readr::write_csv(df, file = path, append = FALSE)
    print("Save successful.")
  } else {
    # Write *.csv file for human-readable intermediate
    # data format that isn't a text file.
    # append = FALSE to overwrite file.
    readr::write_csv(df, file = path, append = FALSE)
    print("Save successful.")
    warning("Non-normal path used. File cached as *.csv file.")
  }
}

## ---- import-liver-data ----

# Import the liver data object into memory.
import.liver <- function(path = liver.filepaths()$rds) {
  print("Importing dataset...")
  if(!file.exists(path)) {
    print(sprintf("No dataset exists at path. Attempting to import from source..."))
    # If cached file doesn't exist,
    # import raw data and cache it.
    cache.liver(path, data = read.liver(liver.filepaths()$source))
  }
  df <- read.liver(path)
  print("Dataset imported.")
  return(df)
}

## ---- display-liver-data ----

# tbl is easier to work with than data.frame
liver_df <- import.liver()
