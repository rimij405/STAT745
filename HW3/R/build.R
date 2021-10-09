# build.R
#
# Common functions for building / rendering.

## ---- build-dataset ----

# Build the dataset.
# - Adds dataset files to cache.
# - Adds the dataset to the global environment.
make.dataset <- function(dataset = "liver_df") {
  if(!exists(dataset)) {
    # Depends on functions imported from `data-raw/liver.R`
    source(here::here("data-raw/liver.R"))
  } else {
    print("Dataset already exists.")
  }
  print(liver_df)
}

# Clean the dataset.
# Remove dataset from environment.
# Remove dataset from cache files.
clean.dataset <- function(dataset = "liver_df") {
  if(exists(dataset)) {
    print("Removing dataset.")
    rm(list=dataset, envir = .GlobalEnv)
  }
  if(exists("liver.filepaths")) {
    print("Removing filepath generator.")
    rm(list="liver.filepaths", envir = .GlobalEnv)
  }
  if(exists("liver.colnames")) {
    print("Removing column name generator.")
    rm(list="liver.colnames", envir = .GlobalEnv)
  }
  if(exists("liver.coltypes")) {
    print("Removing column type generator.")
    rm(list="liver.coltypes", envir = .GlobalEnv)
  }
  if(exists("import.liver")) {
    print("Removing dataset importer.")
    rm(list="import.liver", envir = .GlobalEnv)
  }
  if(exists("read.liver")) {
    print("Removing dataset parser.")
    rm(list="read.liver", envir = .GlobalEnv)
  }
  if(exists("cache.liver")) {
    print("Removing dataset cache function.")
    rm(list="cache.liver", envir = .GlobalEnv)
  }
  if(file.exists(here::here("data-raw/Liver.csv"))){
    print("Deleting intermediate data file.")
    unlink(here::here("data-raw/Liver.csv"))
  }
  if(file.exists(here::here("data/Liver.rds"))){
    print("Deleting cached data file.")
    unlink(here::here("data/Liver.rds"))
  }
  print("Run `make.dataset()` to rebuild dataset.")
}

## ---- build-report ----

# Render the report with knitr, rmarkdown
# into the output folder.
make.report <- function(path = "vignettes/HW3_report.Rmd",
                        outputDir = "vignettes/out") {

  # Formatted print function.
  printf <- function(format, ...) {
    print(sprintf(format, ...))
  }

  # Read file function.
  read.md5 <- function(path) {
    md5 <- 0 # Value returned if no MD5 file.

    # No MD5 should be returned if original file no longer exists.
    file_path <- here::here(path)
    if(!file.exists(file_path)) {
      printf("No file exists at given path ('%s')", file_path)
      return(md5) # md5 = 0
    }

    # MD5 path should be returned for the related file.
    md5_path <- here::here(paste0(path, ".md5"))
    if(!file.exists(md5_path)) {
      printf("No cached MD5 file exists. Returning ")
    }

    if(file.exists(file_path)) {
      conn <- file(md5_path, open = "r", blocking = FALSE, description = "MD5 Cache file")
      lines <- readLines(conn)
      md5 <- lines[[1]] # MD5 on first line is checked. No other line is read.
      close(conn)
      printf("Found MD5 for ")
    } else {
      printf("No MD5 file found  ('%s').", path)
    }
    return(md5)
  }

  # Calculate the md5 for a given file.
  calc.md5 <- function(path) {
    md5 <- 0
    if(file.exists(path)) {
      md5 <- tools::md5sum(path)[[1]]
      printf()
    } else {
      printf("No file exists at given path ('%s').", path)
    }
    return(md5)
  }

  # Check if report exists.
  file_path <- here::here(path)
  if(file.exists(file_path)) {

    # Calculate the current md5 hash.
    md5_current <- tools::md5sum(file_path)[[1]] # TODO: Calculate the MD5 hash.

    # Check if md5 hash report of file exists.
    md5_previous <- read.md5(path = file_path)

    # Read the md5 file if it exists.
    if(file.exists(md5_path)) {
      printf("Cached MD5 file found at '%s'.", md5_path)
      md5_previous <- tools::md5sum(md5_path)[[1]]
    }



  }


}

# Clean the report.

