## code to prepare `Liver.txt` dataset goes here

# Reference(s):
# https://towardsdatascience.com/put-your-data-analysis-in-an-r-package-even-if-you-dont-publish-it-64f2bb8fd791
# https://github.com/hadley/babynames/blob/master/data-raw/applicants.R

# Retrieve paths to data files.
liver.filepath <- system.file(
  "extdata",
  "Liver.txt",
  package = "RIT.STAT745.HW2",
  mustWork = TRUE
)

# Prepare the variable names.
liver.variables <- c(
  # X1 through X5 are quantitative blood test results. (Unknown tests).
  "blood.1",
  "blood.2",
  "blood.3",
  "blood.4",
  "blood.5",
  # X6: No. of alcoholic beverages consumed.
  "drinks",
  # X7: Liver condition severity.
  "severity"
)

# Prepare the variable types.
liver.types <- readr::cols(
  # X1 through X5 - Quantitative blood test results.
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  readr::col_integer(),
  # X6 - No. of alcoholic beverages.
  readr::col_double(),
  # X7 - Severity group.
  readr::col_factor()
)

# Read the dataset into memory.
liver.txt <- readr::read_csv(
  # File is located in inst/extdata
  file = liver.filepath,
  # First row is NOT a header row.
  col_names = liver.variables,
  # Column types known in advance.
  col_types = liver.types
)

# Store as tibble.
liver_data <- dplyr::as_tibble(liver.txt)

# Write *.csv file in data-raw/
readr::write_csv(liver_data, "data-raw/liver_data.csv")

# Save the imported Liver.txt tibble.
usethis::use_data(liver_data, overwrite = TRUE)
