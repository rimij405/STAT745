## code to prepare `Liver.txt` dataset goes here

# Preparing an environment to shield globals.
env <- new.env(parent = emptyenv())

# Get the data location.
env$uri = here::here("data-raw/Liver.txt")

# Prepare columns
env$col_names <- c(paste0("blood.",1:5), "drinks", "severity")
env$col_types <- readr::cols(
    blood.1 = readr::col_double(),
    blood.2 = readr::col_double(),
    blood.3 = readr::col_double(),
    blood.4 = readr::col_double(),
    blood.5 = readr::col_double(),
    drinks = readr::col_double(),
    severity = readr::col_factor(levels = c("1", "2"), ordered = TRUE)
)

# Read in the data.
env$df <- readr::read_csv(
    file = env$uri,
    col_names = env$col_names,
    col_types = env$col_types,
    progress = FALSE
)

# Get column subset containing only feature columns.
env$X <- dplyr::select(env$df, blood.1:blood.5, drinks)
cat("Liver (features): ")
str(env$X)

# Get column subset containing only responses.
env$Y <- (env$df$severity == 1) # 1 for Severe.
cat("Liver (truth): ")
str(env$Y)

# Produce named classes.
env$classes <- forcats::fct_recode(forcats::as_factor(env$Y), "Not Severe" = "FALSE", "Severe" = "TRUE")
cat("Liver (labels): ")
str(env$classes)

# Save all datasets.
readr::write_rds(env$df, here::here("data/Liver.rds"))
readr::write_rds(env$X, here::here("data/Liver.features.rds"))
readr::write_rds(env$Y, here::here("data/Liver.truth.rds"))
readr::write_rds(env$classes, here::here("data/Liver.labels.rds"))

# Unset the envvars.
rm(env)
