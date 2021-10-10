# analysis.R
#
# Perform EDA on the Liver dataset.

# Source the utility functions.
source(here::here("R/build.R"))

## ---- def-setup ----

setup.analysis <- function(dataset = "liver", cache = TRUE) {
  # Setup the analysis.
  printf("Use cached dataset? %s", cache)
  if (!cache) { clean.dataset(dataset = dataset) }
  make.dataset(dataset = dataset, cache = cache)
  df <- get(dataset)
  printf("Use '%s' to access underlying tbl_df.", dataset)
  printf("Dataset of type: '%s'.", class.format(df))
  return(df)
}

## ---- def-makeresponse ----

make.response <- function(.data) {
  printf("Recoding the response in the dataset.")
  df <- .data
  df <- df %>%
    dplyr::mutate(
      severity = forcats::fct_recode(
        severity,
        "Severe" = "1",
        "Not Severe" = "2"
      )
    )
  return(df)
}

## ---- def-makeshape ----

make.shape <- function(.data) {
  # Get the dimensions.
  d <- ncol(.data)
  r <- nrow(.data)
  shape <- list(
    n_samples = r,
    n_features = d
  )
  return(shape)
}

## ---- def-summarize ----

summarize.features <- function(.data, ..., d = ncol(.data)) {
  print("Summarizing dataset features.")
  s <- summary(.data[,-d])
  return(s)
}

summarize.response <- function(.data, ..., d = ncol(.data)) {
  print("Summarizing dataset response.")
  s <- summary(.data[,d])
  return(s)
}

corr.features <- function(.data, ..., d = ncol(.data)) {
  data.corr <- cor(.data[,-d])
  p <- corrplot::corrplot(data.corr)
  return(list(
    corr = data.corr,
    plot = p
  ))
}

## ---- exec-analysis ----

# Prepare for the EDA.
setup.analysis(dataset = "liver", cache = FALSE)
liver <- liver %>% make.response()
liver.shape <- liver %>% make.shape()

# Describe the features.
liver.features <- liver %>% summarize.features()
print(liver.features)

# Describe the response.
liver.response <- liver %>% summarize.response()
print(liver.response)

# Plot the features.
output_file = "vignettes/out/HW3_corrplot.png"
png(output_file)
res <- liver %>% corr.features()
print(res$corr)
dev.off()

