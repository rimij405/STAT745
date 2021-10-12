# R/analysis/eda.R
#
# Handle EDA of the liver dataset.

## ---- analysis::eda::constants ----

.OUTPUT <- list(
  corrplot = list(
    . = "HW3_corrplot.png",
    output_dir = "figures",
    device = png,
    device_opts = list(
      units = "in",
      width = 8,
      height = 8,
      res = 600
    ),
    show = FALSE
  )
)

## ---- analysis::eda::exports ----

#' Recode the response factors.
#'
#' @param .data Dataset.
#'
#' @return Modified dataset.
make.response <- function(.data) {
  # print(.data$severity)
  df <- .data %>%
    dplyr::mutate(
      severity = forcats::fct_recode(severity, "Not Severe" = "2", "Severe" = "1")
    )
  df <- df %>%
    dplyr::mutate(
      severity = forcats::fct_relevel(severity, "Not Severe", "Severe")
    )
  # print(df$severity)
  # print(df %>% dplyr::count(severity))
  return(df)
}

#'Make the shape info object.
#'
#'@param .data Dataset.
#'
#'@return Number of samples and number of features in a list.
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

summarize.features <- function(.data, ..., d = ncol(.data)) {
  s <- summary(.data[,-d])
  return(s)
}

summarize.response <- function(.data, ..., d = ncol(.data)) {
  s <- summary(.data[,d])
  return(s)
}

corr.features <- function(.data, ..., d = ncol(.data)) {
  return(cor(.data[,-d]))
}

corr.plot <- function(.data,
                      output_name = .OUTPUT$corrplot$.,
                      ...,
                      d = ncol(.data),
                      export = .OUTPUT$corrplot$show,
                      output_dir = .OUTPUT$corrplot$output_dir,
                      device = .OUTPUT$corrplot$device,
                      device_opts = .OUTPUT$corrplot$device_opts) {

  # Default output.
  output_file <- stdout()

  # Open the device.
  if (export) {
    # Create directory. .../figures/
    file_path <- here::here(output_dir)
    if (!dir.exists(file_path)) {
      dir.create(file_path, recursive = TRUE)
    }

    # Get file name. .../figures/HW3_corrplot.png
    output_file <- file.path(output_dir, output_name)

    do.call(device, c(
      output_file,
      device_opts
    ))
    on.exit(dev.off())
  }

  # Get the correlation data
  data.corr <- .data %>% corr.features(d = d)

  # Plot the correlation table.
  p <- corrplot::corrplot(data.corr, ...)
  return(p)
}
