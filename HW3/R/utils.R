# R/utils.R
#
# Common utility functions.

.UTILS <- list(
    printf = here::here("R/utils/printf.R"),
    paths = here::here("R/utils/paths.R"),
    memory = here::here("R/utils/memory.R"),
    dependencies = here::here("R/utils/dependencies.R")
)

source.utils <- function(files = names(.UTILS), ..., force = FALSE, verbose = FALSE) {

  # Calculate the current md5.
  current_utils <- as.vector(files)

  # Load the previous md5.
  previous_utils <- as.vector(0)

  # If cache exists, grab it.
  if (exists("utils_cached", envir = .GlobalEnv)) {
    previous_utils <- get("utils_cached", envir = .GlobalEnv)
  }

  # Determine if dirty.
  is_dirty <- (length(previous_utils) == 0
               || !all(current_utils %in% previous_utils))

  # Update the cached value.
  cached_utils <- unique(c(current_utils, previous_utils))
  cached_utils <- cached_utils[cached_utils != 0]
  assign("utils_cached", cached_utils, envir = .GlobalEnv)

  # Update the skip flag.
  # - Always skip if dirty == TRUE
  # - Always skip if force == TRUE
  skip <- !is_dirty && !force

  if (!skip) {
    if (is.na(files) || length(files) == 0) {
      print("No utility functions to load.")
    } else {
      if (verbose) { print(sprintf("Loading %s utilit(y/ies)...", length(files))) }
      for (i in 1:length(files)) {
        file <- files[i]
        path <- get(file, .UTILS)
        if (verbose) { print(sprintf("Loading utility: '%s'", file)) }
        source(path)
      }
    }
  } else if (verbose) {
    print("Utilities already loaded.")
  }

  return(NULL)
}

# Source the utilities.
source.utils(force = FALSE, verbose = FALSE)
