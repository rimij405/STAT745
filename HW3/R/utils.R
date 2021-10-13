# R/utils.R
#
# Common utility functions.

## ---- utils::constants ----

# Create a UTILS pointer in the global scope.
UTILS <<- vector("list", 0L)

# Default settings for the utils.R script.
.UTILS <- list(
    . = here::here("R/utils.R"),
    printf = here::here("R/utils/printf.R"),
    paths = here::here("R/utils/paths.R")
)

## ---- utils::defines ----


.get.cached.modules <- function() {
  if (exists("CACHED_MODULES", envir = .GlobalEnv)) {
    # Read from globals.
    CACHED_MODULES <- get("CACHED_MODULES", envir = .GlobalEnv)
  } else {
    # Default.
    CACHED_MODULES <- list(UTILS$.)
  }
  return(CACHED_MODULES)
}

.set.cached.modules <- function(items) {
  items <- basename(unlist(items, use.names = TRUE))
  assign("CACHED_MODULES", value = as.list(unique(items)), envir = .GlobalEnv)
}

.find.cached.modules <- function(files, current_cache = .get.cached.modules()) {
  a <- basename(unlist(current_cache, use.names = TRUE))
  b <- basename(unlist(files, use.names = TRUE))
  return(b %in% a)
}

## ---- utils::exports ----

#' Source submodule.
#'
#' @param files List of submodule scripts to source.
#' @param ... Arguments passed to the `source()` call.
#' @param verbose Echo source contents?
#'
#' @export
source.submodule <- function(files = UTILS, ..., verbose = FALSE, use_cache = TRUE, dry = FALSE) {

  # Current cache initialized as empty list.
  current_cache <- list()

  # If using cache, we'll grab it from the options.
  if (use_cache) {
    # Only get stored cache if use_cache = TRUE.
    current_cache <- .get.cached.modules()
    if (length(current_cache) > 0) {
      # Only prepare missing files.
      files <- files[!(.find.cached.modules(files, current_cache))]
    }
    current_cache <- c(files, current_cache, list())
  }

  # Update the cache.
  .set.cached.modules(current_cache)

  # Source the missing utilities.
  if (!is.na(files) && length(files) > 0) {
    for (i in 1:length(files)) {
      file <- here::here(files[i])
      if (!dry) {
        message(sprintf("Sourcing external script [%s]...", basename(file)))
        source(file = file, ..., echo = verbose)
      }
      else {
        message(sprintf("Dry run. Skipping [%s]...", basename(file)))
      }
    }
    message(sprintf("Done! Sourced %s file(s).", length(files)))
  } else {
    message("No sourced files to import!")
  }
}

# Add filepaths to the main UTILS.
UTILS <- modifyList(UTILS, .UTILS)
