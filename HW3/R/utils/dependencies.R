# R/utils/dependencies.R
#
# Manage dependencies using renv.

# Source the printf utility.
source(here::here("R/utils/printf.R"))

#' Write dependencies out from renv.
write.deps <- function(file = "requirements.txt") {
  file_path <- here::here(file)
  deps <- renv::dependencies()[,"Package"]
  deps <- as.vector(deps)
  deps <- unique(deps)
  deps <- sort(deps, decreasing = FALSE)
  if (length(deps) > 0) {
    printf("Writing project dependencies to '%s': ", file_path)
    conn <- file(file_path, blocking = TRUE)
    on.exit(close(conn))
    writeLines(deps, conn)
  }
  return(deps)
}

#' Read dependencies from a requirements.txt file.
read.deps <- function(file = "requirements.txt") {
  file_path <- here::here(file)
  deps <- NULL

  # Load the dependencies if requirements.txt exists.
  if (file.exists(file_path)) {
    conn <- file(file_path, open = "r", blocking = FALSE)
    on.exit(close(conn))
    deps <- as.vector(readLines(conn, skipNul = TRUE))
  }

  # Parse dependencies.
  pkgs <- deps[!(deps %in% installed.packages()[, "Package"])]
  return(pkgs)
}

#' Install required packages.
install.deps <- function(file = "requirements.txt") {
  if (renv::status()$synchronized == FALSE) {
    print("Installing dependencies...")
    pkgs <- read.deps(file)
    if(length(pkgs) > 0) {
      print("Installing packages...")
      install.packages(pkgs, dep=TRUE)
    }
    renv::restore()
  }
}
