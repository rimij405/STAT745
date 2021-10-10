# R/build/report.R
#
# Contains make/clean functions for making
# and cleaning the report files.

# Import the utilities.
source(here::here("R/utils.R"))


# Render the report with knitr, rmarkdown
# into the output folder.
make.report <- function(inputFile = "vignettes/HW3_report.Rmd",
                        encoding = "UTF-8",
                        output_dir = "vignettes/out",
                        output_format = "all",
                        force = FALSE) {

  # Prepare filepaths.
  file_path <- here::here(inputFile)
  md5_path <- paste0(file_path, ".md5")
  output_path <- here::here(output_dir)

  # Check if the report currently exists.
  if (!file.exists(file_path) || (is.na(inputFile) || inputFile == '')) {
    # No report? Nothing to make.
    printf("No report '%s' to knit.", inputFile)
    return(NULL)
  }

  # Compare checksums.
  checksums <- function(md5a, md5b) {
    comparison <- identical(md5a, md5b)
    printf("Does [%s] match [%s]? %s", md5a, md5b, comparison)
    return(comparison)
  }

  # Calculate the md5 for a given file.
  calc.md5 <- function(file) {
    # Expects path to file.
    file_path <- here::here(file)
    md5 <- as.vector(0)
    if (file.exists(file_path)) {
      md5 <- as.vector(tools::md5sum(file_path))
    } else {
      printf("No file exists at given location '%s'.", file_path)
    }
    return(md5)
  }

  # Write md5 for file function.
  write.md5 <- function(file) {
    # Expects path to file.
    file_path <- here::here(file)
    md5_path <- paste0(file_path, ".md5")
    md5_path <- file.path(output_path, basename(md5_path))

    md5 <- calc.md5(file_path)
    msg <- sprintf("MD5 hash for '%s' to '%s'.", basename(file_path), basename(md5_path))
    # Only write the MD5, if the file exists.
    if (file.exists(file_path) && !is.na(md5)) {
      conn <- file(md5_path, blocking = FALSE)
      writeLines(md5, conn)
      close(conn)
      return(list(
        status = TRUE,
        message = paste0("Saved ", msg),
        md5 = md5
      ))
    } else {
      printf("File '%s' does not exist. No MD5 written to '%s'.", basename(file_path), basename(md5_path))
      return(list(
        status = FALSE,
        message = paste0("Failed to write ", msg),
        md5 = md5
      ))# Failed.
    }
  }

  # Read file function.
  read.md5 <- function(file) {
    # Expects path to file.
    file_path <- here::here(file)
    md5_path <- paste0(file_path, ".md5")
    md5_path <- file.path(output_path, basename(md5_path))

    # MD5 initialized to default value.
    md5 <- as.vector(0)
    if (!file.exists(md5_path) || is.na(md5)) {
      # If MD5 file doesn't exist,  calculate and write it out.
      printf("No MD5 hash found for '%s'.", basename(file_path))
    } else {
      # Else, MD5 can be read from md5_path file's first line.
      conn <- file(md5_path, open = "r", blocking = FALSE)
      lines <- readLines(conn)
      md5 <- as.vector(lines[[1]]) # MD5 on first line is checked. No other line is read.
      close(conn)
    }

    # Return whatever md5 was calculated.
    return(md5)
  }

  # Check if file has been modified.
  is.dirty <- function(file) {
    if (force == TRUE) {
      print("[FLAG] Forcing render of document.")
      return(TRUE)
    }
    # Get the current md5.
    md5_current <- calc.md5(file_path)
    # Get the previous md5.
    md5_previous <- read.md5(file_path)
    # Compare the md5 checksums.
    if (!checksums(md5_current, md5_previous)) {
      printf("Checksums do not match. File '%s' is dirty.", basename(file))
      return(TRUE)
    } else {
      printf("Checksums match. No changes found in '%s'.", basename(file))
      return(FALSE)
    }
  }

  # Knit the report.
  knit.report <- function(file) {
    if (is.na(file) || file == '') {
      print("No file to render.")
      return(NULL)
    }
    if (file.exists(file)) {
      rmarkdown::render(
        file,
        encoding = encoding,
        output_dir = output_dir,
        output_format = output_format
      )
    }
    print("Done rendering report.")
  }

  # If file is dirty, render.
  if (is.dirty(file_path)) {
    printf("Rendering report to '%s' directory...", output_path)
    knit.report(file_path)
  } else {
    print("Skipping render.")
  }

  # Update MD5 checksums.
  res <- write.md5(file_path)
  printf("%s: [%s]", res$message, res$md5)

  # Error handling.
  if (res$status) {
    # Return path of the output file.
    output_glob <- file.path(
      # ex. "*/vignettes/out/"
      here::here(output_dir),
      # ex. "<basename>.*"
      paste0(basename(tools::file_path_sans_ext(file_path)), ".*")
      # ex. "Path/To/ProjectRoot/vignettes/out/<basename>.*"
    )

    # List all files in the output.
    return(Sys.glob(output_glob))
  } else {
    return(NA)
  }

}

# Clean the report.

clean.report <- function(inputFile = "vignettes/HW3_report.Rmd",
                         output_dir = "vignettes/out") {

  remove.file <- function(file) {
    msg <- "[%s]: %s"
    if (is.na(file) || file == '' || !file.exists(file)) {
      printf("No file '%s' exists. Nothing to remove.", file)
      return(sprintf(msg, file, FALSE))
    } else {
      unlink(file)
      printf("Removed '%s' successfully.", file)
      return(sprintf(msg, file, TRUE))
    }
  }

  remove.files <- function(files) {
    if (is.na(files) || length(files) == 0) {
      print("No files to remove.")
      return(NULL)
    }
    foreach::foreach(i = 1:length(files), .combine = "c") %do% remove.file(files[i])
  }

  # Return path of the output file.
  file_path <- here::here(inputFile)
  output_glob <- file.path(
    # ex. "*/vignettes/out/"
    here::here(output_dir),
    # ex. "<basename>.*"
    paste0(basename(tools::file_path_sans_ext(file_path)), ".*")
    # ex. "Path/To/ProjectRoot/vignettes/out/<basename>.*"
  )

  # List all files in the output.
  files <- Sys.glob(output_glob)
  files <- c(files)

  # Return results.
  remove.files(files)
}
