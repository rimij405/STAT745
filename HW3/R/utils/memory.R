# R/utils/memory.R
#
# Memory management functions.


#' Given a target file, generate file path, md5 file path,
#' basename, and file extension and return as list.
#'
#' @param target Relative or absolute path to target file.
get.target.info <- function(target) {
  file_path <- here::here(target)
  md5_path <- paste0(file_path, ".md5")
  return(list(
    file = file_path,
    md5 = md5_path,
    basename = basename(target),
    ext = tools::file_ext(target)
  ))
}
