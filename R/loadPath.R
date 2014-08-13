#' Small function to allow easier joining of file paths and file names for reading in files
#' @param path Path directory
#' @param fname File name
#' @return Complete file string
#' @details Currently not set up for Windows path variables. Will need an extra line or two. 

loadPath <- function(path, fname) {
  pend <- substr(path, nchar(path), nchar(path))
  pout <- ifelse(pend != "/", paste(path, "/", fname, sep = ""), paste(path, fname, sep = ""))
  pout
}
