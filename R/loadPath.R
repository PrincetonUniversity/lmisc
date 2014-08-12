# loadPath.R
# Small function to allow easier joining of file paths and file names for reading in files

loadPath <- function(path, fname) {
  pend <- substr(path, nchar(path), nchar(path))
  pout <- ifelse(pend != "/", paste(path, "/", fname, sep = ""), paste(path, fname, sep = ""))
  pout
}
