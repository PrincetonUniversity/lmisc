#' Faster version of readLines
#' @param fname File name to read
#' @return Vector of strings from input file
#' @author mlt
#' @references http://www.r-bloggers.com/faster-files-in-r/
#' @export
read_lines2 <- function(fname) {
  s <- file.info(fname)$size 
  buf <- readChar(fname, s, useBytes = TRUE)
  strsplit(buf,"\n", fixed = TRUE, useBytes = TRUE)[[1]]
}
