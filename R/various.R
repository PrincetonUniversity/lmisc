#' Easier joining of file paths and file names for reading in files
#' 
#' @param path Path directory
#' @param fname File name
#' @return Complete file string
#' @details Currently not set up for Windows path variables. Will need an extra line or two. 
#' @export
full_path <- function(path, fname) {
  pend <- substr(path, nchar(path), nchar(path))
  if(pend != .Platform$file.sep) {
    pout <- paste(path, .Platform$file.sep, fname, sep = "") 
  } else { 
    pout <- paste(path, fname, sep = "")
  }
  pout
}

#' Creates root directory for project (one above project directory), to facilitate projects across computers
#' 
#' @param proj.dir Character vector specifying name of project working directory (not a full file path)
#' @return A file path with the platform-specific path structure 
#' @export
proj_root <- function(proj.dir) {
  pwd <- getwd()
  pwd_split <- strsplit(pwd, "/")[[1]]
  ind <- which(pwd_split %in% proj.dir)
  if(length(ind) > 0) {
    v <- pwd_split[1:(ind - 1)]
    root_dir <- paste(paste(v, collapse = .Platform$file.sep), .Platform$file.sep, sep = "")
  } else {
    print(paste("Current working directory is ", pwd, ", which is outside of your project", sep = ""))
    root_dir <- pwd
  }
  return(root_dir)
}






