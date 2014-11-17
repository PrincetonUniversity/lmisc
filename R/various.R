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

#' Create root directory for project
#' @description Create root directory for project (one above project directory), to facilitate projects across 
#' computers
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

#' Names and returns object inside nested apply functions 
#' @description This little function assigns a name to an object created by an inner apply function and returns 
#' it
#' @param ret Object
#' @param nm Name to assign to object
#' @return Named object out of loop
#' @details The entire purpose of this function is simply to save a line when writing nested apply functions 
#' so that names can be assigned to object and returned without taking up needless space
#' @examples
#' ab <- list("a" = c(1:10), "b" = c(11:20))
#' d <- sample(-5:5, size = 10, replace = TRUE)
#' lapply(ab$a, function(x) {
#'   o <- lapply(d, function(j) {
#'     x * ab$b * d
#'   })  
#'   named_out(o, paste("a", x, "b", gsub("-", "neg", d), sep = "")) 
#' })
#' @export
named_out <- function(ret, nm) {
  names(ret) <- nm
  return(ret)
}
