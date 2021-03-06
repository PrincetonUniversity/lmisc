% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/various.R
\name{proj_root}
\alias{proj_root}
\title{Create root directory for project}
\usage{
proj_root(proj.dir)
}
\arguments{
\item{proj.dir}{Character vector specifying name of project working directory (not a full file path)}
}
\value{
A file path with the platform-specific path structure
}
\description{
Create root directory for project (one above project directory), to facilitate projects across 
computers
}

