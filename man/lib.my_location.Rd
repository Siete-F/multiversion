% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{lib.my_location}
\alias{lib.my_location}
\title{Return the `multiversion` installation directory.}
\usage{
lib.my_location()
}
\description{
Returns the location of the `multiversion` package. It is a more complicated search than expected since it will find the development folder in a few cases.
The only way to guarantee that the correct folder is found is by checking if the `INDEX` folder is present in the `multiversion` folder.
This folder is only there when it is the installed instance of multiversion.
}

