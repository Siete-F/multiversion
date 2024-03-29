% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{lib.printVerboseLibCall}
\alias{lib.printVerboseLibCall}
\title{Print example \code{lib.load} call.}
\usage{
lib.printVerboseLibCall(packNameVersion, .forceToPrint = FALSE)
}
\arguments{
\item{packNameVersion}{A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).}

\item{.forceToPrint}{For testing, I need to be able to overrule the 'interactive()' criteria for printing this example library call.}
}
\description{
Prints the library call that you can use, based on a name/version input vector.
}
