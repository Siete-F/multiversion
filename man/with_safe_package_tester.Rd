% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_functions.R
\name{with_safe_package_tester}
\alias{with_safe_package_tester}
\title{Create a safe environment in which certain expressions can be tested}
\usage{
with_safe_package_tester(expr, also_clean_install_dir = FALSE)
}
\arguments{
\item{expr}{The expression that needs to be evaluated in this protected environment.}

\item{also_clean_install_dir}{If \code{lib.clean_install_dir()} must be run before and after the test.}
}
\description{
Will reset the .libPaths, the 'R_MV_LIBRARY_LOCATION' environment variable to their old values
and will unload 'package.a' till 'package.f' when finishing the execution. \cr \cr
}
\details{
Before execution it will set the following values:
\enumerate{
\item{.libPaths - will be set to .Library only.}
\item{\code{R_MV_LIBRARY_LOCATION} - will contain '../test_library/' or 'tests/test_library/' depending on the current directory}
}
}
