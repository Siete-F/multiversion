% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VC_library_reading.R
\name{lib.load}
\alias{lib.load}
\title{Load package from R_MV_library}
\usage{
lib.load(
  ...,
  loadPackages = NULL,
  lib_location = lib.location(),
  dry.run = FALSE,
  quietly = FALSE,
  verbose = FALSE,
  appendLibPaths = FALSE,
  pick.last = FALSE,
  also_load_from_temp_lib = FALSE,
  .packNameVersionList = c(),
  .skipDependencies = c()
)
}
\arguments{
\item{...}{All packages and their versions you would like to load e.g. \code{\link{lib.load}(DBI = '0.5', assertthat = '', R6 = '', quietly = TRUE)}. Input names like \code{quietly} will be recognized and interpreted as expected.}

\item{loadPackages}{Supports providing a named character vector of packages and their versions in the shape that is supported by all other functions in this package. e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}}

\item{lib_location}{The folder containing a structure where this package must load packages from. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.}

\item{dry.run}{Will make it perform a dry run. It will check all dependencies and if \code{appendLibPaths} it will add
their paths to \code{.libPaths} but it will not load those packages. If the paths are added this way, you should be able to just call the located packages with \code{library(...)}}

\item{quietly}{Indicates if the loading must happen silently. No messages and warnings will be shown if the value is set to true.}

\item{verbose}{Indicates if additional information must be shown that might help with debugging the decission flow of this function. More specifically, when false, it will wrap 'library' calls in \code{suppressWarnings(suppressMessages(...))} and suppress unloading attempts.}

\item{appendLibPaths}{When true, the path to every package that is loaded will be appended to \code{.libPath(...)}. That configured path is the location where \code{library()} will look for packages. For a usecase for this feature, see the description above.}

\item{pick.last}{Changes the way a decision is made. In the scenario where a dependency of \code{>} or \code{>=} is defined, multiple versions may be available to choose from. By default, the lowest compliant version is chosen. Setting this to true will choose the highest version.}

\item{also_load_from_temp_lib}{when true, it will also load packages from the temporary installation directory (created when packages are installed in the R_MV_library). Can be usefull when installing using: \code{lib.install("new package!", install_temporarily = T)}.}

\item{.packNameVersionList}{See main description. Should be left blank.}

\item{.skipDependencies}{See main description. Should be left blank.}
}
\description{
There are two ways you can provide a package or vector of packages that need to be loaded: \cr
1: just provide them directly (the \code{...} input). All not recognized named variables
   will be interpreted as package names or (if it's a named argument) as a package name=version combination. \code{\link{lib.load}(DBI = '0.5', assertthat, R6)}
2: provide the \code{loadPackages} input in the following way: \code{\link{lib.load}(loadPackages = c(DBI = '0.5', assertthat = '', R6 = ''))} \cr
\cr
If an empty string e.g. \code{dplyr = ''}, or only the package name is specified, one of two things will happen:
- if one version is available, this one is used.
- if multiple versions are available, the first or last version is loaded depending on the 'pick.last' value (FALSE by default). \cr
\cr
if >= or > is used, as in \code{dplyr = '>= 2.5'}, it will decide for the first or last compatible version, depending on the 'pick.last' parameter.
If another version is desired, please define it in the input list of packages to load, prior to the package that depends on it.
}
\details{
Dependencies are checked by recursively running this function with \code{dry.run = TRUE}.
Then the paths of the found dependencies are temporarily appended (\code{.libPaths()}) when the actual package is loaded.
This makes that dependencies are not loaded automatically, but are added to the namespace.
To access a dependency directly, load it explicitly.
Because the \code{.libPaths()} does not include the package it's location, this still needs to be done by \code{lib.load}.\cr
In other words, dependencies are remembered, but not loaded.\cr
\cr
Using \code{dry.run} will show the packages that will be used and will crash when no option is feasable (not installed or not compliant packages).
If you are trying to setup a propper \code{\link{lib.load}} call, it is always a good idea to work with dry.run's.
Once an incorrect package has been loaded, it is very likely you will have to restart your R session to unload it (Cntrl+shift+F10). Unloading packages in R often leaves traces. \cr
\cr
The .libPaths of specific package versions can be appended when using 'appendLibPaths = TRUE'.
Afterwards, the normal \code{library} call can be used to load the package since it's path is in the \code{.LibPaths} vector.
For example: \cr
\code{
lib.load(c(dplyr = '0.5.0'), dry.run = TRUE, appendLibPaths = TRUE)
library(dplyr)} \cr
\cr
How this works is that \code{dry.run} skips the loading step, and \code{appendLibPaths} adds the paths of dplyr and it's dependencies to \code{.libPaths}, which make a \code{library} call work. \cr
\cr
One reason to use \code{appendLibPaths = TRUE} is to make these packages accessible by a new 'child' R session. This is the case if \code{devtools::test()} is ran
by using \code{cntrl} + \code{shift} + \code{T} in Rstudio. When running it directly, it will use the packages it can find in the available libraries (\code{.libPath()}) and return an error if they cannot be found. \cr

The inputs .packNameVersionList [vector of named versions] and .skipDependencies [vector of names] can be
left blank in general. They are used internally and might be depricated in the future.
}
\section{Major version differences}{

By default, when chosing the right version to load, only versions are looked up within the same major version.
For example, when \code{pick.last = TRUE}, the version \code{'> 15.3.0'} is requested and the versions \code{c('15.5.0', '15.9.0', '16.0.0')} are available, the version \code{15.9.0} is chosen.
When a requested (dependency) version \code{'>= 0.5'} is provided, and only the versions \code{c('0.4.0', '1.5.0', '1.7.0')} are available,
it will throw a warning that the first available version is a major release higher, and pick \code{'1.5.0'} or  \code{'1.7.0'} depending on the \code{pick.last} value.\cr
\cr
This behaviour can be disabled by setting \bold{\code{options(mv_prefer_within_major_version = 'no')}}.
}

\section{Base packages}{

The packages within the directory returned by \code{.Library} are considered 'base packages'. Of these, only one version can exist, and these cannot be included in the multiversion library. \cr
}

\section{Problem solving}{

If you receive the error "\code{cannot unload ...}" it means that it tries to load a package, but another version is already loaded.
To unload this other (older) version, run detach(package = '...'). If it is a dependency of an other package, you will receive this error.
Try restarting your RStudio with a clean workspace (environment). If that doesn't help, the only workaround (when using this in R studio) is to close your Rstudio session (NOTE: save your unsaved process before proceding!!), rename (or remove) the folder
"\code{YourRProject/.Rproj.user/.../sources/prop}" and start Rstudio again. If it doesn't work, try "\code{/sources/per}" also. Where the \code{...} stands for a hash that is used in the current session e.g. \code{/F3B1663E/}.
After this, the packages should be unloaded and you should be able to load a new batch of packages. Most times it will suffice to clear the workspace (environment) and reload the project while saving the empty environment.
}

