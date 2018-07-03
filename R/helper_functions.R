# ============== PATHS ==============


#' Temporary directory location.
#'
#' Indicates the default directory for initially installing a package before it is 'converted' to the final VC library structure (see: \code{convert_to_VC_library()}).
#' This folder can be cleaned up using \code{cleanTempInstallFolder()} after installing the package succeeded.
#' This is not done automatically but won't influence the installation of other packages.
#'
#' @param lib.location By default the default library path obtained with \code{R_VC_library_location()}.
#'
#' @export
#'
R_VC_temp_lib_location <- function(lib.location = R_VC_library_location()) {
    install.location <- gsub(normalizePath(paste0(lib.location, '/TEMP_install_location'), winslash = '/', mustWork = FALSE), pat = '/$', rep = '')
    dir.create(install.location, showWarnings = FALSE)
    return(install.location)
}


#' Clear the temp install folder.
#'
#' The temporary installation folder (indicated by \code{R_VC_temp_lib_location()}) is used to install the package before moving ('converting') it to the final location.
#' This function removes this temporary folder. Make sure that all installed packages that are desired to keep are converted.
#' You can run the \code{convert_to_VC_library()} once again to make sure this is the case.
#'
#' @param install.location By default the default temporary directory path obtained with `R_VC_temp_lib_location()`.
#'
#' @export
#'
clearTempInstallFolder <- function(install.location = R_VC_temp_lib_location()) {
    return(unlink(install.location, recursive = TRUE, force = TRUE))
}


#' The R_VC_library location.
#'
#' This function will look for the environment variable \code{R_VC_LIBRARY_LOCATION} indicating the R_VC_library location.
#' Alternatively you can provide a path for this session only using \code{R_VC_library_location(yourPath)}.
#'
#' @param set_session_path (optional) If no environment variable has been set to indicate the library location,
#' You can call this function and let it set the environment variable for this session only.
#'
#' @export
#'
R_VC_library_location <- function(set_session_path = NULL) {
    # If input is provided, set that value as library location.
    if (!is.null(set_session_path)) {
        Sys.setenv(R_VC_LIBRARY_LOCATION = set_session_path)
        return(set_session_path)
    }

    # Check if environment variable is present
    if (nchar(Sys.getenv('R_VC_LIBRARY_LOCATION')) == 0) {
        stop(paste('No environment variable has been set for me to find the R_VC_library location.\n',
                   'Please fill the environment variable `R_VC_LIBRARY_LOCATION` with a path to an empty\n',
                   'or already created library base folder, and restart R!!\nAlternatively provide it for\n',
                   'this session using `R_VC_library_location(YourPath)`.\n\n'))
    }
    lib.location <- Sys.getenv('R_VC_LIBRARY_LOCATION')

    # force a forward slash and remove an ending slash.
    lib.location <- gsub(gsub(lib.location, pat = '\\\\', rep = '/'), pat = '/$', rep = '')
    return(lib.location)
}


# ============== INPUT PARSERS ==============

#' Parse direct unquoted input to package name/version vector.
#'
#' Converts input like \code{library_VC(hoi = 3.4, hai = '>= 7.9.2', FIETS)} \cr
#' to a named character vector like \code{c(hoi = '3.4', hai = '>= 7.9.2', FIETS = '')} which is compatible with all code that follows. \cr
#' \cr
#' Must be called like \code{raw_input_parser(as.list(match.call()), c('named_param1', 'named_param2', 'named_param3'))}. \cr
#' It will return all (name) value pairs if values are available excluding the named parameters provided in the second argument. \cr
#'
#' @param arguments The \code{as.list(match.call())} list returned from the calling function. It creates a list of all provided arguments.
#' @param varnames_to_exclude A character vector with var names to exclude. Normally the remaining variable names after the `n`
#'
raw_input_parser = function(arguments, varnames_to_exclude) {
    arguments[1] <- NULL

    # get input characteristics
    noName  <- names(arguments) == '' || is.null(names(arguments)) & length(arguments) == 1
    varName <- names(arguments) %in% varnames_to_exclude
    isNum   <- sapply(arguments, function(x) {class(x) == 'numeric'})

    # convert input to consistant named character vector.
    names(arguments)[noName] <- arguments[noName]
    arguments[noName]  <- ''
    arguments[isNum]   <- as.character(arguments[isNum])
    arguments[varName] <- NULL
    return(unlist(arguments, use.names = TRUE))
}


#' Parse single string to named character vector.
#'
#' Parses a string shaped like: \cr
#' \code{   assertthat (>= 0.1), R6 (>= 2.1.2), Rcpp (>= 0.12.3), tibble (>= 1.2), magrittr (>= 1.5), lazyeval (>= 0.2.0), DBI (>= 0.4.1)} \cr
#' to match the normal package name/version layout like: \cr
#' \code{   assertthat          R6         Rcpp      tibble    magrittr     lazyeval          DBI} \cr
#' \code{     ">= 0.1"  ">= 2.1.2"  ">= 0.12.3"    ">= 1.2"    ">= 1.5"   ">= 0.2.0"   ">= 0.4.1"} \cr \cr
#' Used by \code{getOnlineDependencies()} to interpret the by CRAN provided dependencies,
#' used to parse the 'vc_override_dependencies.txt' files and the dependencies mentioned in the 'DESCRIPTION' files of the installed packages.
#'
#' @param deps A string of format X to convert to a named character vector Y.
#'
cleanupDependencyList <- function(deps) {
    if (is.null(deps) || length(deps) == 0 || is.na(deps)) {
        return(as.character())
    }

    deps <- gsub(trimws(deps), pat = ',\\s?$|\n|^,\\s?', rep = '') # remove newlines, starting and ending comma's
    deps <- trimws(strsplit(deps, ',')[[1]])     # split on comma's, remove start/end whitespaces
    hasVersions <- grepl('\\(.*\\)', deps)       # get version if applicable
    versions <- strRemain('.*\\s?\\(', '\\)', deps)
    versions[!hasVersions] <- ''                 # if no version, give it ''
    names(versions) <- gsub('\\s?\\(.*\\)', '', deps)


    return(versions[names(versions) != 'R'])
}


# ============== REMAINING ==============

#' Removes pattern A and pattern B from a string.
#'
#' @param patA The first regex pattern to remove from the string.
#' @param patB The second regex pattern to remove from the remaining of the first removal.
#' @param str The string that needs cleaning up.
#'
strRemain <- function(patA, patB, str) {
    # easy way to remain a middle section by defining 2 regexp's.
    gsub(patB, '', gsub(patA, '', str))
}


#' Normalize path with backslashes.
#'
#' This short-hand function normalizes the path and makes sure only forward slashes are used.
#' Other slashes are not usable in `grepl` statements directly for example, the '\\' is parsed to '\' before being used as regex.
#'
#' @param path The path which needs to be normalized. Will make `C:/PROGRA~1/R/R-33~1.1/library` into `C:/Program Files/R/R-3.3.1/library`.
#'
normPath <- function(path) {
    return(gsub('\\\\', '/', normalizePath(path)))
}


#' Append all package locations to `.libPaths()`.
#'
#' Adds the path of the package that is specified (and likely loaded before) to the `.libPaths`.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.05', ggplot = '')`).
#' The path that is appended to the `.libPaths` is constructed based on the name and version provided.
#' @param lib.location The R_VC_library location path (no default configured here).
#'
add_package_VC_libPaths <- function(packNameVersion, lib.location) {
    .libPaths(c(.Library, paste(lib.location, names(packNameVersion), packNameVersion, sep = '/')))
}


#' Loads `devtools` version 1.13.1 and it's dependencies.
#'
#' During the library call, `appendLibPaths` is TRUE, making sure that some devtools functionallity
#' (like running tests) in child R instances will still work and know where to load their libraries from.
#'
#' @export
#'
loadDevtools <- function() {
    library_VC(digest = '0.6.9', R6 = '2.2.1', testthat = '1.0.2', stringi = '1.1.5', Rcpp = '0.12.11', backports = '1.1.0', roxygen2 = '6.0.1', RMySQL = '', devtools = '1.13.1', appendLibPaths = TRUE)
}


#' Create unique list of highest package versions.
#'
#' Will uniquify the named character vector with package versions to remain the highest functions. (for now only used for printing)
#'
#' @param packNameVersion provide a packageNameVersion list like so: `printExampleLibCall(c(dplyr = '0.5.0', R6 = '', R6 = 0.5))`
#' @param return_as_df {FALSE} if the output should remain a structured dataframe, or if it should return a named character vector.
#'
unique_highest_package_versions <- function(packNameVersion, return_as_df = FALSE) {
    nameVer <- data.frame(names = names(packNameVersion), version = packNameVersion, ordering = seq_len(length(packNameVersion)))

    nameVerU <- do.call(rbind, lapply(split(nameVer, as.factor(nameVer$names)), function(x) {return(x[which.max(x$version),])}))
    nameVerU <- nameVerU[order(nameVerU$ordering),]

    if (return_as_df) {
        return(nameVerU)
    } else {
        return(setNames(nameVerU, names(nameVerU)))
    }
}


#' Print example \code{library_VC} call.
#'
#' Prints the library call that you can use based on a name/version input vector.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).
#'
printExampleLibCall <- function(packNameVersion) {
    if (!interactive()) {return()}
    if (length(packNameVersion) == 0) {return()}

    nameVer <- unique_highest_package_versions(packNameVersion, return_as_df = TRUE)

    p  = paste
    p0 = paste0
    cat('\nExample call to the library: ')
    cat(p0('library_VC( ', p(p(nameVer$names, p0("'", nameVer$version, "'"), sep = ' = '), collapse = ', '), ')\n\n'))
}


#' Detach package if it exists.
#'
#' @param packageNames A vector of package names which need to be detached. Silently ignores when the package is not loaded.
#'
#' @export
#'
detachIfExisting <- function(packageNames) {

    # if not prefixed with 'package:', add it. (not all have prefix 'package:', if it is different, you can check with search() and provide it.)
    incompleteNames <- !grepl(':', packageNames)
    packageNames[incompleteNames] <- paste0('package:', packageNames[incompleteNames])

    # check if exists in current search
    presentPackages <- search()[search() %in% packageNames]

    # detach these:
    for(iPackage in presentPackages) {
        detach(pos = which(search() %in% iPackage))
        dll <- getLoadedDLLs()[[gsub(iPackage, pattern = 'package:', rep = '')]]

        if(!is.null(dll)) {
            tryCatch(library.dynam.unload(iPackage, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
        }
    }
}
