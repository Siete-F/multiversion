# =================================================================
#     RVClibrary, multi-version package library management tool
#     Copyright (C) 2019 S.C. Frouws, The Hague, The Netherlands
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License ('COPYING.LESSER') for more details.
# =================================================================


# ============== PATHS ==============

#' Return the RVClibrary installation directory.
#'
#' Returns the location of the RVClibrary package. It is a more complicated search than expected since it will find the development folder in a few cases.
#' The only way to guarantee that the correct folder is found is by checking if the `INDEX` folder is present in the RVClibrary folder.
#' This folder is only there when it is the installed instance of RVClibrary.
#'
RVClibrary_package_install_location <- function() {
    RVClib_package_location <- find.package(package = 'RVClibrary', lib.loc = .libPaths(), quiet = T, verbose = F)

    if (length(RVClib_package_location) == 0) {
        RVClib_package_location <- find.package(package = 'RVClibrary', lib.loc = NULL, quiet = T, verbose = F)
    }

    if (length(RVClib_package_location) == 0 || !file.exists(file.path(RVClib_package_location, 'INDEX'))) {
        stop(paste0('I need to be able to find the `RVClibrary` package location. To do that, it must be present in the searchpath `.libPaths()`.',
                    'Please make sure the library search paths include the location of that package.'))
    }
    return(RVClib_package_location)
}


#' List all untracked library folders
#'
#' List all untracked direcories (libraries) within the R_VC_library. The returned untracked directories are cleaned up
#' and printed so that only the unique combinations of each library and it's version is shown once.
#'
#' @param lib.location By default the default library path obtained with \code{R_VC_library_location()}.
#'
#' @export
#'
list_untracked_VC_libs <- function(lib.location = R_VC_library_location()) {
    if (!file.exists(lib.location)) {
        stop(sprintf('Please specify an existing directory for the `lib.location`. Provided was "%s".', lib.location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib.location), intern = T)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib.location))
    }

    temp_installed_libs <- dir(R_VC_temp_lib_location(lib.location))

    listed_files <- system(sprintf('git -C "%s" ls-files --others --exclude-standard', lib.location), intern = T)
    listed_files <- listed_files[grepl('DESCRIPTION', listed_files)]
    # Now remove from e.g. "htmlTable/1.13.1/htmlTable/DESCRIPTION" the "/htmlTable/DESCRIPTION" part to remain "htmlTable/1.13.1"
    added_dirs   <- gsub('/[^/]*/DESCRIPTION$', '', listed_files)
    cat(sprintf('The following libraries are installed but untracked:\n%s\n', paste0(added_dirs, collapse = '\n')))

    if (length(added_dirs) == 0) {
        added_dirs <- ''
    }
    if (length(temp_installed_libs) == 0) {
        cat('\nAnd the following libraries are temporarily installed:\n\n')
        return(invisible())
    }
    cat(sprintf('\nAnd the following libraries are temporarily installed:\n%s\n%s\n', sprintf('%20s - %s', 'libs:', 'already converted:'),
                paste0(sprintf('%20s - %s', temp_installed_libs,
                               as.character(apply(matrix(sapply(temp_installed_libs, grepl, added_dirs), ncol = length(temp_installed_libs)), 2, any))), collapse = '\n')))
}


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


#' Clean R_VC_library, revert to state of last commit.
#'
#' Clean up all untracked (not committed) installed libraries in the R_VC_library environment.
#' Will additionally also clean up the TEMP_install_location directory (this is an 'ignored' directory).
#'
#' Since it involves a quite invasive operation, it asks for permission when being called in an interactive session.
#'
#' @param lib.location By default the default library path obtained with \code{R_VC_library_location()}.
#'
#' @export
#'
clean_library <- function(lib.location = R_VC_library_location(), clean_temp_lib = TRUE) {
    if (!file.exists(lib.location)) {
        stop(sprintf('Please specify an existing directory for the `lib.location`. Provided was "%s".', lib.location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib.location), intern = T)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib.location))
    }

    list_untracked_VC_libs(lib.location = lib.location)

    if (interactive()){
        choice <- menu(c('yes', 'no'), title = '\nAre you sure you want to undo all changes made to the R_VC_library and go back to the last commit?')

        if (choice != 1) {return(invisible())}
    }

    # You can undo changes to tracked files with: git reset HEAD --hard
    # You can remove untracked files with: git clean -f
    # You can remove untracked files and directories with: git clean -fd              <- this one is applied below.
    # You can remove ignored and untracked files and directories git clean -fdx.
    system(sprintf('git -C "%s" clean -fd', lib.location), intern = T)

    if (clean_temp_lib) {
        # Additionally clear the temp_install directory:
        # The temp lib location is based on the main lib location. The folder 'TEMP_install_location' is appended to it.
        clean_temp_library(R_VC_temp_lib_location(lib.location))
    }
}


#' Clear the temp install folder.
#'
#' The temporary installation folder (indicated by \code{R_VC_temp_lib_location()}) is used to install the package before moving ('converting') it to the final location.
#' This function removes this temporary folder. Make sure that all installed packages that are desired to keep are converted.
#' You can run the \code{convert_to_VC_library()} once again to make sure this is the case.
#'
#' @param install.location By default the default temporary directory path obtained with \code{R_VC_temp_lib_location()}.
#'
#' @export
#'
clean_temp_library <- function(temp_install.location = R_VC_temp_lib_location()) {
    return(unlink(temp_install.location, recursive = TRUE, force = TRUE))
}


#' Clean package download catch
#'
#' Clean the catch folder 'downloaded_packages' which lives in the temporary R session folder `tempdir()`.
#'
#' @export
#'
clean_download_catch <- function() {
    unlink(file.path(tempdir(), 'downloaded_packages'), recursive = TRUE, force = TRUE)
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
    # if null, not 1 name is given, so all elements need a name.
    if (is.null(names(arguments))) {
        noName  <- rep(TRUE, length(arguments))
    } else {
        noName  <- names(arguments) == ''
    }

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
#' @export
#'
parse_dependency_string <- function(deps) {
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


# #' Remove empty folders recursively
# #'
# #' When maintaining a git repository with an RVClibrary, and incorrectly packages are removed by the user using git,
# #' The folders will remain behind, empty. This influences the loading behaviour and causes errors like 'No DESCRIPTION file could be found'.
# #' This function can remove all empty folders to X levels deep.
# #'
# #' @param parent_dir A directory path where to start to remove all empty folders recursively.
# #' @param depth (default = 2, proper for RVClib) The depth that needs to be searched. 0 is only provided directory is searched, 1 is 1 subdirectory is searched to.
# #'
# #' @export
# #'
# unlink_empty_dirs <- function(parent_dir, depth = 2) {
#     # dir(gsub('[/\\\\]?$', '/', parent_dir))
# }


#' Remove `>` or `>=` from version string.
#'
#' @param packageVersion A version indication you would like to remove `>` and `>=` from.
#'
bareVersion <- function(packageVersion) {
    gsub('>?=?\\s?', '', packageVersion)
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
    return(gsub('\\\\', '/', normalizePath(path, mustWork = F)))
}


#' Append all package locations to `.libPaths()`.
#'
#' Adds the path of the package that is specified (and likely loaded before) to the `.libPaths`.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.05', ggplot = '')`).
#' The path that is appended to the `.libPaths` is constructed based on the name and version provided.
#' @param lib.location The R_VC_library location path (no default configured here).
#'
add_package_VC_libPaths <- function(packNameVersion, lib.location, additional_lib_paths = c()) {
    .libPaths(c(.Library, paste(lib.location, names(packNameVersion), packNameVersion, sep = '/'), additional_lib_paths))
}

#' Exclude not relevant search paths.
#'
#' Excludes all .libPaths other then those needed for library_VC().
#'
#' @param lib.location The folder which contains the R_VC_library structure. All directories in .libPaths containing this path will be kept.
#' By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} to find this directory.
#' @param dry.run If TRUE, will not change the paths but will print the paths that would remain after cleaning up the .libPaths() list.
#'
#' @export
#'
remove_undesired_libPaths <- function(lib.location = R_VC_library_location(), dry.run = FALSE) {
    correct_paths <- grepl(normPath(lib.location), normPath(.libPaths())) | normPath(.libPaths()) == normPath(.Library)
    if (dry.run) {
        cat(sprintf('The following paths will be excluded:\n - "%s"\n\n', paste(collapse = '"\n - "', normPath(.libPaths()[!correct_paths]))))
    } else {
        .libPaths(.libPaths()[correct_paths])
    }
}


#' Installs the incredible `devtools` package.
#'
#' Will install devtools and it's dependencies into the library provided by `R_VC_library_location()`.
#' If the
#'
#' @param lib.location The library (can be an empty folder) to install Devtools in.
#' @param force_install FALSE by default, if the package `devTools` is already installed, it will return silently.
#'
#' @export
#'
installDevtools <- function(lib.location = R_VC_library_location(), force_install = FALSE) {
    if (!force_install && length(availablePackageVersions('devtools')) > 0) {
        cat('The package devtools seems to be already installed. Please set `force_install` to true if you would like to overwrite or update devtools.')
        return()
    }
    if (length(libs <- dir(lib_dir <- R_VC_temp_lib_location(lib.location))) > 0) {
        list_untracked_VC_libs()
        stop(sprintf('These libraries are still present in the install directory "%s":\n%s.\nPlease run `clean_temp_library(yourLib)` to clean up if possible and run me again.', lib_dir, paste0(collapse = ', ', "'", libs, "'")))
    }
    install.packages_VC('devtools', lib.location = lib.location, overwrite_this_package = TRUE)
    clean_temp_library(lib.location)
}


#' Loads `devtools` version 1.13.1 and it's dependencies.
#'
#' During the library call, `appendLibPaths` is TRUE, making sure that some devtools functionallity
#' (like running tests) in child R instances will still work and know where to load their libraries from.
#'
#' @param lib.location The (version controlled) library to load devtools from.
#' Use installDevtools to install devtools, if you have not done so already.
#'
#' @export
#'
loadDevtools <- function(lib.location = R_VC_library_location()) {

    library_VC(devtools = '>= 1.13.1', testthat, Rcpp, roxygen2, stringi, digest, pick.last = T, appendLibPaths = TRUE, quietly = T)
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
#' @export
#'
printExampleLibCall <- function(packNameVersion) {
    if (!interactive()) {return(invisible())}
    if (length(packNameVersion) == 0) {return(invisible())}

    nameVer <- unique_highest_package_versions(packNameVersion, return_as_df = TRUE)

    p  = paste
    p0 = paste0
    cat('\nVerbose example call to the library (use `quetly = T` to supress this message): ')
    cat(p0('\nlibrary_VC( ', p(p(nameVer$names, p0("'", nameVer$version, "'"), sep = ' = '), collapse = ', '), ')\n\n'))
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


#' Throw error because this package is already loaded and not compatible with the requested version.
#'
#' @param requested_package_name A single package name of the package that was desired to be loaded.
#' @param requested_version The version definition (like: ">= 3.2.1") of the package that was tried to be loaded.
#' @param already_loaded_version The version of the package that is already loaded, which causes this error to be fired.
#'
error_packageAlreadyLoaded <- function(requested_package_name, requested_version, already_loaded_version) {
    stop(sprintf(paste('An already loaded package "%s" (version: %s) did not comply with the required version here (%s).',
                       '\nWe will not try to detach since that could cause unexpected behaviour.',
                       '\nPlease detach it manually (e.g. `detachAll(packageList = \'%s\')`, where %s are depending on it) and',
                       'don\'t load it explicitly before this package is loaded.\n\n'),
                 requested_package_name, already_loaded_version, requested_version, requested_package_name,
                 paste(collapse = ', ', paste0('\'', getNamespaceUsers(requested_package_name), '\''))))
}
