# =================================================================
#     multiversion, multi-version package library management tool
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
# Lesser General Public License for more details.
# =================================================================


# ============= ALIASES =============
# The following functions are deprecated:
# library_VC, install.packages_VC_tarball, install.packages_VC, dependencies, installed.packages_VC, execute_with_packages


# ============== PATHS ==============

# #' Return the `multiversion` installation directory.
# #'
# #' Returns the location of the `multiversion` package. It is a more complicated search than expected since it will find the development folder in a few cases.
# #' The only way to guarantee that the correct folder is found is by checking if the `INDEX` folder is present in the `multiversion` folder.
# #' This folder is only there when it is the installed instance of multiversion.
# #'
# lib.my_location <- function() {
#
#     MV_package_location <- find.package(package = 'multiversion', lib.loc = .libPaths(), quiet = TRUE, verbose = FALSE)
#
#     if (length(MV_package_location) == 0) {
#         MV_package_location <- find.package(package = 'multiversion', lib.loc = NULL, quiet = TRUE, verbose = FALSE)
#     }
#
#     if (length(MV_package_location) == 0 || !file.exists(file.path(MV_package_location, 'DESCRIPTION'))) {
#         stop(paste0('I need to be able to find the `multiversion` package location. To do that, it must be present in the searchpath `.libPaths()`.',
#                     'Please make sure the library search paths include the location of this package.'))
#     }
#     return(MV_package_location)
# }


#' List all un-tracked library folders
#'
#' List all un-tracked directories (libraries) within the multiversion library. The returned un-tracked directories are cleaned up
#' and printed so that only the unique combinations of each library and it's version is shown once.
#'
#' @param lib_location By default the default library path obtained with \code{lib.location()}.
#'
#' @return No return value, is called for the printed feedback. Will show which packages inside the library are not yet tracked by git (when that is desired).
#'   It is recommended to track packages with git so that
#'
#' @export
#'
lib.git_show_untracked <- function(lib_location = lib.location()) {
    if (!file.exists(lib_location)) {
        stop(sprintf('Please specify an existing directory for the `lib_location`. Provided was "%s".', lib_location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib_location), intern = TRUE)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib_location))
    }

    temp_installed_libs <- dir(lib.location_install_dir(lib_location))

    listed_files <- system(sprintf('git -C "%s" ls-files --others --exclude-standard', lib_location), intern = TRUE)
    listed_files <- listed_files[grepl('DESCRIPTION', listed_files)]
    # Now remove from e.g. "htmlTable/1.13.1/htmlTable/DESCRIPTION" the "/htmlTable/DESCRIPTION" part to remain "htmlTable/1.13.1"
    added_dirs   <- gsub('/[^/]*/DESCRIPTION$', '', listed_files)
    message(sprintf('The following libraries are installed but untracked:\n%s', paste0(added_dirs, collapse = '\n')))

    if (length(added_dirs) == 0) {
        added_dirs <- ''
    }
    if (length(temp_installed_libs) == 0) {
        message('\nAnd the following libraries are temporarily installed:\n')
        return(invisible())
    }
    message(sprintf('\nAnd the following libraries are temporarily installed:\n%s\n%s', sprintf('%20s - %s', 'libs:', 'already converted:'),
                    paste0(sprintf('%20s - %s', temp_installed_libs,
                                   as.character(apply(matrix(sapply(temp_installed_libs, grepl, added_dirs), ncol = length(temp_installed_libs)), 2, any))), collapse = '\n')))
}


#' Temporary directory location.
#'
#' Indicates the default directory for initially installing a package before it is 'converted' to the final multiversion library structure (see: \code{lib.convert()}).
#' This folder can be cleaned up using \code{cleanTempInstallFolder()} after installing the package succeeded.
#' This is not done automatically but won't influence the installation of other packages.
#'
#' @param lib_location By default the default library path obtained with \code{lib.location()}.
#' @param do_create When it doesn't exist yet, create the folder.
#'
#' @return The temporary folder location where packages are installed before they
#'   are moved to their final location in the multiversion library. When \code{do_create == TRUE},
#'   the folder will be created when it does not yet exist.
#'
#' @export
#'
lib.location_install_dir <- function(lib_location = lib.location(), do_create = TRUE) {
    install.location <- gsub(normalizePath(paste0(lib_location, '/TEMP_install_location'), winslash = '/', mustWork = FALSE), pattern = '/$', replacement = '')
    if (do_create) {
        dir.create(install.location, showWarnings = FALSE)
    }
    return(install.location)
}


#' Clean multiversion library, revert to state of last commit.
#'
#' Clean up all un-tracked (not committed) installed libraries in the multiversion library.
#' Will additionally also clean up the TEMP_install_location directory (this is an 'ignored' directory).
#'
#' Since it involves a quite invasive operation, it asks for permission when being called in an interactive session.
#'
#' @param lib_location By default the library path returned by \code{lib.location()} is used. See Note.
#' @param clean_temp_lib If true, will also run \code{lib.clean_install_dir()}.
#'
#' @note It will build the most likely installation directory based on the
#' \code{lib_location} you provide. See \code{\link{lib.location_install_dir}}. Which is \code{<your lib>/TEMP_install_location}.
#'
#' @return Clean up all un-tracked (not committed) installed libraries in the multiversion library.
#'   Will additionally also clean up the TEMP_install_location directory (this is an 'ignored' directory).
#'   Will ask for permission when being called in an interactive session.
#'
#' @importFrom utils menu
#'
#' @export
#'
lib.clean <- function(lib_location = lib.location(), clean_temp_lib = TRUE) {
    if (!file.exists(lib_location)) {
        stop(sprintf('Please specify an existing directory for the `lib_location`. Provided was "%s".', lib_location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib_location), intern = TRUE)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib_location))
    }

    lib.git_show_untracked(lib_location = lib_location)

    if (interactive()){
        choice <- utils::menu(c('yes', 'no'), title = '\nAre you sure you want to undo all changes made to the \'multiversion library\' and go back to the last commit?')

        if (choice != 1) return(invisible())
    }

    # You can undo changes to tracked files with: git reset HEAD --hard
    # You can remove un-tracked files with: git clean -f
    # You can remove un-tracked files and directories with: git clean -fd              <- this one is applied below.
    # You can remove ignored and un-tracked files and directories git clean -fdx.
    system(sprintf('git -C "%s" clean -fd', lib_location), intern = TRUE)

    if (clean_temp_lib) {
        # Additionally clear the temp_install directory:
        # The temp lib location is based on the main lib location. The folder 'TEMP_install_location' is appended to it.
        lib.clean_install_dir(lib_location = lib_location)
    }
}


#' Clear the temp install folder.
#'
#' The temporary installation folder (indicated by \code{lib.location_install_dir()}) is used to install the package before moving ('converting') it to the final location.
#' This function removes this temporary folder. Make sure that all installed packages that are desired to keep are converted.
#' You can run the \code{\link{lib.convert}()} once again to make sure this is the case.
#'
#' @param temp_install_location The folder that is emptied by this function.
#' @param lib_location By default the library path returned by \code{lib.location()} is used.
#'   It is only used to build the temp_install.location when that argument is not provided.
#'
#' @return No return value, it is called for it's side-effect of removing the
#'   temporary installation folder (located in \code{<multiversion_lib>/TEMP_install_location}).
#'   This must be called after every installation.
#'
#' @export
#'
lib.clean_install_dir <- function(lib_location = lib.location(),
                                  temp_install_location = lib.location_install_dir(lib_location)) {
    return(unlink(temp_install_location, recursive = TRUE, force = TRUE))
}


#' Clean package download catch
#'
#' Clean the catch folder 'downloaded_packages' which lives in the temporary R session folder `tempdir()`.
#'
#' @return No return value, is called for it's side-effect of removing the \code{file.path(tempdir(), 'downloaded_packages')} folder.
#' @export
#'
clean_download_catch <- function() {
    unlink(file.path(tempdir(), 'downloaded_packages'), recursive = TRUE, force = TRUE)
}


#' The R_MV_library location.
#'
#' This function will look for the environment variable \code{R_MV_LIBRARY_LOCATION} indicating the R_MV_library location.
#' Alternatively you can provide a path for this session only, using \code{\link{lib.location}(yourPath)}.
#' This will set the environment variable for this session.
#' (You might want to consider to add this to your \code{.Rprofile} file, see \code{?Startup})
#'
#' @param set_session_path (optional) If no environment variable has been set to indicate the library location,
#' You can call this function and let it set the environment variable for this session only.
#'
#' @return When a path is provided, this will be stored as
#'   the multiversion library location to use during this session (via the environment
#'   variable "R_MV_LIBRARY_LOCATION"). In all cases, it will return the location of
#'   the multiversion library. When it cannot be found, it will return an error indicating what to do.
#'
#' @export
#'
lib.location <- function(set_session_path) {
    # If input is provided, set that value as library location.
    if (!missing(set_session_path)) {
        if (!dir.exists(set_session_path)) {
            stop('The provided path does not exist. Please create an empty folder if the library needs to be created.')
        }
        path <- normalizePath(set_session_path, '/', mustWork = TRUE)
        old <- Sys.getenv('R_MV_LIBRARY_LOCATION')
        Sys.setenv(R_MV_LIBRARY_LOCATION = path)
        # Only show this message once per change
        if (interactive() && old != path) {
            message('For this session, the environment variable `R_MV_LIBRARY_LOCATION` has been set to:\n"', path, '"')
        }
        return(invisible(path))
    }

    # Check if environment variable is present (checking 'R_VC_LIBRARY_LOCATION' for backwards compatibility)
    A <- Sys.getenv('R_VC_LIBRARY_LOCATION')
    B <- Sys.getenv('R_MV_LIBRARY_LOCATION')

    if (!nzchar(A) && !nzchar(B)) {
        stop(paste('No environment variable has been set for me to find the multiversion library location.\n',
                   'Please fill the environment variable `R_MV_LIBRARY_LOCATION` with a path to an empty\n',
                   'or existing multiversion library folder, and restart R!!\nAlternatively provide it for\n',
                   'this session using `lib.location(YourPath)`.\n\n'))
    }
    lib_location <- ifelse(nzchar(A), A, B)

    # force a forward slash and remove an ending slash.
    lib_location <- gsub(gsub(lib_location, pattern = '\\\\', replacement = '/'), pattern = '/$', replacement = '')
    return(lib_location)
}


# ============== INPUT PARSERS ==============

#' Parse direct unquoted input to package name/version vector.
#'
#' Converts input like \code{\link{lib.load}(hoi = 3.4, hai = '>= 7.9.2', FIETS)} \cr
#' to a named character vector like \code{c(hoi = '3.4', hai = '>= 7.9.2', FIETS = '')} \cr
#' which is compatible with all code that follows. \cr
#' \cr
#' Must be called like \code{raw_input_parser(as.list(match.call()), c('named_param1', 'named_param2', 'named_param3'))}. \cr
#' It will return all (name) value pairs if values are available excluding the named parameters provided in the second argument. \cr
#'
#' @param arguments The \code{as.list(match.call())} list returned from the calling function. It creates a list of all provided arguments.
#' @param varnames_to_exclude A character vector with var names to exclude. Normally that includes all arguments after \code{...}.
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
    # `as.logical` serves for the case 'arguments' is empty.
    isNum   <- as.logical(sapply(arguments, function(x) {class(x) == 'numeric'}))

    # convert input to consistent named character vector.
    names(arguments)[noName] <- arguments[noName]
    arguments[noName]  <- ''
    arguments[isNum]   <- as.character(arguments[isNum])
    arguments[varName] <- NULL

    return(trimws(unlist(arguments, use.names = TRUE)))
}


#' Parse single string to named character vector.
#'
#' Parses a string shaped like: \cr
#' \code{   assertthat (>= 0.1), R6 (>= 2.1.2), Rcpp (>= 0.12.3), tibble (>= 1.2), magrittr (>= 1.5), lazyeval (>= 0.2.0), DBI (>= 0.4.1)} \cr
#' to match the normal package name/version layout like: \cr
#' \code{   assertthat          R6         Rcpp      tibble    magrittr     lazyeval          DBI} \cr
#' \code{     ">= 0.1"  ">= 2.1.2"  ">= 0.12.3"    ">= 1.2"    ">= 1.5"   ">= 0.2.0"   ">= 0.4.1"} \cr \cr
#' Used by \code{\link{lib.dependencies_online}} to interpret the by CRAN provided dependencies,
#' used to parse the 'vc_override_dependencies.txt' files and the dependencies mentioned in the 'DESCRIPTION' files of the installed packages.
#'
#' @param deps A string of length one with the format shown in the description. This will be converted to a named character vector.
#'
#' @return Returns a character vector with the packages and their versions that it could derive from the single provided string.
#'
#' @export
#'
lib.packs_str2vec <- function(deps) {
    if (is.null(deps) || length(deps) == 0 || is.na(deps)) {
        return(as.character())
    }
    stopifnot(length(deps) == 1)

    deps <- gsub(trimws(deps), pattern = ',\\s?$|\n|^,\\s?', replacement = '') # remove newlines, starting and ending comma's
    deps <- trimws(strsplit(deps, ',')[[1]])     # split on comma's, remove start/end white spaces
    hasVersions <- grepl('\\(.*\\)', deps)       # get version if applicable
    versions <- strRemain('.*\\s?\\(', '\\)', deps)
    versions[!hasVersions] <- ''                 # if no version, give it ''
    names(versions) <- gsub('\\s?\\(.*\\)', '', deps)


    return(versions[names(versions) != 'R'])
}


#' Remove `>` or `>=` from version string.
#'
#' @param packVersion A version indication you would like to remove `>` and `>=` from.
#'
bareVersion <- function(packVersion) {
    trimws(gsub('>?=?\\s?', '', packVersion))
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
#' Other slashes are not usable in \code{grepl} statements directly for example, the '\\' is parsed to '\' before being used as regexp.
#'
#' @param path The path which needs to be normalized. Will make \code{C:/PROGRA~1/R/R-33~1.1/library} into \code{C:/Program Files/R/R-3.3.1/library}.
#'
normPath <- function(path) {
    return(gsub('\\\\', '/', normalizePath(path, '/', mustWork = FALSE)))
}


#' Set \code{.libPaths()} to the provided version specific package locations.
#'
#' Adds \code{.Library} and the paths of the specific versions of the provided
#' packages that are specified (and likely loaded before) to the \code{.libPaths}.
#' Note that this function will erase any current \code{.libPaths()} configuration silently.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. \code{c(dplyr = '>= 0.05', ggplot = '')}).
#' Or the special string 'all', which will add the paths of all directories of the latest versions of every package in the R_MV_library.
#' The path that is appended to the \code{.libPaths()} is constructed based on the name and version provided.
#' @param lib_location The multiversion library location path (no default configured here!).
#' @param additional_lib_paths Any additional \code{.libPaths()} that needs to be set. Namely used for the temporary installation directory.
#'
#' @return
#' The old \code{.libPaths()} content is returned invisibly.
#'
#' @importFrom stats setNames
#'
lib.set_libPaths <- function(packNameVersion, lib_location, additional_lib_paths = c()) {
    old_paths <- .libPaths()

    if (length(packNameVersion) == 1 && packNameVersion == 'all') {
        packageList <- list.dirs(lib_location, recursive = FALSE, full.names = FALSE)
        packageList <- packageList[!packageList %in% c('.git', 'TEMP_install_location')]

        packNameVersion <- c()
        for (packageName in packageList) {
            packVersionList <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

            # Pick the highest version...       packVersionList <- c('0.1.0', '0.3-5', '0.2.5-2')
            packVersion <- packVersionList[numeric_version(packVersionList) == max(numeric_version(packVersionList))]
            packNameVersion <- c(packNameVersion, stats::setNames(packVersion, packageName))
        }
        packNameVersion <- c(packNameVersion, lib.location_install_dir(lib_location, do_create = FALSE))
    }

    # non existing paths are silently ignored by `.libPaths()`
    .libPaths(c(.Library, paste(lib_location, names(packNameVersion), packNameVersion, sep = '/'), additional_lib_paths))
    return(invisible(old_paths))
}


#' Exclude not relevant search paths.
#'
#' Excludes all \code{.libPaths} other then those needed for lib.load().
#'
#' @param lib_location The folder which contains the multiversion library. All directories in \code{.libPaths()} containing this path will be kept.
#' By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} to find this directory.
#' @param dry_run If TRUE, will not change the paths but will print the paths that would be removed by cleaning up the \code{.libPaths()} list.
#'
#' @return No return value, called for it's side effect of cleaning the \code{.libPaths} by removing any non-multiversion library locations.
#'
#' @export
#'
lib.clean_libPaths <- function(lib_location = lib.location(), dry_run = FALSE) {
    correct_paths <- grepl(normPath(lib_location), normPath(.libPaths())) | normPath(.libPaths()) == normPath(.Library)
    if (dry_run) {
        message(sprintf('The following paths will be excluded:\n - "%s"\n', paste(collapse = '"\n - "', normPath(.libPaths()[!correct_paths]))))
    } else {
        .libPaths(.libPaths()[correct_paths])
    }
}


#' Loads `devtools` version 1.13.1 and it's dependencies.
#'
#' During the library call, \code{appendLibPaths} is TRUE, making sure that some devtools functionality
#' (like running tests) in child R instances will still work and know where to load their libraries from.
#'
#' @param lib_location The (version controlled) library to load devtools from.
#' Use \code{lib.install('devtools', allow_overwrite_on_convert = TRUE)} to install devtools, if you have not done so already.
#'
#' @return No return value, called for it's side-effect of loading the devtools and
#'   testthat packages. Also the library paths of both packages will be added to the \code{.libPaths()}
#'
#' @export
#'
lib.devtools_load <- function(lib_location = lib.location()) {
    lib.load(loadPackages = c(devtools = '', testthat = ''),
             appendLibPaths = TRUE, pick.last = TRUE, quietly = TRUE, lib_location = lib_location)
    if (numeric_version(lib.package_version_loaded('testthat')) >= '3.0.0') {
        lib.load(loadPackages = c(waldo = ''),
                 appendLibPaths = TRUE, pick.last = TRUE, quietly = TRUE, lib_location = lib_location)
    }
}


#' Create unique list of highest package versions.
#'
#' Creates a vector with the unique set of with package name = versions and will keep
#' the highest version when multiple versions of one package are defined.
#'
#' @param packNameVersion provide a package name list like so: \code{c(dplyr = '0.5.0', R6 = '', R6 = 0.5)}
#' @param return_as_df {FALSE} if the output should remain a structured dataframe, or if it should return a named character vector.
#'
#' To get a feel with the function, you can try:
#' \preformatted{
#'   multiversion:::unique_highest_package_versions(
#'       c(pack.a = '0.1.0', pack.c = '5.2', package.b = '1.9',   pack.c = '99.99'))
#' }
#'
#' @importFrom stats setNames
#'
unique_highest_package_versions <- function(packNameVersion, return_as_df = FALSE) {
    if (length(packNameVersion) == 0) {
        return(if (return_as_df) {data.frame(names = '', version = '')[0,]} else {c()})
    }
    nameVer <- data.frame(names    = names(packNameVersion),
                          version  = packNameVersion,
                          ordering = seq_len(length(packNameVersion)))

    nameVerU <- do.call(rbind, lapply(split(nameVer, nameVer$names), function(x) {
        return(x[which.max(numeric_version(x$version) == max(numeric_version(x$version))),])
    }))
    nameVerU <- nameVerU[order(nameVerU$ordering),]
    nameVerU <- nameVerU[,-3]

    if (return_as_df) {
        return(nameVerU)
    } else {
        return(setNames(as.character(nameVerU$version), as.character(nameVerU$names)))
    }
}


#' Load namespaces
#'
#' Load (but do not attach) the namespaces of a list of packages.
#'
#' @param packages_to_load_in_ns A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).
#' @param lib_location The folder which contains the multiversion library. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} to find this directory, see \code{lib.location()}.
#' @param additional_lib A single or multiple paths that must be used in addition to the lib_location for looking up the packages. Non existing paths are silently ignored.
#'
lib.load_namespaces <- function(packages_to_load_in_ns, lib_location = lib.location(), additional_lib) {
    if (length(packages_to_load_in_ns) == 0) {
        return()
    }

    # filter list so that only the latest versions remain of the list of required packages, i.e. the list is unique.
    packages_to_load_in_ns <- unique_highest_package_versions(packages_to_load_in_ns)

    currentLibs <- .libPaths()
    # Reset when done (or when crashing)
    on.exit({.libPaths(currentLibs)}, add = TRUE)

    # Set only the directories that we may observe in our search path, i.e.  .libPaths().
    lib.set_libPaths(packages_to_load_in_ns, lib_location, additional_lib)

    failed_to_load <- c()
    for (iPack in names(packages_to_load_in_ns)) {
        packVer <- packages_to_load_in_ns[iPack]

        # Keep.source = TRUE makes sure that (if source is included in install), the source is visible when debugging the package.
        # (keep.source is potentially depricated)
        #
        tryCatch(
            loadNamespace(iPack, lib.loc = .libPaths(), keep.source = TRUE),
            error = function(e) {
                warning('The following error occured when trying to load the namespace of package: ', iPack,
                        '\nThe following error was returned:\n', e$message)
                failed_to_load <<- append(failed_to_load, packVer)
            }
        )
    }

    if (length(failed_to_load) > 0) {
        stop('multiversion: The namespaces of the following packages could not be loaded: ', lib.packs_vec2str(failed_to_load, do_return = TRUE),
             '.\nPlease make sure this package is present, as it is a required dependency (of something, see printed messages).',
             ' Otherwise specify it explicitly in your lib.load call.')
    }
}


#' Print example \code{lib.load} call.
#'
#' Prints the library call that you can use, based on a name/version input vector.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).
#' @param .forceToPrint For testing, I need to be able to overrule the 'interactive()' criteria for printing this example library call.
#'
lib.printVerboseLibCall <- function(packNameVersion, .forceToPrint = FALSE) {
    if (!.forceToPrint && (!interactive() || length(packNameVersion) == 0)) {
        return(invisible())
    }

    nameVer <- unique_highest_package_versions(packNameVersion, return_as_df = TRUE)

    p <- paste; p0 <- paste0
    message('\nVerbose example call to the library (use `quetly = TRUE` to supress this message):')
    message('lib.load( ', p(collapse = ', ', p(sep = ' = ', nameVer$names, p0("'", nameVer$version, "'"))), ')\n')
}


#' Detach package if it exists.
#'
#' @param packageNames A vector of package names which need to be detached. Silently ignores when the package is not loaded.
#'
#' @return No return value, this function is called for it's side-effect. Detaches
#' a package if it can find it. Will also try to unload package DLLs that might be loaded using \code{library.dynam.unload}.
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
        dll <- getLoadedDLLs()[[gsub(iPackage, pattern = 'package:', replacement = '')]]

        if (!is.null(dll)) {
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
