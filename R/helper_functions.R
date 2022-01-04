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


# # ============= ALIASES =============
#
# #' Alias for \code{\link{lib.load}}
# #' @export
# library_VC <- lib.load
#
# #' Alias for \code{\link{lib.install_tarball}}
# #' @export
# install.packages_VC_tarball <- lib.install_tarball
#
# #' Alias for \code{\link{lib.install}}
# #' @export
# install.packages_VC <- lib.install
#
# #' Alias for \code{\link{lib.dependencies}}
# #' @export
# dependencies <- lib.dependencies
#
# #' Alias for \code{\link{lib.installed_packages}}
# #' @export
# installed.packages_VC <- lib.installed_packages
#
# #' Alias for \code{\link{lib.execute_using_packagelist}}
# #' @export
# execute_with_packages <- lib.execute_using_packagelist


# ============== PATHS ==============

#' Return the `multiversion` installation directory.
#'
#' Returns the location of the `multiversion` package. It is a more complicated search than expected since it will find the development folder in a few cases.
#' The only way to guarantee that the correct folder is found is by checking if the `INDEX` folder is present in the `multiversion` folder.
#' This folder is only there when it is the installed instance of multiversion.
#'
lib.my_location <- function() {
    MV_package_location <- find.package(package = 'multiversion', lib.loc = .libPaths(), quiet = T, verbose = F)

    if (length(MV_package_location) == 0) {
        MV_package_location <- find.package(package = 'multiversion', lib.loc = NULL, quiet = T, verbose = F)
    }

    if (length(MV_package_location) == 0 || !file.exists(file.path(MV_package_location, 'INDEX'))) {
        stop(paste0('I need to be able to find the `multiversion` package location. To do that, it must be present in the searchpath `.libPaths()`.',
                    'Please make sure the library search paths include the location of this package.'))
    }
    return(MV_package_location)
}


#' List all untracked library folders
#'
#' List all untracked direcories (libraries) within the multiversion library. The returned untracked directories are cleaned up
#' and printed so that only the unique combinations of each library and it's version is shown once.
#'
#' @param lib_location By default the default library path obtained with \code{lib.location()}.
#'
#' @export
#'
lib.git_show_untracked <- function(lib_location = lib.location()) {
    if (!file.exists(lib_location)) {
        stop(sprintf('Please specify an existing directory for the `lib_location`. Provided was "%s".', lib_location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib_location), intern = T)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib_location))
    }

    temp_installed_libs <- dir(lib.location_install_dir(lib_location))

    listed_files <- system(sprintf('git -C "%s" ls-files --others --exclude-standard', lib_location), intern = T)
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
#'
#' @export
#'
lib.location_install_dir <- function(lib_location = lib.location()) {
    install.location <- gsub(normalizePath(paste0(lib_location, '/TEMP_install_location'), winslash = '/', mustWork = FALSE), pattern = '/$', replace = '')
    dir.create(install.location, showWarnings = FALSE)
    return(install.location)
}


#' Clean multiversion library, revert to state of last commit.
#'
#' Clean up all untracked (not committed) installed libraries in the multiversion library.
#' Will additionally also clean up the TEMP_install_location directory (this is an 'ignored' directory).
#'
#' Since it involves a quite invasive operation, it asks for permission when being called in an interactive session.
#'
#' @param lib_location By default the library path returned by \code{lib.location()} is used.
#'
#' @export
#'
lib.clean <- function(lib_location = lib.location(), clean_temp_lib = TRUE) {
    if (!file.exists(lib_location)) {
        stop(sprintf('Please specify an existing directory for the `lib_location`. Provided was "%s".', lib_location))
    }
    if (!suppressWarnings(grepl('^true$', system(sprintf('git -C "%s" rev-parse --is-inside-work-tree', lib_location), intern = T)))) {
        stop(sprintf('The provided directory "%s" is not a git repository.', lib_location))
    }

    lib.git_show_untracked(lib_location = lib_location)

    if (interactive()){
        choice <- menu(c('yes', 'no'), title = '\nAre you sure you want to undo all changes made to the \'multiversion library\' and go back to the last commit?')

        if (choice != 1) {return(invisible())}
    }

    # You can undo changes to tracked files with: git reset HEAD --hard
    # You can remove untracked files with: git clean -f
    # You can remove untracked files and directories with: git clean -fd              <- this one is applied below.
    # You can remove ignored and untracked files and directories git clean -fdx.
    system(sprintf('git -C "%s" clean -fd', lib_location), intern = T)

    if (clean_temp_lib) {
        # Additionally clear the temp_install directory:
        # The temp lib location is based on the main lib location. The folder 'TEMP_install_location' is appended to it.
        lib.clean_install_dir(lib.location_install_dir(lib_location))
    }
}


#' Clear the temp install folder.
#'
#' The temporary installation folder (indicated by \code{lib.location_install_dir()}) is used to install the package before moving ('converting') it to the final location.
#' This function removes this temporary folder. Make sure that all installed packages that are desired to keep are converted.
#' You can run the \code{\link{lib.convert}()} once again to make sure this is the case.
#'
#' @param temp_install.location By default the default temporary directory path obtained with \code{lib.location_install_dir()}.
#'
#' @export
#'
lib.clean_install_dir <- function(temp_install.location = lib.location_install_dir()) {
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
#' @export
#'
lib.location <- function(set_session_path) {
    # If input is provided, set that value as library location.
    if (!missing(set_session_path)) {
        if (!dir.exists(set_session_path)) {
            stop('The provided path does not exist. Please create an empty folder if the library needs to be created.')
        }
        path <- normalizePath(set_session_path, '/', mustWork = T)
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
        stop(paste('No environment variable has been set for me to find the R_MV_library location.\n',
                   'Please fill the environment variable `R_MV_LIBRARY_LOCATION` with a path to an empty\n',
                   'or already created library base folder, and restart R!!\nAlternatively provide it for\n',
                   'this session using `lib.location(YourPath)`.\n\n'))
    }
    lib_location <- ifelse(nzchar(A), A, B)

    # force a forward slash and remove an ending slash.
    lib_location <- gsub(gsub(lib_location, pattern = '\\\\', replace = '/'), pattern = '/$', replace = '')
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
    isNum   <- sapply(arguments, function(x) {class(x) == 'numeric'})

    # convert input to consistant named character vector.
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
#' @param deps A string of format X to convert to a named character vector Y.
#'
#' @export
#'
lib.packs_str2vec <- function(deps) {
    if (is.null(deps) || length(deps) == 0 || is.na(deps)) {
        return(as.character())
    }

    deps <- gsub(trimws(deps), pattern = ',\\s?$|\n|^,\\s?', replace = '') # remove newlines, starting and ending comma's
    deps <- trimws(strsplit(deps, ',')[[1]])     # split on comma's, remove start/end whitespaces
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
#' Other slashes are not usable in `grepl` statements directly for example, the '\\' is parsed to '\' before being used as regex.
#'
#' @param path The path which needs to be normalized. Will make `C:/PROGRA~1/R/R-33~1.1/library` into `C:/Program Files/R/R-3.3.1/library`.
#'
normPath <- function(path) {
    return(gsub('\\\\', '/', normalizePath(path, '/', mustWork = F)))
}


#' Append all package locations to `.libPaths()`, including .Library, but leaving out old values.
#'
#' Adds the path of the package that is specified (and likely loaded before) to the `.libPaths`.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.05', ggplot = '')`).
#' The path that is appended to the `.libPaths` is constructed based on the name and version provided.
#' @param lib_location The multiversion library location path (no default configured here!).
#'
lib.set_libPaths <- function(packNameVersion, lib_location, additional_lib_paths = c()) {
    # non existing paths are silently ignored by `.libPaths()`
    .libPaths(c(.Library, paste(lib_location, names(packNameVersion), packNameVersion, sep = '/'), additional_lib_paths))
}


#' Exclude not relevant search paths.
#'
#' Excludes all .libPaths other then those needed for lib.load().
#'
#' @param lib_location The folder which contains the multiversion library. All directories in \code{.libPaths()} containing this path will be kept.
#' By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} to find this directory.
#' @param dry.run If TRUE, will not change the paths but will print the paths that would remain after cleaning up the \code{.libPaths()} list.
#'
#' @export
#'
lib.clean_libPaths <- function(lib_location = lib.location(), dry.run = FALSE) {
    correct_paths <- grepl(normPath(lib_location), normPath(.libPaths())) | normPath(.libPaths()) == normPath(.Library)
    if (dry.run) {
        message(sprintf('The following paths will be excluded:\n - "%s"\n', paste(collapse = '"\n - "', normPath(.libPaths()[!correct_paths]))))
    } else {
        .libPaths(.libPaths()[correct_paths])
    }
}


#' Installs the incredible `devtools` package.
#'
#' Will install devtools and it's dependencies into the multiversion library provided by `lib.location()`.
#' An alternative library location can optionally be specified.
#'
#' @param lib_location The library (can be an empty folder) to install Devtools in. Defaults to \code{lib.location()}
#' @param force_install FALSE by default, if the package `devTools` is already installed, it will return silently.
#'
#' @export
#'
lib.devTools_install <- function(lib_location = lib.location(), force_install = FALSE) {
    if (!force_install && length(lib.available_versions('devtools')) > 0) {
        message('The package devtools seems to be already installed. Please set `force_install` to true if you would like to overwrite or update devtools.')
        return()
    }
    if (length(libs <- dir(lib_dir <- lib.location_install_dir(lib_location))) > 0) {
        lib.git_show_untracked()
        stop(sprintf('These libraries are still present in the install directory "%s":\n%s.\nPlease run `lib.clean_install_dir(yourLib)` to clean up if possible and run me again.', lib_dir, paste0(collapse = ', ', "'", libs, "'")))
    }
    lib.install('devtools', lib_location = lib_location, overwrite_this_package = TRUE)
    lib.clean_install_dir(lib_location)
}


#' Loads `devtools` version 1.13.1 and it's dependencies.
#'
#' During the library call, `appendLibPaths` is TRUE, making sure that some devtools functionallity
#' (like running tests) in child R instances will still work and know where to load their libraries from.
#'
#' @param lib_location The (version controlled) library to load devtools from.
#' Use lib.devTools_install to install devtools, if you have not done so already.
#'
#' @export
#'
lib.devtools_load <- function(lib_location = lib.location()) {

    lib.load(devtools = '>= 1.13.1', testthat, Rcpp, roxygen2, stringi, digest, pick.last = T, appendLibPaths = TRUE, quietly = T)
}


#' Create unique list of highest package versions.
#'
#' Will uniquify the named character vector with package versions to remain the highest functions. (for now only used for printing)
#'
#' @param packNameVersion provide a packageNameVersion list like so: `lib.printVerboseLibCall(c(dplyr = '0.5.0', R6 = '', R6 = 0.5))`
#' @param return_as_df {FALSE} if the output should remain a structured dataframe, or if it should return a named character vector.
#'
unique_highest_package_versions <- function(packNameVersion, return_as_df = FALSE) {
    if (length(packNameVersion) == 0) {
        return(if (return_as_df) {data.frame(names = '', version = '')[0,]} else {c()})
    }
    nameVer <- data.frame(names    = names(packNameVersion),
                          version  = packNameVersion,
                          ordering = seq_len(length(packNameVersion)))

    nameVerU <- do.call(rbind, lapply(split(nameVer, nameVer$names), function(x) {return(x[which.max(x$version),])}))
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
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).
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

        # Keep.source = T makes sure that (if source is included in install), the source is visible when debugging the package.
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
        stop('multiversion: The namespaces of the following packages could not be loaded: ', lib.packs_vec2str(failed_to_load, do_return = T),
             '.\nPlease make sure this package is present, as it is a required dependency (of something, see printed messages).',
             ' Otherwise specify it explicitly in your lib.load call.')
    }
}


#' Print example \code{lib.load} call.
#'
#' Prints the library call that you can use based on a name/version input vector.
#'
#' @param packNameVersion A named character vector with package names and their version indication (e.g. `c(dplyr = '>= 0.4.0', ggplot = '')`).
#'
#' @export
#'
lib.printVerboseLibCall <- function(packNameVersion) {
    if (!interactive() || length(packNameVersion) == 0) {
        return(invisible())
    }

    nameVer <- unique_highest_package_versions(packNameVersion, return_as_df = TRUE)

    p = paste; p0 = paste0
    message('\nVerbose example call to the library (use `quetly = T` to supress this message):')
    message('lib.load( ', p(collapse = ', ', p(sep = ' = ', nameVer$names, p0("'", nameVer$version, "'"))), ')\n')
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
        dll <- getLoadedDLLs()[[gsub(iPackage, pattern = 'package:', replace = '')]]

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
