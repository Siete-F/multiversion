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


# ============== MAIN FUNCTIONALITY ==============

#' Load package from R_VC_library
#'
#' Two ways of package/versions input are accepted:
#' 1: just provide them directly (the \code{...} input). All not recognized named variables
#'    will be interpreted like package names or package name/version combination. \code{\link{lib.load}(DBI = '0.5', assertthat, R6)}
#' 2: provide the \code{loadPackages} input in the following way: \code{\link{lib.load}(loadPackages = c(DBI = '0.5', assertthat = '', R6 = ''))} \cr
#' \cr
#' If an empty string e.g. \code{dplyr = ''}, or only the package name is specified, one of two things will happen:
#' - if one version is available, this one is used.
#' - if multiple versions are available, the first or last version depending on the 'pick.last' parameter. \cr
#' \cr
#' if >= or > is used, as in \code{dplyr = '>= 2.5'}, it will list the options and take the first or last version depending on the 'pick.last' parameter.
#' If another version is desired, please define it in the input list of packages to load before the package that depends on it is loaded.
#' \cr
#' Dependencies are checked and then loaded by recursively running this function with \code{dry.run = TRUE}.
#' This makes that dependencies are not loaded automatically, but are added to the namespace and accesible by its caller.
#' To access a dependency directly, load it explicitly. \cr
#' In other words, dependencies are remembered, but not loaded. So all strings are released (figurely speaking), but the dependency is there for the depending package. \cr
#' \cr
#' The inputs packNameVersionList [list of named versions] and skipDependencies [list of names] can be
#' left blank in general. They are used by other functionallity like \code{\link{lib.testload}()} and \code{\link{lib.install}}
#' \code{dry.run} will show the packages that will be used and will crash when no option is feasable (not installed or not compliant packages).
#' If you are trying to setup a propper \code{\link{lib.load}} call, it is always a good idea to work with dry.run's.
#' Once an incorrect package has been loaded, it is very likely you will have to restart your R session. \cr
#' \cr
#' "strings" can stay attached (as in, the .libPaths can be appended) when using 'appendLibPaths = TRUE'.
#' Afterwards, the normal \code{library} call can be used to load the not yet loaded but attached package.
#' This is more or less the same as doing the following: \cr
#' \code{
#' lib.load(c(dplyr = '0.5.0'), dry.run = TRUE, appendLibPaths = TRUE)
#' library(dplyr)} \cr
#' \cr
#' Note that the second call is a standard \code{library} call. How this works is that \code{dry.run} skips the loading step, and \code{appendLibPaths} adds the paths of dplyr and it's dependencies to \code{.libPaths}, which make a \code{library} call work. \cr
#' \cr
#' One reason to use \code{appendLibPaths = TRUE} is to make these packages accessible by a new 'child' R session. This is the case if \code{devtools::test()} is ran
#' by using \code{cntrl} + \code{shift} + \code{T} in Rstudio. When running it directly, it will use the packages it can find in the available libraries (\code{.libPath()}) and return an error if they cannot be found. \cr
#'
#' @details
#' All packages within the directory returned by \code{.Library} cannot be version controlled and will be considered 'base packages'. \cr
#' \cr
#' ERRORS: \cr If you receive the error "\code{cannot unload ...}" it means that it tries to load a package, but another version is already loaded.
#' To unload this other (older) version, run detach(package = '...'). If it is a dependency of an other package, you will receive this error.
#' Try restarting your RStudio with a clean workspace (environment). If that doesn't help, the only workaround (when using this in R studio) is to close your Rstudio session (NOTE: save your unsaved process before proceding!!), rename (or remove) the folder
#' "\code{YourRProject/.Rproj.user/.../sources/prop}" and start Rstudio again. If it doesn't work, try "\code{/sources/per}" also. At the \code{...} there should be a hash used in the current session e.g. \code{/F3B1663E/}.
#' After this, the packages should be unloaded and you should be able to load a new batch of packages. Most times it will just do to clear the workspace (environment) and reload the project while saving the empty environment.
#'
#' @param ... All packages and their versions you would like to load e.g. \code{\link{lib.load}(DBI = '0.5', assertthat = '', R6 = '', quietly = TRUE)}. Input names like \code{quietly} will be recognized and interpreted as expected.
#' @param loadPackages Supports providing a named character vector of packages and their versions in the shape that is supported by all other functions in this package. e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}
#' @param lib_location The folder containing a structure where this package must load packages from. By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} for this directory.
#' @param dry.run (default: FALSE) Will make it perform a dry run. It will check all dependencies and if \code{appendLibPaths} it will add
#' their paths to \code{.libPaths} but it will not load those packages. If the paths are added this way, you should be able to just call the located packages with \code{library(...)}
#' @param quietly (default: FALSE) Indicates if the loading must happen silently. No messages and warnings will be shown if TRUE.
#' @param appendLibPaths (default: FALSE) If TRUE, the path to every package that is loaded will be appended to \code{.libPath(...)}. That configured path is the location where \code{library()} will look for packages. For a usecase for this feature, see the description above.
#' @param pick.last (default: FALSE) Changes the way a decision is made. In the scenario where a dependency of \code{>} or \code{>=} is defined, multiple versions may be available to choose from. By default, the lowest compliant version is chosen. Setting this to TRUE will choose the highest version.
#' @param also_load_from_temp_lib (default: TRUE)
#'
#' @param packNameVersionList See main description. Should be left blank.
#' @param skipDependencies See main description. Should be left blank.
#'
#' @export
#'
lib.load <- function(..., loadPackages = NULL, lib_location = lib.location(), dry.run = FALSE, quietly = TRUE, appendLibPaths = FALSE, pick.last = FALSE, also_load_from_temp_lib = FALSE, packNameVersionList = c(), skipDependencies = c()) {

    # If `loadPackages` is not provided, make use of the ... input via `match.call()`.
    # It will list all input names and values from which I will use all but the ones excluded.
    if (is.null(loadPackages)) {
        loadPackages <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('loadPackages', 'lib_location', 'dry.run', 'quietly', 'appendLibPaths', 'pick.last', 'also_load_from_temp_lib', 'packNameVersionList', 'skipDependencies'))
    }

    if (length(loadPackages) == 1 && strtrim(names(loadPackages), 2) == 'c(') {
        stop('Please make sure that you call `lib.load(loadPackages = c(xx = yy))` when you would like to use a named character vector.\nAlternatively, remove the vector elements and call `lib.load(xx = yy)` directly.')
    }

    # If still other libraries are set as active libraries, reset the library to just 1 lib for the build in functions (= `.Library`).
    # if (!quietly & interactive() & length(sys.calls()) == 1 & !all(grepl(normPath(lib_location), normPath(.libPaths())) | grepl(normPath(.Library), normPath(.libPaths())))) {
    #     warning(paste0('\nlib.load: Extra libraries were found.\n',
    #                    'lib.load will exclude those when loading packages, please be aware `library()` does not\n',
    #                    'and might load a package from an unexpected location.\n',
    #                    'Please use `.libPaths(.Library)` before using lib.load to suppress this warning.\n\n'))
    # }

    # check if the package version that is provided is a correct version (this catches a wrong input like `lib.load(dplyr = '< 0.5.0')`)
    if (length(loadPackages) != 0 && !is.na(loadPackages) &&
        any(sapply(loadPackages, function(x) {attributes(regexpr('>?=?\\s?\\d+(\\.\\d+){1,3}', x))$match.length != nchar(x) && nchar(x) > 0}) & !lib.is_basepackage(names(loadPackages)))) {
        stop(sprintf('Not all package versions that are provided seem to be valid version numbers. The following was received:\n%s', paste(paste0(names(loadPackages), ' (', loadPackages, ')'), collapse = ', ')))
    }

    n_skipped <- 0
    for (iPackage in unique(names(loadPackages))) {
        # Here, the package loading string prefix is created:
        n_recursive <- sum(grepl('^lib.load', sapply(sys.calls(), function(x) {x[[1]]})))
        if (n_recursive == 1) {
            stackStr <- '+_'
        } else {
            # if it is the first of the packages to be load, start with backward-slash:
            startChar <- ifelse(iPackage == names(loadPackages)[1 + n_skipped], ' \\_', '  |')
            # starting from the second 'dependency' depth, an additional pipe needs to be printed:
            stackStr <- paste(collapse = '', c(rep('  |', n_recursive - 2), startChar))
        }
        stackStr <- paste0(collapse = '', c(stackStr, rep('_', max(16 - nchar(stackStr), 1))))


        # if base package, simply load and continue
        if (lib.is_basepackage(iPackage)) {
            library(iPackage, character.only = TRUE, quietly = quietly)
            n_skipped <- n_skipped + 1
            next
        }

        if (iPackage %in% c(skipDependencies, 'RVClibrary')) {
            if (lib.check_compatibility(loadPackages[iPackage], packNameVersionList[iPackage])) {
                n_skipped <- n_skipped + 1
                next
            } else {
                # Package was explicitly requested by previous load operation, and is already loaded:
                if (paste0('package:', iPackage) %in% search()) {
                    # ERROR
                    error_packageAlreadyLoaded(iPackage, packNameVersionList[iPackage], lib.package_version_loaded(iPackage))
                }
                packNameVersionList <- packNameVersionList[!names(packNameVersionList) == iPackage]
            }
            # If package is loaded directly or indirectly:
        } else if (isNamespaceLoaded(iPackage)) {
            if (!lib.check_compatibility(loadPackages[iPackage], lib.package_version_loaded(iPackage))) {
                error_packageAlreadyLoaded(iPackage, loadPackages[iPackage], lib.package_version_loaded(iPackage))
            }
        }

        cat(stackStr)

        temp_lib_dir <- lib.location_install_dir(lib_location)
        additional_lib <- c()
        if (also_load_from_temp_lib) {
            additional_lib <- temp_lib_dir
        }

        if (also_load_from_temp_lib && length(lib.available_versions(iPackage, lib_location)) == 0 && file.exists(file.path(temp_lib_dir, iPackage))) {
            package_loc <- temp_lib_dir
            packVersion <- packageDescription(iPackage, package_loc)$Version
            cat(sprintf("Version %-7s INSTALLED  for package '%s'\n", packVersion, iPackage))
        } else {
            # Messages like: "Version ... is chosen  for package '...'" are printed here.
            packVersion <- lib.decide_version(loadPackages[iPackage], lib_location, pick.last = pick.last, quietly = quietly)
            package_loc <- paste(lib_location, iPackage, packVersion, sep = '/')
        }

        if (!dir.exists(package_loc)) {
            stop(sprintf('The package "%s" or its version "%s" could not be accessed and might not be present.', iPackage, packVersion))
        }

        # load dependencies: [1] from an override dependency file [2] from the original dependencies.
        # The override dependency file never exists when the package is loaded from the temp_install_library.
        overrideFile <- file.path(package_loc, 'vc_override_dependencies.txt')
        if (file.exists(overrideFile)) {
            dependingPackages <- lib.packs_str2vec(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- packageDescription(iPackage, lib.loc = package_loc)
            dependingPackages <- lib.packs_str2vec(gsub(paste0(packDesc$Depends, ',', packDesc$Imports), pat = ',,', rep = ','))
        }

        # recusively load dependencies
        # The skipDependencies is necessary for `dry.run=TRUE`. With `dry.run=FALSE` the dependency is loaded and skipped in the next itteration.
        packNameVersionList <- lib.load(loadPackages        = dependingPackages,
                                        lib_location        = lib_location,
                                        packNameVersionList = packNameVersionList,
                                        skipDependencies    = c( names(packNameVersionList), skipDependencies ),
                                        pick.last           = pick.last,
                                        also_load_from_temp_lib = also_load_from_temp_lib,
                                        dry.run             = TRUE)

        packNameVersionList <- append(packNameVersionList, setNames(packVersion, iPackage))

        currentLibs <- .libPaths()

        # Reset .libPaths on failure
        on.exit({
            if (appendLibPaths) {
                lib.add_libPaths(packNameVersionList, lib_location, additional_lib)
                .libPaths(c(.libPaths(), currentLibs))
            }
        }, add = TRUE)

        if (!dry.run) {
            lib.add_libPaths(packNameVersionList, lib_location, additional_lib)
            lib.clean_libPaths()

            if (quietly) {
                suppressWarnings(suppressMessages(library(iPackage, lib.loc = package_loc, character.only = TRUE, quietly = quietly)))
            } else {
                library(iPackage, lib.loc = package_loc, character.only = TRUE, quietly = quietly)
            }

            .libPaths(currentLibs)
        }

    }

    # if not quietly, and at top level of stack, show an example library call.
    if (!quietly && length(sys.calls()) == 1) {
        lib.printVerboseLibCall(packNameVersionList)
    }

    return(if(quietly) {invisible(packNameVersionList)} else {packNameVersionList})
}
