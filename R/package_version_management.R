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


# -------------- version checking --------------

#' Just checks the multiversion library for all available versions installed for a specific package.
#' If no name is provided, an error is returned.
#'
#' @param packageName The name of the package for which all versions must be returned.
#' @param lib_location The folder containing the structure where this package his versions need to be checked.
#'
#' @export
#'
lib.available_versions <- function(packageName, lib_location = lib.location()) {
    if (is.na(packageName) || length(packageName) == 0 || nchar(packageName) == 0) {
        stop('The package name cannot be empty.')
    }

    available_pack_versions <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)
    if (any(available_pack_versions %in% c("data", "doc", "help", "html", "Meta", "R"))) {
        stop(paste0('The library I am trying to load from, is not shaped as expected.\n',
                    'It seems to be `myLibrary/%s/DESCRIPTION` instead of `myLibrary/%s/1.2.3/%s/DESCRIPTION`.\n',
                    'This desired format can be achieved by using `lib.convert` on an ordinary library structure.'))
    }
    return(as.character(sort(numeric_version(available_pack_versions))))
}


#' Choose version based on the version indication, and available versions.
#'
#' Obtains the correct version based on the version instruction provided (e.g. \code{>= 0.5}), the package name and it's available versions.
#' If no compatible version is found between the available versions a suitable error is thrown.
#' All different version indications should be handled in this function, including:
#' \enumerate{
#'   \item{a version with \code{>} or \code{>=} indicator.}
#'   \item{just a version e.g. \code{'0.5.0'} (most specific)}
#'   \item{a zero length char e.g. \code{''}}
#' }
#'
#' Note that both (1) and (3) are effected by 'pick.last'.
#'
#' @param packVersion A single named version value. i.e. A package name and it's version requirement like: \code{c(dplyr = '>= 0.4.0')}.
#' @param versionList A list of available versions for this package to choose from. It is the list to choose from and check availability. Created with \code{lib.available_versions}.
#' @param packageName  It is used for clear error handling. It should be the package name it is trying to load so we can mention it when crashing.
#' @param pick.last See details.
#' @param warn_for_major_diff If true, it will throw a warning when the requested package is a major release higher than that is specified.
#'
#' @details
#' If a version like \code{>= 0.5} is given and multiple versions exist, a choice needs to be made.
#' By default it will take the same or first higher version (when it exists, just \code{0.5} in the example).
#' This most likely leads to not changing the behaviour of the code. Alternatively, picking the latest version is most
#' likely to be accepted by other packages their dependencies (e.g. if a package that is loaded in the future depends on this package but asks for \code{> 0.6}, it will likely fail).
#' The downside of this is that an update could be a major one, going from \code{0.5} to \code{2.0}, where allot of things can have changed and your code that used to work fine is at risk.
#'
chooseVersion <- function(packVersion, versionList, packageName = '', pick.last = FALSE, warn_for_major_diff = TRUE) {

    compliantVersions <- c()
    num_ver_list <- numeric_version(versionList)
    if (!(is.na(packVersion) || nchar(packVersion) == 0)) {
        first_next_major <- package_version(sprintf('%1.0f.0.0', package_version(bareVersion(packVersion))$major+1))
    }

    # It is possible that this package is already loaded as a dependency of another package.
    if (isNamespaceLoaded(packageName)) {

        if (grepl('^[0-9.\\\\-]+', packVersion) && !numeric_version(packVersion) %in% num_ver_list) {
            stop(sprintf('The requested version "%s" for package "%s" is not installed.', packVersion, packageName))
        }
        if (.Platform$GUI == "RStudio" && packageName == 'yaml') {
            # 'yaml' seems to be a strange case. It is loaded by Rstudio from your local library, which is likely a different
            # version than is present in the R_MV_library (in my case yaml=2.1.13, where the version in my R_MV_library is 2.1.14).
            # I did find a way to unload it so that we can also alter this package his version, even when it is always loaded automatically on startup.

            unloadNamespace(asNamespace('yaml'))
            # Try again after unload is successful.
            return(chooseVersion(packVersion, versionList, packageName = 'yaml', pick.last = pick.last, warn_for_major_diff = warn_for_major_diff))
        }
        alreadyLoadedVersion <- lib.package_version_loaded(packageName)

        if (!lib.check_compatibility(packVersion, alreadyLoadedVersion)) {
            # ERROR
            error_packageAlreadyLoaded(packageName, packVersion, alreadyLoadedVersion)
        }

        # Check if the package version that was already loaded is available in the R_MV_library directory.
        if (!numeric_version(alreadyLoadedVersion) %in% num_ver_list) {
            # The most likely location is your default library. This method is build in because `system.file` showed some unpredictable behaviour.
            # It makes use of `.getNamespaceInfo(asNamespace(pkg), "path")` on the background, which found my repository path, not the installed version.
            potential_locations <- file.path(.libPaths(), packageName)
            loaded_pack_loc <- potential_locations[file.exists(file.path(potential_locations, "DESCRIPTION"))]
            if (length(loaded_pack_loc) == 0) {
                loaded_pack_loc <- system.file(package = packageName)
            }
            stop(sprintf(paste('An already loaded package "%s" (version: %s) is compatible with the requested version, but',
                               '\nwas not loaded from the R_MV_library directory and not available (instead from: "%s").',
                               '\nRequested was %s (available versions: %s).',
                               '\nIf this problem persists, try cleaning the RStudio catch folder as described in DETAILS of ?lib.load.',
                               '\nWe will not try to detach since that could cause unexpected behaviour.',
                               '\nPlease detach it manually (e.g. `detachAll(packageList = \'%s\')`, where %s are depending on it)\n\n'),
                         packageName, alreadyLoadedVersion, loaded_pack_loc, packVersion,
                         paste0(collapse = ', ', "'", as.character(num_ver_list), "'"), packageName,
                         paste(collapse = ', ', paste0("'", getNamespaceUsers(packageName), "'"))))
        }  # `getNamespaceUsers` returns the packages that use a certain namespace.

        # When the already loaded version simply complies, just return that version.
        return(alreadyLoadedVersion)
    }

    # First case covers '>=' and '>'
    if (grepl('^>', packVersion)) {
        ge_or_gt <- ifelse(grepl('>=', packVersion), `>=`, `>`)

        pref_major <- getOption('mv_prefer_within_major_version', default = 'yes')
        pref_major <- tolower(pref_major) %in% c('yes', 'true')

        if (pref_major) {
            # First try if a valid version can be found within the requested major version.
            validVersions  <- ge_or_gt(num_ver_list, numeric_version(bareVersion(packVersion))) & num_ver_list < first_next_major
        }

        if (!pref_major || !any(validVersions)) {
            # Otherwise pick from complete list (throw optional warning).
            validVersions <- ge_or_gt(num_ver_list, numeric_version(bareVersion(packVersion)))
            if (pref_major && any(validVersions) && warn_for_major_diff) {
                warning(sprintf('For package "%s" the lowest optional version is a major release higher. Requested: "%s", first option "%s".',
                                packageName, packVersion, versionList[validVersions][[1]]))
            }
        }

        compliantVersions <- versionList[validVersions]

    } else if (is.na(packVersion) || nchar(packVersion) == 0) {

        compliantVersions <- versionList

        if (length(versionList) < 1) {
            stop(paste0('There is no package "', packageName, '" installed (yet). (requested version: "', packVersion, '")'))
        }

        # deal with concrete version number (e.g. `dplyr = 0.5.0`):
    } else if (packVersion %in% versionList) {
        # if available, go with it.
        compliantVersions <- packVersion
    }

    if (length(compliantVersions) == 0) {
        stop(sprintf('The requested version "%s" for package "%s" is not installed.', packVersion, packageName))
    }

    # Decide on the version after the (>=, >) choice is made and multiple choices remain.
    # This should only be the case when `>` or `>=` scenarios are dealt with.
    # Other scenarios should be checked (stopped) above.
    # For more details on this choice, read the instruction for the `pick.last` parameter.
    index <- ifelse(pick.last, length(compliantVersions), 1)
    myChoice <- sort(numeric_version(compliantVersions))[index]

    return(as.character(myChoice))
}

#' check if version indication is compliant.
#'
#' Returns TRUE if the 'condition' complies with the already available 'version'.
#' This function is vectorized.
#'
#' @param condition A version indication like `>= 4.5.1` or `2.3.4` or `> 1.2.3` or `''` (empty) or `NA`.
#' @param version A version number like `1.2.3`, or a vector of version strings (will be converted to `numeric_version('1.2.3') during comparison).
#'
#' @export
#'
lib.check_compatibility <- function(condition, version) {
    stopifnot(length(condition) != 0)
    if (length(condition) > 1) {
        stop('Please provide one condition only in `lib.check_compatibility`!')
    }
    if (length(version) > 1) {
        return(sapply(version, lib.check_compatibility, condition = condition))
    }

    # If no reference was supplied, all conditions are acceptable.
    if (is.null(version) || is.na(version)) {
        return(TRUE)
    }

    # If '', NA or an equal version as the existing version is returned, pass.
    if (is.na(condition) || nchar(condition) == 0 || trimws(condition) == version) {
        return(TRUE)
    }

    numCondition <- numeric_version(bareVersion(condition))
    numVersion <- numeric_version(version)

    # If the version that is requested is indeed high
    if (grepl('>=', condition)) {
        return(numVersion >= numCondition)
    } else if (grepl('>', condition)) {
        return(numVersion > numCondition)
    } else {
        # normally a case where `condition == version` is required and the versions differ.
        return(FALSE)
    }
}


#' Choose correct package version, and print decision.
#'
#' Obtains the correct version based on the version instruction provided (e.g. \code{>= 0.5}). It will print which version is chosen if `verbose = TRUE`.
#' if no compatible version is found between the available versions, the function 'chooseVersion' will return an error to notify you.
#'
#' @param packVersion A named character vector with package names and their version indication (e.g. \code{c(dplyr = '>= 0.4.0', ggplot = '')}).
#' @param lib_location The location of the R_MV_library folder.
#' @param print_version_choice if true, it will print the choices it made.
#' @param pick.last If a version like\code{>= 0.5} is given and multiple versions exist, a choice needs to be made.
#' By default it will take the first higher version (when it exists, just\code{0.5}, which is often the case).
#' This because this is most likely to not change the behavior of the code. Picking the latest version is most
#' compatible with matching other packages their dependencies (e.g. if a later package depends on this package but asks for\code{> 0.6}, it will crash).
#' The downside of this is that an update could be a major one, going from\code{0.5} to\code{2.0}, where allot of things can change and code is likely to not work anymore.
#' @param warn_for_major_diff If true, the default, will return warnings if the loaded package is a major release higher then the package that was requested.
#'
lib.decide_version <- function(packVersion, lib_location, pick.last = FALSE, print_version_choice = TRUE, warn_for_major_diff = TRUE) {
    packageName <- names(packVersion)
    origVersion <- unname(packVersion)

    if (length(packVersion) == 0) {
        stop("The length of packVersion cannot be 0, if it is desired to not specify the version specifically, use e.g. `dplyr = ''`.")
    }

    # handle higher then, higher-or-equal, equal to (just a version) or '' (auto determine) version indications:
    packVersionList <- lib.available_versions(packageName, lib_location)
    packVersion <- chooseVersion(packVersion, versionList = packVersionList, packageName, pick.last = pick.last, warn_for_major_diff = warn_for_major_diff)

    # print instructive message:
    if (print_version_choice) {
        if (!is.na(origVersion) && strtrim(origVersion, 1) == '>') {
            message(sprintf("Version %-7s is chosen  for package '%s' (%s)", packVersion, packageName, origVersion))

            # When no version was specified:
        } else if (is.na(origVersion) || nchar(origVersion) == 0) {
            if (length(packVersionList) > 1) {
                message(sprintf("Version %-7s is picked  for package '%s'", packVersion, packageName))
            } else {
                message(sprintf("Only    %-7s is there   for package '%s'", packVersion, packageName))
            }
        } else if (packVersion == origVersion)
            message(sprintf("Exactly %-7s is used    for package '%s'", packVersion, packageName))
    }

    return(packVersion)
}


#' Check if a package belongs to the standard R (base) packages.
#'
#' To check if the package is a base package, we look it up among all packages
#' in the \code{.Library} directory (\code{list.dirs(.Library, full.names = FALSE, recursive = FALSE)}).
#' We cannot version control packages which are located in this library since
#' the \code{.Library} will always be added to the \code{.libPaths}.
#' For base packages, this is acceptable, but it appears that this directory
#' is not always as clean as we would wish. Because of this reason, we do not
#' check the more widely accepted \code{rownames(installed.packages(priority="base"))}.
#'
#' @param packageName The package name to check.
#'
#' @export
#'
lib.is_basepackage <- function(packageName) {
    # Featuring direct call like: `lib.is_basepackage(stats)`
    # This can't be used interactively, when providing a variable with a name it would use the var name as name.
    # packageName = as.character(substitute(packageName))

    basePackages <- list.dirs(.Library, full.names = FALSE, recursive = FALSE)
    return(packageName %in% c(basePackages, 'R'))
}


#' Check the versions of an already loaded package.
#'
#' This works for both packages within the R_MV_library (which are loaded) and
#' for packages outside the library.
#'
#' @param packageNames The name or a vector of names of the packages for which to obtain the version.
#' @param exclude_not_loaded If true, the default, it will not try to find a 'loaded version' of a package that is not loaded.
#'
#' @importFrom stats setNames
#' @importFrom utils sessionInfo
#' @export
#'
lib.package_version_loaded <- function(packageNames, exclude_not_loaded = TRUE) {
    if (exclude_not_loaded) {
        packageNames <- packageNames[packageNames %in% unique(
            c(names(utils::sessionInfo()$otherPkgs), names(utils::sessionInfo()$loadedOnly)))]
    }

    # determines the versions of the loaded packages given
    version <- c()
    for (iPackage in packageNames) {
        version <- append(version, stats::setNames(as.character(utils::packageVersion(iPackage)), iPackage))
    }
    return(version)
}


#' Check a package his online dependencies
#'
#' Returns a \code{c(name = '<version spec>')} array which can be used for \code{lib.load()},
#' \code{lib.install_if_not_compatible()} or \code{lib.dependsOnMe()}.
#'
#' @param packageName The package name to check.
#' @param cran_url Defaults to 'https://cran.rstudio.com/'.
#'
#' @importFrom stats na.omit
#' @importFrom utils available.packages
#' @export
#'
lib.dependencies_online <- function(packageName, cran_url = 'https://cran.rstudio.com/'){

    # It reads fields = c('Depends', 'Imports') by default.
    # It won't read less data when providing this argument. The packages list is cached.
    packList <- utils::available.packages(repos = cran_url)
    if (!(packageName %in% packList)) {
        stop(sprintf('Package "%s" is not available on CRAN (%s). Install from source using "lib.install_tarball(path, depends)".', packageName, cran_url))
    }
    dependsOn <- paste(stats::na.omit(packList[packageName, "Depends"]), na.omit(packList[packageName, "Imports"]), sep = ', ')
    dependingPackages <- lib.packs_str2vec(dependsOn)

    return(dependingPackages)
}
