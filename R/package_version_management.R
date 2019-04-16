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


# -------------- version checking --------------

#' Just checks the R_VC_library folder structure for all available versions installed for a specific package.
#' If no name is provided, an error is returned.
#'
#' @param packageName The name of the package for which all versions must be returned.
#' @param lib.location The folder containing the structure where this package his versions need to be checked.
#'
#' @export
#'
availablePackageVersions <- function(packageName, lib.location = R_VC_library_location()) {
    if (is.na(packageName) || length(packageName) == 0 || nchar(packageName) == 0) {stop('The package name cannot be empty.')}

    available_pack_versions <- list.dirs(paste(lib.location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)
    return(available_pack_versions)
}


#' Choose version based on the version indication, and available versions.
#'
#' Obtains the correct version based on the version instruction provided (e.g. \code{>= 0.5}), the package name and it's available versions.
#' If no compatible version is found between the available versions a suitable error is thrown.
#' All different version indications should be handled in this function, including:
#' 1. a version with \code{>} or \code{>=} indicator
#' 2. a version without \code{>}, so just a version e.g. \code{'0.5.0'} (most specific)
#' 3. a zero length char e.g. \code{''} (auto determine, based on `pick.last`)
#'
#'
#' @param packVersion A single named version value. i.e. A package name and it's version requirement like: \code{c(dplyr = '>= 0.4.0')}.
#' @param versionList A list of available versions for this package to choose from. It is the list to choose from and check availability. Created with \code{availablePackageVersions}.
#' @param packageName  It is used for clear error handling. It should be the package name it is trying to load so we can mention it when crashing.
#' @param pick.last If a version like \code{>= 0.5} is given and multiple versions exist, a choice needs to be made.
#' By default it will take the same or first higher version (when it exists, just \code{0.5}, which is often the case).
#' This because this is most likely to not change the behaviour of the code. Alternatively, picking the latest version is most
#' likely to be accepted by other packages their dependencies (e.g. if a package that is loaded later on depends on this package but asks for \code{> 0.6}, it will crash).
#' The downside of this is that an update could be a major one, going from \code{0.5} to \code{2.0}, where allot of things can have changed and your workable code is at risk.
#'
chooseVersion <- function(packVersion, versionList, packageName = '', pick.last = FALSE, quietly = FALSE) {

    compliantVersions <- c()
    num_ver_list <- numeric_version(versionList)
    if (!(is.na(packVersion) || nchar(packVersion) == 0)) {
        first_next_major <- package_version(sprintf('%1.0f.0.0', package_version(bareVersion(packVersion))$major+1))
    }

    # It is possible that this package is already loaded as a dependency of another package.
    if (isNamespaceLoaded(packageName)) {
        if (.Platform$GUI == "RStudio" && packageName == 'yaml') {
            # 'yaml' seems to be a strange case. It is loaded by Rstudio from your local library, which is likely a different
            # version than is present in the VC library. (in my case 2.1.13, where VC version 2.1.14 is present).
            # I did find a way to unload it so that we can also alter this package his version, even when it is always loaded automatically on startup.
            unloadNamespace(asNamespace('yaml'))
            # Try again after unload is successful.
            return(chooseVersion(packVersion, versionList, packageName = 'yaml', pick.last = FALSE, quietly = FALSE))
        }
        alreadyLoadedVersion <- loadedPackageVersion(packageName)
        if (!isVersionCompatible(packVersion, alreadyLoadedVersion)) {
            # ERROR
            error_packageAlreadyLoaded(packageName, packVersion, alreadyLoadedVersion)
        }
        # Check if the package version that was already loaded is available in the R_VC_library directory.
        if (!numeric_version(alreadyLoadedVersion) %in% num_ver_list) {
            # The most likely location is your default library. This method is build in because `system.file` showed some unpredictable behaviour.
            # It makes use of `.getNamespaceInfo(asNamespace(pkg), "path")` on the background, which found my repository path, not the installed version.
            potential_locations <- file.path(.libPaths(), packageName)
            loaded_pack_loc <- potential_locations[file.exists(file.path(potential_locations, "DESCRIPTION"))]
            if (length(loaded_pack_loc) == 0) {
                loaded_pack_loc <- system.file(package = packageName)
            }
            stop(sprintf(paste('An already loaded package "%s" (version: %s) is compatible with the requested version, but',
                               '\nwas not loaded from the R_VC_library directory and not available (instead from: "%s").',
                               '\nRequested was %s (available versions: %s).',
                               '\nIf this problem persists, try cleaning the RStudio catch folder as described in DETAILS of ?library_VC.',
                               '\nWe will not try to detach since that could cause unexpected behaviour.',
                               '\nPlease detach it manually (e.g. `detachAll(packageList = \'%s\')`, where %s are depending on it)\n\n'),
                         packageName, alreadyLoadedVersion, loaded_pack_loc, packVersion,
                         paste0(collapse = ', ', "'", as.character(num_ver_list), "'"), packageName,
                         paste(collapse = ', ', paste0("'", getNamespaceUsers(packageName), "'"))))
        }
        return(alreadyLoadedVersion)
    }

    if (grepl('>=', packVersion)) {
        validVersions  <- num_ver_list >= numeric_version(bareVersion(packVersion)) & num_ver_list < first_next_major
        if (!any(validVersions)) {
            validVersions  <- num_ver_list > numeric_version(bareVersion(packVersion))
            if (any(validVersions) & !quietly) warning(sprintf('For package "%s" the lowest optional version is a major release higher. Requested: "%s", first option "%s".', packageName, packVersion, versionList[validVersions][[1]]))
        }
        compliantVersions <- versionList[validVersions]

    } else if (grepl('>', packVersion)) {
        validVersions  <- num_ver_list >  numeric_version(bareVersion(packVersion)) & num_ver_list < first_next_major
        # If no valid versions found within this major release, look for all versions above the minimal version.
        if (!any(validVersions)) {
            validVersions  <- num_ver_list > numeric_version(bareVersion(packVersion))
            if (any(validVersions) & !quietly) warning(sprintf('For package "%s" the lowest optional version is a major release higher. Requested: "%s", first option "%s".', packageName, packVersion, versionList[validVersions][[1]]))
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

    # Decide on the version after the (>=, >) coice is made and multiple choices remain.
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
#'
#' @param condition A version indication like `>= 4.5.1` or `2.3.4` or `> 1.2.3` or `''` (empty) or `NA`.
#' @param version A version number like `1.2.3`.
#'
#' @export
#'
isVersionCompatible <- function(condition, version) {
    # If no reference was supplied, all conditions are acceptable.
    if (is.null(version) || is.na(version)) return(TRUE)

    # If '', NA or an equal version as the existing version is returned, pass.
    if (is.na(condition) || nchar(condition) == 0) {
        return(TRUE)
    } else if (condition == version) {
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
        # normally a case of (condition != with_version) where condition is an exact number.
        return(FALSE)
    }
}


#' Choose correct package version, and print decission.
#'
#' Obtains the correct version based on the version instruction provided (e.g. \code{>= 0.5}). It will print which version is chosen if `verbose = TRUE`.
#' if no compatible version is found between the available versions, the function 'chooseVersion' will return an error to notify you.
#'
#' @param packVersion A named character vector with package names and their version indication (e.g. \code{c(dplyr = '>= 0.4.0', ggplot = '')}).
#' @param lib.location The location of the R_VC_library folder.
#' @param verbose if TRUE, it will print the choices it makes. If the session is not interactive, or verbose = FALSE, nothing will be printed.
#' @param pick.last If a version like\code{>= 0.5} is given and multiple versions exist, a choice needs to be made.
#' By default it will take the first higher version (when it exists, just\code{0.5}, which is often the case).
#' This because this is most likely to not change the behaviour of the code. Picking the latest version is most
#' compatible with matching other packages their dependencies (e.g. if a later package depends on this package but asks for\code{> 0.6}, it will crash).
#' The downside of this is that an update could be a major one, going from\code{0.5} to\code{2.0}, where allot of things can change and code is likely to not work anymore.
#' @param quietly If FALSE, the default, will return warnings if the loaded package is a major release higher then the package that was requested.
#'
getCorrectVersion <- function(packVersion, lib.location, verbose = TRUE, pick.last = FALSE, quietly = FALSE) {
    packageName <- names(packVersion)
    origVersion <- unname(packVersion)

    if (length(packVersion) == 0)
        stop("The length of packVersion cannot be 0, if it is desired to not specify the version specifically, use e.g. `dplyr = ''`.")

    # handle higher then, higher-or-equal, equal to (just a version) or '' (auto determine) version indications:
    packVersionList <- availablePackageVersions(packageName, lib.location)
    packVersion <- chooseVersion(packVersion, versionList = packVersionList, packageName, pick.last = pick.last, quietly = quietly)

    # print instructive message:
    if (verbose || !interactive()) {
        if (!is.na(origVersion) && strtrim(origVersion, 1) == '>') {
            cat(sprintf("Version %-7s is chosen  for package '%s'\n", packVersion, packageName))

        } else if (is.na(origVersion) || nchar(origVersion) == 0) {
            cat(sprintf("Only    %-7s is there   for package '%s'\n", packVersion, packageName))

        } else if (packVersion == origVersion)
            cat(sprintf("Exactly %-7s is used    for package '%s'\n", packVersion, packageName))
    }

    return(packVersion)
}


#' Check if a package belongs to the standard R (base) packages.
#'
#' To check if the package is a basepackage, we look it up among all packages in the \code{.Library} directory (\code{list.dirs(.Library, full.names = FALSE, recursive = FALSE)}).
#' We cannot version control packages which are located in this library since the \code{.Library} will always be added to the \code{.libPaths}.
#' For base packages, this is acceptable, but it apears that this directory is not always as clean as we would wish.
#' Because of this reason, we do not check the more widely accepted \code{rownames(installed.packages(priority="base"))}.
#'
#' @param packageName The package name to check.
#'
#' @export
#'
checkIfBasePackage <- function(packageName) {
    # Featuring direct call like: `checkIfBasePackage(stats)`
    # This can't be used interactively, when providing a variable with a name it would use the var name as name.
    # packageName = as.character(substitute(packageName))

    basePackages <- list.dirs(.Library, full.names = FALSE, recursive = FALSE)
    return(packageName %in% c(basePackages, 'R'))
}


#' Check the versions of an already loaded package.
#'
#' @param packageName The name or a vector of names of the packages for which to obtain the version.
#'
#' @export
#'
loadedPackageVersion <- function(packageNames, dont_exclude_not_loaded = F) {
    if (!dont_exclude_not_loaded) {
        packageNames <- packageNames[packageNames %in% unique(c(names(sessionInfo()$otherPkgs), names(sessionInfo()$loadedOnly)))]
    }

    # determines the versions of the loaded packages given
    version <- c()
    for (iPackage in packageNames) {
        version <- append(version, setNames(as.character(packageVersion(iPackage)), iPackage))
    }
    return(version)
}


#' Check if an package belongs to the standard R packages
#'
#' @param packageName The package name to check.
#'
#' @export
#'
getOnlineDependencies <- function(packageName, cran_url = 'https://cran.rstudio.com/'){
    packList <- available.packages(repos = cran_url)
    if (!(packageName %in% packList)) {
        stop(sprintf('Package "%s" is not available on CRAN. Install from source using "install.packages_VC_tarball(path, depends)".', packageName))
    }
    dependsOn <- paste(na.omit(packList[packageName, "Depends"]), na.omit(packList[packageName, "Imports"]), sep = ', ')
    dependingPackages <- parse_dependency_string(dependsOn)

    return(dependingPackages)
}
