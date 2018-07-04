
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
#' 3. a zero length char e.g. \code{''} (auto determine)
#'
#'
#' @param packVersion A single named version with package name and it's version like \code{c(dplyr = '>= 0.4.0')}.
#' @param versionList A list of available versions for this package to choose from. It is the list to choose from and check availability. Created with \code{availablePackageVersions}.
#' @param packageName  It is used for clear error handling. It should be the package name it is trying to load so we can mention it when crashing.
#' @param pick.last If a version like \code{>= 0.5} is given and multiple versions exist, a choice needs to be made.
#' By default it will take the same or first higher version (when it exists, just \code{0.5}, which is often the case).
#' This because this is most likely to not change the behaviour of the code. Alternatively, picking the latest version is most
#' likely to be accepted by other packages their dependencies (e.g. if a package that is loaded later on depends on this package but asks for \code{> 0.6}, it will crash).
#' The downside of this is that an update could be a major one, going from \code{0.5} to \code{2.0}, where allot of things can change and code is likely to not work anymore.
#'
chooseVersion <- function(packVersion, versionList, packageName = '', pick.last = FALSE) {

    decidedVersion <- c()

    if (grepl('>=', packVersion)) {
        validVersions  <- numeric_version(versionList) >= numeric_version(gsub('>=\\s?', '', packVersion))
        decidedVersion <- versionList[validVersions]

    } else if (grepl('>', packVersion)) {
        validVersions  <- numeric_version(versionList) >  numeric_version(gsub('>\\s?', '', packVersion))
        decidedVersion <- versionList[validVersions]

    } else if (is.na(packVersion) || nchar(packVersion) == 0) {
        # deal with all scenario's where there are x versions available, and none are defined (e.g. `dplyr = ''`).

        if (length(versionList) > 1) {
            # If multiple versions are available, we need to choose. Check if it is an interactive session.
            # If so, list the options to choose from, if not, throw an error (we can't make that decission automatically).
            if (interactive()){
                choice <- menu(versionList, title = paste0('\nMultiple versions of the package "', packageName,
                                                           '" are found, please select the desired version (0 to cancel):'))
                if (choice == 0) {
                    stop('There is more then one version of the package available but the user could not make a decission.')
                }
                decidedVersion <- versionList[choice]

            } else {
                stop(paste0('There is more then one version of the package "', packageName,
                            '", please define one of the following versions specifically:\n', paste0(versionList, collapse = ',  '),
                            '\nAlternatively, create or update a `vc_override_dependencies.txt` file, located in the version folder\n',
                            'containing an alternative set of dependencies (normaly more specific).\nExample content: "R6 (>= 2.1.2), Rcpp (>= 0.12.3), tibble (>= 1.2)"\n'))
            }
        } else if (length(versionList) < 1) {
            stop(paste0('There is no package "', packageName, '" installed (yet). (requested version: "', packVersion, '")'))
        } else {
            # if there is only 1 version available and none requested
            decidedVersion <- versionList
        }
        # deal with concrete version number (e.g. `dplyr = 0.5.0`):
    } else if (packVersion %in% versionList) {
        # if available, go with it.
        decidedVersion <- packVersion
    }

    if (length(decidedVersion) == 0) {
        stop(sprintf('The requested version "%s" for package "%s" is not installed.', packVersion, packageName))
    }

    # Decide on the version after the (>=, >) coice is made and multiple choices remain.
    # This should only be the case when `>` or `>=` scenarios are dealt with.
    # Other scenarios should be checked (stopped) above.
    # For more details on this choice, read the instruction for the `pick.last` parameter.
    index <- ifelse(pick.last, length(decidedVersion), 1)
    myChoice <- sort(numeric_version(decidedVersion))[index]

    return(as.character(myChoice))
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
#'
getCorrectVersion <- function(packVersion, lib.location, verbose = TRUE, pick.last = FALSE) {
    packageName <- names(packVersion)
    origVersion <- unname(packVersion)

    if (length(packVersion) == 0)
        stop("The length of packVersion cannot be 0, if it is desired to not specify the version specifically, use e.g. `dplyr = ''`.")

    # handle higher then, higher-or-equal, equal to (just a version) or '' (auto determine) version indications:
    packVersionList <- availablePackageVersions(packageName, lib.location)
    packVersion <- chooseVersion(packVersion, versionList = packVersionList, packageName, pick.last)

    # print instructive message:
    if (verbose || !interactive()) {
        if (!is.na(origVersion) && strtrim(origVersion, 1) == '>') {
            cat(sprintf("Version %-6s is chosen  for package '%s'\n", packVersion, packageName))

        } else if (is.na(origVersion) || nchar(origVersion) == 0) {
            cat(sprintf("Only    %-6s is there   for package '%s'\n", packVersion, packageName))

        } else if (packVersion == origVersion)
            cat(sprintf("Exactly %-6s is used    for package '%s'\n", packVersion, packageName))
    }

    return(packVersion)
}


#' Check if a package belongs to the standard R (base) packages.
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
loadedPackageVersion <- function(packageName) {
    # determines the versions of the loaded packages given
    version <- c()
    for (iPackage in packageName) {
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
getOnlineDependencies <- function(packageName){
    packList <- available.packages()
    if (!(packageName %in% packList)) {
        stop(sprintf('Package "%s" is not available on CRAN. Install from source using "install.packages_VC_tarball(path, depends)".', packageName))
    }
    dependsOn <- paste(na.omit(packList[packageName, "Depends"]), na.omit(packList[packageName, "Imports"]), sep = ', ')
    dependingPackages <- cleanupDependencyList(dependsOn)

    return(dependingPackages)
}
