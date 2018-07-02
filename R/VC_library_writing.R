
# ============== MAIN FUNCTIONALITY ==============

# -------------- install.packages --------------

#' Install a CRAN package in the {default or specified} R_VC_library. For installing packages from source use `install.packages_VC_tarball`
#'
#' @param installPackages Provide a vector of package names. A version cannot be supplied.
#' @param lib.location The folder containing a structure where this package can be installed.
#' The package will first be test installed in a temporary install folder `TEMP_install_location`
#' indicated by the `defaultTempInstallPAth` function. If `add_to_VC_library` is set to TRUE {default}
#' the installed package(s) is moved to the destination location automatically.
#' @param add_to_VC_library If set to TRUE {default}, the installed package(s) is moved to the final destination automatically.
#' Otherwise it is necessary to run `convert_to_VC_library()` manually after the installation into the temporary folder finished.
#'
install.packages_VC <- function(installPackages = NULL, lib.location = R_VC_library_location(), add_to_VC_library = TRUE) {
    # installPackages

    if (any(names(installPackages) != '')) stop('Please provide a vector of names no name-version combinations.')
    if (length(installPackages) == 0) return()

    install.location <- defaultTempInstallPath(lib.location)
    # loaded packages
    currentlyLoaded <- detachAll()
    # library paths
    currentLibs <- .libPaths()
    .libPaths(.Library)
    if (!all(currentLibs == .libPaths()) && interactive()) {cat('(in case of crash...) Current libraries were:', paste(paste0('\n- "', currentLibs, '"'), collapse = ''), '\n',
                                                                paste0('[reset:] .libPaths(c("', paste(currentLibs, collapse = '", "'), '"))\n'))}

    # loop through packages to install
    for (iPackage in installPackages) {

        dependsOn <- getOnlineDependencies(iPackage)

        # try loading dependencies, so that thay're skipped during instalation.
        succesfullLoads <- tryLoadPackages_VC(dependsOn, lib.location)

        # If all dependencies were already there, check if the package is already installed.
        if (all(names(dependsOn) %in% names(succesfullLoads) | checkIfBasePackage(names(dependsOn)))) {
            finalPackSucces <- tryLoadPackages_VC(setNames('', iPackage), lib.location)

            if (iPackage %in% names(finalPackSucces)) {
                cat(sprintf('\n - The "%s" package seems to be already installed...\n\n', iPackage))
                reset.libPaths(currentLibs)

                cat('\nResetting your loaded packages.\n')
                library_VC(currentlyLoaded, lib.location = lib.location, quietly = TRUE)
                return()
            }
        }

        # add the succesfully loaded packages to the .libPaths, so the installer knows that they are there.
        add_package_VC_libPaths(succesfullLoads, lib.location)

        detachAll()

        # because the dependencies are on the libPath, only the not present dependencies will be installed.
        # keeping them loaded would raise a popup of Rstudio.
        install.packages(iPackage, lib = install.location, quiet = TRUE)
        reset.libPaths(currentLibs)

        if (add_to_VC_library) {
            convert_to_VC_library(install.location, lib.location)
        }
    }

    cat('\nResetting your loaded packages.\n')
    invisible(library_VC(currentlyLoaded, lib.location = lib.location, quietly = TRUE))
}


# -------------- install.packages [TARBALL] --------------

install.packages_VC_tarball <- function(packagePath, dependencies, lib.location = R_VC_library_location()) {
    # dependencies must be defined like so:
    # c(dplyr = '>= 0.5', data.table = '', R6 = '0.1.1')
    if (!exists('library_VC')) stop('please also source the functions `library_VC` and its associates.')

    install.location <- defaultTempInstallPath(lib.location)

    currentLibs <- .libPaths()
    .libPaths(.Library)
    if (!all(currentLibs == .libPaths())) {cat('(in case of crash...) Current libraries were:', paste(paste0('\n- "', currentLibs, '"'), collapse = ''), '\n',
                                               paste0('[reset:] .libPaths(c("', paste(currentLibs, collapse = '", "'), '"))\n'))}

    # install all non existing dependencies
    succesfullLoads <- tryLoadPackages_VC(dependencies, lib.location)
    missingDependencies <- names(dependencies)[!(names(dependencies) %in% names(succesfullLoads))]
    if (length(missingDependencies) > 0) {
        cat('\nI will install missing dependencies and try again...\n\n')
        install.packages_VC(missingDependencies, lib.location = lib.location, add_to_VC_library = TRUE)
        succesfullLoads <- tryLoadPackages_VC(dependencies, lib.location)
        if (all(names(dependencies) %in% names(succesfullLoads))) cat('\nNow we succeeded, continuing... \n\n')
    }

    currentlyLoaded <- detachAll()
    cat('\n')

    # check again if all wend well, if not all dependencies are there, reset and abort.
    isSuccesfull <- names(dependencies) %in% names(succesfullLoads)
    if (!all(isSuccesfull | checkIfBasePackage(names(dependencies)))) {
        reset.libPaths(currentLibs)

        cat('\nResetting your loaded packages.\n')
        invisible(library_VC(currentlyLoaded, lib.location = lib.location, quietly = TRUE))

        stop(paste0('\nThe following dependencies failed installation:', paste(paste0('\n- "', names(dependencies)[!isSuccesfull], '"'), collapse = ''), '\n\n'))
    }

    # if all dependencies could be found/installed, add all recursive dependency libraries to the search path.
    add_package_VC_libPaths(succesfullLoads, lib.location)

    # install
    install.packages(packagePath, lib = install.location, type = "source", repos = NULL)

    reset.libPaths(currentLibs)

    cat('\nResetting your loaded packages.\n')
    invisible(library_VC(currentlyLoaded, lib.location = lib.location, quietly = TRUE))
}


# -------- convert  temp_lib  to  lib_VC ---------

convert_to_VC_library <- function(normalLibrary = defaultTempInstallPath(), VC_library_location = R_VC_library_location()) {
    # create parallel package library from ordinary library
    # lib1/BH/DESCRIPTION   becomes  lib2/BH/1.60.0-2/BH/DESCRIPTION

    normalLibrary <- gsub(normalLibrary, pat = '\\\\', rep = '/')
    VC_library_location <- gsub(VC_library_location, pat = '\\\\', rep = '/')

    libContent  <- list.files(normalLibrary, all.files = T, recursive = T, no.. = T, full.names = T)
    packageName <- strRemain(paste0(normalLibrary, '/'), paste0('/.*'), libContent)

    uniquePackages <- unique(packageName)

    packageVersions <- c()
    for (iPackage in uniquePackages) {
        packageVersions[grepl(iPackage, x = packageName)] <- as.character(numeric_version(packageDescription(iPackage, normalLibrary)$Version))
    }

    newLocation <- paste(VC_library_location, packageName, packageVersions, gsub(libContent, pat = paste0(normalLibrary, '/'), rep = ''), sep = '/')

    lapply(unique(dirname(newLocation)), dir.create, recursive = TRUE, showWarnings = FALSE)
    succes <- file.copy(libContent, newLocation, overwrite = FALSE)

    cat('\nSuccesfully copied files:\n')
    cat(summary(data.frame(succesfully_copied = packageName[ succes]), maxsum = 80))
    cat('\nFailed copying files (might be already installed or `TEMP_install_location` was not cleaned up, can be done by running `clearTempInstallFolder()`):\n')
    cat(summary(data.frame(failed_copied      = packageName[!succes]), maxsum = 80))
    cat('\n')
}


# ============== DETATCH/ATTACH NAMESPACE ==============

detachAll <- function(dryRun = FALSE, packageList = names(sessionInfo()$otherPkgs)) {
    currentPackageAndVersions <- getLoadedVersion(packageList)

    if (is.null(packageList)) {
        cat('No packages are loaded, nothing to detach.\n')

    } else if (!dryRun) {
        lapply(paste0('package:', packageList), detach, character.only = TRUE, unload = TRUE)
    }

    # also unload all namespaces (not always stable!)
    if (!dryRun) {
        loadedNS = rev(names(sessionInfo()$loadedOnly))
        allLoadedPackages = unique(c(packageList, loadedNS))
        cnt = 1
        while(length(loadedNS) && cnt < 1000) {
            sapply(loadedNS, function(x) {
                result = tryCatch(unloadNamespace(getNamespace(x)), error = function(e) e)
                if(!is(result, "error")) {
                }
            })
            loadedNS = rev(names(sessionInfo()$loadedOnly))
            cnt = cnt +1
        }

        # if while loop never naturally completed
        if(cnt == 1000)
            warning("Unable to unload all namespaces")

        #deal with all DLLs now that the rest is done.
        invisible(sapply(allLoadedPackages, function(x) {
            dll = getLoadedDLLs()[[x]]

            if(!is.null(dll))
                tryCatch(library.dynam.unload(x, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
        }))
    }

    return(if(dryRun) {currentPackageAndVersions} else {invisible(currentPackageAndVersions)})
}


# ------------- [install.packages] helper functions --------------

tryLoadPackages_VC <- function(packages, lib.location) {
    # tries to load packages and returns a vector with all failed load attempts ('yet to be installed' packages)
    loadedPackages <- c()
    for (iDependency in seq_along(packages)) {
        iDependency <- packages[iDependency]
        cat(sprintf('\n - Trying to load version %6s of package %s\n', iDependency, names(iDependency)))

        newlyLoaded <-  tryCatch({
            library_VC(loadPackages = iDependency, lib.location = lib.location, dry.run = TRUE, skipDependencies = names(loadedPackages))
        }, error = function(e) {
            message(e); cat('\n')
        })

        loadedPackages <- append(loadedPackages,newlyLoaded)
    }

    return(loadedPackages)
}


reset.libPaths <- function(currentLibs) {
    .libPaths(currentLibs)
    cat('The libraries are reset.\n')
}


# ---------------- additional functions -----------------

dependencies <- function(packageName, lib.location = R_VC_library_location()) {
    if (!is.null(names(packageName))) stop('please only provide the name of the package. all versions will be shown')
    if (!nzchar(packageName, keepNA = TRUE)) return(invisible())

    # if (is.null(names(packageName))) {packageName <- setNames('> 0.0.0', packageName)}

    # check if input package is realistic. if no version is selected (no name value pair is provided) the oldest version is used.
    # packageName <- setNames(getCorrectVersion(packageName, lib.location, pick.last = TRUE), names(packageName))

    packVersionList <- list.dirs(paste(lib.location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

    for (packVersion in packVersionList) {

        package.location <- paste(lib.location, packageName, packVersion, sep = '/')

        overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
        if (file.exists(overrideFile)) {
            dependingPackages <- cleanupDependencyList(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- packageDescription(packageName, lib.loc = package.location)
            dependingPackages <- cleanupDependencyList(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
        }

        cat(sprintf('package: %s version %s requires ', packageName, packVersion))
        if (file.exists(overrideFile)) {cat('(shadowed) ')}
        cat('dependencies: '); printPackageList(dependingPackages[1:3])
        if (length(dependingPackages) > 3) {
            for (index in 2: ceiling(length(dependingPackages)/3)) {
                cat(strrep(' ', 64)); printPackageList(dependingPackages[(((index-1)*3):(index*3-1))+1])
            }
        }
    }
}


#   ------------------- dependsOnMe -------------------
printPackageList <- function(x) {
    if (!is.null(x)) {x <- x[!is.na(x)]} else {return(cat('\n'))}
    cat(gsub(pat = '\\s\\(\\)', rep = '', sprintf('%s\n', paste(paste(names(x), paste0("(", x, ")")), collapse = ', '))))
}


loadDevtools <- function() {
    library_VC( c(digest = '0.6.9', R6 = '2.2.1', testthat = '1.0.2', stringi = '1.1.5', Rcpp = '0.12.11', backports = '1.1.0', roxygen2 = '6.0.1', RMySQL = '', devtools = '1.13.1'), appendLibPaths = TRUE)
}


dependsOnMe <- function(checkMyDeps, lib.location = R_VC_library_location()) {
    if (is.null(names(checkMyDeps))) {checkMyDeps <- setNames('> 0.0.0', checkMyDeps);cat('The latest version,\n')}

    # check if input package is realistic. if no version is selected (no name value pair is provided) the oldest version is used.
    if (!names(checkMyDeps) == 'all') checkMyDeps <- setNames(getCorrectVersion(checkMyDeps, lib.location, pick.last = TRUE), names(checkMyDeps))


    packageList <- list.dirs(lib.location, recursive = FALSE, full.names = FALSE)
    packageList <- packageList[!packageList %in% c('.git', 'TEMP_install_location')]

    for (packageName in packageList) {
        # cat(packageName, '\n')
        packVersionList <- list.dirs(paste(lib.location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

        for (packVersion in packVersionList) {

            package.location <- paste(lib.location, packageName, packVersion, sep = '/')

            overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
            if (file.exists(overrideFile)) {
                dependingPackages <- cleanupDependencyList(readChar(overrideFile, file.info(overrideFile)$size))
            } else {
                packDesc <- packageDescription(packageName, lib.loc = package.location)
                dependingPackages <- cleanupDependencyList(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
            }

            dependingPackages <- dependingPackages[!checkIfBasePackage(names(dependingPackages))]

            if (!names(checkMyDeps) == 'all') {
                depThatMatters <- dependingPackages[names(dependingPackages) %in% names(checkMyDeps)]
                if (length(depThatMatters) == 0) {next}

                if        (grepl('>=', depThatMatters)) {
                    valid <- numeric_version(checkMyDeps) >= numeric_version(gsub('>=\\s?', '', depThatMatters))
                } else if (grepl('>' , depThatMatters)) {
                    valid <- numeric_version(checkMyDeps) >  numeric_version(gsub('>\\s?', '', depThatMatters))
                } else if (depThatMatters == '') {
                    valid = TRUE
                } else { # if dependency version is just a version (probably only possible when dependency is shadowed)
                    valid <- numeric_version(checkMyDeps) == numeric_version(depThatMatters)
                }
            } else { # in case 'all' is requested.
                valid = TRUE
            }

            if (valid) {
                cat(sprintf('package: %15s version %7s requires ', packageName, packVersion))
                if (file.exists(overrideFile)) {cat('(shadowed) ')}
                cat('dependencies: '); printPackageList(dependingPackages[1:3])
                if (length(dependingPackages) > 3) {
                    for (index in 2: ceiling(length(dependingPackages)/3)) {
                        cat(strrep(' ', 64)); printPackageList(dependingPackages[(((index-1)*3):(index*3-1))+1])
                    }
                }
            }
        }
    }
}
