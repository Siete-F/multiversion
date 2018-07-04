
# ============== MAIN FUNCTIONALITY ==============

# -------------- install.packages --------------

#' Install CRAN package into VC library.
#'
#' This function can be used to install a CRAN package in the (default or specified) R_VC_library.
#' For installing packages from source use \code{install.packages_VC_tarball}.
#'
#' @param installPackages Provide a vector of package names. A version cannot be supplied.
#' @param lib.location The folder containing a structure where this package can be installed.
#' The package will first be test installed in a temporary install folder \code{TEMP_install_location}
#' indicated by the \code{R_VC_temp_lib_location} function. If \code{add_to_VC_library} is set to TRUE (the default).
#' the installed package(s) is moved to the destination location automatically.
#' @param add_to_VC_library If TRUE (the default), the installed package(s) is (are) moved to the final destination automatically.
#' Otherwise it is necessary to run \code{convert_to_VC_library()} manually after the installation into the temporary folder finished.
#'
#' @export
#'
install.packages_VC <- function(installPackages = NULL, lib.location = R_VC_library_location(), add_to_VC_library = TRUE) {

    if (any(names(installPackages) != '')) stop('Please provide a vector of names no name-version combinations.')
    if (length(installPackages) == 0) return(invisible())

    install.location <- R_VC_temp_lib_location(lib.location)
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
                library_VC(loadPackages = currentlyLoaded, lib.location = lib.location, quietly = TRUE)
                return(invisible())
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
    invisible(library_VC(loadPackages = currentlyLoaded, lib.location = lib.location, quietly = TRUE))
}


# -------------- install.packages [TARBALL] --------------

#' Install Tarball into VC library.
#'
#' This function can try to install a tarball based on the tarball location and it's dependencies.
#'
#'
#' @param packagePath Provide the complete path to the tarball file that you would like to install.
#' @param dependencies Provide the dependencies like a package version combination: \code{c(dplyr = '>= 0.5', data.table = '', R6 = '0.1.1')}
#' @param lib.location The folder containing a structure where this package needs to be installed in. By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} for this directory.
#' @param execute_with_Rscript When true, it will try to install a tarball using a sepparate Rscipt R instance. This saves you from the hasle to prepare your Rstudio environment to match the one that the tarball requires (incl. dependencies).
#' It will run a script provided with the tarball location and it's dependencies. That script will load \code{RVClibrary} and call \code{install.packages_VC_tarball} directly.
#' @param parse_dependencies If true, the dependencies are expected to be provided in a single string kind of format. This is the case when this function is called by the installation script.
#'
#' @note Hopefully I will be able to implement a method to run this function in a new R instance clear of loaded packages.
#'       \code{install.packages_VC_tarball_with_Rscript} is a sketch to reach meet that wish.
#' @export
#'
install.packages_VC_tarball <- function(packagePath, dependencies, lib.location = R_VC_library_location(), execute_with_Rscript = TRUE, parse_dependencies = FALSE) {

    if (execute_with_Rscript) {
        script_location <- normPath(paste0(path.package('RVClibrary'), '/exec/install.packages_VC_tarball_script.R'))
        Rscript_dir <- normPath(system('where Rscript', intern = T)[1])
        system(sprintf('"%s" --vanilla "%s" "%s" "%s"', Rscript_dir, script_location, packagePath, printPackageList(dependencies, do_return = TRUE)))
        return(invisible())
    }

    install.location <- R_VC_temp_lib_location(lib.location)

    if (parse_dependencies) {dependencies <- cleanupDependencyList(dependencies)}

    currentLibs <- .libPaths()
    .libPaths(.Library)
    on.exit(reset.libPaths(currentLibs), add = TRUE)

    succesfullLoads <- tryLoadPackages_VC(dependencies, lib.location)
    missingDependencies <- names(dependencies)[!(names(dependencies) %in% names(succesfullLoads))]

    # Install all not yet existing dependencies:
    if (length(missingDependencies) > 0) {
        cat('\nI will install missing dependencies and try again...\n\n')
        install.packages_VC(missingDependencies, lib.location = lib.location, add_to_VC_library = TRUE)
        succesfullLoads <- tryLoadPackages_VC(dependencies, lib.location)
        if (all(names(dependencies) %in% names(succesfullLoads))) {cat('\nNow we succeeded, continuing... \n\n')}
    }

    # Check again if all wend well, if not all dependencies are there, reset and abort.
    isSuccesfull <- names(dependencies) %in% names(succesfullLoads)

    if (!all(isSuccesfull | checkIfBasePackage(names(dependencies)))) {
        stop(paste0('\nThe following dependencies were missing and failed installation:', paste(paste0('\n- "', names(dependencies)[!isSuccesfull], '"'), collapse = ''), '\n\n'))
    }

    currentlyLoaded <- detachAll()
    currentlyLoaded <- currentlyLoaded[!names(currentlyLoaded) == 'RVClibrary']

    cat('\n')

    if (interactive()) {
        on.exit({
            cat('\nResetting your loaded packages...\n\n');
            library_VC(loadPackages = currentlyLoaded, lib.location = lib.location, quietly = TRUE)
        }, add = TRUE)
    }

    # If all dependencies could be found/installed: add all recursive dependency libraries to the search path.
    add_package_VC_libPaths(succesfullLoads, lib.location)

    # Install the tarbal!
    install.packages(packagePath, lib = install.location, type = "source", repos = NULL)

    return(invisible())
}


# -------- convert  temp_library  to  VC_library ---------

#' Move normally installed packages to R_VC_library structure.
#'
#' After this conversion is complete and you set the directory (temporarily by using \code{R_VC_library_location(...)}
#' or for eternity by setting the equally named environment variable) you are good to go! You can directly use \code{library_VC}
#' for loading packages. Thanks for using \code{RVClibrary}!! \cr
#' \cr
#' This functionallity is also used for converting installed packages from the temporary installation directory to the final R_VC_library format. \cr
#' \cr
#' Note that it is realy no problem to perform a conversion again, it will only move new versions of already present packages and will never overwrite.
#'
#' @param normalLibrary The temporary library where a package was temporarily installed (having a normal library structure).
#' By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} and appends \code{/TEMP_install_location}.
#' @param VC_library_location The folder containing a structure where all packages in the temp folder must be moved to.
#' By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} for this directory.
#'
#' @example convert_to_VC_library(normalLibrary = Sys.getenv("R_LIBS_USER"), VC_library_location = "C:/REMOVE_ME_example_library")
#'
#' @export
#'
convert_to_VC_library <- function(normalLibrary = R_VC_temp_lib_location(), VC_library_location = R_VC_library_location()) {
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

#' Detach all loaded packages and namespaces.
#'
#' Tries to detach all loaded packages and namespaces. Not always stable (within Rstudio).
#' A restart of Rstudio might be required since it will often hold on to certain namespaces.
#' A proper reset of all libraries is not possible, this is the best we can do. \cr
#' \cr
#' In general, it is possible to create a complete clean environment by clearing your workspace,
#' running \code{detachAll} and then restarting Rstudio. If problems with package loading still persists,
#' then follow the final alternative solution described in the details section of the documentation of \code{library_VC}.
#'
#' @param dryRun If TRUE, lists all packages that will be cleaned up.
#' @param packageList A character vector with the packages to detach/unload. Defaults to all packages.
#'
#' @export
#'
detachAll <- function(dryRun = FALSE, packageList = names(sessionInfo()$otherPkgs)) {
    currentPackageAndVersions <- loadedPackageVersion(packageList)

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

#' Test load package list.
#'
#' Internally used for 'test loading' a list of dependencies when installing a package.
#' Returns a list of succesfull load operations.
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


#' Minor wrapper to reset libraries to the provided state.
#'
reset.libPaths <- function(currentLibs) {
    .libPaths(currentLibs)
    if (interactive()) {cat('The libraries are reset.\n')}
}


# ---------------- additional functions -----------------

#' List the dependencies of a package.
#'
#' Provide a package name (can be without quotes) to show its dependencies.
#' To list all dependencies of the complete library, use the inversed function "\code{dependsOnMe(all)}" with the value 'all'.
#' That function also does not require quotes.
#'
#' @param packageName The (unquoted) package name for which you would like to print the dependencies.
#'
#' @export
#'
dependencies <- function(packageName, lib.location = R_VC_library_location()) {

    # Featuring direct call like: `dependencies(dplyr)`
    packageName = as.character(substitute(packageName))

    if (!is.null(names(packageName))) stop('please only provide the name of the package. all versions will be shown')
    if (!nzchar(packageName, keepNA = TRUE)) return(invisible())

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

        cat(sprintf('%23s : %-8s ', packageName, packVersion))
        if (file.exists(overrideFile)) {cat('(shadowed)| ')} else {cat('          | ')}
        printPackageList(dependingPackages[1:3])
        if (length(dependingPackages) > 3) {
            for (index in 2: ceiling(length(dependingPackages)/3)) {
                cat(strrep(' ', 43), '... '); printPackageList(dependingPackages[(((index-1)*3):(index*3-1))+1])
            }
        }
    }
}


#   ------------------- dependsOnMe -------------------

#' Convert package name/version vector to single string.
#'
#' Used to print a set of package names and their version criteria in a way that \code{cleanupDependencyList()} can parse it again to a package list.
#' This way we can list the dependencies of a function easily and it makes it possible to use it when performing a commandline call.
#'
#' @param x A named character vector with package names/versions.
#'
printPackageList <- function(x, do_return = FALSE) {
    if (!is.null(x)) {x <- x[!is.na(x)]} else {return(cat('\n'))}
    str <- gsub(pat = '\\s\\(\\)', rep = '', sprintf('%s\n', paste(paste(names(x), paste0("(", x, ")")), collapse = '   ')))
    if (do_return) {return(gsub('   ', ', ', str))} else {cat(str)}
}


#' Show the complete library content.
#'
#' Use to print all available packages in the VC_library with all their versions including their dependencies.
#' Simply performs a call to \code{dependsOnMe(all)}.
#' @export
#'
installed.packages_VC <- function() {
    dependsOnMe(all)
}


#' Shows the dependencies of (all or) a certain function(s).
#'
#' Can be called without using quotes like \code{dependsOnMe(dplyr)}. A second very usefull feature would be the \code{dependsOnMe(all)},
#' which will print a list of all packages available with their dependencies. \cr
#' \cr
#' A simple wrapper to do precisely that is "\code{installed.packages_VC}".
#'
#' @param ... All packages and their versions you would like to load e.g. \code{dependsOnMe(DBI = '0.5', assertthat, R6 = '0.6', quietly = TRUE)}.
#' @param checkMyDeps Supports providing a named character vector of packages and their versions instead of the direct input.
#' Use it like this when calling it via another function. It is the shape that is supported by all other functions in this package.
#' e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}
#' @param lib.location The folder containing a structure where this package observe the dependencies from. By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} for this directory.
#'
#' @export
#'
dependsOnMe <- function(..., checkMyDeps = NULL, lib.location = R_VC_library_location()) {

    if (is.null(checkMyDeps)) {
        checkMyDeps <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('lib.location', 'checkMyDeps'))
    }

    if (is.null(names(checkMyDeps))) {checkMyDeps <- setNames('> 0.0.0', checkMyDeps); cat('The latest version,\n')}

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
                cat(sprintf('%23s : %-8s ', packageName, packVersion))
                if (file.exists(overrideFile)) {cat('(shadowed)| ')} else {cat('          | ')}
                printPackageList(dependingPackages[1:3])
                if (length(dependingPackages) > 3) {
                    for (index in 2: ceiling(length(dependingPackages)/3)) {
                        cat(strrep(' ', 45), '... '); printPackageList(dependingPackages[(((index-1)*3):(index*3-1))+1])
                    }
                }
            }
        }
    }
}
