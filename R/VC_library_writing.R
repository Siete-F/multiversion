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


# ============== MAIN FUNCTIONALITY ==============

# -------------- install.packages --------------

#' Install CRAN package into VC library.
#'
#' This function can be used to install a CRAN package in the (default or specified) multiversion library.
#' For installing packages from source use \code{lib.install_tarball}.
#'
#' @param installPackages Provide a vector of package names. A version cannot be supplied.
#' @param lib_location The folder where this package can be installed.
#' The package will first be installed in a temporary install folder \code{yourLib/TEMP_install_location}
#' indicated by the \code{\link{lib.location_install_dir}} function. If \code{install_temporarily} is set to FALSE (the default).
#' the installed package(s) is moved to the destination location automatically.
#' @param install_temporarily If FALSE, the installed package(s) is (are) moved to the final destination automatically.
#' Otherwise it is necessary to run \code{\link{lib.convert}()} manually after the installation into the temporary folder finished. To clean up the temporary folder, run \code{lib.clean_install_dir()}.
#' When \code{overwrite_this_package} is TRUE, and \code{install_temporarily} is not provided, it will be set to FALSE automatically.
#' Installing a package temporarily gives you the oppertunity to test the package before adding it to the multiversion library structure.
#' Loading packages including those in the temporary library (\code{\link{lib.location_install_dir}()}) can be done using: \code{\link{lib.load}(..., also_load_from_temp_lib = TRUE)}.
#' @param execute_with_Rscript When TRUE (the default), it will try to install the package using a sepparate Rscipt R instance.
#' This simplifies installing and leaves changing loaded packages etc. to another R instance which we can kill :).
#' It will run a script which it provides with your list of packages to install.
#' That script will load this \code{multiversion} package and call \code{\link{lib.install}()} directly.
#' @param cran_url Will be passed trough to the install.packages command.
#'
#' @export
#'
lib.install <- function(installPackages = NULL, lib_location = lib.location(), install_temporarily = FALSE,
                        overwrite_this_package = FALSE, execute_with_Rscript = TRUE, verbose = FALSE, cran_url = "http://cran.us.r-project.org") {

    if (any(names(installPackages) != '')) stop('Please provide a vector of names, no name-version combinations.')
    if (length(installPackages) == 0) return(invisible())

    # If executing using Rscript, this block is run and calls an R script which will run all except this block.
    if (execute_with_Rscript) {
        temp_install_dir <- lib.location_install_dir()
        curr_packs <- list.dirs(temp_install_dir, full.names = F, rec = F)

        Rscript_dir <- normPath(system('where Rscript', intern = T)[1])

        if (grepl('Could not find files for the given pattern(s)', Rscript_dir)) {
            stop('Please make sure `where Rscript` results in one or more valid paths. First one is used.')
        }

        multiversion_location <- lib.my_location()
        script_location <- normPath(paste0(multiversion_location, '/exec/lib.install_script.R'))

        status <- system(sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s" "%s"', Rscript_dir, script_location, lib_location, paste(collapse = ',', installPackages), multiversion_location, as.character(overwrite_this_package), as.character(verbose)))

        new_packs <- list.dirs(temp_install_dir, full.names = F, rec = F)
        added_packages <- setdiff(new_packs, curr_packs)

        message(sprintf('>> Instalation attempt finished with status %s, added were: %s <<\n', as.character(status), paste(collapse = ', ', added_packages)))
        if (!install_temporarily & status == 0) {
            lib.convert(lib_location        = lib_location,
                        force_overwrite     = overwrite_this_package,
                        packages_to_convert = added_packages)
        }
        return(invisible())
    }

    install.location <- normPath(lib.location_install_dir(lib_location))

    # loaded packages
    nms <- names(sessionInfo()$otherPkgs)
    currentlyLoaded <- detachAll(packageList = nms[!nms %in% c('RVClibrary', 'multiversion')])

    # The following should only be executed once.
    if (sum(grepl('^lib.install', sapply(sys.calls(), function(x) {x[[1]]}))) <= 1) {
        if (!nzchar(Sys.which('make'))) {
            stop(paste('Please make sure the `make.exe` application of `Rtools` can be',
                       '\nfound so that packages can be compiled from source if necessary!',
                       '\nAdd e.g. `C:/Rtools/bin` to the PATH env var if you have the program already installed',
                       '\nor download Rtools (and check `Add paths` during install, and restart R) from "https://cran.r-project.org/bin/windows/Rtools/".'))
        }
        # Just took some
        if (any(!nzchar(Sys.which(c('aspell', 'basename', 'cat', 'cp'))))) {
            stop(sprintf(paste('%sAdd e.g. `C:/Rtools/bin` to the PATH env var if you have Rtools already installed',
                       '\nor download Rtools (and check `Add paths` during install, and restart R) from "https://cran.r-project.org/bin/windows/Rtools/".'),
                       ifelse(nzchar(Sys.getenv('MAKE')), '',
                              'The MAKE variable is specified, but this is not sufficient for an Rscript instance to compile from source properly. ')))
        }
        if (!grepl('\\$\\(WIN\\)', Sys.getenv('BINPREF'))) {
            stop(sprintf(paste0('Please make sure you also have the BINPREF environment variable set properly.',
                               '\nCurrent value: "%s", expected value should be like: "C:/Rtools/mingw_$(WIN)/bin/",',
                               '\nwhere "$(WIN)" is set by R to either "32" or "64" depending on the target.',
                               '\nIf your PATH and BINPREF variables are set correctly, we should be ready',
                               '\nfor compiling from source (being able to install the latest pacakges).'), Sys.getenv('BINPREF')))
        }

        if (interactive()) {
            on.exit({
                cat('\nResetting your loaded packages.\n')
                invisible(lib.load(loadPackages = currentlyLoaded, lib_location = lib_location, quietly = TRUE))
            }, add = TRUE)
        }
        prev_setting <- getOption("install.packages.compile.from.source")
        options(install.packages.compile.from.source = "always")

        # library paths
        currentLibs <- .libPaths()
        .libPaths(.Library)

        on.exit({
            options(install.packages.compile.from.source = prev_setting)
            .libPaths(currentLibs)
        }, add = TRUE)
    }



    # loop through packages to install
    for (iPackage in installPackages) {

        dependsOn <- lib.dependencies_online(iPackage, cran_url = cran_url)

        # try loading dependencies, so that they're skipped during instalation.
        succesfullLoads <- lib.testload(dependsOn, lib_location, pick.last = TRUE, msg_str = paste0('"', iPackage, '" his dependencies'), verbose = verbose)

        failedLoads <- !names(dependsOn) %in% names(succesfullLoads) & !lib.is_basepackage(names(dependsOn))
        failedLoads <- names(dependsOn)[failedLoads]

        if (length(failedLoads) > 0) {
            cat(sprintf('\n - We will install for package "%s" the missing dependencies: %s.\n\n',
                        iPackage, paste0(collapse = ',', '"', failedLoads, '"')))

            lib.install(installPackages = failedLoads, lib_location = lib_location, install_temporarily = TRUE, execute_with_Rscript = execute_with_Rscript, verbose = verbose)

            # When dependencies have been installed, try loading them again to create a more complete list of available
            succesfullLoads <- lib.testload(dependsOn, lib_location, pick.last = TRUE, verbose = verbose)
        }


        # If all dependencies were already there, check if the package is already installed.
        # When overwriting this package (or installing the latest version) we are not interested in knowing if the package already exists.
        if (!overwrite_this_package && all(names(dependsOn) %in% names(succesfullLoads) | lib.is_basepackage(names(dependsOn)))) {
            finalPackSucces <- lib.testload(setNames('', iPackage), lib_location, pick.last = TRUE, verbose = verbose, msg_str = '')

            if (iPackage %in% names(finalPackSucces)) {
                cat(sprintf('\n - The "%s" package seems to be already installed...\n\n', iPackage))
                next
            }
        }

        # add the succesfully loaded packages to the .libPaths, so the installer knows that they are there.
        lib.add_libPaths(succesfullLoads, lib_location, additional_lib_paths = install.location)

        message('\nINSTALLING: ', iPackage)
        # because the dependencies are on the libPath, only the not present dependencies will be installed.
        # keeping them loaded would raise a popup of Rstudio.
        with_build_tools(install.packages(iPackage, lib = install.location, quiet = TRUE, repos = cran_url))
        message('INSTALLING DONE.')

        if (!install_temporarily) {
            lib.convert(install.location, lib_location, force_overwrite = overwrite_this_package)
        }
    }

}


# Borrowed from: https://github.com/rstudio/packrat/blob/master/R/install.R
with_build_tools <- function(code) {
    check <- getOption("buildtools.check", NULL)
    if (!is.null(check)) {
        if (check("Installing R packages from source")) {
            with <- getOption("buildtools.with", NULL)
            if (!is.null(with))
                with(code)
            else
                force(code)
        }
    }
    else {
        force(code)
    }
}


# -------------- install.packages [TARBALL] --------------

#' Install Tarball into VC library.
#'
#' This function can try to install a tarball based on the tarball location and it's dependencies.
#'
#'
#' @param packagePath Provide the complete path to the tarball file that you would like to install.
#' @param dependencies Provide the dependencies like a package version combination: \code{c(dplyr = '>= 0.5', data.table = '', R6 = '0.1.1')}
#' @param lib_location The folder containing a structure where this package needs to be installed in. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#' @param install_temporarily If FALSE, the installed package(s) is (are) moved to the final destination automatically.
#' Otherwise it is necessary to run \code{\link{lib.convert}()} manually after the installation into the temporary folder finished. To clean up the temporary folder, run \code{\link{lib.clean_install_dir}()}.
#' Installing a package temporarily gives you the oppertunity to test the package before adding it to the multiversion library structure.
#' Loading packages including those in the temporary library (\code{\link{lib.location_install_dir}()}) can be done using: \code{\link{lib.load}(..., also_load_from_temp_lib = TRUE)}.
#' @param overwrite_this_package If TRUE, the installed package is added and overwrites the existing package.
#' @param execute_with_Rscript When true, it will try to install a tarball using a sepparate Rscipt R instance. This saves you from the hasle to prepare your Rstudio environment to match the one that the tarball requires (incl. dependencies).
#' It will run a script provided with the tarball location and it's dependencies. That script will load \code{multiversion} and call \code{\link{lib.install_tarball}} directly.
#'
#' @note Hopefully I will be able to implement a method to run this function in a new R instance clear of loaded packages.
#'       \code{lib.install_tarball_with_Rscript} is a sketch to reach meet that wish.
#' @export
#'
lib.install_tarball <- function(packagePath, dependencies, lib_location = lib.location(),
                                install_temporarily = FALSE, overwrite_this_package = FALSE,
                                execute_with_Rscript = TRUE) {

    if (execute_with_Rscript) {
        Rscript_dir <- normPath(system('where Rscript', intern = T)[1])
        if (grepl('Could not find files for the given pattern(s)', Rscript_dir)) {stop('Please make sure `where Rscript` results in one or more valid paths. First one is used.')}
        multiversion_location <- lib.my_location()
        script_location <- normPath(paste0(multiversion_location, '/exec/lib.install_tarball_script.R'))
        status <- system(sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s"', Rscript_dir, script_location, lib_location, packagePath, lib.packs_vec2str(dependencies, do_return = TRUE), multiversion_location))
        if (!install_temporarily & status == 0) {lib.convert(lib_location = lib_location, force_overwrite = overwrite_this_package)}
        return(invisible())
    }

    install.location <- lib.location_install_dir(lib_location)

    currentLibs <- .libPaths()
    .libPaths(.Library)
    on.exit(reset.libPaths(currentLibs), add = TRUE)

    succesfullLoads <- lib.testload(dependencies, lib_location, msg_str = paste0('"', basename(packagePath), '" his dependencies'))
    missingdependencies <- names(dependencies)[!(names(dependencies) %in% names(succesfullLoads))]

    # Install all not yet existing dependencies:
    if (length(missingdependencies) > 0) {
        cat('\nI will install missing dependencies and try again...\n\n')
        lib.install(missingdependencies, lib_location = lib_location, install_temporarily = install_temporarily)
        succesfullLoads <- lib.testload(dependencies, lib_location, msg_str = paste0('"', basename(packagePath), '" his dependencies (second try)'))
        if (all(names(dependencies) %in% names(succesfullLoads))) {cat('\nNow we succeeded, continuing... \n\n')}
    }

    # Check again if all wend well, if not all dependencies are there, reset and abort.
    isSuccesfull <- names(dependencies) %in% names(succesfullLoads)

    if (!all(isSuccesfull | lib.is_basepackage(names(dependencies)))) {
        stop(paste0('\nThe following dependencies were missing and failed installation:', paste(paste0('\n- "', names(dependencies)[!isSuccesfull], '"'), collapse = ''), '\n\n'))
    }

    nms <- names(sessionInfo()$otherPkgs)
    currentlyLoaded <- detachAll(packageList = nms[!nms %in% c('RVClibrary', 'multiversion')])

    cat('\n')

    if (interactive() && length(sys.calls()) == 1) {
        on.exit({
            cat('\nResetting your loaded packages...\n\n');
            lib.load(loadPackages = currentlyLoaded, lib_location = lib_location, quietly = TRUE)
        }, add = TRUE)
    }

    # If all dependencies could be found/installed: add all recursive dependency libraries to the search path.
    lib.add_libPaths(succesfullLoads, lib_location, additional_lib_paths = install.location)

    # Install the tarbal!
    install.packages(packagePath, lib = install.location, type = "source", repos = NULL)
    cat('\n')

    if (!install_temporarily) {lib.convert(install.location, lib_location, force_overwrite = overwrite_this_package)}

    return(invisible())
}


# -------- convert  temp_library  to  VC_library ---------

#' Move normally installed packages to R_VC_library structure.
#'
#' After this conversion is complete and you set the directory (temporarily by using \code{lib.location(...)}
#' or for eternity by setting the equally named environment variable) you are good to go! You can directly use \code{lib.load}
#' for loading packages. Thanks for using \code{multiversion}!! \cr
#' \cr
#' This function creates the VC library structure by moving normaly installed packages to a more parallel oriented library structure.
#' lib1/BH/DESCRIPTION   becomes  lib2/BH/1.60.0-2/BH/DESCRIPTION
#' \cr
#' This functionallity is also used for converting installed packages from the temporary installation directory to the final R_VC_library format. \cr
#' \cr
#' Note that it is realy no problem to perform a conversion again, it will only move new versions of already present packages and will never overwrite.
#' To continue with a clean Temp folder, run \code{lib.clean_install_dir()}, it will remove the folder.
#'
#' @param normalLibrary The temporary library where a package is temporarily installed (having a normal library structure).
#' By default, the path is generated using \code{lib.location_install_dir()} on the \code{lib_location} that is provided which appends \code{/TEMP_install_location}.
#' @param lib_location The folder containing a structure where all packages in the temp folder must be moved to.
#' By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#' @param force_overwrite If you are experimenting and you would like to overwrite the newly installed package.
#' Normally only desired when the package you are experimenting with is a self maintained package and you are sure you increased the version to a new one.
#'
#' @examples
#' # As an experiment (or when getting started) you could run this with your complete standard library (not your base library).
#' lib.convert(normalLibrary = Sys.getenv("R_LIBS_USER"),
#'             lib_location  = "C:/REMOVE_ME_example_library")
#'
#' # Just running it will use the VC library defined by the environment variable and look inside for the Temp folder to use.
#' lib.convert()
#'
#' # It is sufficient to only provide the lib_location input, it will look for the temporary folder within automatically.
#' lib.convert(lib_location = "M:/R_VC_library")
#'
#' @export
#'
lib.convert <- function(installLibrary      = lib.location_install_dir(lib_location),
                        lib_location        = lib.location(),
                        force_overwrite     = FALSE,
                        packages_to_convert = c()) {

    installLibrary <- normPath(installLibrary)
    lib_location   <- normPath(lib_location)

    libContent   <- list.files(installLibrary, all.files = T, recursive = T, no.. = T, full.names = T)
    packageNames <- strRemain(paste0(installLibrary, '/'), '/.*', libContent)

    uniquePackages <- unique(packageNames)

    # If no specific packages are provided, convert them all to the multiversion library.
    if (length(packages_to_convert) == 0) {packages_to_convert <- uniquePackages}
    uniquePackages <- uniquePackages[uniquePackages %in% packages_to_convert]

    packageVersions <- c()
    for (iPackage in uniquePackages) {
        packageVersions[grepl(iPackage, packageNames)] <- as.character(numeric_version(packageDescription(iPackage, lib.loc = installLibrary)$Version))
    }

    newLocation <- paste(lib_location, packageNames, packageVersions, gsub(libContent, pat = paste0(installLibrary, '/'), rep = ''), sep = '/')

    lapply(unique(dirname(newLocation)), dir.create, recursive = TRUE, showWarnings = FALSE)
    succes <- file.copy(libContent, newLocation, overwrite = force_overwrite)

    cat('\nSuccesfully copied files:\n')
    cat(summary(data.frame(succesfully_copied = packageNames[ succes]), maxsum = 80))
    cat('\nFailed copying files (might be already installed or `TEMP_install_location` was not cleaned up, can be done by running `lib.clean_install_dir()`):\n')
    cat(summary(data.frame(failed_copied      = packageNames[!succes]), maxsum = 80))
    cat('\n')
}


# ============== DETACH/ATTACH NAMESPACE ==============

#' Detach all loaded packages and namespaces.
#'
#' Tries to detach all loaded packages and namespaces. Not always stable (within Rstudio).
#' A restart of Rstudio might be required since it will often hold on to certain namespaces.
#' A proper reset of all libraries is not possible, this is the best we can do. \cr
#' \cr
#' In general, it is possible to create a complete clean environment by clearing your workspace,
#' running \code{detachAll} and then restarting Rstudio. If problems with package loading still persists,
#' then follow the final alternative solution described in the details section of the documentation of \code{lib.load}.
#'
#' @param dryRun If TRUE, lists all packages that will be cleaned up.
#' @param packageList A character vector with the packages to detach/unload. Defaults to all packages.
#'
#' @export
#'
detachAll <- function(reload_multiversion = FALSE, dryRun = FALSE, packageList = names(sessionInfo()$otherPkgs)) {
    currentPackageAndVersions <- lib.package_version_loaded(packageList)

    if (is.null(packageList)) {
        cat('No packages are loaded, nothing to detach.\n')

    } else if (!dryRun) {
        lapply(sprintf('package:%s', packageList), detach, character.only = TRUE, unload = TRUE)
    }

    # also unload all namespaces (not always stable!)
    if (!dryRun) {
        loadedNS = rev(names(sessionInfo()$loadedOnly))
        allLoadedPackages = unique(c(packageList, loadedNS))
        cnt = 1
        while(length(loadedNS) && cnt < 1000) {
            sapply(loadedNS, function(x) {
                result = tryCatch(unloadNamespace(getNamespace(x)), error = function(e) e)
                # if(!is(result, "error")) {
                # }
            })
            loadedNS = rev(names(sessionInfo()$loadedOnly))
            cnt = cnt + 1
        }

        # if while loop never naturally completed
        if(cnt == 1000)
            warning("Unable to unload all namespaces")

        # deal with all DLLs now that the rest is done.
        invisible(sapply(allLoadedPackages, function(x) {
            dll = getLoadedDLLs()[[x]]

            if(!is.null(dll))
                tryCatch(library.dynam.unload(x, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
        }))
    }

    if (reload_multiversion) {
        library(multiversion, lib.loc = Sys.getenv("R_LIBS_USER"))
    }

    if (!'package:multiversion' %in% search()) {cat('Note that the package "multiversion" is also detached:\n>  library(multiversion, lib.loc = Sys.getenv("R_LIBS_USER"))\n')}

    return(if(dryRun) {currentPackageAndVersions} else {invisible(currentPackageAndVersions)})
}


# ------------- [install.packages] helper functions --------------

#' Test load package list.
#'
#' Internally used for 'test loading' a list of dependencies when installing a package.
#' Returns a list of succesfull load operations.
#'
#' @param packages A named character vector of package/version names. None are really loaded, `lib.load` is used with the `dry.run` turned on.
#' @param lib_location The folder containing a structure where we will try to find our required packages in.
#' By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#' @param pick.last Changes the way a decision is made. In the scenario where a dependency of \code{>} or \code{>=} is defined, multiple versions may be available to choose from. By default, the lowest compliant version is chosen. Setting this to TRUE will choose the highest version.
#' @param verbose If TRUE, the default, will show all \code{lib.load} messages.
#' @param msg_str Can be either logical FALSE, or a character string which will be (the %s) part of the printed string ' - Trying to load %s: ...'.
#' Defaults to 'packages', making the sentence: ' - Trying to load packages: lattice, RColorBrewer, grid'
#'
lib.testload <- function(packages, lib_location = lib.location(), pick.last = FALSE, verbose = TRUE, msg_str = 'packages') {
    # tries to load packages and returns a vector with all successful load attempts ('yet to be installed' packages)

    # Handle 'verbose' options
    execute <- if (is.logical(verbose) && verbose) {
        function(x) {x}
    } else {
        verbose <- FALSE  # Any value is allowed to get here, so I force a FALSE for further code to work.
        function(x) {invisible(capture.output(x)); return(x)}
    }

    basePacks <- lib.is_basepackage(packages)
    loadedPackages <- packages[basePacks]
    packages       <- packages[!basePacks]

    # Return directly when no 'dependencies' or packages are requested to load.
    if (length(packages) == 0) {return(loadedPackages)}

    if ((!verbose && missing(msg_str)) || is.character(msg_str) && length(msg_str) == 1) {
        cat(sprintf('\n - Trying to load %s: %s\n', msg_str, lib.packs_vec2str(packages, do_return = TRUE)))
    }

    for (iDependency in seq_along(packages)) {
        iDependency <- packages[iDependency]

        # TryCatch returns empty when error is caught.
        newlyLoaded <-  tryCatch({
            suppressWarnings(execute(lib.load(loadPackages = iDependency, lib_location = lib_location, dry.run = TRUE, also_load_from_temp_lib = TRUE,
                                              skipDependencies = names(loadedPackages), pick.last = pick.last, quietly = !verbose)))
        }, error = function(e) {
            if (verbose) {
                # `message` appends a linefeed itself.
                message(sprintf('When loading package "%s": %s', names(iDependency), gsub('\n$', '', gsub('Error ', '', e))))
            }
        })

        loadedPackages <- append(loadedPackages, newlyLoaded)
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
#' To list all dependencies of the complete library, use the inversed function "\code{lib.dependsOnMe(all)}" with the value 'all'.
#' That function also does not require quotes.
#'
#' @param packageName The (unquoted) package name for which you would like to print the dependencies.
#' @param do_print If true (default), prints the dependencies. In both cases, the dependencies are returned invisibly.
#' @param character.only If TRUE, (FALSE by default), the package names can be provided as character vector. Otherwise, direct unquoted package names are supported.
#' @param lib_location The folder containing a structure where this function observe the dependencies from. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#'
#' @examples
#' lib.dependencies(dplyr)
#' lib.dependencies('devtools', character.only = T)
#' devtools_deps <- lib.dependencies(devtools, do_print = F)
#'
#' @export
#'
lib.dependencies <- function(packageName, do_print = TRUE, character.only = FALSE, lib_location = lib.location()) {

    # Featuring direct call like: `lib.dependencies(dplyr)`
    if (!character.only) {
        packageName = as.character(substitute(packageName))
    }

    if (!is.null(names(packageName))) stop('Please only provide the name of the package. All versions will be shown.')
    if (!nzchar(packageName, keepNA = TRUE)) return(invisible())

    packVersionList <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

    listed_dependencies <- list()
    for (packVersion in packVersionList) {

        package.location <- paste(lib_location, packageName, packVersion, sep = '/')

        overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
        if (file.exists(overrideFile)) {
            dependingPackages <- lib.packs_str2vec(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- packageDescription(packageName, lib.loc = package.location)
            dependingPackages <- lib.packs_str2vec(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
        }
        listed_dependencies[[packVersion]] <- dependingPackages

        if (do_print) {
            cat(sprintf('%23s : %-8s ', packageName, packVersion))
            if (file.exists(overrideFile)) {cat('(shadowed)| ')} else {cat('          | ')}
            lib.packs_vec2str(dependingPackages[1:3])
            if (length(dependingPackages) > 3) {
                for (index in 2: ceiling(length(dependingPackages)/3)) {
                    cat(strrep(' ', 43), '... '); lib.packs_vec2str(dependingPackages[(((index-1)*3):(index*3-1))+1])
                }
            }
        }
    }
    return(invisible(listed_dependencies))
}


#   ------------------- lib.dependsOnMe -------------------

#' Convert package name/version vector to single string.
#'
#' Used to print a set of package names and their version criteria in a way that \code{lib.packs_str2vec()} can parse it again to a package vector.
#' This way we can list the dependencies of a function easily and support better commandline interaction.
#'
#' @param x A named character vector with package names/versions. `c(dplyr = '>= 1.5.0', data.table = '')`
#' @param do_return If FALSE (the default) the package sting is printed, if TRUE, it is returned as a character string and not printed.
#'
#' @export
#'
lib.packs_vec2str <- function(x, do_return = FALSE) {
    if (!is.null(x)) {x <- x[!is.na(x)]} else if (do_return) {return('')} else {cat('\n')}
    str <- gsub(pat = '\\s\\(\\)', rep = '',paste(paste(names(x), paste0("(", x, ")")), collapse = '   '))
    if (do_return) {return(gsub('   ', ', ', str))} else {cat(str, '\n')}
}


#' Show the complete library content.
#'
#' Use to print all available packages in the VC_library with all their versions including their dependencies.
#' Simply performs a call to \code{lib.dependsOnMe(all)}.
#' @export
#'
lib.installed_packages <- function(lib_location = lib.location()) {
    lib.dependsOnMe(all, lib_location = lib_location)
}


#' Shows the dependencies of (all or) a certain function(s).
#'
#' Can be called without using quotes like \code{lib.dependsOnMe(dplyr)}. A second very usefull feature would be the \code{lib.dependsOnMe(all)},
#' which will print a list of all packages available with their dependencies. \cr
#' \cr
#' A simple wrapper to do precisely that is "\code{lib.installed_packages}".
#'
#' @param ... All packages and their versions you would like to load e.g. \code{lib.dependsOnMe(DBI = '0.5', assertthat, R6 = '0.6', quietly = TRUE)}.
#' @param checkMyDeps Supports providing a named character vector of packages and their versions instead of the direct input.
#' Use it like this when calling it via another function. It is the shape that is supported by all other functions in this package.
#' e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}
#' @param lib_location The folder containing a structure where this function observe the dependencies from. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#'
#' @export
#'
lib.dependsOnMe <- function(..., checkMyDeps = NULL, lib_location = lib.location()) {

    if (is.null(checkMyDeps)) {
        checkMyDeps <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('lib_location', 'checkMyDeps'))
    }

    if (is.null(names(checkMyDeps))) {checkMyDeps <- setNames('> 0.0.0', checkMyDeps); cat('The latest version,\n')}

    # check if input package is realistic. if no version is selected (no name value pair is provided) the oldest version is used.
    if (!names(checkMyDeps) == 'all') checkMyDeps <- setNames(lib.decide_version(checkMyDeps, lib_location, pick.last = TRUE), names(checkMyDeps))


    packageList <- list.dirs(lib_location, recursive = FALSE, full.names = FALSE)
    packageList <- packageList[!packageList %in% c('.git', 'TEMP_install_location')]

    for (packageName in packageList) {
        # cat(packageName, '\n')
        packVersionList <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

        for (packVersion in packVersionList) {

            package.location <- paste(lib_location, packageName, packVersion, sep = '/')

            overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
            if (file.exists(overrideFile)) {
                dependingPackages <- lib.packs_str2vec(readChar(overrideFile, file.info(overrideFile)$size))
            } else {
                packDesc <- packageDescription(packageName, lib.loc = package.location)
                dependingPackages <- lib.packs_str2vec(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
            }

            dependingPackages <- dependingPackages[!lib.is_basepackage(names(dependingPackages))]

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
                lib.packs_vec2str(dependingPackages[1:3])
                if (length(dependingPackages) > 3) {
                    for (index in 2: ceiling(length(dependingPackages)/3)) {
                        cat(strrep(' ', 45), '... '); lib.packs_vec2str(dependingPackages[(((index-1)*3):(index*3-1))+1])
                    }
                }
            }
        }
    }
}
