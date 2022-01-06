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
#' Install packages and tarballs into R_MV_library
#'
#' @description
#'
#' This family of functions can help with installing packages without
#' the risk of installing every minor package improvement as soon as it is released.
#'
#' \enumerate{
#'   \item{\code{lib.install_tarball} can install a tarball based on the
#'   tarball location and it's dependencies (like \code{c(dplyr = '> 5.0')}).}
#'   \item{\code{lib.install_if_not_compatible} can install CRAN package
#'   depending on a condition. This is especially useful
#'   (and used on the background) for installing the dependencies for the
#'   tarball installation.}
#'   \item{\code{lib.install} can install CRAN packages into the R_MV_library,
#'   which in return is used by \code{lib.install_if_not_compatible}.}
#' }
#'
#' @param tarball The complete path to the tarball file that you would like to install.
#' @param package_conditions Provide a vector of package
#' name/'version condition' specifications. See section
#' 'limitations for \code{package_conditions}'.
#' @param package_names Provide a vector of package names.
#' A version cannot be supplied.
#' @param dependencies Provide the dependencies like a package version
#' combination: \code{c(dplyr = '>= 0.5', data.table = '', R6 = '0.1.1')}
#'
#' @param quiet Will affect \code{install.packages(..., quiet = quiet)}.
#' @param lib_location The folder where this package can be installed.
#' The package will first be installed in a temporary install folder
#' \code{<multiversion lib>/TEMP_install_location}
#' indicated by the \code{\link{lib.location_install_dir}()} function.
#' If \code{install_temporarily} is set to FALSE (the default).
#' the installed package(s) is moved to the destination location automatically.
#' @param install_temporarily If FALSE, the installed packages are moved
#' to the final destination automatically. Otherwise it is necessary to run
#' \code{\link{lib.convert}()} manually after the installation into the temporary
#' folder finished. To clean up the temporary folder, run \code{lib.clean_install_dir()}.
#' @param cran_url Will be passed trough to the install.packages command.
#' @param allow_overwrite_on_convert Can be used if you are experimenting and
#' you would like to overwrite the installed (tarball) package. See details below.
#'
# @param execute_with_Rscript When TRUE (the default), it will try to install the package using a separate Rscript R instance.
# This simplifies installing and leaves changing loaded packages etc. to another R instance which we can kill :).
# It will run a script which it provides with your list of packages to install.
# That script will load this \code{multiversion} package and call \code{\link{lib.install}()} directly.
#'
#' @section limitations for \code{package_conditions}:
#' All version specifications are allowed except for the exact version indication
#' (e.g. don't provide \code{c(dplyr = '1.2.3')}). It is allowed to provide no
#' specification, which will match any installed version of that package.
#' \bold{If the condition is met, the package is skipped}, which is the desired
#' behavior for dependencies. For an empty condition (e.g. \code{c(dplyr = '')}),
#' it will only install the package when no version is installed at all.
#'
#' @section Allow overwrite on convert:
#' When an installed package is converted to the R_MV_library, it would normally
#' show that it failed to copy the packages of which that version was already present.
#' This means that these packages were already converted from the temporary library
#' to the R_MV_library structure before, and no \code{lib.clean_install_dir()} was performed yet.
#' In case you are experimenting with a self made tarball package, and you are
#' developing the package within the same package version,
#' it is some times desired to overwrite the already present installed package with a new installation.
#' For CRAN packages, this options doesn't make sense.\cr\cr
#' Only for \code{lib.install_tarball}, the options \code{TRUE}, \code{FALSE}, and additionally
#' \code{"tarball"} \code{"dependencies"} are allowed. 'dependencies' will affect
#' all packages that are in the temporary installation location except for the tarball package.
#' 'tarball' will only overwrite the tarball package.
#'
#' @section Installing temporarily:
#' Installing a package temporarily gives you the opportunity to test the package
#' before adding it to the multiversion library structure. Loading packages, including those in the
#' temporary library (\code{\link{lib.location_install_dir}()}) can be done using:
#' \code{\link{lib.load}(..., also_load_from_temp_lib = TRUE)}.
#'
#' @name lib.install
NULL

#' @rdname lib.install
#' @importFrom utils install.packages
#' @export
#'
lib.install <- function(package_names = NULL, lib_location = lib.location(), install_temporarily = FALSE,
                        allow_overwrite_on_convert = FALSE, quiet = TRUE, cran_url = "http://cran.us.r-project.org") {

    if (!is.null(names(package_names))) {
        stop('Please provide a vector of names, not a vector like c(name = "1.0.0").')
    }
    if (length(package_names) == 0) {
        return(invisible())
    }

    # If executing using Rscript, this block is run and calls an R script which will run all except this block.
    # if (execute_with_Rscript) {
    #     temp_install_dir <- lib.location_install_dir()
    #     curr_packs <- list.dirs(temp_install_dir, full.names = F, recursive = F)
    #
    #     Rscript_dir <- normPath(system('where Rscript', intern = T)[1])
    #
    #     if (grepl('Could not find files for the given pattern(s)', Rscript_dir)) {
    #         stop('Please make sure `where Rscript` results in one or more valid paths. First one is used.')
    #     }
    #
    #     multiversion_location <- lib.my_location()
    #     script_location <- normPath(paste0(multiversion_location, '/exec/lib.install_script.R'))
    #
    #     status <- system(sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s" "%s"', Rscript_dir, script_location, lib_location, paste(collapse = ',', package_names), multiversion_location, as.character(allow_overwrite_on_convert), as.character(verbose)))
    #
    #     new_packs <- list.dirs(temp_install_dir, full.names = F, recursive = F)
    #     added_packages <- setdiff(new_packs, curr_packs)
    #
    #     message(sprintf('>> Installation attempt finished with status %s, added were: %s <<\n', as.character(status), paste(collapse = ', ', added_packages)))
    #     if (!install_temporarily & status == 0) {
    #         lib.convert(lib_location        = lib_location,
    #                     force_overwrite     = allow_overwrite_on_convert,
    #                     packages_to_convert = added_packages)
    #     }
    #     return(invisible())
    # }

    install.location <- normPath(lib.location_install_dir(lib_location))

    # loaded packages
    # nms <- names(sessionInfo()$otherPkgs)
    # currentlyLoaded <- detachAll(packageList = nms[!nms %in% c('RVClibrary', 'multiversion')])

    # The following should only be executed once.
    # if (sum(grepl('^lib.install', sapply(sys.calls(), function(x) {x[[1]]}))) <= 1) {
    #     if (!nzchar(Sys.which('make'))) {
    #         stop(paste('Please make sure the `make.exe` application of `Rtools` can be',
    #                    '\nfound so that packages can be compiled from source if necessary!',
    #                    '\nAdd e.g. `C:/Rtools/bin` to the PATH env var if you have the program already installed',
    #                    '\nor download Rtools (and check `Add paths` during install, and restart R) from "https://cran.r-project.org/bin/windows/Rtools/".'))
    #     }
    #     # Just took some
    #     if (any(!nzchar(Sys.which(c('aspell', 'basename', 'cat', 'cp'))))) {
    #         stop(sprintf(paste('%sAdd e.g. `C:/Rtools/bin` to the PATH env var if you have Rtools already installed',
    #                            '\nor download Rtools (and check `Add paths` during install, and restart R) from "https://cran.r-project.org/bin/windows/Rtools/".'),
    #                      ifelse(nzchar(Sys.getenv('MAKE')), '',
    #                             'The MAKE variable is specified, but this is not sufficient for an Rscript instance to compile from source properly. ')))
    #     }
    #     if (!grepl('\\$\\(WIN\\)', Sys.getenv('BINPREF'))) {
    #         stop(sprintf(paste0('Please make sure you also have the BINPREF environment variable set properly.',
    #                             '\nCurrent value: "%s", expected value should be like: "C:/Rtools/mingw_$(WIN)/bin/",',
    #                             '\nwhere "$(WIN)" is set by R to either "32" or "64" depending on the target.',
    #                             '\nIf your PATH and BINPREF variables are set correctly, we should be ready',
    #                             '\nfor compiling from source (being able to install the latest pacakges).'), Sys.getenv('BINPREF')))
    #     }
    #
    #     if (interactive()) {
    #         on.exit({
    #             message('\nResetting your loaded packages.')
    #             invisible(lib.load(loadPackages = currentlyLoaded, lib_location = lib_location, quietly = TRUE))
    #         }, add = TRUE)
    #     }
    #     prev_setting <- getOption("install.packages.compile.from.source")
    #     options(install.packages.compile.from.source = "always")
    #
    #     # library paths
    #     currentLibs <- .libPaths()
    #     .libPaths(.Library)
    #
    #     on.exit({
    #         options(install.packages.compile.from.source = prev_setting)
    #         .libPaths(currentLibs)
    #     }, add = TRUE)
    # }

    currentLibs <- lib.set_libPaths('all', lib_location, additional_lib_paths = install.location)
    on.exit(.libPaths(currentLibs), add = TRUE, after = FALSE)


    # loop through packages to install
    for (iPackage in package_names) {

        # dependsOn <- lib.dependencies_online(iPackage, cran_url = cran_url)
        #
        # # try loading dependencies, so that they're skipped during installation.
        # successfulLoads <- lib.testload(dependsOn, lib_location, pick.last = TRUE, msg_str = paste0('"', iPackage, '" his dependencies'), verbose = verbose)
        #
        # failedLoads <- !names(dependsOn) %in% names(successfulLoads) & !lib.is_basepackage(names(dependsOn))
        # failedLoads <- names(dependsOn)[failedLoads]
        #
        # if (length(failedLoads) > 0) {
        #     message(sprintf('\n - We will install for package "%s" the missing dependencies: %s.\n',
        #                     iPackage, paste0(collapse = ',', '"', failedLoads, '"')))
        #
        #     lib.install(package_names = failedLoads, lib_location = lib_location, install_temporarily = TRUE, execute_with_Rscript = execute_with_Rscript, verbose = verbose)
        #
        #     # When dependencies have been installed, try loading them again to create a more complete list of available
        #     successfulLoads <- lib.testload(dependsOn, lib_location, pick.last = TRUE, verbose = verbose)
        # }
        #
        #
        # # If all dependencies were already there, check if the package is already installed.
        # # When overwriting this package (or installing the latest version) we are not interested in knowing if the package already exists.
        # if (!allow_overwrite_on_convert && all(names(dependsOn) %in% names(successfulLoads) | lib.is_basepackage(names(dependsOn)))) {
        #     finalPackSucces <- lib.testload(stats::setNames('', iPackage), lib_location, pick.last = TRUE, verbose = verbose, msg_str = '')
        #
        #     if (iPackage %in% names(finalPackSucces)) {
        #         message(sprintf('\n - The "%s" package seems to be already installed...\n', iPackage))
        #         next
        #     }
        # }
        #
        # # add the successfully loaded packages to the .libPaths, so the installer knows that they are there.
        # lib.set_libPaths(successfulLoads, lib_location, additional_lib_paths = install.location)
        #
        message('INSTALLING: ', iPackage)
        # because the dependencies are on the libPath, only the not present dependencies will be installed.
        # keeping them loaded would raise a popup of Rstudio.

        with_build_tools(utils::install.packages(iPackage, lib = install.location, quiet = quiet, repos = cran_url))
        message('INSTALLING DONE.')

    }
    if (!install_temporarily) {
        lib.convert(install.location, lib_location, force_overwrite = allow_overwrite_on_convert)
    }
}


#' @rdname lib.install
#' @export
#'
lib.install_if_not_compatible <- function(package_conditions, lib_location = lib.location(), install_temporarily = FALSE,
                                          allow_overwrite_on_convert = FALSE, quiet = TRUE, cran_url = "http://cran.us.r-project.org") {

    if (length(package_conditions) == 0) {
        return(invisible())
    }
    if (is.null(names(package_conditions))) {
        stop('Please provide a vector of named version specifications, like `c(name = "> 1.0.0")`.',
             'When the condition cannot be met, the package will be installed.\n',
             'Note that specific versions are not allowed because we cannot install a specific version.')
    }
    if (any(grepl('^[-0-9. ]+$', package_conditions) && nchar(package_conditions) != 0)) {
        stop("It is not possible to provide exact versions like c(package.a = '1.0.0'). ",
             "This is because this is the only version specification for which installing ",
             "the latest version from CRAN will likely not solve the dependency.")
    }

    # loop through packages to install
    for (iPackage in seq_along(package_conditions)) {
        packVer <- package_conditions[iPackage]
        packName <- names(packVer)

        av_ver <- lib.available_versions(packName, lib_location = lib_location)

        if (length(av_ver) != 0 && any(lib.check_compatibility(packVer, av_ver))) {
            message(sprintf('\nPASS - The version condition "%s" for package "%s" can be matched just fine.\nAvailable versions are: %s',
                            packVer, packName, paste0(collapse = ', ', '"', av_ver, '"')))
            next
        }
        message(sprintf('\nINSTALL - For "%s" the version condition "%s" for the package can not be met.\n(Available versions are: %s)',
                        packName, packVer, paste0(collapse = ', ', '"', av_ver, '"')))

        lib.install(packName, lib_location = lib_location, install_temporarily = TRUE,
                    allow_overwrite_on_convert = allow_overwrite_on_convert, quiet = quiet, cran_url = cran_url)

    }
    if (!install_temporarily) {
        lib.convert(lib.location_install_dir(lib_location), lib_location, force_overwrite = allow_overwrite_on_convert)
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

#' @rdname lib.install
#' @importFrom utils install.packages
#' @export
#'
lib.install_tarball <- function(tarball, dependencies = c(), lib_location = lib.location(),
                                install_temporarily = FALSE, allow_overwrite_on_convert = c('tarball', 'dependencies'),
                                cran_url = "http://cran.us.r-project.org") {

    if (missing(allow_overwrite_on_convert)) {
        allow_overwrite_on_convert <- 'tarball'
    }

    stopifnot(file.exists(tarball))
    stopifnot(is.logical(allow_overwrite_on_convert) || is.character(allow_overwrite_on_convert))

    overwrite <- if (is.character(allow_overwrite_on_convert)) {
        val <- match.arg(tolower(allow_overwrite_on_convert),
                         c('tarball', 'dependencies', 'true', 'false'))
        data.frame(tarball = val %in% c('tarball',      'true'),
                   dep     = val %in% c('dependencies', 'true'))
    } else {
        # In case of 'TRUE' or 'FALSE'.
        data.frame(tarball = allow_overwrite_on_convert,
                   dep     = allow_overwrite_on_convert)
    }

    # if (execute_with_Rscript) {
    #     Rscript_dir <- normPath(system('where Rscript', intern = T)[1])
    #     if (grepl('Could not find files for the given pattern(s)', Rscript_dir)) {
    #         stop('Please make sure `where Rscript` results in one or more valid paths. First one is used.')
    #     }
    #     script_location <- system.file(package = 'multiversion', 'lib.install_tarball_script.R')
    #
    #     status <- system(sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s"', Rscript_dir, script_location,
    #                              lib_location, packagePath, lib.packs_vec2str(dependencies, do_return = TRUE),
    #                              dirname(dirname(script_location))))
    #
    #     if (!install_temporarily & status == 0) {
    #         lib.convert(source_lib = lib_location, force_overwrite = allow_overwrite_on_convert)
    #     }
    #     return(invisible())
    # }

    install.location <- lib.location_install_dir(lib_location)

    if (!missing(dependencies) && length(dependencies) > 0) {
        message('First start with installing the provided dependencies (temporarily): ',
                paste0(collapse = ', ' , paste0(names(dependencies), ': "', dependencies, '"'), '...'))

        lib.install_if_not_compatible(dependencies,
                                      lib_location = lib_location,
                                      install_temporarily = TRUE,
                                      cran_url = cran_url)
        message('Finished installing the dependencies!\n\nINSTALLING the tarball...\n"', tarball, '"')

    }

    currentLibs <- lib.set_libPaths('all', lib_location, additional_lib_paths = install.location)
    on.exit(.libPaths(currentLibs), add = TRUE, after = FALSE)

    # successfulLoads <- lib.testload(dependencies, lib_location, msg_str = paste0('"', basename(packagePath), '" his dependencies'))
    # missingdependencies <- names(dependencies)[!(names(dependencies) %in% names(successfulLoads))]

    # Install all not yet existing dependencies:
    # if (length(missingdependencies) > 0) {
    #     message('\nI will install missing dependencies and try again...\n')
    #     lib.install(missingdependencies, lib_location = lib_location, install_temporarily = install_temporarily)
    #     successfulLoads <- lib.testload(dependencies, lib_location, msg_str = paste0('"', basename(packagePath), '" his dependencies (second try)'))
    #
    #     if (all(names(dependencies) %in% names(successfulLoads))) {
    #         message('\nNow we succeeded, continuing... \n')
    #     }
    # }

    # Check again if all wend well, if not all dependencies are there, reset and abort.
    # isSuccesfull <- names(dependencies) %in% names(successfulLoads)

    # if (!all(isSuccesfull | lib.is_basepackage(names(dependencies)))) {
    #     stop(paste0('\nThe following dependencies were missing and failed installation:', paste(paste0('\n- "', names(dependencies)[!isSuccesfull], '"'), collapse = ''), '\n\n'))
    # }

    # nms <- names(sessionInfo()$otherPkgs)
    # currentlyLoaded <- detachAll(packageList = nms[!nms %in% c('RVClibrary', 'multiversion')])

    # message('') # (applying a newline)

    # if (interactive() && length(sys.calls()) == 1) {
    #     on.exit({
    #         message('\nResetting your loaded packages...\n');
    #         lib.load(loadPackages = currentlyLoaded, lib_location = lib_location, quietly = TRUE)
    #     }, add = TRUE, after = FALSE)
    # }

    # If all dependencies could be found/installed: add all recursive dependency libraries to the search path.
    # lib.set_libPaths(successfulLoads, lib_location, additional_lib_paths = install.location)

    # Install the tarball!
    utils::install.packages(tarball, lib = install.location, type = "source", repos = NULL)

    if (!install_temporarily) {
        # tarball
        tarball_package_name <- gsub('_[0-9.-]+\\.tar\\.gz', '', basename(tarball))
        lib.convert(install.location, lib_location, force_overwrite = overwrite$tarball,
                    packages_to_convert = tarball_package_name)

        # dependencies
        deps <- list.dirs(install.location, FALSE, FALSE)
        lib.convert(install.location, lib_location, force_overwrite = overwrite$dep,
                    packages_to_convert = deps[deps != tarball_package_name])
    }

    return(invisible())
}


# -------- convert ordinary  (temp_)library  to  R_MV_library ---------

#' Move normally installed packages to R_MV_library structure.
#'
#' After this conversion is completed and you configure (temporarily by using \code{lib.location(...)}
#' or for eternity by setting the equally named environment variable) the R_MV_LIBRARY_LOCATION env var, you are good to go! You can directly use \code{lib.load}
#' for loading packages. Thanks for using \code{multiversion}!! \cr
#' \cr
#' This function creates the R_MV_library structure by moving normally installed packages to a parallel library structure.
#' \code{<lib1>/BH/DESCRIPTION} becomes\code{<lib2>/BH/1.60.0-2/BH/DESCRIPTION} so that also \code{1.60.0-3} etc. can be installed.
#' \cr
#' This functionality is also used (with it's default values) for converting installed packages from the temporary installation directory to the final R_MV_library.
#' The TEMP installation directory is in a standard flat library structure.\cr
#' \cr
#' Note that it is really no problem to perform a conversion again, it will only move new versions of already present packages and will never overwrite.
#' To continue with a clean Temp folder, run \code{lib.clean_install_dir()} which will remove the folder.
#'
#' @param source_lib The temporary library where a package is temporarily installed (having a normal library structure).
#' By default, the path is generated using \code{lib.location_install_dir()} on the \code{destination_mv_lib} that is provided which appends \code{/TEMP_install_location}.
#' @param destination_mv_lib The folder containing a structure where all packages in the temp folder must be moved to.
#' By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#' @param force_overwrite If you are experimenting and you would like to overwrite the newly installed package.
#' Normally only desired when the package you are experimenting with is a self maintained package and you are sure you increased the version to a new one.
#' @param packages_to_convert A character vector with the names of the packages that need to be converted to the R_MV_library. If missing or empty, all will be converted.
#'
#' @examples
#' # As an experiment (or when getting started) you could run this with
#' # your complete standard library (not your base library).
#'
#' #> lib.convert(source_lib = Sys.getenv("R_LIBS_USER"),
#' #>             destination_mv_lib  = "./REMOVE_ME_example_library")
#'
#' # Running the same operation a second time will result
#' # in a notification that all files were already copied.
#'
#' # Just running it will use the R_MV_library defined by the environment
#' # variable and look inside for the Temp folder to use.
#'
#' #> lib.convert()
#'
#' # It is sufficient to only provide the destination_mv_lib,
#' # it will look for the "/TEMP_install_location" folder as the 'source_lib' by default.
#'
#' #> lib.convert(destination_mv_lib = "./R_MV_library")
#'
#'
#' @importFrom utils packageDescription
#' @export
#'
lib.convert <- function(source_lib          = lib.location_install_dir(destination_mv_lib),
                        destination_mv_lib  = lib.location(),
                        force_overwrite     = FALSE,
                        packages_to_convert) {

    source_lib   <- normPath(source_lib)
    lib_location <- normPath(destination_mv_lib)

    stopifnot(dir.exists(source_lib))
    all_present_packages <- list.dirs(source_lib, FALSE, FALSE)
    if (missing(packages_to_convert)) {
        packages_to_convert <- all_present_packages
    } else {
        if (length(packages_to_convert) == 0) {
            message('Nothing to convert.')
            return(invisible())
        }
        # Crash if non existing.
        if (!all(tmp <- packages_to_convert %in% all_present_packages)) {
            stop('The following packages in the list of packages to convert are',
                 ' not present in the installation library!\nNot present: ',
                 paste0(collapse = ', ' , '"', packages_to_convert[!tmp], '"'))
        }
    }

    libContent   <- list.files(paste0(source_lib, '/', packages_to_convert), all.files = T, recursive = T, no.. = T, full.names = T)
    packageNames <- gsub(paste0(source_lib, '/([^/]+)/.*'), '\\1', libContent)

    # Generally the same as 'packages_to_convert'
    uniquePackages <- unique(packageNames)

    packageVersions <- c()
    for (iPackage in uniquePackages) {
        packageVersions[packageNames == iPackage] <- as.character(
            numeric_version(utils::packageDescription(iPackage, lib.loc = source_lib)$Version))
    }

    newLocation <- paste(lib_location, packageNames, packageVersions,
                         gsub(libContent, pattern = paste0(source_lib, '/'), replacement = '', fixed = TRUE), sep = '/')

    lapply(unique(dirname(newLocation)), dir.create, recursive = TRUE, showWarnings = FALSE)
    succes <- file.copy(libContent, newLocation, overwrite = force_overwrite)

    message('\nSuccesfully copied files:')
    message(summary(data.frame(dummy = packageNames[ succes]), maxsum = 80))
    message('\nFailed copying files (might be already installed or `TEMP_install_location`',
            ' was not cleaned up, can be done by running `lib.clean_install_dir()`):')
    message(summary(data.frame(dummy = packageNames[!succes]), maxsum = 80))
    message('')
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
#' @param reload_multiversion If multiversion needs to be loaded again after everything (or all mentioned in \code{packageList}) is unloaded.
#' @param packageList A character vector with the packages to detach/unload. Defaults to all packages
#'        (\code{names(sessionInfo()$otherPkgs}). When package X depends on package Y, make sure you first specify X then Y.
#' @param dryRun If TRUE, lists all packages that will be cleaned up.
#'
#' @importFrom utils sessionInfo
#' @export
#'
detachAll <- function(reload_multiversion = FALSE, packageList = 'all', dryRun = FALSE) {
    do_all <- FALSE
    if (!missing(packageList) && length(packageList) == 0) {
        return()
    }
    if (missing(packageList) || packageList == 'all') {
        packageList <- names(utils::sessionInfo()$otherPkgs)
        do_all <- TRUE
    }
    currentPackageAndVersions <- lib.package_version_loaded(packageList)

    if (missing(packageList) && is.null(packageList)) {
        message('No packages are loaded, nothing to detach.')

    } else if (!dryRun) {
        in_search <- paste0('package:', packageList) %in% search()
        lapply(sprintf('package:%s', packageList[in_search]), detach, character.only = TRUE, unload = TRUE)
    }

    # also unload all namespaces (not always stable!)
    if (!dryRun) {

        if (do_all) {
            loadedNS <- rev(names(utils::sessionInfo()$loadedOnly))
            allLoadedPackages <- unique(c(packageList, loadedNS))

            cnt <- 1
            while(length(loadedNS) && cnt < 1000) {
                sapply(loadedNS, function(x) {
                    result <- tryCatch(unloadNamespace(getNamespace(x)), error = function(e) NULL)
                })
                loadedNS <- rev(names(utils::sessionInfo()$loadedOnly))

                cnt <- cnt + 1
            }

            # if while loop never naturally completed
            if(cnt == 1000)
                warning("Unable to unload all namespaces")
        } else {
            for (itter in 1:10) {
                sapply(packageList, function(x) {
                    tryCatch(unloadNamespace(getNamespace(x)), error = function(e) NULL)
                })
            }
            allLoadedPackages <- packageList
        }


        # deal with all DLLs now that the rest is done.
        suppressWarnings(sapply(allLoadedPackages, function(x) {
            dll <- getLoadedDLLs()[[x]]

            if(!is.null(dll))
                tryCatch(library.dynam.unload(x, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
        }))
    }

    if (reload_multiversion) {
        library(multiversion, lib.loc = Sys.getenv("R_LIBS_USER"))
    }

    if (!'package:multiversion' %in% search()) {
        message('Note that the package "multiversion" is also detached:',
                '\n>  library(multiversion, lib.loc = Sys.getenv("R_LIBS_USER"))')}

    return(if(dryRun) {currentPackageAndVersions} else {invisible(currentPackageAndVersions)})
}


# ------------- [install.packages] helper functions --------------

# # Test load package list.
# #
# # Internally used for 'test loading' a list of dependencies when installing a package.
# # Returns a list of successful load operations.
# #
# # @param packages A named character vector of package/version names. None are really loaded, `lib.load` is used with the `dry.run` turned on.
# # @param lib_location The folder containing a structure where we will try to find our required packages in.
# # By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
# # @param pick.last Changes the way a decision is made. In the scenario where a dependency of \code{>} or \code{>=} is defined, multiple versions may be available to choose from. By default, the lowest compliant version is chosen. Setting this to TRUE will choose the highest version.
# # @param verbose If TRUE, the default, will show all \code{lib.load} messages.
# # @param msg_str Can be either logical FALSE, or a character string which will be (the %s) part of the printed string ' - Trying to load %s: ...'.
# # Defaults to 'packages', making the sentence: ' - Trying to load packages: lattice, RColorBrewer, grid'
# #
# lib.testload <- function(packages, lib_location = lib.location(), pick.last = FALSE, verbose = TRUE, msg_str = 'packages') {
#     # tries to load packages and returns a vector with all successful load attempts ('yet to be installed' packages)
#
#     # Handle 'verbose' options
#     execute <- if (is.logical(verbose) && verbose) {
#         function(x) {x}
#     } else {
#         verbose <- FALSE  # Any value is allowed to get here, so I force a FALSE for further code to work.
#         function(x) {invisible(capture.output(x)); return(x)}
#     }
#
#     basePacks <- lib.is_basepackage(packages)
#     loadedPackages <- packages[basePacks]
#     packages       <- packages[!basePacks]
#
#     # Return directly when no 'dependencies' or packages are requested to load.
#     if (length(packages) == 0) {return(loadedPackages)}
#
#     if ((!verbose && missing(msg_str)) || is.character(msg_str) && length(msg_str) == 1) {
#         message(sprintf('\n - Trying to load %s: %s', msg_str, lib.packs_vec2str(packages, do_return = TRUE)))
#     }
#
#     for (iDependency in seq_along(packages)) {
#         iDependency <- packages[iDependency]
#
#         # TryCatch returns empty when error is caught.
#         newlyLoaded <-  tryCatch({
#             suppressWarnings(execute(lib.load(loadPackages = iDependency, lib_location = lib_location, dry.run = TRUE, also_load_from_temp_lib = TRUE,
#                                               skipDependencies = names(loadedPackages), pick.last = pick.last, quietly = !verbose)))
#         }, error = function(e) {
#             if (verbose) {
#                 # `message` appends a linefeed itself.
#                 message(sprintf('When loading package "%s": %s', names(iDependency), gsub('\n$', '', gsub('Error ', '', e))))
#             }
#         })
#
#         loadedPackages <- append(loadedPackages, newlyLoaded)
#     }
#
#     return(loadedPackages)
# }


# ---------------- additional functions -----------------

#' List the dependencies of a package.
#'
#' Provide a package name (can be without quotes) to show its dependencies.
#' To list all dependencies of the complete library, use the inverse function "\code{lib.dependsOnMe(all)}" with the value 'all'.
#' That function also does not require quotes.
#'
#' @param packageName The (unquoted) package name for which you would like to print the dependencies.
#' @param do_print If true (default), prints the dependencies. In both cases, the dependencies are returned invisibly.
#' @param character.only If TRUE, (FALSE by default), the package names can be provided as character vector. Otherwise, direct unquoted package names are supported.
#' @param lib_location The folder containing the R_MV_library structure where this function observes the dependencies. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#'
#' @examples
#' \dontrun{
#'     lib.dependencies(dplyr)
#'     lib.dependencies('devtools', character.only = T)
#'     devtools_deps <- lib.dependencies(devtools, do_print = F)
#' }
#'
#' @importFrom utils packageDescription
#' @export
#'
lib.dependencies <- function(packageName, do_print = TRUE, character.only = FALSE, lib_location = lib.location()) {

    # Featuring direct call like: `lib.dependencies(dplyr)`
    if (!character.only) {
        packageName = as.character(substitute(packageName))
        if (packageName[1] == 'c') {
            stop('Please only provide the name of the package. All versions will be shown.')
        }
    }

    if (!is.null(names(packageName))) {
        stop('Please only provide the name of the package. All versions will be shown.')
    }
    if (!nzchar(packageName)) {
        return(invisible())
    }
    stopifnot(length(packageName) == 1, !is.na(packageName))

    packVersionList <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

    listed_dependencies <- list()
    for (packVersion in packVersionList) {

        package.location <- paste(lib_location, packageName, packVersion, sep = '/')

        overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
        if (file.exists(overrideFile)) {
            dependingPackages <- lib.packs_str2vec(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- utils::packageDescription(packageName, lib.loc = package.location)
            dependingPackages <- lib.packs_str2vec(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pattern = ',,', replacement = ','))
        }
        listed_dependencies[[packVersion]] <- dependingPackages

        if (do_print) {
            message(sprintf('%23s : %-8s ', packageName, packVersion), appendLF = F)
            message(ifelse(file.exists(overrideFile), '(shadowed)| ', '          | '), appendLF = F)
            lib.packs_vec2str(dependingPackages[1:3])
            if (length(dependingPackages) > 3) {
                for (index in 2: ceiling(length(dependingPackages)/3)) {
                    message(strrep(' ', 43), '... ', appendLF = F); lib.packs_vec2str(dependingPackages[(((index-1)*3):(index*3-1))+1])
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
    if (!is.null(x)) {x <- x[!is.na(x)]} else if (do_return) {return('')} else {message('')}
    str <- gsub(pattern = '\\s\\(\\)', replacement = '',paste(paste(names(x), paste0("(", x, ")")), collapse = '   '))
    if (do_return) {return(gsub('   ', ', ', str))} else {message(str)}
}


#' Show the complete library content.
#'
#' Use to print all available packages in the R_MV_library with all their versions including their dependencies.
#' Simply performs a call to \code{lib.dependsOnMe(all)}.
#'
#' @param lib_location The R_MV_library location.
#'
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
#' A simple wrapper "\code{lib.installed_packages}", will do precisely that.
#'
#' @param ... All packages and their versions you would like to load e.g. \code{lib.dependsOnMe(DBI = '0.5', assertthat, R6 = '0.6', quietly = TRUE)}.
#' @param checkMyDeps Supports providing a named character vector of packages and their versions instead of the direct input.
#' Use it like this when calling it via another function.
#' @param lib_location The folder containing a structure where this function observe the dependencies from. By default, it checks the environment variable \code{R_MV_LIBRARY_LOCATION} for this directory.
#'
#' @return
#' It returns a is the shape that is supported by all other functions in this package e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}
#'
#' @importFrom stats setNames
#' @importFrom utils packageDescription
#'
#' @export
#'
lib.dependsOnMe <- function(..., checkMyDeps = NULL, lib_location = lib.location()) {

    if (is.null(checkMyDeps)) {
        checkMyDeps <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('lib_location', 'checkMyDeps'))
    }
    stopifnot(length(checkMyDeps) > 0)

    if (length(checkMyDeps) > 1 && 'all' %in% checkMyDeps) {
        stop('When requesting `all` dependencies (equal to `lib.installed_packages()`),',
             ' no other package names can be requested in the same call to `lib.dependsOnMe`.')
    }
    if (length(checkMyDeps) > 1) {
        for (icheck in seq_along(checkMyDeps)) {
            lib.dependsOnMe(checkMyDeps = checkMyDeps[icheck], lib_location = lib_location)
        }
        return(invisible())
    }

    if (is.null(names(checkMyDeps))) {
        av_ver <- lib.available_versions(checkMyDeps)
        checkMyDeps <- stats::setNames('999999.99.99', checkMyDeps)
        message('Showing all that depends on `', names(checkMyDeps), '`, (available are: ', paste0(collapse = ', ', '"', av_ver, '"'), '):')
    }

    # check if input package is realistic.
    # if no version is selected (no name value pair is provided) all packages that depend on any version of 'checkMyDeps' will be shown.
    if (!names(checkMyDeps) == 'all' && !checkMyDeps == '999999.99.99') {
        av_ver <- lib.available_versions(names(checkMyDeps))
        av_ver_apply <- sapply(av_ver, FUN = function(x) {lib.check_compatibility(checkMyDeps, x)})
        if (any(av_ver_apply)) {
            checkMyDeps <- stats::setNames(av_ver[max(which(av_ver_apply))], names(checkMyDeps))
            message('Showing all that depends on `',
                    names(checkMyDeps), '`, version "', checkMyDeps, '":')
        } else if (length(av_ver_apply) == 0) {
            message('Cannot match dependency... it appears that this package is not installed.')
        } else {
            # show all for this version
            message('Cannot match dependency...\nShowing all that depends on `',
                    names(checkMyDeps), '`, (available are: ', paste0(collapse = ', ', '"', av_ver, '"'), '):')
            checkMyDeps <- stats::setNames('999999.99.99', names(checkMyDeps))
        }
    }

    packageList <- list.dirs(lib_location, recursive = FALSE, full.names = FALSE)
    packageList <- packageList[!packageList %in% c('.git', 'TEMP_install_location')]

    for (packageName in packageList) {
        packVersionList <- list.dirs(paste(lib_location, packageName, sep = '/'), recursive = FALSE, full.names = FALSE)

        for (packVersion in packVersionList) {

            package.location <- paste(lib_location, packageName, packVersion, sep = '/')

            overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
            if (file.exists(overrideFile)) {
                dependingPackages <- lib.packs_str2vec(readChar(overrideFile, file.info(overrideFile)$size))
            } else {
                packDesc <- utils::packageDescription(packageName, lib.loc = package.location)
                dependingPackages <- lib.packs_str2vec(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pattern = ',,', replacement = ','))
            }

            dependingPackages <- dependingPackages[!lib.is_basepackage(names(dependingPackages))]

            if (!names(checkMyDeps) == 'all') {
                depThatMatters <- dependingPackages[names(dependingPackages) == names(checkMyDeps)]
                if (length(depThatMatters) == 0) {next}
                valid <- lib.check_compatibility(depThatMatters, checkMyDeps)

            } else { # in case 'all' is requested.
                valid = TRUE
            }

            if (valid) {
                message(sprintf('%23s : %-8s ', packageName, packVersion), appendLF = F)
                message(ifelse(file.exists(overrideFile), '(shadowed)| ', '          | '), appendLF = F)
                lib.packs_vec2str(dependingPackages[1:3])
                if (length(dependingPackages) > 3) {
                    for (index in 2: ceiling(length(dependingPackages)/3)) {
                        message(strrep(' ', 45), '... ', appendLF = F)
                        lib.packs_vec2str(dependingPackages[(((index-1)*3):(index*3-1))+1])
                    }
                }
            }
        }
    }
}
