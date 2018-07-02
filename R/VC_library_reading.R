
# ============== MAIN FUNCTIONALITY ==============

#' Two ways of package/versions input are accepted:
#' 1: just provide them directly (the `...` input). All not recognized named variables
#'    will be interpreted like package names or package name/version combination. `library_VC(DBI = '0.5', assertthat = '', R6 = '')`
#' 2: provide the `loadPackages` input in the following way: `library_VC(loadPackages = c(DBI = '0.5', assertthat = '', R6 = ''))`
#'
#' If an empty string e.g. `dplyr = ''` is specified, one of two things will happen:
#' - if one version is available, this one is used.
#' - if more are available, in interactive mode, it will ask the user to make a decission, or if `interactive()` is FALSE, it will return an error.
#'
#' if >= or > is used, as in `dplyr = '>= 2.5'`, it will list the options and take the first one.
#' If another version is desired, please define it in the input list of packages to load
#' before the package that depends on it is loaded.
#'
#' Dependencies are checked and then loaded by recursively running this function with `dry.run = TRUE`.
#' This makes that dependencies are not loaded automatically, but are added to the namespace and accesible by its caller.
#'
#' The inputs packNameVersionList [list of named versions] and skipDependencies [list of names] can be
#' left blank in general. These are used by other functionallity like `tryLoadPackages_VC()` and `install.packages_VC`
#' dry.run will show the packages that will be used and will crash when no option is feasable (not installed or not compliant packages).
#'
#' If content of a package is directly called (like databasequiries in the DTF shiny app), but is also a dependency of
#' another package (e.g. datatransfer), it still needs to be loaded directly, because it will not be loaded automatically by the main package.
#' The namespace will be attached though, but not loaded. therefore it is not necessary to keep the .libPaths on the dependency packages.
#'
#' In other words, dependencies are remembered, but not loaded. So all strings are released (figurely speaking), but the dependency is there.
#'
#' "strings" can stay attached (as in, the .libPaths can be appended) when using 'appendLibPaths = TRUE'.
#' Afterwards, the normal library function can be used to load the not yet loaded but attached package.
#' This is more or less the same as doing the following:
#' ----
#' library_VC(c(dplyr = '0.5.0'), dry.run = TRUE, appendLibPaths = TRUE)
#' library(dplyr)
#' ----
#' `dry.run` skips the loading step, and `appendLibPaths` adds the paths of dplyr and it's dependencies to `.libPaths`, which make a `library` call work.
#'
#' One reason to use `appendLibPaths = TRUE` is to make these packages accessible by a new 'child' R session. This is the case if
#' `devtools::test()` is run by using `cntrl` + `shift` + `T` in Rstudio. When running it directly, it will use the currently loaded libraries.
#'
#' ERRORS:
#' If you receive the error `cannot unload ... ` It means that it tries to load a package, but another version is already loaded.
#' To unload this other (older) version, it may not be a dependency of other packages, if it is, you will receive this error.
#' The only workaround (when using this in R studio) is to close your Rstudio session, rename (or remove) the folder `RVClibrary\.Rproj.user\...\sources\prop` and start Rstudio again.
#' At the `...` there should be a hash used in the current session e.g. `\F3B1663E\` and the project `RVClibrary` might be any Rstudio project. After this,
#' the packages should be unloaded and you should be able to load a new batch of packages.
#'
#' @param ... All packages and their versions you would like to load e.g. `library_VC(DBI = '0.5', assertthat = '', R6 = '', quietly = TRUE)`. Other params like `quietly` will be ignored.
#' @param loadPackages {NULL} Supports providing a named character vector of packages and their versions in the shape that is supported by all other functions in this package. e.g. `c(DBI = '0.5', assertthat = '', R6 = '')`
#' @param lib.location {R_VC_library_location()} The folder containing a structure where this package must be loaded from.
#' @param dry.run {FALSE} Will make it perform a dry run. It will check all dependencies and if `appendLibPaths` it will add
#' their paths to `.libPaths` but it will not load those packages. If the paths are added this way, you should be able to just call the located packages with `library(...)`
#' @param quietly {FALSE} Indicates if the loading must happen silently. No messages and warnings will be shown if TRUE.
#' @param packNameVersionList {c()} See main description. Should be left blank.
#' @param skipDependencies {c()} See main description. Should be left blank.
#' @param appendLibPaths {FALSE} If TRUE, the path to every package that is loaded will be appended to `.libPath(...)`. That configured path is the location where `library()` will look for packages.
#'
library_VC <- function(..., loadPackages = NULL, lib.location = R_VC_library_location(), dry.run = FALSE, quietly = FALSE, packNameVersionList = c(), skipDependencies = c(), appendLibPaths = FALSE) {

    # If `loadPackages` is not provided, make use of the ... input via `match.call()`.
    # It will list all input names and values from which I will use all but the ones excluded.
    if (is.null(loadPackages)) {
        loadPackages <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('loadPackages', 'lib.location', 'dry.run', 'quietly', 'packNameVersionList', 'skipDependencies', 'appendLibPaths'))
    }

    # If still other libraries are set as active libraries, reset the library to just 1 lib for the build in functions (= `.Library`).
    if (!all(grepl(normPath(lib.location), normPath(.libPaths())) | grepl(normPath(.Library), normPath(.libPaths())))) {.libPaths(.Library); cat('\nExtra libraries were found. Those are excluded from .libPaths().\n\n')}

    # check if the package version that is provided is a correct version (this catches a wrong input like `c(dplyr = '0.5.0', databasequeries')`  )
    if (!is.na(loadPackages) && length(loadPackages)!=0 &&
        any(sapply(loadPackages, function(x) {attributes(regexpr('>?=?\\s?\\d+(\\.\\d+){1,3}', x))$match.length != nchar(x) && nchar(x) > 0}) & !checkIfBasePackage(names(loadPackages)))) {
        stop(sprintf('Not all package versions that are provided seem to be valid version numbers. The following was received:\n%s', paste(paste0(names(loadPackages), ' (', loadPackages, ')'), collapse = ', ')))
    }

    for (iPackage in unique(names(loadPackages))) {

        # if already loaded in previous recursive iteration where dry.run was TRUE (appended to skipDependencies)
        if (iPackage %in% skipDependencies) {next}

        # if base package, simply load and continue
        if (checkIfBasePackage(iPackage)) {
            library(iPackage, character.only = TRUE, quietly = quietly)
            next
        }

        packVersion <- getCorrectVersion(loadPackages[iPackage], lib.location)

        package.location <- paste(lib.location, iPackage, packVersion, sep = '/')

        if (!dir.exists(package.location)) {
            stop(sprintf('The package "%s" or its version "%s" could not be accessed and might not be present.', iPackage, packVersion))
        }

        # load dependencies: [1] from an override dependency file [2] from the original dependencies.
        overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
        if (file.exists(overrideFile)) {
            dependingPackages <- cleanupDependencyList(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- packageDescription(iPackage, lib.loc = package.location)
            dependingPackages <- cleanupDependencyList(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
        }

        # recusively load dependencies
        # The skipDependencies is necessary for `dry.run=TRUE`. With `dry.run=FALSE` the dependency is loaded and skipped in the next itteration.
        packNameVersionList <- library_VC(loadPackages = dependingPackages, lib.location = lib.location,
                                          packNameVersionList = packNameVersionList, skipDependencies = c(names(packNameVersionList), skipDependencies), dry.run = TRUE)

        packNameVersionList <- append(packNameVersionList, setNames(packVersion, iPackage))

        currentLibs <- .libPaths()

        if (!dry.run) {
            add_package_VC_libPaths(packNameVersionList, lib.location)

            if (quietly) {
                suppressWarnings(suppressMessages(library(iPackage, lib.loc = package.location, character.only = TRUE, quietly = quietly)))
            } else {
                library(iPackage, lib.loc = package.location, character.only = TRUE, quietly = quietly)
            }

            .libPaths(currentLibs)
        }

        if (appendLibPaths) {
            add_package_VC_libPaths(packNameVersionList, lib.location)
            .libPaths(c(currentLibs, .libPaths()))
        }
    }

    if (!dry.run && !quietly) printExampleLibCall(packNameVersionList)

    return(if(quietly) {invisible(packNameVersionList)} else {packNameVersionList})
}
