
# ============== MAIN FUNCTIONALITY ==============

#' Load package from R_VC_library
#'
#' Two ways of package/versions input are accepted:
#' 1: just provide them directly (the \code{...} input). All not recognized named variables
#'    will be interpreted like package names or package name/version combination. \code{library_VC(DBI = '0.5', assertthat = '', R6 = '')}
#' 2: provide the \code{loadPackages} input in the following way: \code{library_VC(loadPackages = c(DBI = '0.5', assertthat = '', R6 = ''))} \cr
#' \cr
#' If an empty string e.g. \code{dplyr = ''} is specified, one of two things will happen:
#' - if one version is available, this one is used.
#' - if more are available, in interactive mode, it will ask the user to make a decission, or if \code{interactive()} is FALSE, it will return an error. \cr
#' \cr
#' if >= or > is used, as in \code{dplyr = '>= 2.5'}, it will list the options and take the first one.
#' If another version is desired, please define it in the input list of packages to load
#' before the package that depends on it is loaded.
#' \cr
#' Dependencies are checked and then loaded by recursively running this function with \code{dry.run = TRUE}.
#' This makes that dependencies are not loaded automatically, but are added to the namespace and accesible by its caller. \cr
#' \cr
#' The inputs packNameVersionList [list of named versions] and skipDependencies [list of names] can be
#' left blank in general. These are used by other functionallity like \code{tryLoadPackages_VC()} and \code{install.packages_VC}
#' \code{dry.run} will show the packages that will be used and will crash when no option is feasable (not installed or not compliant packages). \cr
#' \cr
#' If content of a package is directly called (like databasequiries in the DTF shiny app), but is also a dependency of
#' another package (e.g. datatransfer), it still needs to be loaded directly, because it will not be loaded automatically by the main package.
#' The namespace will be attached though, but not loaded. therefore it is not necessary to keep the .libPaths on the dependency packages. \cr
#' \cr
#' In other words, dependencies are remembered, but not loaded. So all strings are released (figurely speaking), but the dependency is there. \cr
#' \cr
#' "strings" can stay attached (as in, the .libPaths can be appended) when using 'appendLibPaths = TRUE'.
#' Afterwards, the normal library function can be used to load the not yet loaded but attached package.
#' This is more or less the same as doing the following: \cr
#' \code{
#' library_VC(c(dplyr = '0.5.0'), dry.run = TRUE, appendLibPaths = TRUE)
#' library(dplyr)} \cr
#' \cr
#' \code{dry.run} skips the loading step, and \code{appendLibPaths} adds the paths of dplyr and it's dependencies to \code{.libPaths}, which make a \code{library} call work. \cr
#' \cr
#' One reason to use \code{appendLibPaths = TRUE} is to make these packages accessible by a new 'child' R session. This is the case if \code{devtools::test()} is run
#' by using \code{cntrl} + \code{shift} + \code{T} in Rstudio. When running it directly, it will use the currently loaded libraries. \cr
#'
#' @details ERRORS: \cr If you receive the error "\code{cannot unload ...}" It means that it tries to load a package, but another version is already loaded.
#' To unload this other (older) version, it may not be a dependency of other packages, if it is, you will receive this error.
#' The only workaround (when using this in R studio) is to close your Rstudio session, rename (or remove) the folder "\code{RVClibrary/.Rproj.user/.../sources/prop}" and start Rstudio again.
#' At the \code{...} there should be a hash used in the current session e.g. \code{/F3B1663E/} and the project \code{RVClibrary} might be any Rstudio project. After this,
#' the packages should be unloaded and you should be able to load a new batch of packages.
#'
#' @param ... All packages and their versions you would like to load e.g. \code{library_VC(DBI = '0.5', assertthat = '', R6 = '', quietly = TRUE)}. Other params like \code{quietly} will be ignored.
#' @param loadPackages Supports providing a named character vector of packages and their versions in the shape that is supported by all other functions in this package. e.g. \code{c(DBI = '0.5', assertthat = '', R6 = '')}
#' @param lib.location The folder containing a structure where this package must load packages from. By default, it checks the environment variable \code{R_VC_LIBRARY_LOCATION} for this directory.
#' @param dry.run Will make it perform a dry run. It will check all dependencies and if \code{appendLibPaths} it will add
#' their paths to \code{.libPaths} but it will not load those packages. If the paths are added this way, you should be able to just call the located packages with \code{library(...)}
#' @param quietly  Indicates if the loading must happen silently. No messages and warnings will be shown if TRUE.
#' @param appendLibPaths  If TRUE, the path to every package that is loaded will be appended to \code{.libPath(...)}. That configured path is the location where \code{library()} will look for packages.
#' @param pick.last Changes the way a decision is made. In the scenario where a dependency of \code{>} or \code{>=} is defined, multiple versions may be available to choose from. By default, the lowest compliant version is chosen. Setting this to TRUE will choose the highest version.
#'
#' @param packNameVersionList See main description. Should be left blank.
#' @param skipDependencies See main description. Should be left blank.
#'
#' @export
#'
library_VC <- function(..., loadPackages = NULL, lib.location = R_VC_library_location(), dry.run = FALSE, quietly = FALSE, appendLibPaths = FALSE, pick.last = FALSE, packNameVersionList = c(), skipDependencies = c()) {

    # If `loadPackages` is not provided, make use of the ... input via `match.call()`.
    # It will list all input names and values from which I will use all but the ones excluded.
    if (is.null(loadPackages)) {
        loadPackages <- raw_input_parser(as.list(match.call()), varnames_to_exclude = c('loadPackages', 'lib.location', 'dry.run', 'quietly', 'appendLibPaths', 'pick.last', 'packNameVersionList', 'skipDependencies'))
    }

    if (length(loadPackages) == 1 && strtrim(names(loadPackages), 2) == 'c(') {
        stop('Please make sure that you call `library_VC(loadPackages = c(xx = yy))` when you would like to use a named character vector.\nAlternatively, remove the vector elements and call `library_VC(xx = yy)` directly.')
    }

    # If still other libraries are set as active libraries, reset the library to just 1 lib for the build in functions (= `.Library`).
    if (!quietly & interactive() & length(sys.calls()) == 1 & !all(grepl(normPath(lib.location), normPath(.libPaths())) | grepl(normPath(.Library), normPath(.libPaths())))) {
        warning(paste0('\nlibrary_VC: Extra libraries were found.\n',
                       'Library_VC will exclude those when loading packages, please be aware `library()` does not\n',
                       'and might load a package from an unexpected location.\n',
                       'Please use `.libPaths(.Library)` before using library_VC to suppress this warning.\n\n'))
    }

    # check if the package version that is provided is a correct version (this catches a wrong input like `c(dplyr = '0.5.0', databasequeries')`  )
    if (length(loadPackages) != 0 && !is.na(loadPackages) &&
        any(sapply(loadPackages, function(x) {attributes(regexpr('>?=?\\s?\\d+(\\.\\d+){1,3}', x))$match.length != nchar(x) && nchar(x) > 0}) & !checkIfBasePackage(names(loadPackages)))) {
        stop(sprintf('Not all package versions that are provided seem to be valid version numbers. The following was received:\n%s', paste(paste0(names(loadPackages), ' (', loadPackages, ')'), collapse = ', ')))
    }

    n_skipped <- 0
    for (iPackage in unique(names(loadPackages))) {
        startChar <- ifelse(iPackage == names(loadPackages)[1 + n_skipped], ' \\', '  |')
        stackStr <- sprintf('%s%s', paste(collapse = '  ', rep('|', sum(grepl('library_VC', sys.calls())) - 1)), startChar)
        stackStr <- gsub('^  \\||^ \\\\', '*__', paste0(collapse = '', c(stackStr, rep('_', max(16 - nchar(stackStr), 1))))) # adds remaining dashes and changes stack '0' to 'new package' indication ('*__')

        # if base package, simply load and continue
        if (checkIfBasePackage(iPackage)) {
            library(iPackage, character.only = TRUE, quietly = quietly)
            n_skipped <- n_skipped + 1
            next
        }

        if (iPackage %in% c(skipDependencies, 'RVClibrary')) {
            if (isVersionCompatible(loadPackages[iPackage], packNameVersionList[iPackage])) {
                n_skipped <- n_skipped + 1
                next
            } else {
                if (paste0('package:', iPackage) %in% search()) {
                    stop(sprintf(paste('An already loaded package "%s" (version: %s) is from an earlier version than requested here (%s).',
                                       '\nWe will not try to detach since that could cause unexpected behaviour.',
                                       '\nPlease detach it manually (e.g. `detachAll(packageList = \'%s\')`) and',
                                       'don\'t load it explicitly before this package is loaded.'), iPackage, packNameVersionList[iPackage], loadPackages[iPackage], iPackage))
                }
                packNameVersionList <- packNameVersionList[!names(packNameVersionList) == iPackage]
            }
        }

        cat(stackStr)
        packVersion <- getCorrectVersion(loadPackages[iPackage], lib.location, pick.last = pick.last, quietly = quietly)
        # if already loaded in previous recursive iteration where dry.run was TRUE (appended to skipDependencies)

        package.location <- paste(lib.location, iPackage, packVersion, sep = '/')

        if (!dir.exists(package.location)) {
            stop(sprintf('The package "%s" or its version "%s" could not be accessed and might not be present.', iPackage, packVersion))
        }

        # load dependencies: [1] from an override dependency file [2] from the original dependencies.
        overrideFile <- paste(package.location, c('vc_override_dependencies.txt'), sep = '/')
        if (file.exists(overrideFile)) {
            dependingPackages <- parse_dependency_string(readChar(overrideFile, file.info(overrideFile)$size))
        } else {
            packDesc <- packageDescription(iPackage, lib.loc = package.location)
            dependingPackages <- parse_dependency_string(gsub(paste(packDesc$Depends, packDesc$Imports, sep = ','), pat = ',,', rep = ','))
        }

        # recusively load dependencies
        # The skipDependencies is necessary for `dry.run=TRUE`. With `dry.run=FALSE` the dependency is loaded and skipped in the next itteration.
        packNameVersionList <- library_VC(loadPackages        = dependingPackages,
                                          lib.location        = lib.location,
                                          packNameVersionList = packNameVersionList,
                                          skipDependencies    = c( names(packNameVersionList), skipDependencies ),
                                          pick.last           = pick.last,
                                          dry.run             = TRUE)

        packNameVersionList <- append(packNameVersionList, setNames(packVersion, iPackage))

        currentLibs <- .libPaths()

        if (!dry.run) {
            add_package_VC_libPaths(packNameVersionList, lib.location)
            remove_undesired_libPaths()

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

    # if not quietly, and at top level of stack, show an example library call.
    if (!quietly && length(sys.calls()) == 1) {
        printExampleLibCall(packNameVersionList)
    }

    return(if(quietly) {invisible(packNameVersionList)} else {packNameVersionList})
}
