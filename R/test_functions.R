.test_env <- new.env(parent = emptyenv())

.test_env$test_lib_path <- normalizePath(paste0('./test_library'), mustWork = FALSE)
cat('temp_dir is located here: ', .test_env$test_lib_path, '\n')

# .test_env$test_lib_path <- if (interactive()) {
#     # For interactive testing, use './tests/test_library'
#     paste0(normalizePath(ifelse(file.exists('../testthat.R'), '..', 'tests'), mustWork = TRUE), '/test_library')
# } else {
#     # For non-interactive (CRAN) use `paste0(tempdir(), '/test_library')`
#     normalizePath(paste0(tempdir(), '/test_library'), mustWork = FALSE)
# }



#' Set lib.location to test_library
#'
#' @importFrom utils unzip download.file
#'
.set_test_lib_location <- function() {
    # Reset the value after finishing in the parent function
    old <- Sys.getenv('R_MV_LIBRARY_LOCATION')
    do.call(on.exit,
            list(substitute(Sys.setenv(R_MV_LIBRARY_LOCATION = old)),
                 add = TRUE), envir = parent.frame())

    test_lib_path <- get('test_lib_path', envir = .test_env)

    if (!dir.exists(test_lib_path)) {

        # Defining paths:
        temp_zip_file <- paste0(dirname(test_lib_path), '/tmp/test_lib.zip')
        temp_tarball_dir <- paste0(dirname(test_lib_path), '/tmp/test_lib_tarballs')
        test_install_tarball_file <- paste0(dirname(test_lib_path), '/test_library_tarballs/package.a_0.4.0.tar.gz')

        # creating directories:
        dir.create(temp_tarball_dir, recursive = TRUE)
        dir.create(dirname(test_install_tarball_file))

        # Downloading files. One zip with the test lib tarballs and one seperate tarball which is used for a test installation.
        cat('Downloading the test lib zip file...\n')
        if (!file.exists(temp_zip_file)) {
            utils::download.file('https://github.com/Siete-F/multiversion/raw/test_lib_tarballs_0.3.4/tests/test_library.zip',
                                 destfile = temp_zip_file)
        }
        cat('Downloading the install test tarball...\n')
        if (!file.exists(test_install_tarball_file)) {
            utils::download.file('https://github.com/Siete-F/multiversion/raw/test_lib_tarballs_0.3.4/tests/package_for_install_testing/package.a_0.4.0.tar.gz',
                                 destfile = test_install_tarball_file)
        }
        cat('Unzipping and installing test library...\n')
        cat('tarballs are unzipped to: ', temp_tarball_dir, '\n')
        cat('zip file should be here: ', temp_zip_file, '\n')
        utils::unzip(normalizePath(temp_zip_file, mustWork = TRUE), exdir = temp_tarball_dir)
        on.exit(unlink(dirname(temp_tarball_dir), recursive = TRUE, force = TRUE), add = TRUE)

        dir.create(test_lib_path, recursive = T)

        lib.execute_using_packagelist(func_handle = lib.install_tarball,
                                      tarball = normalizePath(paste0(temp_tarball_dir, '/', c(
                                          "package.e_1.5.0.tar.gz",  "package.e_1.7.0.tar.gz",
                                          "package.f_1.0.0.tar.gz",  "package.f_2.0.0.tar.gz",
                                          "package.c_15.2-9.tar.gz", "package.c_15.2.8.tar.gz",
                                          "package.d_1.0.tar.gz",    "package.d_2.0.0.tar.gz",
                                          "package.b_1.0.0.tar.gz",  "package.a_0.1.0.tar.gz",
                                          "package.a_0.2.0.tar.gz",  "package.a_0.3.0.tar.gz"
                                      )), mustWork = TRUE),
                                      lib_location = test_lib_path,
                                      .lib_location = test_lib_path, .run_quietly = FALSE)
        Sys.sleep(120)

        # Place two 'override dependency' files
        writeChar('package.c (6.0.0)',     paste0(test_lib_path, '/package.d/1.0/vc_override_dependencies.txt'), eos = NULL)
        writeChar('package.c (>= 99.0.0)', paste0(test_lib_path, '/package.d/2.0.0/vc_override_dependencies.txt'), eos = NULL)
    # } else {
        # cat('The test lib already exists: ', test_lib_path, '\n')
    }

    # Setting the library location for the duration of a test
    suppressMessages(lib.location(test_lib_path))
}


#' Create a safe environment in which certain expressions can be tested
#'
#' Will reset the .libPaths, the 'R_MV_LIBRARY_LOCATION' environment variable to their old values
#' and will unload 'package.a' till 'package.f' when finishing the execution. \cr \cr
#'
#' Before execution it will set the following values:
#' \enumerate{
#' \item{.libPaths - will be set to .Library only.}
#' \item{\code{R_MV_LIBRARY_LOCATION} - will contain '../test_library/' or 'tests/test_library/' depending on the current directory}
#' }
#'
#' @param expr The expression that needs to be evaluated in this protected environment.
#' @param also_clean_install_dir If \code{lib.clean_install_dir()} must be run before and after the test.
#'
with_safe_package_tester <- function(expr, also_clean_install_dir = FALSE) {
    # Gather the current state
    old_paths <- .libPaths()

    # Define how to reset to unchanged environment (so how to return to the clean slate and undo potential effects done by 'expr')
    withr::defer({
        # Sorted in the order of dependencies...
        detachAll(packageList = c('package.a', 'package.b', 'package.d', 'package.c', 'package.f', 'package.e'))
        if (also_clean_install_dir) {
            lib.clean_install_dir()
        }
        .libPaths(old_paths)
    })

    # This sets the environment variable 'R_MV_LIBRARY_LOCATION' to the test_library, and reverts it when finished here.
    # When this is called for the first time, it will obtain the test packages to test the loading behavior.
    .set_test_lib_location()

    # Define your clean slate:
    detachAll(packageList = c('package.a', 'package.b', 'package.d', 'package.c', 'package.f', 'package.e'))
    if (also_clean_install_dir) {
        lib.dependencies
    }

    # It seems that testthat wants to find other packages like 'waldo' (for comparison) during execution
    # Therefore we cannot set the .libPaths to only '.Library'
    .libPaths(c(dirname(system.file(package = 'testthat')), dirname(system.file(package = 'waldo')), .Library))

    force(expr)
}

