args <- commandArgs(trailingOnly = TRUE)
lib.location <- args[1]
packagePath  <- args[2]
dependencies <- args[3]
RVClibrary.location <- args[4]

error_str <- function(prestring, ...) {paste0(sprintf(prestring, ...), '\nExpected input:\n1) a valid VC library location.\n',
                                              '2) a character string with the package to install.\n',
                                              '3) a packages string (e.g. "dplyr (0.5.0), ggplot2, tidyr (1.2.3)")',
                                              ' indicating the dependencies that needs to be loaded.\n',
                                              '4) and optionally the directory where the `RVClibrary` package can be loaded.',
                                              ' Defaults to `Sys.getenv("R_LIBS_USER")`, your default lib.\n')}
H <- list()
tryCatch(withCallingHandlers({

    # input checks:
    if (is.na(RVClibrary.location)) {
        RVClibrary.location <- Sys.getenv("R_LIBS_USER")
    }

    if (!dir.exists(lib.location)) {stop(error_str('The version controlled library could not be found at the provided location: "%s".', lib.location))}

    if (!file.exists(packagePath)) {stop(error_str('The file "%s" provided does not exist.', packagePath))}

    if (!dir.exists(RVClibrary.location)) {stop(error_str('The `RVClibrary` package was not found. The directory "%s" does not exist.', RVClibrary.location))}

    if (suppressMessages(!require(RVClibrary, lib.loc = gsub('[/\\]RVClibrary$', '', RVClibrary.location)))) {
        stop(paste0("\nPlease install this single tarball manually. It can be installed using something like:\n",
                    "`install.packages('.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n"))
    }

    install.packages_VC_tarball(packagePath, dependencies = parse_dependency_string(dependencies), lib.location = lib.location, execute_with_Rscript = FALSE)

}, error = function(e) H <<- sys.calls()), error = function(e) {message('ERROR: The following error was returned:\n', e$message, '\n\n')})


# If an error occured:
if (length(H) != 0) {
    H <- as.list(H)
    H[c(1:4, length(H)-1, length(H))] <- NULL # remove verbose stack details.
    cat('\nError stack:\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')
    cat(paste0(collapse = '\n\n\n', '[', seq_along(H), '] ', H), '\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')

    # Quit the R script execution with failed status.
    quit(save = 'no', status = 2)
}

# Quit the R script execution.
quit(save = 'no')
