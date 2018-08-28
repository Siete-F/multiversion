args <- commandArgs(trailingOnly = TRUE)
lib.location <- args[1]
packagesToInstall <- args[2]
RVClibrary.location <- args[3]

error_str <- function(prestring, ...) {paste0(sprintf(prestring, ...), '\nExpected input:\n1) a valid VC library location.\n',
                                              '2) a character string with the package pane to install.\n',
                                              '3) and optionally the directory where the `RVClibrary` package can be loaded. Defaults to `Sys.getenv("R_LIBS_USER")`, your default lib.\n')}

H <- list()
tryCatch(withCallingHandlers({

    if (is.na(RVClibrary.location)) {
        RVClibrary.location <- Sys.getenv("R_LIBS_USER")
    }

    if (!dir.exists(lib.location)) {stop(error_str('The version controlled library could not be found at the provided location: "%s".', lib.location))}

    if (!dir.exists(RVClibrary.location)) {stop(error_str('The `RVClibrary` package was not found. The directory "%s" does not exist.', RVClibrary.location))}

    # input checks:
    if (suppressMessages(!require(RVClibrary, lib.loc = RVClibrary.location))) {
        stop(paste0("\nPlease install this single tarball manually. It can be installed using something like:\n",
                    "`install.packages('.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n"))
    }

    install.packages_VC(installPackages      = trimws(strsplit(packagesToInstall, split = ',')[[1]]),
                        lib.location         = lib.location,
                        add_to_VC_library    = FALSE,
                        execute_with_Rscript = FALSE)

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
