args <- commandArgs(trailingOnly = TRUE)
lib.location <- args[1]
packagesToInstall <- args[2]

# input checks:

# Set library
.libPaths(c(.Library, Sys.getenv("R_LIBS_USER")))

H <- list()
tryCatch(withCallingHandlers({

    if (suppressMessages(!require(RVClibrary))) {
        stop("\nPlease install this single tarball manually using something like:\n `install.packages('M:/.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n")
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
    cat(paste(collapse = '', rep('-', 60)), '\n')
    cat(paste(collapse = '\n\n\n', H), '\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')

    # Quit the R script execution with failed status.
    quit(save = 'no', status = 2)
}

# Quit the R script execution.
quit(save = 'no')
