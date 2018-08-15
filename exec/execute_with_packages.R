args <- commandArgs(trailingOnly = TRUE)
lib.location <- args[1]
load_packages <- args[2]
string_2_execute <- args[3]

if (!dir.exists(lib.location)) {stop('Please provide a valid VC library location. Followed by a packages string (e.g. "dplyr (0.5.0), ggplot2, tidyr (1.2.3)"), the function and arguments to call.')}

# Set library
.libPaths(c(.Library, Sys.getenv("R_LIBS_USER")))

H <- list()
tryCatch(withCallingHandlers({

    if (suppressMessages(!require(RVClibrary))) {
        stop("\nPlease install this single tarball manually using something like:\n `install.packages('M:/.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n")
    }

    library_VC(lib.location = lib.location, loadPackages = parse_dependency_string(load_packages))

    eval(parse(text = string_2_execute))

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
