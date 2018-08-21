args <- commandArgs(trailingOnly = TRUE)

lib.location <- args[1]
load_packages <- args[2]
input_arguments_file <- args[3]
RVClibrary.location <- args[4]

# By default we expect that the `RVClibrary` is installed in the default user library.

error_str <- function(prestring, ...) {paste0(sprintf(prestring, ...), '\nExpected input:\n1) a valid VC library location.\n',
                                             '2) a packages string (e.g. "dplyr (0.5.0), ggplot2, tidyr (1.2.3)")',
                                             ' indicating the dependencies that needs to be loaded.\n',
                                             '3) the location of the `Rds` file with a list containing the ',
                                             'function (function or string) and arguments to call it with.\n',
                                             '4) and optionally the directory where the `RVClibrary` package can be loaded.',
                                             ' Defaults to `Sys.getenv("R_LIBS_USER")`, your default lib.\n')}

H <- list()
tryCatch(withCallingHandlers({

    if (is.na(RVClibrary.location)) {
        RVClibrary.location <- Sys.getenv("R_LIBS_USER")
    }

    if (!dir.exists(RVClibrary.location)) {stop(error_str('The `RVClibrary` package was not found. The directory %s" does not exist.', RVClibrary.location))}

    if (!dir.exists(lib.location)) {stop(error_str('The version controlled library could not be found at the provided location: %s', lib.location))}

    # Set library location for loading the `RVClibrary` package.
    RVC_lib_path <- gsub('[/\\]RVClibrary$', '', RVClibrary.location)

    if (suppressMessages(!require(RVClibrary, lib.loc = RVC_lib_path))) {
        stop(sprintf(paste0("The package `RVClibrary` could not be found at the provided (or default) location: %s.",
                            "\nPlease install this single tarball manually using something like:\n",
                            "install.packages('M:/.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n"),
                     RVC_lib_path))
    }
    .libPaths(.Library)

    library_VC(lib.location = lib.location, loadPackages = parse_dependency_string(load_packages))

    # loading the input...
    func_args <- readRDS(input_arguments_file)

    output <- do.call(func_args[[1]], func_args[seq_along(func_args) != 1])

    # save the output to a file
    temp_output_save_location <- paste0(dirname(tempdir()), '/R_execute_with_packages_output_', gsub('\\s|-|:', '_', format(Sys.time())), '.Rds')
    saveRDS(output, file = temp_output_save_location)
    if (!file.exists(temp_output_save_location)) {stop('The file we tried to save was not saved correctly.')}
    cat(paste0('\noutput file:<', temp_output_save_location, '>\n\n'))

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
