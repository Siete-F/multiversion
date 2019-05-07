# =================================================================
#     RVClibrary, multi-version package library management tool
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
# Lesser General Public License ('COPYING.LESSER') for more details.
# =================================================================


args <- commandArgs(trailingOnly = TRUE)

lib_location         <- args[1]
load_packages        <- args[2]
input_arguments_file <- args[3]
RVClibrary.location  <- args[4]
process_log          <- args[5]


error_str <- function(prestring, ...) {paste0(sprintf(prestring, ...), '\nExpected input:\n1) a valid VC library location.\n',
                                              '2) a packages string (e.g. "dplyr (0.5.0), ggplot2, tidyr (1.2.3)")',
                                              ' indicating the dependencies that needs to be loaded.\n',
                                              '3) the location of the `Rds` file with a list containing the ',
                                              'function (function or string) and arguments to call it with.\n',
                                              '4) and optionally the directory where the `RVClibrary` package can be loaded.',
                                              ' Defaults to `Sys.getenv("R_LIBS_USER")`, your default lib.\n')}
# If not provided
if (is.na(process_log)) {
    process_log <- normalizePath("executeWithPackage.log", mustWork = F)
}

if (!dir.exists(dirname(process_log))) {dir.create(dirname(process_log), recursive = TRUE)}

message(sprintf('Logs process in file: %s', process_log))
fid <- file(process_log, open = "wt")
sink(file = fid, type = 'message')
sink(file = fid)

H <- list()
tryCatch(withCallingHandlers({


    # By default we expect that the `RVClibrary` is installed in the default user library.
    if (is.na(RVClibrary.location)) {
        RVClibrary.location <- Sys.getenv("R_LIBS_USER")
    }

    if (!dir.exists(RVClibrary.location)) {stop(error_str('The `RVClibrary` package was not found. The directory %s" does not exist.', RVClibrary.location))}

    if (!dir.exists(lib_location)) {stop(error_str('The version controlled library could not be found at the provided location: %s', lib_location))}

    # Set library location for loading the `RVClibrary` package.
    RVC_lib_path <- gsub('[/\\]RVClibrary$', '', RVClibrary.location)

    if (suppressMessages(!require(RVClibrary, lib.loc = RVC_lib_path))) {
        stop(sprintf(paste0("The package `RVClibrary` could not be found at the provided (or default) location: %s.",
                            "\nPlease install this single tarball manually. It can be installed using something like:\n",
                            "`install.packages('.../RVClibrary_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n"),
                     RVC_lib_path))
    }
    .libPaths(.Library)

    lib.load(lib_location = lib_location, loadPackages = lib.packs_str2vec(load_packages), quietly = TRUE)

    # loading the input...
    func_args <- readRDS(input_arguments_file)

    output <- do.call(func_args[[1]], func_args[seq_along(func_args) != 1])

    # save the output to a file
    temp_output_save_location <- paste0(dirname(tempdir()), '/R_lib.execute_using_packagelist_output_', gsub('\\s|-|:', '_', format(Sys.time())), '.Rds')
    saveRDS(output, file = temp_output_save_location)
    if (!file.exists(temp_output_save_location)) {stop('The file we tried to save was not saved correctly.')}
    cat(paste0('\noutput file:<', temp_output_save_location, '>\n\n'))

}, error = function(e) H <<- sys.calls()), error = function(e) {message('ERROR: The following error was returned:\n', e$message, '\n\n')})


status <- 0
# If an error occured:
if (length(H) != 0) {
    H <- as.list(H)
    H[c(1:4, length(H)-1, length(H))] <- NULL # remove verbose stack details.
    cat('Error stack:\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')
    cat(paste0(collapse = '\n\n\n', '[', seq_along(H), '] ', H), '\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')

    # Quit the R script execution with failed status.
    status <- 2
}

sink(type = 'message')
sink()

# if succesfully finished, print the output file location string
if ('temp_output_save_location' %in% ls()) {cat(paste0('\noutput file:<', temp_output_save_location, '>\n\n'))}

# Quit the R script execution.
quit(save = 'no', status = status)
