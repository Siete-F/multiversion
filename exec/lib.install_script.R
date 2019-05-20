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


args <- commandArgs(trailingOnly = TRUE)

lib_location            <- args[1]
packagesToInstall       <- args[2]
multiversion.location   <- args[3]
overwrite_or_add_update <- args[4]
verbose                 <- args[5]

# DEV CODE ONLY!!!
# cat('SCRIPT HERE!!!')
# #cat(is.null(options()$buildtools.with), '\n')
# #cat(is.null(options()$buildtools.check), '\n')
# cat(paste(names(Sys.getenv(c('PATH', 'MAKE', 'BINPREF'))), ':', Sys.getenv(c('PATH', 'MAKE', 'BINPREF')), '\n'))

error_str <- function(prestring, ...) {paste0(sprintf(prestring, ...), '\nExpected input:\n1) a valid VC library location.\n',
                                              '2) a character string with the package pane to install.\n',
                                              '3) and optionally the directory where the `multiversion` package can be loaded. Defaults to `Sys.getenv("R_LIBS_USER")`, your default lib.\n')}

H <- list()
tryCatch(withCallingHandlers({

    # By default we expect that the `multiversion` is installed in the default user library.
    if (is.na(multiversion.location)) {
        multiversion.location <- Sys.getenv("R_LIBS_USER")
    }

    if (!dir.exists(lib_location)) {stop(error_str('The version controlled library could not be found at the provided location: "%s".', lib_location))}

    if (!dir.exists(multiversion.location)) {stop(error_str('The `multiversion` package was not found. The directory "%s" does not exist.', multiversion.location))}

    # input checks:
    if (suppressMessages(!require(multiversion, lib.loc = gsub('[/\\]multiversion$', '', multiversion.location)))) {
        stop(paste0("\nPlease install this single tarball manually. It can be installed using something like:\n",
                    "`install.packages('.../multiversion_0.1.0.tar.gz', lib = Sys.getenv('R_LIBS_USER'), type = 'source', repos = NULL)`\n\n"))
    }

    lib.install(installPackages        = trimws(strsplit(packagesToInstall, split = ',')[[1]]),
                lib_location           = lib_location,
                install_temporarily    = TRUE,
                execute_with_Rscript   = FALSE,
                overwrite_this_package = as.logical(overwrite_or_add_update),
                verbose                = as.logical(verbose))

}, error = function(e) H <<- sys.calls()), error = function(e) {message('ERROR: The following error was returned:\n', e$message, '\n\n')})

# If an error occured:
if (length(H) != 0) {
    H <- as.list(H)
    H[c(1:4, length(H)-1, length(H))] <- NULL # remove verbose stack details.
    cat('Error stack:\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')
    cat(paste0(collapse = '\n\n\n', '[', seq_along(H), '] ', H), '\n')
    cat(paste(collapse = '', rep('-', 60)), '\n')

    # Quit the R script execution with failed status.
    quit(save = 'no', status = 2)
}

# Quit the R script execution.
quit(save = 'no')
