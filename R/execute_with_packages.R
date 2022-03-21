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


#' Perform operation with a certain set of packages.
#'
#' This function can be used to perform R operations with a configured set of packages to load in a separate R process.
#' The package \code{callr} is required to use this functionality.
#' It will start a new process, then load the provided packages and execute your function.
#' The \code{callr} package may be provided via the R_MV_library or your standard library,
#' in which case it must be in a library where \code{.libPaths} is pointing to.
#'
#' @param packages_to_load An array indicating which packages must be loaded
#'   like "\code{c(dplyr = '0.5.0', ggplot2 = '', tidyr = '> 1.2.3')}".
#' @param func_handle A function object or the function name as a character string.
#' @param ... Provide all the remaining arguments which will be arguments for the function handle.
#' Note that every argument must be named and must match an argument in your func_handle.
#' @param .lib_location The location of the version controlled library.
#'   Defaults to lib.location(), which is the directory provided by the environment variable.
#' @param .wait_for_response If false, it will fire and forget and return immediately
#'  using \code{callr::r_process}, otherwise will use \code{callr::r}.
#' @param .run_quietly Controls the 'show' parameter of \code{callr::r} or \code{callr::r_process}.
#' @param .callr_arguments List specifying additional arguments for \code{callr::r} or \code{callr::r_process}
#' (depending on the \code{.wait_for_response} value). Note that \code{func}, \code{args},
#' \code{show} and \code{libpath} are already in use. Every parameter must be named.
#' @param .pick_last Passed to \code{lib.load(packages_to_load, ...)} inside the fired \code{callr} process.
#' @param .also_load_from_temp_lib Passed to \code{lib.load(packages_to_load, ...)} inside the fired \code{callr} process.
#'
#' @details
#' The additional arguments to callr: \code{.callr_arguments}, can for example be used
#' to keep a log of a detached process. By including the following
#' \code{.callr_arguments} for example: \cr
#' \preformatted{lib.execute_using_packagelist(
#'     ...,
#'     .callr_arguments   = list(stdout = paste0('./execution_', gsub('\\s|-|:', '_', format(Sys.time())), '.log'), stderr = "2>&1")
#' )
#' }
#' See the example below for a complete example.
#' When you do this, it somehow swallows the first character of every \code{stderr}
#' that is directly returned (also from \code{message} calls) when
#' \code{run_quietly = FALSE}, but the log file seems intact.
#'
#' @section Example:
#' If you would like to log the outcomes, provide the .callr_arguments:
#' \preformatted{
#' lib.execute_using_packagelist(
#'     packages_to_load   = c(package.a =  '0.1.0'),
#'     func_handle        = function() {an_important_value(); package_a1(5, 10)},
#'     .wait_for_response = TRUE,
#'     .callr_arguments   = list(stdout = paste0('./execution_', gsub('\\s|-|:', '_', format(Sys.time())), '.log'), stderr = "2>&1"),
#'     .run_quietly       = TRUE
#' )
#' }
#' Another more simple example:
#' \preformatted{
#' lib.execute_using_packagelist(
#'     packages_to_load   = c(dplyr =  ''),
#'     func_handle        = function() {mtcars}
#' )
#' }
#'
#' @return
#' Will return the outcome of your \code{func_handle}.
#'
#' @export
#'
lib.execute_using_packagelist <- function(packages_to_load = c(),
                                          func_handle,
                                          ...,
                                          .lib_location = lib.location(),
                                          .pick_last = FALSE,
                                          .also_load_from_temp_lib = FALSE,
                                          .wait_for_response = TRUE,
                                          .run_quietly = FALSE,
                                          .callr_arguments = list()) {

    # Check where to obtain the callr package.
    callr_available <- length(lib.available_versions('callr', lib_location = .lib_location)) > 0
    if (!callr_available) {
        callr_available <- requireNamespace("callr", quietly = TRUE)
        if (!callr_available) {
            stop('For this feature to work, callr needs to be installed.',
                 ' This can be installed in your R_MV_library or your standard library.')
        }
    } else {
        lib.load(loadPackages = c(callr = ''), lib_location = .lib_location)
    }

    # Check whether to use `r` or `r_process`
    callr_r <- if (!.wait_for_response) {
        callr::r_bg
    } else {
        # Add 'show' if running while waiting on the process
        .callr_arguments <- c(list(show = !.run_quietly), .callr_arguments)
        callr::r
    }

    # Gather func_handle arguments
    func_args <- list(...)

    # call callr with `.callr_arguments` and inside: func_handle with `...` (`func_args`) args
    do.call(
        args = .callr_arguments,
        function(...) {
            callr_r(
                function(packages_to_load, .lib_location, func_handle, .multiversion_package_path
                         , .pick_last, .also_load_from_temp_lib
                         , ...) {  # Refers to '...' input, for your custom function_handle.

                    chck <- require(multiversion, lib.loc = .multiversion_package_path, warn.conflicts = FALSE)

                    # When no installed instance is present (but it is loaded
                    # with devtools::load_all), just source it for development purposes.
                    if (!chck && dir.exists(paste0(.multiversion_package_path, '/multiversion/.git'))) {
                        message('Sourcing the locally checked out multiversion package.')
                        sapply(list.files(recursive = TRUE, full.names = TRUE, pattern = '.*\\.R',
                                          paste0(.multiversion_package_path, '/multiversion/R')), source)
                    }

                    # Load required packages
                    lib.load(loadPackages = packages_to_load,
                             lib_location = .lib_location,
                             pick.last    = .pick_last,
                             also_load_from_temp_lib = .also_load_from_temp_lib)

                    # EXECUTE
                    message('Executing your code...\n')
                    func_handle(...)
                }
                ,
                args = c(
                    list(.lib_location              = .lib_location,
                         packages_to_load           = packages_to_load,
                         func_handle                = func_handle,
                         .pick_last                 = .pick_last,
                         .also_load_from_temp_lib   = .also_load_from_temp_lib,
                         .multiversion_package_path = dirname(system.file(package = 'multiversion'))),
                    func_args),  # Refers to '...' input, for your custom function_handle.

                libpath = .Library,
                ...)  # End "callr::r"    '...' refers to 'callr_arguments'
        })
}
