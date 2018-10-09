
#' Perform operation with a certain set of packages.
#'
#' This function can be used to perform some R operation with a configured set of loaded packages.
#'
#' @param packages_to_load A sting indicating which packages must be loaded (e.g. `"dplyr (0.5.0), ggplot2, tidyr (1.2.3)"`).
#' It is shaped the way like `depends` field is showing the dependencies of the different packages in `available.packages()`, which is equal to the `DESCRIPTION` file.
#' It will be parsed using `parse_dependency_string` to become an offten used format looking like: `"c(dplyr = '0.5.0', ggplot2 = '', tidyr = '1.2.3')"`, converting back can be done using `packageList2str()`.
#' @param func_handle A function object or the function name as a character string.
#' @param ... Provide all the remaining arguments which will be arguments for the function handle.
#' @param lib.location The location of the version controlled library. Defaults to R_VC_library_location(), which is the directory provided by the environment variable.
#' @param execute_quietly The complete process content will be printed when the process is finished. If FALSE, only the returned file name will be printed.
#'
#'
#'
#' @export
#'
execute_with_packages <- function(packages_to_load = NULL, func_handle, ...,
                                  execution_log_location = normPath(paste0('./execution_logs/execution_', gsub('\\s|-|:', '_', format(Sys.time())), '.txt')),
                                  lib.location = R_VC_library_location(), wait_for_response = TRUE, execute_quietly = FALSE) {
        Rscript_dir <- normPath(system('where Rscript', intern = T)[1])
        if (grepl('Could not find files for the given pattern(s)', Rscript_dir)) {stop('Please make sure `where Rscript` results in one or more valid paths. First one is used.')}

        RVClib_package_location <- RVClibrary_package_install_location()
        script_location <- normPath(file.path(RVClib_package_location, 'exec/execute_with_packages_script.R'))
        # stdout_assistent <- normPath(file.path(RVClib_package_location, 'exec/wtee.exe'))
        # if (!file.exists(stdout_assistent)) {stop('The tiny executable `wtee.exe` within the `exec` folder of the installed `RVClibrary` package could not be found.')}

        # Save the input as list on a temporary location
        temp_input_save_location <- paste0(dirname(tempdir()), '/R_execute_with_packages_input_', gsub('\\s|-|:', '_', format(Sys.time())), '.Rds')
        function_input <- list(func_handle, ...)
        saveRDS(function_input, file = temp_input_save_location)

        cmd <- sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s" "%s"', Rscript_dir, script_location, lib.location, packages_to_load, temp_input_save_location, RVClib_package_location, execution_log_location)
        # cmd <- sprintf('"%s" --vanilla "%s" "%s" "%s" "%s" "%s" | "%s" "cmd_std_out.log"', Rscript_dir, script_location, lib.location, packages_to_load, temp_input_save_location, RVClib_package_location, stdout_assistent)

        cat(sprintf("\nExecuting command...\n%s\n\n", cmd))
        system(cmd, wait = wait_for_response)

        # If the application is fired off solely to finish a task on it's own, just continue.
        if (!wait_for_response) {return()}

        # Print the output to the console if desired.
        return_str <- readChar(execution_log_location, file.info(execution_log_location)$size)
        if (!execute_quietly) {
            cat(paste0(collapse = '\n', c(return_str, '\n\n')))
        }

        output_index <- which(grepl('output file:<.:[/\\].*>', return_str))
        if (length(output_index) == 1) {

            returned_str <- gsub('output file:<|>$', '', return_str[output_index])
            cat(sprintf('loading results file: "%s".\n', returned_str))
            if (!file.exists(returned_str)) {stop(sprintf('The returned path: "%s" does not exist!', returned_str))}
            output_data <- readRDS(returned_str)
        } else if (length(output_index) > 1) {

            stop(paste0(collapse = '\n', c(return_str, '\n\n!!!Furthermore, it seems that 2 return strings were printed. This process might be called recursively, which is not supported!!!')))
        } else {

            stop(paste0(collapse = '\n', return_str))
        }

        # not caught in an `on.exit` statement because this is preferred for debugging and the files are in general very small.
        file.remove(temp_input_save_location)

        return(output_data)
}