
#' Set lib.location to test_library
#'
.set_test_lib_location <- function() {
    # Reset the value after finishing in the parent function
    old <- Sys.getenv('R_MV_LIBRARY_LOCATION')
    do.call(on.exit,
            list(substitute(Sys.setenv(R_MV_LIBRARY_LOCATION = old)),
                 add = TRUE), envir = parent.frame())

    suppressMessages(lib.location(
        ifelse(dir.exists('../test_library/'),
               '../test_library/', 'tests/test_library/'
        )))
}


# # Test functions to help testing in seperate processes. -------------------
#
#
# tst_env <- new.env(parent = emptyenv())
# tst_env$desc = ''
# tst_env$tests = list()
#
#
# #' Functions to help with testing inside seperate processes.
# #'
# #' The functions can be used in the following way:\cr\cr
# #' \enumerate{
# #' \item{Use \code{.initialize_test(desc = 'My first test')} to setup for a new test and create a clean slate.}
# #' \item{Then use the different helper functions \code{expect_...} to perform your tests just like you would do with \code{testthat::expect_...} functions.}
# #' \item{To give back the test outcomes, since it is a separate process, you must use \code{test_outcomes <- .get_tests()}.}
# #' \item{Finally, when all the tests are done and the results are passed back, perform: \code{.run_test_batch(test_outcomes)}.}
# #' }
# #'
# #' @name test_helpers
# #'
# NULL
#
# #' @rdname test_helpers
# #'
# .initialize_test <- function(desc) {
#     stopifnot(is.character(desc), length(desc) == 1)
#     assign('desc',  desc,   envir = tst_env)
#     assign('tests', list(), envir = tst_env)
# }
#
# #' @rdname test_helpers
# #'
# .add_test <- function(type, A, B) {
#     call <- as.character(sys.call(-1))
#
#     assign('tests', c(get('tests', tst_env), list(list(
#         type = type, a = A, b = B,
#         # The reference (value of B) is already printed by testthat,
#         # so there is no need to include the original value of that one.
#         # call[2] refers to the code that produced the first input argument of the .exp... function.
#         call = call[2]))), envir = tst_env)
# }
#
# #' @rdname test_helpers
# #'
# expect_error <- function(expr, exp_msg) {
#     err_msg <- ''
#     tryCatch({expr}, error = function(err) {
#         err_msg <<- err$message
#     })
#     .add_test('error', err_msg, exp_msg)
# }
#
# #' @rdname test_helpers
# #'
# expect_match <- function(expr, regex) {
#     .add_test(type = 'match', A = expr, B = regex)
# }
#
# #' @rdname test_helpers
# #'
# expect_equal <- function(expr, ref) {
#     .add_test('equal', expr, ref)
# }
#
# #' @rdname test_helpers
# #'
# expect_false <- function(expr) {
#     .add_test('false', expr, FALSE)
# }
#
# #' @rdname test_helpers
# #'
# expect_true <- function(expr) {
#     .add_test('true', expr, TRUE)
# }
#
# #' @rdname test_helpers
# #'
# .get_tests <- function() {
#     as.list(tst_env)
# }
#
# #' @rdname test_helpers
# #'
# .run_test_batch <- function(test_outcomes = .get_tests()) {
#     testthat::test_that(test_outcomes$desc, {
#         for (test in test_outcomes$tests) {
#
#             # 'test' is a list with the fields 'type', 'a', 'b' and 'call'.
#             # Where 'type' can contain 'match', 'error', 'true', 'false' or 'equal'.
#             if (test$type == 'equal') {
#                 with(test, expect_equal(a, b, label = call))
#
#             } else if (test$type == 'true') {
#                 expect_true(test$a, label = test$call)
#
#             } else if (test$type == 'false') {
#                 expect_false(test$a, label = test$call)
#
#             } else if (test$type %in% c('match', 'error')) {
#
#                 with(test, expect_match(a, b, label = call))
#             }
#         }
#     })
# }

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
    .set_test_lib_location()

    # Define your clean slate:
    detachAll(packageList = c('package.a', 'package.b', 'package.d', 'package.c', 'package.f', 'package.e'))
    if (also_clean_install_dir) {
        lib.dependencies
    }

    # It seems that testthat wants to find other packages like 'waldo' (for comparison) during execution
    # Therefore we cannot set the .libPaths to only '.Library'
    .libPaths(c(dirname(system.file(package = 'testthat')), .Library))

    force(expr)
}

