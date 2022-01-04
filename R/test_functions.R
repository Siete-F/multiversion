
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


# Test functions to help testing in seperate processes. -------------------


tst_env <- new.env(parent = emptyenv())
tst_env$desc = ''
tst_env$tests = list()

#' Functions to help with testing inside seperate processes.
#'
#' The functions can be used in the following way:\cr\cr
#' \enumerate{
#' \item{Use \code{.initialize_test(desc = 'My first test')} to setup for a new test and create a clean slate.}
#' \item{Then use the different helper functions \code{.exp_...} to perform your tests just like you would do with \code{testthat::expect_...} functions.}
#' \item{To give back the test outcomes, since it is a separate process, you must use \code{test_outcomes <- .get_tests()}.}
#' \item{Finally, when all the tests are done and the results are passed back, perform: \code{.run_test_batch(test_outcomes)}.}
#' }
#'
#' @name test_helpers
#'
NULL

#' @rdname test_helpers
#'
.initialize_test <- function(desc) {
    stopifnot(is.character(desc), length(desc) == 1)
    tst_env$desc = desc
    tst_env$tests = list()
}

#' @rdname test_helpers
#'
.add_test <- function(type, A, B) {
    call <- as.character(sys.call(-1))

    tst_env$tests[[length(tst_env$tests) + 1]] <<- list(
        type = type, a = A, b = B,
        # The reference (value of B) is already printed by testthat,
        # so there is no need to include the original value of that one.
        # call[2] refers to the code that produced the first input argument of the .exp... function.
        call = call[2])

}

#' @rdname test_helpers
#'
.exp_error <- function(expr, exp_msg) {
    err_msg <- ''
    tryCatch({expr}, error = function(err) {
        err_msg <<- err$message
    })
    .add_test('error', err_msg, exp_msg)
}

#' @rdname test_helpers
#'
.exp_match <- function(expr, regex) {
    .add_test(type = 'match', A = expr, B = regex)
}

#' @rdname test_helpers
#'
.exp_equal <- function(expr, ref) {
    .add_test('equal', expr, ref)
}

#' @rdname test_helpers
#'
.exp_false <- function(expr) {
    .add_test('false', expr, FALSE)
}

#' @rdname test_helpers
#'
.exp_true <- function(expr) {
    .add_test('true', expr, TRUE)
}

#' @rdname test_helpers
#'
.get_tests <- function() {
    return(as.list(tst_env))
}

#' @rdname test_helpers
#'
.run_test_batch <- function(test_outcomes = .get_tests()) {
    testthat::test_that(test_outcomes$desc, {
        for (test in test_outcomes$tests) {

            # 'test' is a list with the fields 'type', 'a', 'b' and 'call'.
            # Where 'type' can contain 'match', 'error', 'true', 'false' or 'equal'.
            if (test$type == 'equal') {
                with(test, expect_equal(a, b, label = call))

            } else if (test$type == 'true') {
                expect_true(test$a, label = test$call)

            } else if (test$type == 'false') {
                expect_false(test$a, label = test$call)

            } else if (test$type %in% c('match', 'error')) {

                with(test, expect_match(a, b, label = call))
            }
        }
    })
}
