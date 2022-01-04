# source('../test_helpers.R')

test_outcomes1 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all(export_all = TRUE)))

        multiversion:::.initialize_test(desc = "load package.b, check if package.c is only in namespace")
        multiversion:::.set_test_lib_location()

        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package.b
        msg <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')

        # check which packages are loaded
        multiversion:::.exp_true('package:package.b' %in% search())
        multiversion:::.exp_false('package:package.c' %in% search())
        multiversion:::.exp_false(any(grepl('package.c', .libPaths())))

        # Because the namespace is loaded, does not mean the package can be loaded
        multiversion:::.exp_error(library(package.c), 'there is no package called .*')

        # Although direct package use is simply supported because it was a dependency.
        multiversion:::.exp_equal(package.c::what_version_are_you(), '15.2.9')

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes1)


test_outcomes2 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = "latently loading c version 15.2.9 will force this version to be loaded")
        .set_test_lib_location()

        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package a
        lib.load(package.b , pick.last = TRUE, quietly = TRUE)

        # check which packages are loaded
        .exp_true('package:package.b' %in% search())
        .exp_false('package:package.c' %in% search())

        # Because the namespace is loaded, does not mean the package can be loaded
        .exp_error(library(package.c), 'there is no package called .*')
        lib.load(package.c, pick.last = FALSE, quietly = TRUE)

        # Although direct package use is simply supported because it was a dependency.
        .exp_equal(what_version_are_you(), '15.2.9')

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes2)


test_outcomes3 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = "Silence is obeyed")
        .set_test_lib_location()

        # load package a
        msg <- capture.output(lib.load(package.b, quietly = TRUE, verbose = FALSE), type = 'message')
        .exp_equal(msg, character())

        msg <- capture.output(lib.load(package.b, quietly = FALSE, verbose = FALSE), type = 'message')
        .exp_equal(length(msg), 2)

        .exp_error(lib.load(package.b, quietly = TRUE, verbose = TRUE),
                   'We cannot be quiet and verbose at the same time\\.\\.\\.')

        msg <- capture.output(lib.load(package.c, quietly = FALSE, verbose = TRUE), type = 'message')
        msg <- msg[nzchar(msg)]
        .exp_match(msg[[1]], '15.2.8.*package.c')
        .exp_match(msg[[2]], 'Attaching package: .package\\.c.') # (the dots represent the quotes in regex)
        .exp_match(msg[[3]], 'masked') # the function "what_version_are_you()" is masked

        msg <- capture.output(lib.load(package.c, quietly = TRUE), type = 'message')
        .exp_equal(msg, character())

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes3)
