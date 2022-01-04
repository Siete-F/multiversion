# source('../test_helpers.R')

with_safe_package_tester({

    test_that(desc = "load package.b, check if package.c is only in namespace", {
        .set_test_lib_location()

        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package.b
        msg <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')

        # check which packages are loaded
        expect_true('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())
        expect_false(any(grepl('package.c', .libPaths())))

        # Because the namespace is loaded, does not mean the package can be loaded
        expect_error(library(package.c), 'there is no package called .*')

        # Although direct package use is simply supported because it was a dependency.
        expect_equal(package.c::what_version_are_you(), '15.2.9')

        # Test loading a base package:
        expect_silent(lib.load(base))
})
})


with_safe_package_tester({

    test_that(desc = "latently loading package.c version 15.2.9 will fix the version", {
        .set_test_lib_location()

        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package a
        lib.load(package.b , pick.last = TRUE, quietly = TRUE)

        # check which packages are loaded
        expect_true('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())

        # Because the namespace is loaded, does not mean the package can be loaded
        expect_error(library(package.c), 'there is no package called .*')
        lib.load(package.c, pick.last = FALSE, quietly = TRUE)

        # Although direct package use is simply supported because it was a dependency.
        expect_equal(what_version_are_you(), '15.2.9')

})
})


with_safe_package_tester({

    test_that(desc = "Silence is obeyed", {
        .set_test_lib_location()

        # load package a
        msg <- capture.output(lib.load(package.b, quietly = TRUE, verbose = FALSE), type = 'message')
        expect_equal(msg, character())

        msg <- capture.output(lib.load(package.b, quietly = FALSE, verbose = FALSE), type = 'message')
        expect_equal(length(msg), 2)

        expect_error(lib.load(package.b, quietly = TRUE, verbose = TRUE),
                   'We cannot be quiet and verbose at the same time\\.\\.\\.')

        msg <- capture.output(lib.load(package.c, quietly = FALSE, verbose = TRUE), type = 'message')
        msg <- msg[nzchar(msg)]
        expect_match(msg[[1]], '15.2.8.*package.c')
        expect_match(msg[[2]], 'Attaching package: .package\\.c.') # (the dots represent the quotes in regex)
        expect_match(msg[[3]], 'masked') # the function "what_version_are_you()" is masked

        msg <- capture.output(lib.load(package.c, quietly = TRUE), type = 'message')
        expect_equal(msg, character())

})
})
