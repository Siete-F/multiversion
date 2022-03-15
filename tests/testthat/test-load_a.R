# To work on tests and to understand testcases better, run:
#
# .set_test_lib_location()
# lib.installed_packages()

with_safe_package_tester({

    test_that(desc = "simply load package.a", {
        stopifnot(!'package:package.a' %in% search())

        # load package a
        msg <- capture.output(lib.load(package.a), type = 'message')
        expect_match(msg, '\\+__[_ ]+Version [ ]*0.1.0.*package.a')

        # check which packages are loaded
        expect_true('package:package.a' %in% search())
        expect_equal(package_a1(), 'package_a1')

        # Just test some random other packages
        expect_false('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())
        expect_false('package:package.d' %in% search())
        expect_false('package:package.e' %in% search())

        expect_error(lib.load(package.a = '>=  0.1.0'),
                     'Not all package versions that are provided seem to be valid version numbers.')

    })
})


with_safe_package_tester({

    test_that(desc = "Load package.a with manual path provided", {

        stopifnot(!'package:package.a' %in% search())

        var <- 'R_MV_LIBRARY_LOCATION'
        lib_loc <- Sys.getenv(var)
        withr::with_envvar(setNames('', var), {
            # load package a
            expect_silent(lib.load(package.a, lib_location = lib_loc, quietly = TRUE))
        })
    })
})
