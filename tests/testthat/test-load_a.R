# To work on tests and to understand testcases better, run:
#
# .set_test_lib_location()
# lib.installed_packages()

with_safe_package_tester({

    test_that(desc = "simply load package.a", {
        stopifnot(!'package:package.a' %in% search())
        .set_test_lib_location()

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
