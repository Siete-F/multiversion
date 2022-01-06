# To work on tests and to understand testcases better, run:
#
# .set_test_lib_location()
# lib.installed_packages()

with_safe_package_tester({

    test_that(desc = 'load package.b, pick.last = TRUE', {
        .set_test_lib_location()
        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package b, with latest version of package c
        expect_error(package.c::what_version_are_you(), "there is no package called 'package.c'")

        msg <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')
        expect_match(msg[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
        expect_match(msg[[2]], '\\\\__[_ ]+Version [ ]*15.2.9  is picked.*package.c')

        # check which packages are loaded
        expect_true('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())
        expect_equal(package_b1(), 'package_b1')
        expect_equal(what_version_are_you(), '1.0.0')

        # package.c is a dependency, no specific version.
        # Check if package.c can be accessed non the less. (must be made possible by attaching instead of loading it)
        expect_error(package_c1(), 'could not find function')
        expect_error(package.c::package_c1(), "is not an exported object from 'namespace:package.c'")
        expect_equal(package.c::package_c2(), 'package_c2')
        expect_equal(package.c::what_version_are_you(), '15.2.9')

        # Just test some random other packages
        expect_false('package:package.a' %in% search())
        expect_false('package:package.d' %in% search())
        expect_false('package:package.e' %in% search())

})
})


with_safe_package_tester({

    test_that(desc = 'load package.b, pick.last = FALSE', {
        .set_test_lib_location()
        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package b, with first version of package c (the default behaviour of pick.last = F)
        msg <- capture.output(lib.load(package.b), type = 'message')
        expect_match(msg[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
        expect_match(msg[[2]], '\\\\__[_ ]+Version [ ]*15.2.8  is picked.*package.c')

        # check which packages are loaded
        expect_true('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())
        expect_equal(package_b1(), 'package_b1')
        expect_equal(what_version_are_you(), '1.0.0')

        # package.c is a dependency, no specific version.
        # Check if package.c can be accessed non the less. (must be made possible by attaching instead of loading it)
        expect_error(package_c1(), 'could not find function')
        expect_equal(package.c::package_c1(), 'package_c1')
        expect_equal(package.c::what_version_are_you(), '15.2.8')

        # Just test some random other packages
        expect_false('package:package.a' %in% search())
        expect_false('package:package.d' %in% search())
        expect_false('package:package.e' %in% search())

})
})



# Loading package b, pick.first, should freeze the version of package.c to 15.2.8,
# unless the newly requested version is not compatible with this new demand.
with_safe_package_tester({

    test_that(desc = 'load package.b, pick.last = FALSE then TRUE', {
        .set_test_lib_location()
        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        msg1 <- capture.output(lib.load(package.b, pick.last = FALSE), type = 'message')
        expect_match(msg1[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
        expect_match(msg1[[2]], '\\\\__[_ ]+Version [ ]*15.2.8  is picked.*package.c')

        msg2 <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')
        expect_equal(msg1, msg2)
        # (not necessary, but to make it more clear that really only version 15.2.8 is loaded (found as loaded namespace) and not 15.2.9)
        expect_match(msg2[[2]], '\\\\__[_ ]+Version [ ]*15.2.8  is picked.*package.c')

        # check which packages are loaded
        expect_true('package:package.b' %in% search())
        expect_false('package:package.c' %in% search())
        expect_equal(package_b1(), 'package_b1')
        expect_equal(what_version_are_you(), '1.0.0')
        expect_equal(package.c::what_version_are_you(), '15.2.8')

})
})
