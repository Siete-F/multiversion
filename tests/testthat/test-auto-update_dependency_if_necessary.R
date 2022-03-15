# To work on tests and to understand testcases better, run:
#
# .set_test_lib_location()
# lib.installed_packages()

# when loading package.a 0.3.0 (makes e == 1.5) then package.f (e > 1.6.0), e must be updated.
with_safe_package_tester({

    test_that(desc = "package.e must be updated when loading package.a 0.3.0 (e == 1.5) then package.f (e > 1.6.0)", {

        stopifnot(!'package:package.a' %in% search())
        stopifnot(!'package:package.f' %in% search())
        stopifnot(requireNamespace('waldo', quietly = TRUE))

        # load package.a, where package.f will overwrite the dependency on 'e'
        msg <- capture.output(lib.load(package.a = '0.3.0', pick.last = FALSE), type = 'message')
        expect_match(msg[[1]], '0.3.0 .*package.a')
        expect_match(msg[[2]], '1.5.0 .*package.e')
        expect_match(msg[[3]], '1.0.0 .*package.f')
        expect_match(msg[[4]], '1.7.0 .*package.e')

        ver <- package.e::what_version_are_you()
        expect_equal(ver, '1.7.0')
        expect_true('package:package.a' %in% search())

        # These are not loaded but attached.
        expect_false('package:package.f' %in% search())
        expect_false('package:package.e' %in% search())

        expect_true(isNamespaceLoaded('package.f'))
        expect_true(isNamespaceLoaded('package.e'))

        msg <- capture.output(lib.load(package.a = '0.3.0', pick.last = FALSE), type = 'message')
        # When a namespace is already loaded and compatible, it will default to that version.

        expect_match(msg[[2]], '1.7.0 .*package.e')

})
})
