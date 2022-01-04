# lib.installed_packages()
# READY

# when loading package.a 0.3.0 (makes e == 1.5) then package.f (e > 1.6.0), e must be updated.
test_outcomes1 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = "package.e must be updated when loading package.a 0.3.0 (e == 1.5) then package.f (e > 1.6.0)")
        .set_test_lib_location()

        stopifnot(!'package:package.a' %in% search())
        stopifnot(!'package:package.f' %in% search())

        # load package.a, where package.f will overwrite the dependency on 'e'
        msg <- capture.output(lib.load(package.a = '0.3.0', pick.last = FALSE), type = 'message')
        .exp_match(msg[[1]], '0.3.0 .*package.a')
        .exp_match(msg[[2]], '1.5.0 .*package.e')
        .exp_match(msg[[3]], '1.0.0 .*package.f')
        .exp_match(msg[[4]], '1.7.0 .*package.e')

        .exp_equal(package.e::what_version_are_you(), '1.7.0')
        .exp_true('package:package.a' %in% search())

        # These are not loaded but attached.
        .exp_false('package:package.f' %in% search())
        .exp_false('package:package.e' %in% search())

        .exp_true(isNamespaceLoaded('package.f'))
        .exp_true(isNamespaceLoaded('package.e'))

        msg <- capture.output(lib.load(package.a = '0.3.0', pick.last = FALSE), type = 'message')
        # When a namespace is already loaded and compatible, it will default to that version.

        .exp_match(msg[[2]], '1.7.0 .*package.e')

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes1)
