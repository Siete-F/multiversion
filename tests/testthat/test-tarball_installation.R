
with_safe_package_tester({

    test_that(desc = "install to temp lib and then load from temp lib.", {
        .set_test_lib_location()

        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # We can test this error when it is not installed.
        expect_error(lib.load(package.a = '0.4.0', also_load_from_temp_lib = TRUE),
                     'The requested version "0.4.0" for package "package.a" is not installed.')

        # Now we install it...
        targz_path <- test_path('..', 'package_for_install_testing', 'package.a_0.4.0.tar.gz')

        # the condition for 'testit' is never met (so the package is always installed) because it is not added to the library.
        # Why 'testit'? Because it is rarely used so no danger in it already being loaded.
        lib.install_tarball(tarball = targz_path,
                            dependencies = c(testit = '> 0.1.0'),
                            install_temporarily = TRUE)

        msg <- capture.output(lib.load(package.a = '0.4.0',
                                       pick.last = TRUE,
                                       also_load_from_temp_lib = TRUE), type = 'message')

        expect_match(msg, "Version 0.4.0   INSTALLED  for package 'package.a'")
    })

}, also_clean_install_dir = TRUE)

