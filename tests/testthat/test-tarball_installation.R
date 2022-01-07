
empty_char <- structure(character())
test_dirs <- function() {
    # Due to 'with_safe_package_tester' which in turn calls '.set_test_lib_location',
    # this 'lib.location_install_dir' will refer to './tests/test_library'
    list.dirs(lib.location_install_dir(), full.names = F, recursive = F)
}

with_safe_package_tester({

    test_that(desc = "install to temp lib and then load from temp lib.", {

        stopifnot(!'package:package.a' %in% search())
        stopifnot(test_dirs() == empty_char)

        # The following is already done in with_safe_package_tester
        # > detachAll(packageList = 'package.a') ; lib.clean_install_dir()

        # We can test this error when it is not installed.
        expect_error(lib.load(package.a = '0.4.0', also_load_from_temp_lib = TRUE, quietly = TRUE),
                     'The requested version "0.4.0" for package "package.a" is not installed.')

        # Now we install it...
        targz_path <- test_path('..', 'package_for_install_testing', 'package.a_0.4.0.tar.gz')

        # the condition for 'testit' is never met (so the package is always installed) because it is not added to the library.
        # Why 'testit'? Because it is rarely used so no danger in it already being loaded.
        msg <- capture.output(
            lib.install_tarball(tarball = targz_path,
                                dependencies = c(package.c = '> 15.2.8', testit = '> 0.1.0'),
                                install_temporarily = TRUE)
            , type = 'message')

        expect_equal(test_dirs(), c('package.a', 'testit'))

        msg <- capture.output(lib.load(package.a = '0.4.0',
                                       pick.last = TRUE,
                                       also_load_from_temp_lib = TRUE), type = 'message')

        expect_match(msg, "Version 0.4.0   INSTALLED  for package 'package.a'")

        # Test 'lib.clean_install_dir' in short here.
        detachAll(packageList = 'package.a')
        lib.clean_install_dir()

        expect_equal(test_dirs(), empty_char)

    })

}, also_clean_install_dir = TRUE)


with_safe_package_tester({

    # In this test we work with the standard '\multiversion\tests\test_library'
    # as installation dir. But we work with '\multiversion\tests\testthat\temp_test_lib'
    # to convert to the mv lib structure.

    test_that(desc = "install to temp lib and convert to mv lib", {

        stopifnot(!'package:package.a' %in% search())
        stopifnot(test_dirs() == empty_char)

        # The following is already done in with_safe_package_tester
        # > detachAll(packageList = 'package.a') ; lib.clean_install_dir()

        # INSTALL
        targz_path <- test_path('..', 'package_for_install_testing', 'package.a_0.4.0.tar.gz')

        suppressMessages(
            lib.install_tarball(tarball = targz_path,
                                dependencies = c(testit = ''),
                                install_temporarily = TRUE))

        expect_equal(test_dirs(), c('package.a', 'testit'))

        # Cleanup after convert
        temp_test_lib <- test_path('temp_test_lib')
        unlink(temp_test_lib, recursive = TRUE, force = TRUE) # in case developer messed up.
        on.exit(unlink(temp_test_lib, recursive = TRUE, force = TRUE), add = TRUE)

        # CONVERT
        # failures
        expect_error(lib.convert(
            source_lib = lib.location_install_dir(lib.location()),
            destination_mv_lib = temp_test_lib, packages_to_convert = c('nonex1st1ngpackage', 'flinkbalen99')),
            'Not present: "nonex1st1ngpackage", "flinkbalen99"')

        # success - convert package.a
        msg <- capture.output(lib.convert(
            source_lib = lib.location_install_dir(lib.location()),
            destination_mv_lib = temp_test_lib, packages_to_convert = 'package.a'),
            type = 'message')

        expect_match(msg[2], "Succesfully copied files:")
        expect_match(msg[3], "package.a:22  $")  # only package.a should be converted
        expect_match(msg[5], "Failed copying files.*")
        expect_equal(msg[6], "")  # no files should have failed.

        # empty 'packages_to_convert'
        expect_message(lib.convert(
                source_lib = lib.location_install_dir(lib.location()),
                destination_mv_lib = temp_test_lib, packages_to_convert = c()),
                'Nothing to convert.')

        # success - (try) to convert all (two) packages, but 'package.a' is already there.
        msg <- capture.output(lib.convert(
            source_lib = lib.location_install_dir(lib.location()),
            destination_mv_lib = temp_test_lib),
            type = 'message')

        expect_match(msg[2], "Succesfully copied files:")
        expect_match(msg[3], "testit:22  $")  # testit should be found and converted.
        expect_match(msg[5], "Failed copying files.*")
        expect_match(msg[6], "package.a:22  $")

        # success - convert all two packages and overwrite.
        msg <- capture.output(lib.convert(
            source_lib = lib.location_install_dir(lib.location()),
            destination_mv_lib = temp_test_lib, force_overwrite = TRUE),
            type = 'message')

        expect_match(msg[2], "Succesfully copied files:")
        expect_match(msg[3], "package.a:22  testit   :22  $")  # testit should be found and converted.
        expect_match(msg[5], "Failed copying files.*")
        expect_equal(msg[6], "")

        # LOAD
        msg <- capture.output(lib.load(package.a = '0.4.0',
                                       lib_location = temp_test_lib,
                                       pick.last = TRUE,
                                       also_load_from_temp_lib = TRUE), type = 'message')

        expect_match(msg, "Exactly 0.4.0.*is used.*'package.a'")
        expect_equal(test_dirs(), c('package.a', 'testit'))

        # Before on.exit kicks in, I need to clean this myself
        detachAll(packageList = 'package.a')
    })

}, also_clean_install_dir = TRUE)
