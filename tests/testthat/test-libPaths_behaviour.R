
# Check if libPaths change in unexpected ways

test_outcomes1 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = '.libPaths must remain unchanged by default')
        .set_test_lib_location()

        old_paths <- .libPaths()
        lib.load(package.b, quietly = TRUE)
        .exp_equal(old_paths, .libPaths())

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes1)

test_outcomes2 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = '.libPaths must remain unchanged in dryrun to')
        .set_test_lib_location()

        old_paths <- .libPaths()
        lib.load(package.b, quietly = TRUE, dry.run = TRUE)
        .exp_equal(old_paths, .libPaths())

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes2)


test_outcomes3 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = '.libPaths are updated with appendLibPaths')
        .set_test_lib_location()

        old_paths <- .libPaths()
        lib.load(package.b, quietly = TRUE, appendLibPaths = TRUE, dry.run = TRUE)
        new_paths <- .libPaths()

        # It should result in two NEW paths:
        # [1] ".../multiversion/tests/test_library/package.c/15.2.8"
        # [2] ".../multiversion/tests/test_library/package.b/1.0.0"
        .exp_true(all(basename(setdiff(new_paths, old_paths)) %in% c('15.2.8', '1.0.0')))

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes3)


test_outcomes4 <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = "package.e must be updated when loading package.a 0.3.0 (e == 1.5) then package.f (e > 1.6.0)")
        .set_test_lib_location()

        old_paths <- .libPaths()
        lib.load(package.a = '0.3.0', appendLibPaths = TRUE, quietly = TRUE)
        new_paths <- .libPaths()

        # It should result in three NEW paths:
        # [2] ".../multiversion/tests/test_library/package.e/1.7.0"
        # [3] ".../multiversion/tests/test_library/package.f/1.0.0"
        # [4] ".../multiversion/tests/test_library/package.a/0.3.0"
        .exp_true(all(basename(setdiff(new_paths, old_paths)) %in% c('1.7.0', '1.0.0', '0.3.0')))

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes4)
