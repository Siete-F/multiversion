

test_outcomes <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = 'load package.b, pick.last = TRUE')
        .set_test_lib_location()
        stopifnot(!'package:package.b' %in% search())
        stopifnot(!'package:package.c' %in% search())

        # load package b, with latest version of package c
        .exp_error(package.c::what_version_are_you(), "there is no package called 'package.c'")

        msg <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')
        .exp_match(msg[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
        .exp_match(msg[[2]], '\\\\__[_ ]+Only [ ]*15.2.9.*package.c')

        # check which packages are loaded
        .exp_true('package:package.b' %in% search())
        .exp_false('package:package.c' %in% search())
        .exp_equal(package_b1(), 'package_b1')
        .exp_equal(what_version_are_you(), '1.0.0')

        # package.c is a dependency, no specific version.
        # Check if package.c can be accessed non the less. (must be made possible by attaching instead of loading it)
        .exp_error(package_c1(), 'could not find function')
        .exp_error(package.c::package_c1(), "is not an exported object from 'namespace:package.c'")
        .exp_equal(package.c::package_c2(), 'package_c2')
        .exp_equal(package.c::what_version_are_you(), '15.2.9')

        # Just test some random other packages
        .exp_false('package:package.a' %in% search())
        .exp_false('package:package.d' %in% search())
        .exp_false('package:package.e' %in% search())

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes)

# reporter_origin <- testthat::get_reporter()
#
# reporter <- callr::r(
#     function(reporter) {
#         suppressWarnings(devtools::load_all())
#
#         with_reporter(reporter, {
#
#             test_that("load package.b, pick.last = TRUE", {
#
#
#                 .set_test_lib_location()
#
#                 expect_false('package:package.b' %in% search())
#                 expect_false('package:package.c' %in% search())
#
#                 # load package b, with latest version of package c
#                 expect_error(package.c::what_version_are_you(), "there is no package called 'package.c'")
#
#                 msg <- capture.output(lib.load(package.b, pick.last = TRUE), type = 'message')
#                 expect_match(msg[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
#                 expect_match(msg[[2]], '\\\\__[_ ]+Only [ ]*15.2.9.*package.c')
#
#                 # check which packages are loaded
#                 expect_true('package:package.b' %in% search())
#                 expect_false('package:package.c' %in% search())
#                 expect_equal(package_b1(), 'package_b1')
#                 expect_equal(what_version_are_you(), '1.0.0')
#
#                 # package.c is a dependency, no specific version.
#                 # Check if package.c can be accessed non the less. (must be made possible by attaching instead of loading it)
#                 expect_error(package_c1(), 'could not find function')
#                 expect_error(package.c::package_c1(), "is not an exported object from 'namespace:package.c'")
#                 expect_equal(package.c::package_c2(), 'package_c2')
#                 expect_equal(package.c::what_version_are_you(), '15.2.9')
#
#                 # Just test some random other packages
#                 expect_true('package:package.a' %in% search())
#                 expect_false('package:package.d' %in% search())
#                 expect_false('package:package.e' %in% search())
#
#
#             })
#         })
#     },
#     args = list(reporter = reporter_origin),
#     show = TRUE)

# testthat::set_reporter(reporter)

# test_that('test 2', {
#     expect_equal(2+2, 4)
#     expect_equal(2+2, 8)
# })
