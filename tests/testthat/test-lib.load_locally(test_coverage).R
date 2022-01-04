# test_that("local process lib.load calls (for test coverage)", {
#
#     # Cleanup on exit
#     on.exit(unloadNamespace(asNamespace('package.c')), add = TRUE)
#
#     # Do your thing
#     lib.load(package.c = '15.2.8')
#     expect_true('package:package.c' %in% search())
#     expect_true(isNamespaceLoaded('package.c'))
#     expect_equal(what_version_are_you(), '15.2.8')
#     expect_equal(lib.package_version_loaded('package.c'), structure('15.2.8', .Names = 'package.c'))
# })
