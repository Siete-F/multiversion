
test_that("load package.d with shadowed dependencies", {

    .set_test_lib_location()

    stopifnot(!'package:package.d' %in% search())

    expect_error(lib.load(package.d, pick.last = FALSE, quietly = TRUE),
                 'The requested version "6.0.0" for package "package.c" is not installed.')
    expect_error(lib.load(package.d, pick.last = TRUE, quietly = TRUE),
                 'The requested version ">= 99.0.0" for package "package.c" is not installed.')

})

test_that("lib.load errors", {

    .set_test_lib_location()

    expect_error(lib.load(package.qqq, quietly = TRUE),
                 'There is no package "package.qqq" installed \\(yet\\). \\(requested version: ""\\)')

    expect_error(lib.load(quietly = c(TRUE, TRUE)), "length\\(quietly\\) == 1 is not TRUE")
    expect_error(lib.load(quietly = 'TRUE'),        "is.logical\\(quietly\\) is not TRUE")
    expect_error(lib.load(verbose = c(TRUE, TRUE)), "length\\(verbose\\) == 1 is not TRUE")
    expect_error(lib.load(verbose = 'TRUE'),        "is.logical\\(verbose\\) is not TRUE")
    expect_error(lib.load(dry_run = c(TRUE, TRUE)), "length\\(dry_run\\) == 1 is not TRUE")
    expect_error(lib.load(dry_run = 'TRUE'),        "is.logical\\(dry_run\\) is not TRUE")
    expect_error(lib.load(appendLibPaths = c(TRUE, TRUE)), "length\\(appendLibPaths\\) == 1 is not TRUE")
    expect_error(lib.load(appendLibPaths = 'TRUE'),        "is.logical\\(appendLibPaths\\) is not TRUE")
    expect_error(lib.load(pick.last = c(TRUE, TRUE)),      "length\\(pick.last\\) == 1 is not TRUE")
    expect_error(lib.load(pick.last = 'TRUE'),             "is.logical\\(pick.last\\) is not TRUE")
    expect_error(lib.load(pick.last = 5),                  "is.logical\\(pick.last\\) is not TRUE")
    expect_error(lib.load(also_load_from_temp_lib = c(TRUE, TRUE)), "length\\(also_load_from_temp_lib\\) == 1 is not TRUE")
    expect_error(lib.load(also_load_from_temp_lib = 'TRUE'),        "is.logical\\(also_load_from_temp_lib\\) is not TRUE")

    expect_error(lib.load(dummy = '< 5.0.0'),  "Not all package versions that are provided seem to be valid version numbers")
    expect_error(lib.load(dummy = '== 5.0.0'), "Not all package versions that are provided seem to be valid version numbers")
    expect_error(lib.load(dummy = '<> 5.0.0'), "Not all package versions that are provided seem to be valid version numbers")
    expect_error(lib.load(dummy = '5_0_0'),    "Not all package versions that are provided seem to be valid version numbers")
    expect_error(lib.load(dummy = '5_0_0', package.a = '0.1.0'), "Not all package versions that are provided seem to be valid version numbers")


    expect_error(lib.load(c(dummy = '5_0_0', package.a = '0.1.0')), "Please make sure that you call `lib.load\\(loadPackages = c\\(pack = 'version'\\)\\)`.*")
    expect_error(lib.load(loadPackages = 'dummy'), "Please make sure that you call `lib.load\\(loadPackages = c\\(pack = 'version'\\)\\)`.*")

})

