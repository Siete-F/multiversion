#READY

test_that("lib.dependencies", {
    empty_named_char <- structure(character(), .Names = character())

    .set_test_lib_location()

    a <- lib.dependencies('package.a', do_print = F)
    expect_equal(a$`0.1.0`, structure(character(), .Names = character()))
    expect_equal(a$`0.2.0`, structure(c('', ''), .Names = c('package.e',  'package.f')))
    expect_equal(a$`0.3.0`, structure(c('>= 1.5', ''), .Names = c('package.e',  'package.f')))

    b <- lib.dependencies('package.b', do_print = F)
    expect_equal(b$`1.0.0`, c(package.c = ''))

    c <- lib.dependencies('package.c', do_print = F)
    expect_equal(names(c), c('15.2.8', '15.2.9'))
    expect_equal(c$`15.2.8`, structure(character(), .Names = character()))
    expect_equal(c$`15.2.9`, structure(character(), .Names = character()))

    empty <- lib.dependencies('package.non.existend')

    expect_equal(empty, list())
})


test_that("chooseVersion works", {

    .set_test_lib_location()
    expect_warning(
        chooseVersion(c(package.c = '> 0.1'), c("15.2.8", "15.2.9"), 'a_package'),
        'For package "a_package" the lowest optional version is a major release higher\\. Requested: "> 0\\.1", first option "15\\.2\\.8".'
    )

    expect_silent(chooseVersion('> 0.1', c("15.2.8", "15.2.9"), 'a_package', warn_for_major_diff = FALSE))

    expect_silent(chooseVersion('> 15.1', c("15.2.8", "15.2.9"), 'a_package'))

    expect_equal(chooseVersion('0.1', '0.1', 'a_package'), '0.1')
    expect_error(chooseVersion('0.2', '0.1', 'a_package'), 'The requested version "0.2" for package "a_package" is not installed.')
    expect_equal(chooseVersion('>= 0.1', '0.1', 'a_package'), '0.1')
    expect_equal(chooseVersion('>= 0.1', c('0.1', '0.2', '0.5-8'), 'a_package'), '0.1')

    # No warning because it is a minor version higher, not a major version
    expect_equal(expect_silent(chooseVersion('> 0.1', '0-5.8', 'a_package')), '0.5.8')
    expect_equal(chooseVersion('> 0.1', '0-5-8', 'a_package'), '0.5.8')
})


testthat::test_that("chooseVersion behaviour with major version differences", {

    .set_test_lib_location()

    expect_warning(
        val <- chooseVersion('>= 14.9.9', c("15.2.8", "15.2.9"), 'a_package', pick.last = FALSE),
        'For package "a_package" the lowest optional version is a major release higher\\. Requested: ">= 14\\.9\\.9", first option "15\\.2\\.8".'
    )
    expect_equal(val, '15.2.8')

    expect_warning(
        val <- chooseVersion('>= 14.9.9', c("15.2.8", "15.2.9"), 'a_package', pick.last = TRUE),
        'For package "a_package" the lowest optional version is a major release higher\\. Requested: ">= 14\\.9\\.9", first option "15\\.2\\.8".'
    )
    expect_equal(val, '15.2.9')

    ## Test the effect of the 'mv_prefer_within_major_version' option.
    expect_silent(
        val <- withr::with_options(
            list('mv_prefer_within_major_version' = 'false'), {
                chooseVersion('>= 14.9.9', c("15.2.8", "15.2.9"), 'a_package', pick.last = FALSE)
            }))
    expect_equal(val, '15.2.8')

    expect_warning(
        val <- withr::with_options(list('mv_prefer_within_major_version' = 'true'), {
            chooseVersion('>= 14.9.9', c("15.2.8", "15.2.9"), 'a_package', pick.last = FALSE)
        }), 'For package "a_package" the lowest optional version is a major release higher\\. Requested: ">= 14\\.9\\.9", first option "15\\.2\\.8".')
    expect_equal(val, '15.2.8')


    # Check behaviour when no warning is necessary. It should differ when a version within the requested major is available when pick.last = TRUE
    expect_equal(withr::with_options(list('mv_prefer_within_major_version' = 'false'), {
        chooseVersion('>= 14.9.9', c("14.10.9", "15.2.8", "15.2.9"), 'a_package', pick.last = TRUE)
    }),
    "15.2.9")

    expect_equal(withr::with_options(list('mv_prefer_within_major_version' = 'true'), {
        chooseVersion('>= 14.9.9', c("14.10.9", "15.2.8", "15.2.9"), 'a_package', pick.last = TRUE)
    }),
    "14.10.9")
})


test_that("lib.location works", {

    expect_error(withr::with_envvar(list('R_MV_LIBRARY_LOCATION' = ''), {lib.location()},
                                    action = 'replace'),
                 'No environment variable has been set for me to find the R_MV_library location')

    if (interactive()) {
        expect_message(withr::with_envvar(list('R_MV_LIBRARY_LOCATION' = ''), {lib.location(getwd())},
                                          action = 'replace'),
                       'For this session, the environment variable.*')
    } else {
        skip('test for interactive use')
    }

    # When the path is configured again to the same value, no message is printed.
    expect_silent(withr::with_envvar(list('R_MV_LIBRARY_LOCATION' = getwd()), {lib.location(getwd())},
                                     action = 'replace'))

    expect_error(withr::with_envvar(list('R_MV_LIBRARY_LOCATION' = ''),
                                    {lib.location('./non existing folder')},
                                    action = 'replace'),
                 'The provided path does not exist.')

})
