# To work on tests and to understand testcases better, run:
#
# .set_test_lib_location()
# lib.installed_packages()

# Tests need to be added for:
# - lib.my_location
# - lib.clean
# - raw_input_parser
# - lib.set_libPaths
# - lib.clean_libPaths
# - lib.load_namespaces

test_that("lib.location_install_dir", {
    expect_match(lib.location_install_dir('.'), '.+[/\\]TEMP_install_location')
})

test_that("normPath", {
    expect_match(normPath('.'), gsub('\\\\', '/', getwd()))
    expect_silent(normPath('.\\nonex1stendd1r'))  # no error, so mustWork = F.
})

test_that("lib.printVerboseLibCall", {
    # Important backbone function
    expect_equal(unique_highest_package_versions(c(package.a = '0.1.0', package.b = '99.99.99',
                                                   package.c = '1.0',   package.b = '5.2')),
                 c(package.a = '0.1.0', package.b = '99.99.99',
                   package.c = '1.0'))

    msg <- capture.output(lib.printVerboseLibCall(
        c(package.a = '0.1.0', package.b = '99.99.99',
          package.c = '1.0',   package.b = '5.2'), .forceToPrint = TRUE), type = 'message')
    expect_match(msg[2], 'Verbose example call to the library')
    expect_match(msg[3], "lib\\.load\\( package\\.a = '0\\.1\\.0', package\\.b = '99\\.99\\.99', package\\.c = '1\\.0'\\)")
    # It must return only the highest unique versions.
    expect_true(!any(grepl("package\\.b = '5\\.2'", msg)))

    # Same test must pass if lowest version of package.b is in another place in the array.
    msg <- capture.output(lib.printVerboseLibCall(
        c(package.b = '5.2', package.a = '0.1.0',
          package.b = '99.99.99', package.c = '1.0'), .forceToPrint = TRUE), type = 'message')
    expect_match(msg[2], 'Verbose example call to the library')
    expect_match(msg[3], "lib\\.load\\( package\\.a = '0\\.1\\.0', package\\.b = '99\\.99\\.99', package\\.c = '1\\.0'\\)")
    # It must return only the highest unique versions.
    expect_true(!any(grepl("package\\.b = '5\\.2'", msg)))

})

test_that("lib.dependsOnMe", {
    .set_test_lib_location()

    expect_message(
        expect_message(
            lib.dependsOnMe(checkMyDeps = c('package.a', 'package.b')),
            'Showing all that depends on `package.a`, \\(available are: "0.1.0", "0.2.0", "0.3.0"\\)'),
        'Showing all that depends on `package.b`, \\(available are: "1.0.0"\\):'
    )

    expect_error(lib.dependsOnMe(checkMyDeps = c('all', 'package.a')),
                 'no other package names can be requested in the same call')
    expect_error(lib.dependsOnMe(), '> 0 is not TRUE')
    expect_message(lib.dependsOnMe(package.a), 'Showing all that depends on `package.a`')

    msg <- capture.output(lib.dependsOnMe(package.e = '>1.5'), type = 'message')
    expect_match(msg[1], 'Showing all that depends on .* version ')
    expect_match(msg[2], '\\s+package.a .*0.2.0 .* package.e\\s+package.f')

    msg <- capture.output(lib.dependsOnMe(package.e = '>=1.8'), type = 'message')
    expect_match(msg[1], 'Cannot match dependency...')
    expect_match(msg[2], 'Showing all that depends on `package.e`, \\(available are: "1.5.0", "1.7.0"\\)')
})

test_that("lib.packs_vec2str", {

    expect_equal(lib.packs_vec2str(c(dplyr = '>= 0.5.0', ggplot = '', package.a = '0.1.0'), do_return = TRUE),
                 "dplyr (>= 0.5.0), ggplot, package.a (0.1.0)")
    expect_message(lib.packs_vec2str(c(dplyr = '>= 0.5.0', ggplot = '', package.a = '0.1.0'), do_return = FALSE),
                   "dplyr \\(>= 0.5.0\\)   ggplot   package.a \\(0.1.0\\)")
})

test_that("lib.dependencies errors", {
    expect_error(lib.dependencies(c(package.a = '0.1.0'), character.only = TRUE),
                 'Please only provide the name of the package')

    expect_error(lib.dependencies(package.a = '0.1.0', character.only = TRUE),
                 'unused argument')

    expect_error(lib.dependencies(c(package.a = '0.1.0')),
                 'Please only provide the name of the package')
})

test_that("Expected errors are thrown", {
    .set_test_lib_location()
    expect_error(lib.install(package_names = c(package.a = '5.5.0')),
                 'Please provide a vector of names, not a vector like ')
    expect_silent(lib.install(package_names = c()))

    expect_silent(lib.install_if_not_compatible(package_conditions = c()))
    expect_error(lib.install_if_not_compatible(package_conditions = 'package.a'),
                 'Please provide a vector of named version specifications')
    expect_error(lib.install_if_not_compatible(package_conditions = c('package.a' = '0.4.0')),
                 'It is not possible to provide exact versions like ')

    expect_error(lib.decide_version(c()), 'The length of packVersion cannot be 0')
})

test_that("lib.dependencies_online", {
    expect_error(lib.dependencies_online('Nonex1stendP4ck4g3'),
                 'Package "Nonex1stendP4ck4g3" is not available on CRAN')
    out <- expect_silent(lib.dependencies_online('dplyr'))
    expect_true(all(c('magrittr', 'rlang', 'tidyselect') %in% names(out)))
    # All versions consist of decent version numbers.
    expect_true(all(grepl('^(>?=?\\s*[.0-9]+)$', out) | nchar(out) == 0))
})


test_that('lib.available_versions errors', {
    .set_test_lib_location()
    expect_error(lib.available_versions(NA),
                 'The package name cannot.*')
    expect_error(lib.available_versions(''),
                 'The package name cannot.*')
    expect_error(lib.available_versions(c()),
                 'The package name cannot.*')
    expect_error(lib.available_versions('base', .Library),
                 'The library I am trying to load from, is not shaped as expected.')
})

test_that('lib.check_compatibility errors', {
    expect_error(lib.check_compatibility(c(), version = ''), 'length\\(condition\\) != 0 is not TRUE')
    expect_error(lib.check_compatibility(c('>= 5.0.0', '> 0.5'), version = ''), 'Please provide one condition only in')
    expect_error(lib.check_compatibility('>= 5.0.0', version = ''), 'invalid version specification')
    expect_true(lib.check_compatibility('>= 5.0.0', version = '6'))
    expect_true(lib.check_compatibility('>= 5.0.0', version = NA))

})
