### Version 0.3.4 (submitting to CRAN).

**`R CMD check` passed with no ERROR, WARNING or NOTE!!!**

package waldo could not be found by only a single test:
`expect_equal(package.e::what_version_are_you(), '1.7.0')`
Rewrote it a bit, but made no further changes.

Removed `.set_test_lib_location` references within `with_safe_package_tester` constructs. These were not necessary.

### Version 0.3.3 (submitting to CRAN).

**`R CMD check` passed with no ERROR, WARNING or NOTE!!!**

For CRAN, had to update the location where the tarballs are stored. Now they are
downloaded from the github repository so that the tests will keep working.
Downloading happens from a tagged commit in this repository so that we can change 
the tag that is referenced here in `R/test_functions.R` when we would like to download new data.

Removed the printing of "Failed copying files:" when all copying wend fine. It sounds a bit too alarming.

### Version 0.3.2 (submitting to CRAN).

**`R CMD check` passed with only the one expected Note!!!**

See `comments.md`.

Updates:
- Only a few minor changes were required for the package to pass for R4.1.2.
- There are no functional changes, except for `lib.install_tarball` which can now install multiple tarballs at once.
- (Added `show_n_files` internal function due to the changed behavior of `summary` which was used. Required one test to be updated.)
- (Internal function `unique_highest_package_versions` was broken due to differences that were introduced to the `which.max` function. Fixed it.)

### Version 0.3.1 (submitting to CRAN)

*`R CMD check` passed with only the one expected Note!!!*

Updates:

- Rewrote `lib.execute_using_package_list`
- library_VC, install.packages_VC_tarball, install.packages_VC, dependencies, installed.packages_VC, execute_with_packages are all depricated function names. All are renamed to `lib. ...` functions.
- Renamed `overwrite_this_package` to `allow_overwrite_on_convert` for all `lib.install...` functions.
- Improved how the 'packages_to_convert' input for `lib.convert` is handled.
- Create tests which work with the `with_safe_package_tester` helper. This causes packages that are loaded during tests to be unloaded automatically.
- Added `lib.install_if_not_compatible()` function to the `lib.install...` family.
- Combined documentation for `lib.install...` functions.
- Massively simplified the way package installation is working. It will now load all the latest versions of the different packages in the library and leave the remaining dependencies up to `lib.install`.
- Tests also include a package installation test with tarball and CRAN package in `test-tarball_installation.R`.
- Updated lib.set_libPaths so it can set the .libPaths to all the latest versions of the different packages.
- Removed all the installation scripts altogether, just be careful when installing stuff, and use `lib.execute_using_package_list` if you want to install in a clean environment.
- Updated detachAll to become more stable.
- Added `getOption('mv_prefer_within_major_version')` to configure behavior when having to choose for major versions higher then the provided version.
- Removed `!interactive()` for printing the decisions that are made during package loading. Also non-interactively you will now have to use the `quietly = TRUE`.
- Added additional message during loading: `Version %-7s is picked  for package '%s'`. This makes it much more clear when a choice had to be made between multiple packages, or if just one package was available.
- Also updated message for '>' and '>=' package conditions, and included version specification (like `> 5.0`) so you can understand the decission better.
- lib.load: added dot before .packNameVersionList and .skipDependencies variable names.
- Renamed all mentions of VC library and R_VC_library to R_MV_library
- `lib.convert`: renamed input variables to `source_lib` and `destination_mv_lib`.
- `lib.location`: Added condition for when to message that glob. env. has been set. Now only when it is different and when interactive() it is printed.
- Added trimws for version numbers that are provided. So now also `lib.load(dplyr = ' >= 5.5 ')` works.
