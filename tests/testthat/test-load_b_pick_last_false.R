# source('../test_helpers.R')


test_outcomes <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))

        .initialize_test(desc = 'load package.b, pick.last = FALSE')
    .set_test_lib_location()
    .exp_false('package:package.b' %in% search())
    .exp_false('package:package.c' %in% search())

    # load package b, with first version of package c (the default behaviour of pick.last = F)
    msg <- capture.output(lib.load(package.b), type = 'message')
    .exp_match(msg[[1]], '\\+__[_ ]+Only [ ]*1.0.0.*package.b')
    .exp_match(msg[[2]], '\\\\__[_ ]+Only [ ]*15.2.8.*package.c')

    # check which packages are loaded
    .exp_true('package:package.b' %in% search())
    .exp_false('package:package.c' %in% search())
    .exp_equal(package_b1(), 'package_b1')
    .exp_equal(what_version_are_you(), '1.0.0')

    # package.c is a dependency, no specific version.
    # Check if package.c can be accessed non the less. (must be made possible by attaching instead of loading it)
    .exp_error(package_c1(), 'could not find function')
    .exp_equal(package.c::package_c1(), 'package_c1')
    .exp_equal(package.c::what_version_are_you(), '15.2.8')

    # Just test some random other packages
    .exp_false('package:package.a' %in% search())
    .exp_false('package:package.d' %in% search())
    .exp_false('package:package.e' %in% search())

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes)
