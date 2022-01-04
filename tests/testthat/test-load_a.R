# source('../test_helpers.R')
# READY

test_outcomes <- callr::r(
    function() {
        suppressMessages(suppressWarnings(devtools::load_all()))
        stopifnot(!'package:package.a' %in% search())

        .initialize_test(desc = "simply load package.a")
        .set_test_lib_location()

        # load package a
        msg <- capture.output(lib.load(package.a), type = 'message')
        .exp_match(msg, '\\+__[_ ]+Version [ ]*0.1.0.*package.a')

        # check which packages are loaded
        .exp_true('package:package.a' %in% search())
        .exp_equal(package_a1(), 'package_a1')

        # Just test some random other packages
        .exp_false('package:package.b' %in% search())
        .exp_false('package:package.c' %in% search())
        .exp_false('package:package.d' %in% search())
        .exp_false('package:package.e' %in% search())

        .exp_error(lib.load(package.a = '>=  0.1.0'),
                   'Not all package versions that are provided seem to be valid version numbers.')

        return(.get_tests())
    }
    ,
    show = TRUE)

.run_test_batch(test_outcomes)

