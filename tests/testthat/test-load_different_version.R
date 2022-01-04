# READY

test_outcomes1 <- with_safe_package_tester({

        .initialize_test(desc = 'load different version')
        .set_test_lib_location()

        stopifnot(!'package:package.c' %in% search())

        msg1 <- capture.output(lib.load(package.c = '15.2.8'), type = 'message')
        .exp_match(msg1[[1]], 'Exactly [ ]*15.2.8.*package.c')

        .exp_equal(what_version_are_you(), '15.2.8')

        msg2 <- capture.output(lib.load(package.c = '15.2.9'), type = 'message')
        .exp_match(msg2[[1]], 'Unloaded package.c because the currently attached version was not compatible with the new requirements')
        .exp_match(msg2[[2]], 'Exactly [ ]*15.2.9.*package.c')

        # check which packages are loaded
        .exp_true('package:package.c' %in% search())
        # package.c 15.2.8 is really unloaded?
        .exp_error(package_c1(), 'could not find function')
        .exp_error(package.c::package_c1(), "is not an exported object from 'namespace:package.c'")
        .exp_equal(package.c::package_c2(), 'package_c2')
        .exp_equal(what_version_are_you(), '15.2.9')

        # Just test some random other packages
        .exp_false('package:package.a' %in% search())
        .exp_false('package:package.d' %in% search())
        .exp_false('package:package.e' %in% search())

        .get_tests()
    })


.run_test_batch(test_outcomes1)


test_outcomes2 <- with_safe_package_tester({

        .initialize_test(desc = 'Check package loading messages')
        .set_test_lib_location()

        stopifnot(!'package:package.c' %in% search())

        # All load version '15.2.9'
        msg1 <- capture.output(lib.load(package.c, pick.last = TRUE), type = 'message')
        msg2 <- capture.output(lib.load(package.c),                   type = 'message')
        msg3 <- capture.output(lib.load(package.c = '> 15.2.8'),      type = 'message')
        msg4 <- capture.output(lib.load(package.c = ' >= 15.2.8 '),   type = 'message')  # Testing the trimws
        msg5 <- capture.output(lib.load(package.c = '15.2.9'),        type = 'message')

        # check what messages are printed.
        .exp_match(msg1, "Version 15.2.9  is picked")
        .exp_match(msg2, "Version 15.2.9  is picked")
        .exp_match(msg3, "Version 15.2.9  is chosen.*(> 15.2.8)")
        .exp_match(msg4, "Version 15.2.9  is chosen.*(>= 15.2.8)")
        .exp_match(msg5, "Exactly 15.2.9  is used")

        .get_tests()
    })

.run_test_batch(test_outcomes2)
