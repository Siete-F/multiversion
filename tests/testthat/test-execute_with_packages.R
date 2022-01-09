test_lib <- paste0(test_path('..', 'test_library_R'), R.version$major)

test_that("Test lib.execute_using_packagelist", {
    on.exit(unlink('./execution.log'), add = TRUE)
    expect_equal(lib.execute_using_packagelist(
        packages_to_load   = c(package.a =  '0.1.0'),
        .lib_location      = test_lib,
        func_handle        = function() {an_important_value(); package_a1(5, 10)},
        .wait_for_response = TRUE,
        .callr_arguments   = list(stdout = './execution.log', stderr = "2>&1"),
        .run_quietly       = TRUE
    ), "package_a1")
    expect_true(file.exists('./execution.log'))
})

test_that("Test lib.execute_using_packagelist", {
    expect_match(capture_output(lib.execute_using_packagelist(
                 packages_to_load   = c(package.a =  '0.1.0'),
                 .lib_location      = test_lib,
                 func_handle        = function() {an_important_value(); package_a1(5, 10)},
                 .wait_for_response = TRUE,
                 .run_quietly       = FALSE
             )), "Loading required package.*Exactly 0\\.1\\.0.*")
})

test_that("Test lib.execute_using_packagelist as fire-and-forget", {
    out <- capture.output(lib.execute_using_packagelist(
        packages_to_load   = c(package.a =  '0.1.0'),
        .lib_location      = test_lib,
        func_handle        = function() {an_important_value(); package_a1(5, 10)},
        .wait_for_response = FALSE,
        .run_quietly       = TRUE
    ))
    expect_match(out, 'PROCESS .*, running, pid .*')
})
