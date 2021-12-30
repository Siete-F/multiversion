

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


