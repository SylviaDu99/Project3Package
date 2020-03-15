test_that("my_rf_cv returns a double as output", {
    expect_is(my_rf_cv(5), "numeric")
})

test_that("my_rf_cv throws an error when input is not numeric", {
    expect_error(my_rf_cv("a"))
})
