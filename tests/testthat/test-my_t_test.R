test_that("my_t_test throws an error when input is not valid", {
    expect_error(my_t.test(rnorm(10, 0, 1), "a", 5))
    expect_error(my_t.test("a", "less", 5))
    expect_error(my_t.test(rnorm(10, 0, 1), "less", "5"))
})

test_that("my_t_test returns a list as output", {
    expect_is(my_t.test(rnorm(10, 0, 1), "greater", 5), "list")
})
