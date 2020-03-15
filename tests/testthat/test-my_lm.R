data("mtcars")

test_that("my_lm works", {
    my_result <- my_lm(mpg ~ hp + wt, mtcars)
    sample_result <- summary(lm(mpg ~ hp + wt, mtcars))
    expect_equal(matrix(my_result[, -4]), matrix(sample_result$coefficients[, -4]))
})
