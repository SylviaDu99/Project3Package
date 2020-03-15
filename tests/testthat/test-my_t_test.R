data(my_gapminder)
lifeExp <- my_gapminder$lifeExp

test_that("my_t_test works for two.sided test", {
    my_result <- my_t.test(lifeExp, "two.sided", 60)
    sample_result <- t.test(lifeExp, "two.sided", 60)
    expect_equal(my_result$test_stat, sample_result$statistic)
    expect_equal(my_result$p_val, sample_result$p.value)
    expect_equal(my_result$df, sample_result$parameter)
    expect_match(my_result$alternative, sample_result$alternative)
})

test_that("my_t_test works for lower tailed test", {
    my_result <- my_t.test(lifeExp, "less", 60)
    sample_result <- t.test(lifeExp, "less", 60)
    expect_equal(my_result$test_stat, sample_result$statistic)
    expect_equal(my_result$p_val, sample_result$p.value)
    expect_equal(my_result$df, sample_result$parameter)
    expect_match(my_result$alternative, sample_result$alternative)
})

test_that("my_t_test works for upper tailed test", {
    my_result <- my_t.test(lifeExp, "greater", 60)
    sample_result <- t.test(lifeExp, "greater", 60)
    expect_equal(my_result$test_stat, sample_result$statistic)
    expect_equal(my_result$p_val, sample_result$p.value)
    expect_equal(my_result$df, sample_result$parameter)
    expect_match(my_result$alternative, sample_result$alternative)
})

test_that("my_t_test throws an error when input is not valid", {
    expect_error(my_t_test(lifeExp, "a", 60))
    expect_error(my_t_test("a", "two.sided", 60))
    expect_error(my_t_test(lifeExp, "two.sided", "60"))
})

test_that("my_t_test returns a list as output", {
    expect_is(my_t_test(lifeExp, "greater", 60), "list")
})
