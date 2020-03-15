data("my_gapminder")
lifeExp <- my_gapminder$lifeExp

test_that("my_t_test throws an error when input is not valid", {
    expect_error(my_t.test(lifeExp, "a", 60))
    expect_error(my_t.test("a", "two.sided", 60))
    expect_error(my_t.test(lifeExp, "two.sided", "60"))
})

test_that("my_t_test returns a list as output", {
    expect_is(my_t.test(lifeExp, "greater", 60), "list")
})
