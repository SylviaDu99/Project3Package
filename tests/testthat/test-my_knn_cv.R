data("my_iris")
my_iris <- my_iris

test_that("my_knn_cv works when k_nn = 1", {
    result <- my_knn_cv(my_iris[, 1:4], my_iris$Species, 1, 5)
    expect_equal(result$te, 0)
})

test_that("my_knn_cv throws an error when k_nn and k_cv are not both numeric", {
    expect_error(my_knn_cv(my_iris[, 1:4], my_iris$Species, "a", "b"))
    expect_error(my_knn_cv(my_iris[, 1:4], my_iris$Species, 1, "a"))
    expect_error(my_knn_cv(my_iris[, 1:4], my_iris$Species, "a", 5))
})

test_that("my_knn_cv returns a list as output", {
    expect_is(my_knn_cv(my_iris[, 1:4], my_iris$Species, 1, 5), "list")
})
