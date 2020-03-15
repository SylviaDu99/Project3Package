#' random forest function
#'
#' This function predicts the lifeExp in \code{my_gapminder} dataset by gdpPercap
#' with k-fold Cross-validation and random forest algorithms.
#'
#' @param k Numeric number of folds used in k-fold cross-validation
#' @keywords prediction
#'
#' @return Numeric number of the CV mean squared error
#'
#' @examples
#' my_rf_cv(5)
#'
#' @import class randomForest stringr tidyverse
#'
#' @export
my_rf_cv <- function(k) {
    # check if input is numeric, if not, stop and show error information
    if (!is.numeric(k)) {
        stop("input must be numeric")
    }
    my_gapminder <- my_gapminder
    l <- nrow(my_gapminder)
    # split data in k parts, randomly
    fold <- sample(rep(1:k, length = l))
    data <- my_gapminder %>% select(lifeExp, gdpPercap)
    data[, "split"] <- fold

    mse <- c()
    for (i in 1:k) {
        # define the training data as all the data not in the ith fold
        data_train <- data %>% dplyr::filter(split != i)
        data_test <- data %>% dplyr::filter(split == i)
        # train a random forest model with 50 trees to predict lifeExp using covariate gdpPercap
        my_model <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 50)
        # predict the lifeExp of the ith fold which was used as test data
        my_pred <- predict(my_model, select(data_test, -split))
        # evaluate the MSE, the average squared difference between predicted lifeExp and true lifeExp
        mse <- c(mse, mean(my_pred - data_test$lifeExp)^2)
    }
    output <- mean(mse)
    return(output)
}
