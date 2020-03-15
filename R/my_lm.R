#' linear model function
#'
#' This function fits a linear model to input data.
#'
#' @param formula Numeric input of a formula of the response variable and explainary variable
#' @param data Numeric dataframe containing the data that is used in \code{formula}
#' @keywords inference
#'
#' @return Table object containing estimated coefficients, standard error, t value, and p value
#'
#' @examples
#' my_lm(lifeExp~gdpPercap+continent, my_gapminder)
#'
#' @import magrittr
#'
#' @export

my_lm <- function(formula, data) {
    x <- model.matrix(formula, data)
    f <- model.frame(formula, data)
    y <- model.response(f)
    coeff <- solve(t(x) %*% x) %*% t(x) %*% y
    df <- dim(x)[1] - length(coeff)
    sigma_sqr <- sum((y - x %*% coeff)^2 / df)
    se <- sqrt(diag(sigma_sqr * solve(t(x) %*% x)))
    t_val <- coeff / se
    pr_t <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
    # binding the data together and convert the data matrix into a table
    my_m <- cbind(coeff, se, t_val, pr_t)
    colnames(my_m) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    return(my_m)
}
