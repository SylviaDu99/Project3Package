#' t test function
#'
#' This function applies t test to input data.
#'
#' @param x Numeric input of data to be tested
#' @param alternative String to specify the type of aternative hypothesis, either 'two.sided', 'less', or 'greater'
#' @param mu Numeric input of null hypothesis
#' @keywords inference
#'
#' @return List object containing t score, degree of freedom, type of alternative hypothesis, and p value of \code{x}
#'
#' @examples
#' my_t.test(lifeExp, "two.sided", 60)
#' my_t.test(lifeExp, "less", 60)
#' my_t.test(lifeExp, "greater", 60)
#'
#' @export

my_t.test <- function(x, alternative, mu) {
    # check if x is numeric, if not stop and show error information
    if (!is.numeric(x)) {
        stop("x should be numeric")
    }
    # check if alternative is one of "two.sided", "less", "greater",
    # if not stop and show error information
    else if (alternative != "two.sided" & alternative != "less"
             & alternative != "greater") {
        stop("alternative should be either 'two.sided', 'less', or 'greater'")
    }
    # check if mu is numeric, if not stop and show error information
    else if (!is.numeric(mu)) {
        stop("mu should be numeric")
    }
    se <- sd(x) / sqrt(length(x))
    test_stat <- (mean(x) - mu) / se
    df <- length(x) - 1
    if (alternative == "two.sided") {
        p_val <- 2 * min(pt(test_stat, df, lower.tail = TRUE),
                         pt(test_stat, df, lower.tail = FALSE))
    } else if (alternative == "less") {
        p_val <- pt(test_stat, df, lower.tail = TRUE)
    } else {   # alternative == "greater"
        p_val <- pt(test_stat, df, lower.tail = FALSE)
    }
    result <- list("t score" = test_stat, "degree of freedom" = df,
                   "type of alternative hypothesis" = alternative,
                   "p value" = p_val)
    return(result)
}
