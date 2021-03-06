% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_t_test.R
\name{my_t.test}
\alias{my_t.test}
\title{t test function}
\usage{
my_t.test(x, alternative, mu)
}
\arguments{
\item{x}{Numeric input of data to be tested}

\item{alternative}{String to specify the type of aternative hypothesis, either 'two.sided', 'less', or 'greater'}

\item{mu}{Numeric input of null hypothesis}
}
\value{
List object containing t score, degree of freedom, type of alternative hypothesis, and p value of \code{x}
}
\description{
This function applies t test to input data.
}
\examples{
my_t.test(rnorm(10, 0, 1), "two.sided", 5)
my_t.test(rnorm(10, 0, 1), "less", 5)
my_t.test(rnorm(10, 0, 1), "greater", 5)

}
\keyword{inference}
