\name{dataCopC2}
\alias{dataCopC2}
\docType{data}
\title{
Simulated Dataset
}
\description{
A dataset with two exogenous, normally distributed regressors, \code{X1} and \code{X2}, two endogenous, continuous regressors, \code{P1} and \code{P2}, having a T-distribution with 3 and 5 degrees of freedom respectively, with a correlation of 0.25. The correlation between \code{P1} and the error was set at 0.33, while between \code{P2} and the error, at 0.15. The dataset contains an intercept and the dependent variable, \code{y}. The true parameter value for the model: \code{y = b0 + b1 * X1+ b2 * X2 + a1 * P1 + a2 * P2 + eps}, are: \code{b0 = 2, b1 = 1.5, b2 = -3, a1 = -1, a2 = 0.8}.
}
\usage{data("dataCopC2")}
\format{
  A data frame with 2500 observations on the following 6 variables.
  \describe{
    \item{\code{y}}{   a numeric vector representing the dependent variable.}
    \item{\code{I}}{   a numeric vector representing the intercept.}
    \item{\code{X1}}{  a numeric vector, normally distributed and exogenous.}
    \item{\code{X2}}{  a numeric vector, normally distributed and exogenous.}
    \item{\code{P1}}{  a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
    \item{\code{P2}}{  a numeric vector, continuous and endogenous having T-distribution with 5 degrees of freedom.}
  }
}

\keyword{datasets}
