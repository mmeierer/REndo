\name{dataCopDis}
\alias{dataCopDis}
\docType{data}
\title{
Simulated Dataset
}
\description{
A dataset with an intercept, two exogenous regressors and one endogenous, discrete variable, used for exemplifying the use of \code{\link{copulaCorrection}} function. The true parameter values are: \code{b0 = 2}, \code{b1 = 1.5}, \code{b2 = -3}, and the coefficient of the endogenous variable is set to \code{a1 = -1}. The correlation between the endogenous regressor \code{P} and the error term is 0.33. \code{P} has a Poisson distribution with \code{lambda = 5}.
}
\usage{data("dataCopDis")}
\format{
  A data frame with 2500 observations on the following 5 variables.
  \describe{
    \item{\code{y}}{    a numeric vector representing the dependent variable.}
    \item{\code{I}}{    a numeric vector representing the intercept.}
    \item{\code{X1}}{    a numeric vector, normally distributed and exogenous.}
    \item{\code{X2}}{    a numeric vector, normally distributed and exogenous.}
    \item{\code{P}}{    a numeric vector, discrete and endogenous.}
  }
}

\keyword{datasets}
