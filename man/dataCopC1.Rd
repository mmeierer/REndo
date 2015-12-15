\name{dataCopC1}
\alias{dataCopC1}
\docType{data}
\title{
Simulated Dataset
}
\description{
A dataset with two exogenous regressors, \code{X1},\code{X2}, and one endogenous, continuous regressor, \code{P}, having a T-distribution with 3 degrees of freedom. An intercept and a dependent variable, \code{y}, are also included. The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5}, \code{b2 = -3} and the coefficient of the endogenous regressor is set to \code{a1 = -1}.
}
\usage{data("dataCopC1")}
\format{
  A data frame with 2500 observations on the following 5 variables.
  \describe{
   \item{\code{y}}{    a numeric vector representing the dependent variable.}
    \item{\code{I}}{     a numeric vector representing the intercept.}
    \item{\code{X1}}{    a numeric vector, normally distributed and exogenous.}
    \item{\code{X2}}{    a numeric vector, normally distributed and exogenous.}
    \item{\code{P}}{     a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
  }
}

\keyword{datasets}
