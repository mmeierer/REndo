\name{dataHMLewbel}
\alias{dataHMLewbel}
\docType{data}
\title{
Simulated Dataset
}
\description{
A dataset enclosing a dependent variable, \code{y}, two exogenous regressors, \code{X1} and \code{X2} and one endogenous variable, \code{P}. The endogenous regressor has to have a non-normal distribution for identification. The model is: 
\deqn{y = b0 + b1 * X1 + b2 * X2 + a1 * P + epsilon}
True parameter values are \code{b0 = 2, b1 = 1.5, b2 = -3, a1 = -1}. 
}
\usage{data("dataHMLewbel")}
\format{
  A data frame with 2500 observations on the following 4 variables.
  \describe{
    \item{\code{y}}{  a numeric vector representing the dependent variable.}
    \item{\code{X1}}{  a numeric vector, normally distributed and exogenous.}
    \item{\code{X2}}{  a numeric vector, normally distributed and exogenous.}
    \item{\code{P}}{  a numeric vector, representing an endogenous regressor.}
  }
}

\keyword{datasets}
\seealso{
\code{\link{hmlewbel}}
}
