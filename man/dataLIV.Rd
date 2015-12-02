\name{dataLIV}
\alias{dataLIV}
\docType{data}
\title{
Simulated Dataset
}
\description{
A dataset with one endogenous, discrete regressor used for exemplifying the use of the Latent Instrumental Variable function \code{\link{liv}}.
}
\usage{data("dataLIV")}
\format{
  A data frame with 2500 observations on the following 3 variables.
  \describe{
   \item{\code{y}}{  a numeric vector representing the dependent variable.}
   \item{\code{P}}{  a numeric vector representing a discrete and endogenous regressor.}
   \item{\code{Z}}{  a numeric vector representing the discrete, latent IV used to build P.}
  }
}
\details{
The dataset was modeled according to the following equations:
\deqn{P = g0 * Z + nu}
\deqn{y = b0 + a1 * P + epsilon}
where \code{g0 = 2}, \code{b0 = 3} and \code{a1 = -1}. 
}

\keyword{datasets}
\seealso{
\code{\link{liv}}
}
