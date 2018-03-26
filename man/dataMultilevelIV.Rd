\name{dataMultilevelIV}
\alias{dataMultilevelIV}
\docType{data}
\title{
  Simmulated Dataset
}
\description{
  A dataset simmulated to exemplify the use of the \code{multilevelIV()} function.
}
\usage{data("dataMultilevelIV")}
\format{
  A data frame with 2645 observations, clustered into 40 level-three variables.
  \describe{
    \item{\code{y}}{a numeric vector representing the dependent variable.}
    \item{\code{X11}}{a level-one numeric vector representing a categorical exogenous variable.}
    \item{\code{X12}}{a level-one numeric vector representing a binomial distributedexogenous variable.}
    \item{\code{X13}}{a level-one numeric vector representing a binomial distributed exogenous variable.}
    \item{\code{X14}}{a level-two numeric vector representing a normally distributed exogenous variable.}
    \item{\code{X15}}{a level-two numeric vector representing a normally distributed endogenous variable, correlated with the level-two errors.}
    \item{\code{X21}}{a level-two numeric vector representing a binomial distributed exogenous variable.}
    \item{\code{X22}}{a level-two numeric vector representing a binomial distributed exogenous variable.}
    \item{\code{X23}}{a level-two numeric vector representing a binomial distributed exogenous variable.}
    \item{\code{X24}}{a level-teo numeric vector representing a normally distributed exogenous variable.}
    \item{\code{X31}}{a level-three numeric vector representing a normally distributed exogenous variable.}
    \item{\code{X32}}{a level-three numeric vector representing a truncated normally distributed exogenous variable.}
    \item{\code{X33}}{a level-three numeric vector representing a truncated normally distributed exogenous variable.}
    \item{\code{Intercept}}{the model intercepta.}
    \item{\code{SID}}{a numeric vector identifying each level-three observation.}
    \item{\code{CID}}{a numeric vector identifying each level-two observation.}
    
      }
}
\keyword{datasets}
