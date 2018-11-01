#' @title Simulated Dataset with One Endogenous Continuous Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and one endogenous, continuous regressor,
#'  \code{P}, having a T-distribution with 3 degrees of freedom.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataCopCont
#' @usage dataCopCont
#' @format A data frame with 2500 observations on 5 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataCopCont"


#' @title Simulated Dataset with Two Endogenous Continuous Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and two endogenous, continuous regressors,
#'  \code{P1} and \code{P2}, having a T-distribution with 3 degrees of freedom.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the intercept and the exogenous regressors' coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3}. The coefficient of the endogenous regressor \code{P1} is equal to \code{a1 = -1} and
#'  of \code{P2} is equal to \code{a2 = 0.8}.
#' @name dataCopCont2
#' @usage dataCopCont2
#' @format A data frame with 2500 observations on 6 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P1}}{a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
#' \item{\code{P2}}{a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataCopCont2"


#' @title Simulated Dataset with Two Endogenous Regressors
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and two endogenous regressors,
#'  \code{P1}, having a Poisson distribution with lambda parameter equal to 3, and \code{P2}, having a T-distribution with 3 degrees of freedom.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor \code{P1} is set to \code{a1 = -1} and of \code{P2} is set to \code{a2=0.8}.
#' @name dataCopDisCont
#' @usage dataCopDisCont
#' @format A data frame with 2500 observations on 6 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P1}}{a numeric vector, continuous and endogenous having Poisson distribution with parameter lambda equal to 3.}
#' \item{\code{P2}}{a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataCopDisCont"

#' @title Simulated Dataset with One Endogenous Discrete Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and one endogenous, discrete (Poisson distributed) regressor,
#'  \code{P}.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataCopDis
#' @usage dataCopDis
#' @format A data frame with 2500 observations on 5 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataCopDis"

#' @title Simulated Dataset with Two Endogenous Discrete Regressors
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and two endogenous, discrete (Poisson distributed) regressors,
#'  \code{P1} and \code{P2}.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients of the intercept and the exogenous variables are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3}. The true parameter values for the coefficients of the endogenous regressors are \code{a1 = -1} for \code{P1} and
#'  \code{a2 = 0.8} for \code{P2}.
#' @name dataCopDis2
#' @usage dataCopDis2
#' @format A data frame with 2500 observations on 6 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P1}}{a numeric vector, having a Poisson distribution with parameter lambda equal to 3, and endogenous.}
#' \item{\code{P2}}{a numeric vector, having a Poisson distribution with parameter lambda equal to 3, and endogenous.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataCopDis2"


#' @title Simulated Dataset with One Endogenous Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and one endogenous, continuous regressor \code{P}.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataHigherMoments
#' @usage dataHigherMoments
#' @format A data frame with 2500 observations on 5 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous regressor, normally distributed.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataHigherMoments"

# All the other datasets dont have it either
# @example
# data("dataHigherMoments")
# resultsHM <- higherMoments(y ~ X1 + X2 + P| IIV(X1+X2), data=dataHigherMoments)
# summary(resultsHM)


#' @title Simulated Dataset with One Endogenous Continuous Regressor
#' @description A dataset with one endogenous regressor \code{P}, an instrument \code{Z}
#'  used to build \code{P}, an intercept and a dependent variable, \code{y}.
#'  The true parameter values for the coefficients are: \code{b0 = 3} for the intercept
#'  and \code{a1 = -1} for \code{P}.
#' @name dataLatentIV
#' @usage dataLatentIV
#' @format A data frame with 2500 observations on 4 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{a numeric vector representing the endogenous variable.}
#' \item{\code{Z}}{a numeric vector used in the construction of the endogenous variable, P.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataLatentIV"

#' @title Simulated Dataset with One Endogenous Continuous Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, one endogenous, continuous regressor \code{P}, and the dependent variable \code{y}.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataHetIV
#' @usage dataHetIV
#' @format A data frame with 2500 observations on 4 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous regressor, normally distributed.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataHetIV"
