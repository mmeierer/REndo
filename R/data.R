#' @title Simulated Dataset with One Endogenous Continuous Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and one endogenous, continuous regressor,
#'  \code{P}, having a T-distribution with 3 degrees of freedom.
#'  An intercept and a dependent variable, \code{y}, are also included.
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5},
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataCopCont
#' @usage data("dataCopCont")
#' @format A data frame with 2500 observations on 4 variables:
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
#' @usage data("dataCopCont2")
#' @format A data frame with 2500 observations on 5 variables:
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
#' @usage data("dataCopDisCont")
#' @format A data frame with 2500 observations on 5 variables:
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
#' @usage data("dataCopDis")
#' @format A data frame with 2500 observations on 4 variables:
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
#' @usage data("dataCopDis2")
#' @format A data frame with 2500 observations on 5 variables:
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
#'  \code{b2 = 3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#'
#' @examples
#' data("dataHigherMoments")
#' # to recover the parameters,
#' #   on average over many simulations
#' higherMomentsIV(formula = y ~ X1 + X2 + P|P|IIV(iiv=yp),
#'                data=dataHigherMoments)
#'
#' @name dataHigherMoments
#' @usage data("dataHigherMoments")
#' @format A data frame with 2500 observations on 4 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X1}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{X2}}{a numeric vector, normally distributed and exogenous.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous regressor, normally distributed.}
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataHigherMoments"

#' @title Simulated Dataset with One Endogenous Continuous Regressor
#' @description A dataset with one endogenous regressor \code{P}, an instrument \code{Z}
#'  used to build \code{P}, an intercept and a dependent variable, \code{y}.
#'  The true parameter values for the coefficients are: \code{b0 = 3} for the intercept
#'  and \code{a1 = -1} for \code{P}.
#' @name dataLatentIV
#' @usage data("dataLatentIV")
#' @format A data frame with 2500 observations on 3 variables:
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
#'  \code{b2 = 3} and the coefficient of the endogenous regressor, \code{P}, is equal to \code{a1 = -1}.
#' @name dataHetIV
#' @usage data("dataHetIV")
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

#' @title Multilevel Simulated Dataset - Three Levels
#' @description  A dataset simulated to exemplify the use of the \code{multilevelIV()} function.
#' It has 2767 observations, clustered into 40 level-three variables and 1347 observations at level two. The endogenous regressor is \code{X15} with a true
#' coefficient value of -1.
#' @name dataMultilevelIV
#' @usage data("dataMultilevelIV")
#' @format A data frame with 2767 observations clustered into 40 level-three variables and 1347 level-two variables.
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X11}}{a level-one numeric vector representing a categorical exogenous variable with true parameter value equal to 3.}
#' \item{\code{X12}}{a level-one numeric vector representing a binomial distributed exogenous variable with true parameter value equal to 9.}
#' \item{\code{X13}}{a level-one numeric vector representing a binomial distributed exogenous variable with true parameter value equal to -2.}
#' \item{\code{X14}}{a level-two numeric vector representing a normally distributed exogenous variable with true parameter value equal to 2.}
#' \item{\code{X15}}{a level-two numeric vector representing a normally distributed endogenous variable, correlated with the level-two errors.
#' It true parameter value equals to \eqn{-1} and it has a correlation with the level two errors equal to 0.7.}
#' \item{\code{X21}}{a level-two numeric vector representing a binomial distributed exogenous variable with true parameter value equal to -1.5.}
#' \item{\code{X22}}{a level-two numeric vector representing a binomial distributed exogenous variable with true parameter value equal to -4.}
#' \item{\code{X23}}{a level-two numeric vector representing a binomial distributed exogenous variable with true parameter value equal to -3.}
#' \item{\code{X24}}{a level-teo numeric vector representing a normally distributed exogenous variable with true parameter value equal to 6.}
#' \item{\code{X31}}{a level-three numeric vector representing a normally distributed exogenous variable with true parameter value equal to 0.5.}
#' \item{\code{X32}}{a level-three numeric vector representing a truncated normally distributed exogenous variable with true parameter value equal to 0.1.}
#' \item{\code{X33}}{a level-three numeric vector representing a truncated normally distributed exogenous variable with true parameter value equal to -0.5.}
#' \item{\code{SID}}{a numeric vector identifying each level-three observations.}
#' \item{\code{CID}}{a numeric vector identifying each level-two observations.}}
#'
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
"dataMultilevelIV"

#' @title Simulated Dataset for 2sCOPE - Nonnormal Endogenous and Exogenous Regressors
#' @description A dataset simulated with one endogenous regressor \code{P}
#'   and one exogenous regressor \code{X}, both nonnormal and correlated
#'   with each other (rho_px = 0.5). The endogenous regressor \code{P}
#'   follows a Gamma(1,1) distribution and the exogenous regressor \code{X}
#'   follows an Exp(1) distribution. The endogeneity strength is rho_pxi = 0.5.
#'   This dataset corresponds to Case 1 of Yang et al. (2025) and
#'   demonstrates 2sCOPE's main contribution: consistent estimation when the
#'   endogenous and exogenous regressors are correlated.
#'   The true parameter values are \code{mu = 1} for the intercept,
#'   \code{alpha = 1} for \code{P} and \code{beta = -1} for \code{X}.
#' @name dataCopula2sCOPECase1
#' @usage data("dataCopula2sCOPECase1")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous,
#'   Gamma(1,1) distributed and correlated with \code{X} (rho_px = 0.5).}
#' \item{\code{X}}{a numeric vector, continuous and exogenous,
#'   Exp(1) distributed and correlated with \code{P} (rho_px = 0.5).}
#' }
#' @docType data
#' @references
#' Yang, F., Qian, Y., and Xie, H. (2025). Addressing Endogeneity Using a
#' Two-Stage Copula Generated Regressor Approach. \emph{Journal of
#' Marketing Research}, 62(4), 601-623.
#' \doi{10.1177/00222437241296453}
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase1"

#' @title Simulated Dataset for 2sCOPE - Nonnormal Endogenous and Normal Exogenous Regressor
#' @description A dataset simulated with one endogenous regressor \code{P}
#'   (nonnormal, Gamma(1,1)) and one normally distributed exogenous regressor
#'   \code{X}, correlated with each other (rho_px = 0.5). The endogeneity
#'   strength is rho_pxi = 0.5. This dataset corresponds to Case 2 of Yang et al. (2025)
#'   and demonstrates that 2sCOPE remains consistent when the exogenous regressor \code{X} is
#'   normally distributed.
#'   The true parameter values are \code{mu = 1} for the intercept,
#'   \code{alpha = 1} for \code{P} and \code{beta = -1} for \code{X}.
#' @name dataCopula2sCOPECase2
#' @usage data("dataCopula2sCOPECase2")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous,
#'   Gamma(1,1) distributed and correlated with \code{X} (rho_px = 0.5).}
#' \item{\code{X}}{a numeric vector, continuous and exogenous,
#'   normally distributed and correlated with \code{P} (rho_px = 0.5).}
#' }
#' @docType data
#' @references
#' Yang, F., Qian, Y., and Xie, H. (2025). Addressing Endogeneity Using a
#' Two-Stage Copula Generated Regressor Approach. \emph{Journal of
#' Marketing Research}, 62(4), 601-623.
#' \doi{10.1177/00222437241296453}
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase2"


#' @title Simulated Dataset for 2sCOPE - Normally Distributed Endogenous and Nonnormal Exogenous Regressor
#' @description A dataset simulated with one normally distributed endogenous
#'   regressor \code{P} and one nonnormal exogenous regressor \code{X}
#'   (Exp(1)), correlated with each other (rho_px = 0.5). The endogeneity
#'   strength is rho_pxi = 0.5. This dataset corresponds to another type of Case 2
#'   of Yang et al. (2025) and demonstrates a key advantage of
#'   2sCOPE. The method is still consistently estimating, even when the endogenous regressor \code{P}
#'   is normally distributed, because the identification comes from the nonnormal
#'   exogenous regressor \code{X}.
#'   The true parameter values are \code{mu = 1} for the intercept,
#'   \code{alpha = 1} for \code{P} and \code{beta = -1} for \code{X}.
#' @name dataCopula2sCOPECase3
#' @usage data("dataCopula2sCOPECase3")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous,
#'   normally distributed and correlated with \code{X} (rho_px = 0.5).}
#' \item{\code{X}}{a numeric vector, continuous and exogenous,
#'   Exp(1) distributed and correlated with \code{P} (rho_px = 0.5).}
#' }
#' @docType data
#' @references
#' Yang, F., Qian, Y., and Xie, H. (2025). Addressing Endogeneity Using a
#' Two-Stage Copula Generated Regressor Approach. \emph{Journal of
#' Marketing Research}, 62(4), 601--623.
#' \doi{10.1177/00222437241296453}
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase3"
