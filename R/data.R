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


#' Simulated Dataset for the BMW Nonparametric Control Function Approach
#'
#' A dataset with one exogenous regressor `X` and one
#' continuous endogenous regressor `P`, generated and inspired by DGP1
#' of Breitung, Meyer, and Wied (2024), Section 4, with correlated regressors
#' (`delta = 1`) and endogeneity (`rho = 0.9`).
#' The dependent variable is `y`.
#' The true parameter values are: `mu = 1` (intercept),
#' `beta = -1` (`X`), and `alpha = 1` (`P`).
#'
#' @usage data("dataCopBMW")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `X`: a numeric vector, Gamma(1,1) distributed and exogenous.
#' - `P`: a numeric vector, continuous and endogenous, constructed
#'   as `P = X + e` where `e ~ lognormal(0,1)` is independent of `X`.
#'
#' @template template_references_breitung2024
#' @template template_author_kimberlylew
#' @md
"dataCopBMW"

#' Simulated Dataset for the BMW Approach with Two Endogenous Regressors
#'
#' A dataset with one exogenous regressor `X` and two
#' continuous endogenous regressors `P1` and `P2`, generated
#' as an extension of DGP1 to the multiple endogenous regressor case (Remark 2.1).
#' Both endogenous regressors are correlated with `X` (`delta1 = delta2 = 1`)
#' and shows moderate endogeneity (`rho1 = rho2 = 0.5`).
#' The true parameter values are: `mu = 1` (intercept),
#' `beta = -1` (`X`), `alpha1 = 1` (`P1`),
#' and `alpha2 = 1` (`P2`).
#' The first-stage errors follow \eqn{\text{Gamma}(3,2)} where the
#' distribution of \eqn{e} is closer to Gaussian than \eqn{\text{Gamma}(1,1)}
#' making identification more challenging. It can be seen in Table 1 of J. Breitung
#' et al. (2024) that the method correctly removes bias when \eqn{\delta = 1}.
#'
#' @usage data("dataCopBMWMultiEndo")
#' @format A data frame with 5000 observations on 4 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `X`: a numeric vector, Gamma(1,1) distributed and exogenous.
#' - `P1`: a numeric vector, continuous and endogenous, constructed
#'   as `P1 = X + e1` where `e1 ~ Gamma(3,2)` is independent of `X` and `e2`.
#' - `P2`: a numeric vector, continuous and endogenous, constructed
#'   as `P2 = X + e2` where `e2 ~ Gamma(3,2)` is independent of `X` and `e1`.
#'
#' @template template_references_breitung2024
#' @template template_author_kimberlylew
#' @md
"dataCopBMWMultiEndo"


#' Simulated Dataset for 2sCOPEnp - Continuous Endogenous Regressor without
#' neither Gaussian Copula nor Mean-Dependence Model Assumption
#'
#' A dataset simulated with one continuous endogenous regressor
#' `P` and one exogenous regressor `X`, where neither the joint Gaussian
#' copula nor the mean-dependence-only assumption holds. The exogenous regressor `X`
#' follows a Student-t distribution with 3 degrees of freedom, and the conditional
#' distribution of `P` given `X` is a truncated standard normal with `X`-dependent
#' bounds (Section 4.3, Hu et al. 2025). The endogeneity strength is
#' rho = 0.5. This dataset corresponds to Case 3 of the simulation study
#' in Hu et al. (2025).
#' The true parameter values are `mu = 1` for the intercept,
#' `alpha = 1` for `P` and `beta = 2` for `X`.
#'
#' @usage data("dataCopula2sCOPEnpCont")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P`: a numeric vector, continuous and endogenous, following
#'   a truncated standard normal distribution with `X`-dependent bounds
#'   \eqn{a = \min(0, -2X + 2)} and \eqn{b = \max(2, -2X + 2)}.
#' - `X`: a numeric vector, continuous and exogenous, following
#'   a Student-t distribution with 3 degrees of freedom.
#'
#' @template template_references_hu2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPEnpCont"


#' Simulated Dataset for 2sCOPEnp - Binary Endogenous Regressor
#'
#' A dataset simulated with one binary endogenous regressor
#' `P` (Bernoulli, taking values 0 or 1) and one continuous exogenous
#' regressor `X` following a Student-t distribution with 3 degrees of
#' freedom. The latent variables `(P*, X*, Xi*)` follow a trivariate
#' normal distribution with rho_px = 0.5 and rho_pxi = 0.5
#' (Equation 44, Hu et al. 2025). The binary treatment status `P` is
#' obtained via the indicator function \eqn{P = \mathbf{1}\{\Phi(P^*) > 0.5\}},
#' corresponding to a balanced binary treatment (approximately 50% treated).
#' This dataset corresponds to Case 5 of the simulation study in Hu et al.
#' (2025) and demonstrates the ability of 2sCOPEnp method to handle
#' discrete endogenous regressors.
#' The true parameter values are `mu = 0` for the intercept,
#' `alpha = 1` for `P` and `beta = 2` for `X`.
#'
#' @usage data("dataCopula2sCOPEnpBi")
#' @format A data frame with 2000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P`: an integer vector, binary (0 or 1) and endogenous,
#'   representing a balanced binary treatment with approximately 50%
#'   treated observations.
#' - `X`: a numeric vector, continuous and exogenous, following
#'   a Student-t distribution with 3 degrees of freedom.
#'
#' @template template_references_hu2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPEnpBi"

#' Simulated Dataset for 2sCOPEnp -  Multiple Endogenous and Exogenous Regressors with
#' mixed continuous, ordered and factor variables
#'
#' @description A dataset simulated with to demonstrate how [copula2sCOPEnp()] interacts
#' with different types of variables and multiple endogenous and exogenous regressors. The structural
#' model here is:
#' \deqn{y_i = \mu + \alpha_1 P_{1,i} + \alpha_2 P_{2,i} + \beta_1 X_{1,i} + \beta_2 X_{2,i} + \beta_3 X_{3,i} + \varepsilon_i}
#'
#' `P1` and `P2` are both endogenous regressors correlated with the structural error,
#' \eqn{\rho_{P1. \varepsilon} = 0.5} and \eqn{\rho_{P2, \varepsilon} = 0.7} respectively.
#' They are correlated with each other \eqn{\rho_{P1,P2} = 0.3}. `P1` is correlated with the
#' continuous exogenous regressor `X1`, and `P2` with the continuous exogenous regressor `X2`.
#' `X3` is purely exogenous ordered control variable.
#'
#' `P2` is an ordered factor endogenous regressor with 4 levels: low, medium, high and very high.
#'
#' Note that a stronger endogeneity for `P2` (\eqn{\rho = 0.7}) and larger sample size (\eqn{n = 5000}) were used
#' because the correction term \eqn{\Phi^{-1}(\hat{F}(P_2 | X))} takes only 4 distinct values (one per ordered level),
#' reducing detection power compared to a continuous endogenous regressor.
#'
#' The true parameter values are: `mu =1` (intercept), `alpha1 = -1` (`P1`), `alpha2 = 1`(`P2`, per ordered level),
#' `beta1 =2` (`X1`), `beta2 = 0.5`(`X2`) and `beta3 = 1` (`X3`, per ordered level)
#'
#' @usage data("dataCopula2sCOPEnpMulti")
#' @format A data frame with 5000 observations on 6 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P1`: a numeric vector which is continuous and endogenous following a
#'   lognormal distribution and correlated with structural error (\eqn{\rho = 0.5}) and with `X1`
#' - `P2`: an ordered factor with 4 levels (low, medium, high and very high) endogenous.
#'   `P2` is correlated with the structural error (\eqn{\rho = 0.7}) and with `X2`.
#' - `X1`: a numeric vector which is continuous and exogenous.
#'   It follows a  \eqn{N(0,1)} distribution and is correlated with `P1`.
#' - `X2`: a numeric vector which is continuous and exogenous and approximately
#'   follows a \eqn{N(0,1)} distribution. It is Correlated with `P2`.
#'   Note that this was kept continuous (not a factor) so that cross-validation for \eqn{\hat{F}(P_2 | X)}
#'   finds a finite non-degenerate bandwidth rather than collapsing to exact category matching.
#' - `X3`: an ordered factor with three levels (low, medium and high) purely exogenous.
#'   Used as an  ordered control variable in the outcome equation.
#'   Note that it was not used in the generation of `P2` so that cross-validation does not
#'   collapse to exact category matching on `X3`.
#'
#' @note
#' Both `P1_cop` and `P2_cop` are statistically significant, confirming endogeneity of both
#' regressors. The quadratic contrast `P2.Q` may show a small but significant coefficient even though
#' the true relationship is linear in the ordered levels - this is an expected behaviour of the augmented
#' regression separating the structural `P2` coefficients from the discrete correction term
#' at stronger endogeneity levels (\eqn{\rho = 0.7}).
#' @template template_references_hu2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPEnpMulti"

#' Simulated Dataset for 2sCOPE - Non-normal Endogenous and Exogenous Regressors
#'
#' A dataset simulated with one endogenous regressor `P`
#' and one exogenous regressor `X`, both non-normal and correlated
#' with each other (rho_px = 0.5). The endogenous regressor `P`
#' follows a Gamma(1,1) distribution and the exogenous regressor `X`
#' follows an Exp(1) distribution. The endogeneity strength is rho_pxi = 0.5.
#' This dataset corresponds to Case 1 of Yang et al. (2025) and
#' demonstrates 2sCOPE's main contribution: Consistent estimation when the
#' endogenous and exogenous regressors are correlated.
#' The true parameter values are `mu = 1` for the intercept,
#' `alpha = 1` for `P` and `beta = -1` for `X`.
#'
#' @usage data("dataCopula2sCOPECase1")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P`: a numeric vector, continuous and endogenous,
#'   Gamma(1,1) distributed and correlated with `X` (rho_px = 0.5).
#' - `X`: a numeric vector, continuous and exogenous,
#'   Exp(1) distributed and correlated with `P` (rho_px = 0.5).
#'
#' @template template_references_yang2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPECase1"

#' Simulated Dataset for 2sCOPE - Non-normal Endogenous and Normal Exogenous Regressor
#'
#' A dataset simulated with one endogenous regressor `P`
#' (non-normal, Gamma(1,1)) and one normally distributed exogenous regressor
#' `X`, correlated with each other (rho_px = 0.5). The endogeneity
#' strength is rho_pxi = 0.5. This dataset corresponds to Case 2 of Yang et al. (2025)
#' and demonstrates that 2sCOPE remains consistent when the exogenous regressor `X` is
#' normally distributed.
#' The true parameter values are `mu = 1` for the intercept,
#' `alpha = 1` for `P` and `beta = -1` for `X`.
#'
#' @usage data("dataCopula2sCOPECase2")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P`: a numeric vector, continuous and endogenous,
#'   Gamma(1,1) distributed and correlated with `X` (rho_px = 0.5).
#' - `X`: a numeric vector, continuous and exogenous,
#'   normally distributed and correlated with `P` (rho_px = 0.5).
#'
#' @template template_references_yang2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPECase2"


#' Simulated Dataset for 2sCOPE - Normally Distributed Endogenous and Non-normal Exogenous Regressor
#'
#' A dataset simulated with one normally distributed endogenous
#' regressor `P` and one non-normal exogenous regressor `X`
#' (Exp(1)), correlated with each other (rho_px = 0.5). The endogeneity
#' strength is rho_pxi = 0.5. This dataset corresponds to another type of Case 2
#' of Yang et al. (2025) and demonstrates a key advantage of
#' 2sCOPE. The method is still consistently estimating, even when the endogenous regressor `P`
#' is normally distributed, because the identification comes from the non-normal
#' exogenous regressor `X`.
#' The true parameter values are `mu = 1` for the intercept,
#' `alpha = 1` for `P` and `beta = -1` for `X`.
#'
#' @usage data("dataCopula2sCOPECase3")
#' @format A data frame with 2500 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P`: a numeric vector, continuous and endogenous,
#'   normally distributed and correlated with `X` (rho_px = 0.5).
#' - `X`: a numeric vector, continuous and exogenous,
#'   Exp(1) distributed and correlated with `P` (rho_px = 0.5).
#'
#' @template template_references_yang2025
#' @template template_author_kimberlylew
#' @md
"dataCopula2sCOPECase3"


#' Simulated Dataset for Copula IMA - Continuous Exogenous Regressor
#'
#' A dataset simulated with one exogenous regressor `X` and one
#' endogenous continuous regressor `P`, and a dependent variable `y`.
#' The exogenous regressor `X` follows a normal distribution with mean 1
#' and is correlated with `P` (r = 0.5). The endogenous regressor `P`
#' follows a bounded continuous distribution (`Phi(P*) + 0.5`). The endogeneity
#' strength is rho = 0.5. There is no intercept in the data generating process.
#' The true parameter values are given as `alpha = 1` for `P` and
#' `beta = 1` for `X`.
#'
#' @usage data("dataCopIMAContExo")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `X`: a numeric vector, normally distributed with mean 1, exogenous
#'   and correlated with `P`.
#' - `P`: a numeric vector, continuous and endogenous, following a
#'   bounded distribution `Phi(P*) + 0.5` with values in (0.5, 1.5).
#'
#' @template template_references_haschka2025ima
#' @template template_author_kimberlylew
#' @md
"dataCopIMAContExo"


#' Simulated Dataset for Copula IMA - Binary Exogenous Regressor
#'
#' A dataset simulated with one binary exogenous regressor `X`
#' and one endogenous continuous regressor `P`, and a dependent variable `y`.
#' The exogenous regressor `X` is a binary variable (0 or 1) derived from
#' a latent standard normal variable and is correlated with `P` (r = 0.5).
#' The endogenous regressor `P` follows a bounded continuous distribution
#' (`Phi(P*) + 0.5`). The endogeneity strength is given by rho = 0.5. This dataset
#' illustrates that the IMA estimator does not require the exogenous regressor
#' to be continuously or normally distributed. There is no intercept in the
#' data generating process. The true parameter values are `alpha = 1`
#' for `P` and `beta = 1` for `X`.
#'
#' @usage data("dataCopIMABinExo")
#' @format A data frame with 1000 observations on 3 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `X`: a numeric vector, binary (0 or 1) and exogenous,
#'   correlated with `P`.
#' - `P`: a numeric vector, continuous and endogenous, following a
#'   bounded distribution `Phi(P*) + 0.5` with values in (0.5, 1.5).
#'
#' @template template_references_haschka2025ima
#' @template template_author_kimberlylew
#' @md
"dataCopIMABinExo"

#' Simulated Dataset for Copula IMA - Two Continuous Endogenous Regressors,
#' Intercept and One Exogenous Regressor
#'
#' A dataset simulated with two endogenous continuous regressors
#' `P1` and `P2`, an intercept, a single continuous exogenous regressor `X`
#' and a dependent variable `y`. Both endogenous regressors follow a bounded
#' continuous distribution (`Phi(P*) + 0.5`) and are independent of each other.
#' The structural error and both endogenous regressors are correlated
#' (endogeneity strength rho = 0.5). They are also correlated with the exogenous regressor (rho = 0.5).
#' The true parameter values are `beta = 1` for `X`,
#' `alpha1 = 1` for `P1`, `alpha2 = -1`
#' for `P2`, and `mu = 10` for the intercept. This was simulated as
#' an extension to the dataset 'dataCopIMAContExo'. This dataset was created to test
#' how well [copulaIMA()] works with an intercept and multiple endogenous regressors.
#' N = 5000 to avoid the problem of finite sample bias.
#'
#' @usage data("dataCopIMAMultiEndo")
#' @format A data frame with 5000 observations on 4 variables:
#' - `y`: a numeric vector representing the dependent variable.
#' - `P1`: a numeric vector, continuous and endogenous, following a
#'   bounded distribution `Phi(P1*) + 0.5` with values in (0.5, 1.5)
#' - `P2`: a numeric vector, continuous and endogenous, following a
#'   bounded distribution `Phi(P2*) + 0.5` with values in (0.5, 1.5)
#' - `X`: a numeric vector, continuous and exogenous and following
#'   a distribution \eqn{N(1,1)}. Correlated with both `P1` and `P2`
#'   at \eqn{\rho = 0.5} but uncorrelated with structural error.
#'
#' @template template_references_haschka2025ima
#' @template template_author_kimberlylew
#' @md
"dataCopIMAMultiEndo"
