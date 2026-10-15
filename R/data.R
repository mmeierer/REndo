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


#' @title Simulated Dataset for the BMW Nonparametric Control Function Approach
#' @description A dataset with one exogenous regressor \code{X} and one
#'   continuous endogenous regressor \code{P}, generated and inspired by DGP1
#'   of Breitung, Meyer, and Wied (2024), Section 4, with correlated regressors
#'   (\code{delta = 1}) and endogeneity (\code{rho = 0.9}).
#'   The dependent variable is \code{y}.
#'   The true parameter values are: \code{mu = 1} (intercept),
#'   \code{beta = -1} (\code{X}), and \code{alpha = 1} (\code{P}).
#' @name dataCopBMW
#' @usage data("dataCopBMW")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#'   \item{\code{y}}{a numeric vector representing the dependent variable.}
#'   \item{\code{X}}{a numeric vector, Gamma(1,1) distributed and exogenous.}
#'   \item{\code{P}}{a numeric vector, continuous and endogenous, constructed
#'     as \code{P = X + e} where \code{e ~ lognormal(0,1)} is independent of
#'     \code{X}.}
#' }
#' @docType data
#' @author Kimberly-Anne Lew Chuk Wai \email{kimberlylew12@@gmail.com}
"dataCopBMW"

#' @title Simulated Dataset for the BMW Approach with Two Endogenous Regressors
#' @description A dataset with one exogenous regressor \code{X} and two
#'   continuous endogenous regressors \code{P1} and \code{P2}, generated
#'   as an extension of DGP1 to the multiple endogenous regressor case (Remark 2.1).
#'   Both endogenous regressors are correlated with \code{X} (\code{delta1 = delta2 = 1})
#'   and shows moderate endogeneity (\code{rho1 = rho2 = 0.5}).
#'   The true parameter values are: \code{mu = 1} (intercept),
#'   \code{beta = -1} (\code{X}), \code{alpha1 = 1} (\code{P1}),
#'   and \code{alpha2 = 1} (\code{P2}).
#'   The first-stage errors follow \eqn{\text{Gamma}(3,2)} where the
#'   distribution of \eqn{e} is closer to Gaussian than \eqn{\text{Gamma}(1,1)}
#'   making identification more challenging. It can be seen in Table 1 of J. Breitung
#'   et al. (2024) that the method correctly removes bias when \eqn{\delta = 1}.
#'
#' @name dataCopBMWMultiEndo
#' @usage data("dataCopBMWMultiEndo")
#' @format A data frame with 5000 observations on 4 variables:
#' \describe{
#'   \item{\code{y}}{a numeric vector representing the dependent variable.}
#'   \item{\code{X}}{a numeric vector, Gamma(1,1) distributed and exogenous.}
#'   \item{\code{P1}}{a numeric vector, continuous and endogenous, constructed
#'     as \code{P1 = X + e1} where \code{e1 ~ Gamma(3,2)} is independent of
#'     \code{X} and \code{e2}.}
#'   \item{\code{P2}}{a numeric vector, continuous and endogenous, constructed
#'     as \code{P2 = X + e2} where \code{e2 ~ Gamma(3,2)} is independent of
#'     \code{X} and \code{e1}.}
#' }
#' @docType data
#' @author Kimberly-Anne Lew Chuk Wai \email{kimberlylew12@@gmail.com}
"dataCopBMWMultiEndo"


#' @title Simulated Dataset for 2sCOPEnp - Continuous Endogenous Regressor without
#'   neither Gaussian Copula nor Mean-Dependence Model Assumption.
#' @description A dataset simulated with one continuous endogenous regressor
#'   \code{P} and one exogenous regressor \code{X}, where neither the joint Gaussian
#'   copula nor the mean-dependence-only assumption holds. The exogenous regressor \code{X}
#'   follows a Student-t distribution with 3 degrees of freedom, and the conditional
#'   distribution of \code{P} given \code{X} is a truncated standard normal with \code{X}-dependent
#'   bounds (Section 4.3, Hu et al. 2025). The endogeneity strength is
#'   rho = 0.5. This dataset corresponds to Case 3 of the simulation study
#'   in Hu et al. (2025).
#'   The true parameter values are \code{mu = 1} for the intercept,
#'   \code{alpha = 1} for \code{P} and \code{beta = 2} for \code{X}.
#' @name dataCopula2sCOPEnpCont
#' @usage data("dataCopula2sCOPEnpCont")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous, following
#'   a truncated standard normal distribution with \code{X}-dependent bounds
#'   \eqn{a = \min(0, -2X + 2)} and \eqn{b = \max(2, -2X + 2)}.}
#' \item{\code{X}}{a numeric vector, continuous and exogenous, following
#'   a Student-t distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @template template_references_hu2025
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPEnpCont"


#' @title Simulated Dataset for 2sCOPEnp - Binary Endogenous Regressor
#' @description A dataset simulated with one binary endogenous regressor
#'   \code{P} (Bernoulli, taking values 0 or 1) and one continuous exogenous
#'   regressor \code{X} following a Student-t distribution with 3 degrees of
#'   freedom. The latent variables \code{(P*, X*, Xi*)} follow a trivariate
#'   normal distribution with rho_px = 0.5 and rho_pxi = 0.5
#'   (Equation 44, Hu et al. 2025). The binary treatment status \code{P} is
#'   obtained via the indicator function \eqn{P = \mathbf{1}\{\Phi(P^*) > 0.5\}},
#'   corresponding to a balanced binary treatment (approximately 50\% treated).
#'   This dataset corresponds to Case 5 of the simulation study in Hu et al.
#'   (2025) and demonstrates the ability of 2sCOPEnp method to handle
#'   discrete endogenous regressors.
#'   The true parameter values are \code{mu = 0} for the intercept,
#'   \code{alpha = 1} for \code{P} and \code{beta = 2} for \code{X}.
#' @name dataCopula2sCOPEnpBi
#' @usage data("dataCopula2sCOPEnpBi")
#' @format A data frame with 2000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P}}{an integer vector, binary (0 or 1) and endogenous,
#'   representing a balanced binary treatment with approximately 50\%
#'   treated observations.}
#' \item{\code{X}}{a numeric vector, continuous and exogenous, following
#'   a Student-t distribution with 3 degrees of freedom.}
#' }
#' @docType data
#' @template template_references_hu2025
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPEnpBi"

#' @title Simulated Dataset for 2sCOPEnp -  Multiple Endogenous and Exogenous Regressors with
#' mixed continuous, ordered and factor variables
#' @description A dataset simulated with to demonstrate how \code{\link{copula2sCOPEnp}} interacts
#' with different types of variables and multiple endogenous and exogenous regressors. The structural
#' model here is:
#' \deqn{y_i = \mu + \alpha_1 P_{1,i} + \alpha_2 P_{2,i} + \beta_1 X_{1,i} + \beta_2 X_{2,i} + \beta_3 X_{3,i} + \varepsilon_i}
#'
#' \code{P1} and \code{P2} are both endogenous regressors correlated with the structural error,
#' \eqn{\rho_{P1. \varepsilon} = 0.5} and \eqn{\rho_{P2, \varepsilon} = 0.7} respectively.
#' They are correlated with each other \eqn{\rho_{P1,P2} = 0.3}. \code{P1} is correlated with the
#' continuous exogenous regressor \code{X1}, and \code{P2} with the continuous exogenous regressor \code{X2}.
#' \code{X3} is purely exogenous ordered control variable.
#'
#' \code{P2} is an ordered factor endogenous regressor with 4 levels: low, medium, high and very high.
#'
#' Note that a stronger endogeneity for \code{P2} (\eqn{\rho = 0.7}) and larger sample size (\eqn{n = 5000}) were used
#' because the correction term \eqn{\Phi^{-1}(\hat{F}(P_2 | X))} takes only 4 distinct values (one per ordered level),
#' reducing detection power compared to a continuous endogenous regressor.
#'
#' The true parameter values are: \code{mu =1} (intercept), \code{alpha1 = -1} (\code{P1}), \code{alpha2 = 1}(\code{P2}, per ordered level),
#' \code{beta1 =2} (\code{X1}), \code{beta2 = 0.5}(\code{X2}) and \code{beta3 = 1} (\code{X3}, per ordered level)
#'
#' @name dataCopula2sCOPEnpMulti
#' @usage data("dataCopula2sCOPEnpMulti")
#' @format A data frame with 5000 observations on 6 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P1}}{a numeric vector which is continuous and endogenous following a
#' lognormal distribution and correlated with structural error (\eqn{\rho = 0.5}) and with \code{X1}}
#' \item{\code{P2}}{an ordered factor with 4 levels (low, medium, high and very high) endogenous.
#' \code{P2} is correlated with the structural error (\eqn{\rho = 0.7}) and with \code{X2}.}
#' \item{\code{X1}}{a numeric vector which is continuous and exogenous.
#' It follows a  \eqn{N(0,1)} distribution and is correlated with \code{P1}.}
#' \item{\code{X2}}{a numeric vector which is continuous and exogenous and approximately
#' follows a \eqn{N(0,1)} distribution. It is Correlated with \code{P2}.
#' Note that this was kept continuous (not a factor) so that cross-validation for \eqn{\hat{F}(P_2 | X)}
#' finds a finite non-degenerate bandwidth rather than collapsing to exact category matching.}
#' \item{\code{X3}}{an ordered factor with three levels (low, medium and high) purely exogenous.
#' Used as an  ordered control variable in the outcome equation.
#' Note that it was not used in the generation of \code{P2} so that cross-validation does not
#' collapse to exact category matching on \code{X3}.}
#' }
#' @note
#' Both \code{P1_cop} and \code{P2_cop} are statistically significant, confirming endogeneity of both
#' regressors. The quadratic contrast \code{P2.Q} may show a small but significant coefficient even though
#' the true relationship is linear in the ordered levels — this is an expected behaviour of the augmented
#' regression separating the structural \code{P2} coefficients from the discrete correction term
#' at stronger endogeneity levels (\eqn{\rho = 0.7}).
#' @docType data
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPEnpMulti"

#' @title Simulated Dataset for 2sCOPE - Non-normal Endogenous and Exogenous Regressors
#' @description A dataset simulated with one endogenous regressor \code{P}
#'   and one exogenous regressor \code{X}, both non-normal and correlated
#'   with each other (rho_px = 0.5). The endogenous regressor \code{P}
#'   follows a Gamma(1,1) distribution and the exogenous regressor \code{X}
#'   follows an Exp(1) distribution. The endogeneity strength is rho_pxi = 0.5.
#'   This dataset corresponds to Case 1 of Yang et al. (2025) and
#'   demonstrates 2sCOPE's main contribution: Consistent estimation when the
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
#' @template template_references_yang2025
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase1"

#' @title Simulated Dataset for 2sCOPE - Non-normal Endogenous and Normal Exogenous Regressor
#' @description A dataset simulated with one endogenous regressor \code{P}
#'   (non-normal, Gamma(1,1)) and one normally distributed exogenous regressor
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
#' @template template_references_yang2025
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase2"


#' @title Simulated Dataset for 2sCOPE - Normally Distributed Endogenous and Non-normal Exogenous Regressor
#' @description A dataset simulated with one normally distributed endogenous
#'   regressor \code{P} and one non-normal exogenous regressor \code{X}
#'   (Exp(1)), correlated with each other (rho_px = 0.5). The endogeneity
#'   strength is rho_pxi = 0.5. This dataset corresponds to another type of Case 2
#'   of Yang et al. (2025) and demonstrates a key advantage of
#'   2sCOPE. The method is still consistently estimating, even when the endogenous regressor \code{P}
#'   is normally distributed, because the identification comes from the non-normal
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
#' @template template_references_yang2025
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopula2sCOPECase3"


#' @title Simulated Dataset for Copula IMA - Continuous Exogenous Regressor
#' @description A dataset simulated with one exogenous regressor \code{X} and one
#'   endogenous continuous regressor \code{P}, and a dependent variable \code{y}.
#'   The exogenous regressor \code{X} follows a normal distribution with mean 1
#'   and is correlated with \code{P} (r = 0.5). The endogenous regressor \code{P}
#'   follows a bounded continuous distribution (Phi(P*) + 0.5). The endogeneity
#'   strength is rho = 0.5. There is no intercept in the data generating process.
#'   The true parameter values are given as \code{alpha = 1} for \code{P} and
#'   \code{beta = 1} for \code{X}.
#' @template template_references_haschka2025ima
#' @name dataCopIMAContExo
#' @usage data("dataCopIMAContExo")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X}}{a numeric vector, normally distributed with mean 1, exogenous
#'   and correlated with \code{P}.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous, following a
#'   bounded distribution Phi(P*) + 0.5 with values in (0.5, 1.5).}
#' }
#' @docType data
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopIMAContExo"


#' @title Simulated Dataset for Copula IMA - Binary Exogenous Regressor
#' @description A dataset simulated with one binary exogenous regressor \code{X}
#'   and one endogenous continuous regressor \code{P}, and a dependent variable \code{y}.
#'   The exogenous regressor \code{X} is a binary variable (0 or 1) derived from
#'   a latent standard normal variable and is correlated with \code{P} (r = 0.5).
#'   The endogenous regressor \code{P} follows a bounded continuous distribution
#'   (Phi(P*) + 0.5). The endogeneity strength is given by rho = 0.5. This dataset
#'   illustrates that the IMA estimator does not require the exogenous regressor
#'   to be continuously or normally distributed. There is no intercept in the
#'   data generating process. The true parameter values are \code{alpha = 1}
#'   for \code{P} and \code{beta = 1} for \code{X}.
#' @template template_references_haschka2025ima
#' @name dataCopIMABinExo
#' @usage data("dataCopIMABinExo")
#' @format A data frame with 1000 observations on 3 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{X}}{a numeric vector, binary (0 or 1) and exogenous,
#'   correlated with \code{P}.}
#' \item{\code{P}}{a numeric vector, continuous and endogenous, following a
#'   bounded distribution Phi(P*) + 0.5 with values in (0.5, 1.5).}
#' }
#' @docType data
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopIMABinExo"

#' @title Simulated Dataset for Copula IMA - Two Continuous Endogenous Regressors,
#' Intercept and One Exogenous Regressor
#' @description A dataset simulated with two endogenous continuous regressors
#'  \code{P1} and \code{P2}, an intercept, a single continuous exogenous regressor \code{X}
#'   and a dependent variable \code{y}. Both endogenous regressors follow a bounded
#'   continuous distribution (Phi(P*) + 0.5) and are independent of each other.
#'   The structural error and both endogenous regressors are correlated
#'   (endogeneity strength rho = 0.5). They are also correlated with the exogenous regressor (rho = 0.5).
#'   The true parameter values are \code{beta = 1} for \code{X},
#'    \code{alpha1 = 1} for \code{P1}, \code{alpha2 = -1}
#'   for \code{P2}, and \code{mu = 10} for the intercept.This was simulated as
#'   an extension to the dataset 'dataCopIMAContExo'. This dataset was created to test
#'   how well \code{copulaIMA()} works with an intercept and multiple endogenous regressors.
#'   N = 5000 to avoid the problem of finite sample bias.
#' @template template_references_haschka2025ima
#' @name dataCopIMAMultiEndo
#' @usage data("dataCopIMAMultiEndo")
#' @format A data frame with 5000 observations on 4 variables:
#' \describe{
#' \item{\code{y}}{a numeric vector representing the dependent variable.}
#' \item{\code{P1}}{a numeric vector, continuous and endogenous, following a
#'   bounded distribution Phi(P1*) + 0.5 with values in (0.5, 1.5)}
#' \item{\code{P2}}{a numeric vector, continuous and endogenous, following a
#'   bounded distribution Phi(P2*) + 0.5 with values in (0.5, 1.5)}
#'   \item{\code{X}}{a numeric vector, continuous and exogenous and following
#'    a distribution \eqn{N(1,1)}. Correlated with both \code{P1} and \code{P2}
#'    at \eqn{\rho = 0.5} but uncorrelated with structural error.}
#' }
#' @docType data
#' @author Kimberly Lew \email{kimberlylew12@@gmail.com}
"dataCopIMAMultiEndo"
