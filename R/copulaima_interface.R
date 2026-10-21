#' Copula-based Instrumental Variable Model (IMA)
#'
#' @description
#' Fitting Haschka's copula-based Instrumental Variable Model (IMA) using
#' Gaussian copulas to address endogeneity without external instruments, while
#' allowing dependence between endogenous and exogenous regressors.
#'
#' The copula-based IMA approach corrects endogeneity by constructing
#' control functions from the marginal distributions of the endogenous
#' regressors and incorporating them into the estimation equation.
#' Inference is based on bootstrapping.
#'
#' The key contribution of IMA over the original Park and Gupta (2012) copula
#' correction (\code{\link{copulaCorrection}}) is that it does not require the
#' endogenous and exogenous regressors to be independent. When this independence
#' assumption is violated, which is common in practice, the original copula
#' correction yields severely biased estimates of all model coefficients.
#' IMA corrects for this by exploiting the content of the exogenous
#' variables through a first-stage auxiliary regression, in a similar way to
#' instrumental variable identification.
#'
#' The IMA method requires a normally distributed structural error. At least one endogenous
#' regressor \eqn{P_{i,k}} is continuously and nonnormally distributed. It does not
#' require that the exogenous regressors \eqn{X_i} be independent of the endogenous
#' regressors \eqn{P_{i,k}} or to satisfy any distributional requirement, i.e., \eqn{X_i}
#' may be binary or non-continuously distributed. This method supports only continuous
#' endogenous regressors. At least one exogenous regressor is required. Without
#' exogenous regressors, IMA is equivalent to Park and Gupta (2012)
#' (\code{\link{copulaCorrection}}).
#'
#' @templateVar data.addition Endogenous and exogenous regressors must be \code{numeric}.
#' @template template_param_formuladataverbose
#' @template template_param_numboots
#' @template template_param_cdf
#'
#' @details
#' ## Model
#'
#' Consider the structural regression model with \eqn{K} endogenous regressors:
#'
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i' \beta +
#'       \varepsilon_i}
#'
#' where \eqn{i = 1, \ldots, n} is the number of observations,
#' \eqn{Y_i} is the dependent variable, \eqn{P_{i,k}} are continuous endogenous regressors
#' that may be correlated with both the structural error \eqn{\varepsilon_i}
#' and the exogenous regressors \eqn{X_i},
#' \eqn{X_i} is a vector of exogenous regressors uncorrelated with \eqn{\varepsilon_i}, and
#' \eqn{\mu, \alpha_k, \beta} are the structural model parameters.
#'
#' IMA is an augmented OLS estimator. The estimation proceeds in three steps:
#'
#' \enumerate{
#'   \item For every explanatory variable \eqn{W \in \{X_1, \ldots, X_J,
#'         P_1, \ldots, P_K\}}, compute the normal score
#'         \eqn{W^* = \Phi^{-1}(\hat{F}_W(W))}, where \eqn{\hat{F}_W} is
#'         the estimated marginal CDF of \eqn{W} and \eqn{\Phi^{-1}} is the
#'         standard normal quantile function.
#'   \item For each endogenous regressor \eqn{P_{i,k}^*}, regress it on the normal
#'         scores of the exogenous regressors \eqn{X_1^*, \ldots, X_J^*}
#'         \emph{without intercept} and retain the residuals
#'         \eqn{\hat{\varepsilon}_k} as the copula correction terms. This
#'         first-stage regression exploits the dependence between endogenous
#'         and exogenous regressors in a way analogous to instrumental variable
#'         identification.
#'   \item Augment the structural model with \eqn{\hat{\varepsilon}_1, \ldots,
#'         \hat{\varepsilon}_{K}} as additional regressors and estimate by OLS.
#' }
#'
#' Haschka (2025) reported that simulation results showed that copula-based IMA
#' estimator may exhibit a slightly larger bias than alternative two-stage
#' approaches when the sample size is very small (e.g., n = 100) and with intercept.
#' However, the bias decreases rapidly as sample size increases and become
#' negligible for moderate sample sizes (around n >= 600). The estimator appears
#' asymptotically unbiased.
#'
#' @template template_text_details_cdfmethods
#'
#' @details
#' ## Parameter \code{formula}
#' The \code{formula} argument follows a two part notation separated by \code{|}.
#' The first part specifies the structural model (e.g. \code{y ~ X + P}).
#' The second part identifies the continuous endogenous regressors:
#'
#' \preformatted{y ~ X + P | P                       # endogenous P}
#' \preformatted{y ~ X + P1 + log(P2) | P1 + log(P2) # multiple endogenous regressors}
#' \preformatted{y ~ X + P - 1 | P                   # no intercept (in simulation settings)}
#'
#' In typical applied settings the model includes an intercept. The
#' no-intercept specification (\code{-1}) is primarily used in simulation
#' studies following the design of (Haschka (2025) Section 4.1)
#' and is unlikely to be appropriate for real datasets.
#'
#' The formula may not contain interaction terms (e.g. \code{X:P} or \code{X*P})
#' in either part.
#'
#'
#' @template template_text_details_bootsdegenerates
#'
#' @template template_references_haschka2025ima
#' @template template_references_parkgupta2012
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopula2stage_return(subclass = "rendo.copula.ima")
#'
#' @family copula-based methods
#'
#' @seealso \code{\link[REndo:dataCopIMABinExo]{dataCopIMABinExo},
#' \link[REndo:dataCopIMAContExo]{dataCopIMAContExo},
#' \link[REndo:dataCopIMAMultiEndo]{dataCopIMAMultiEndo}} for detailed
#' information about the simulated datasets
#'
#' @examples
#' #------------------------------------------------------------------------
#' # Example 1: Single endogenous regressor with continuous
#' # and normal exogenous regressor (Haschka 2025, Section 4.1 Scenario 1)
#' # True values: alpha = 1 (P), beta = 1 (X), no intercept
#' #------------------------------------------------------------------------
#' data(dataCopIMAContExo)
#' res <- copulaIMA(
#'   y ~ X + P - 1 | P,
#'   data = dataCopIMAContExo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res)
#'
#' \donttest{
#' #------------------------------------------------------------------------
#' # Example 2: Two endogenous regressors with intercept and one exogenous regressor
#' # Extension of the first example
#' # True values: mu=10, alpha1 = 1 (P1), alpha2 = -1 (P2), beta = 1 (X)
#' #------------------------------------------------------------------------
#' data("dataCopIMAMultiEndo")
#' res2 <- copulaIMA(
#'   y ~ X + P1 + P2 | P1 + P2,
#'   data = dataCopIMAMultiEndo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res2)
#'
#' #------------------------------------------------------------------------
#' # Example 3: Single endogenous regressor with binary exogenous regressor
#' # (Haschka 2025, section 4.1 scenario 2)
#' #
#' # This example shows one of the key examples of IMA method, i.e., the exogenous
#' # regressor does not need to be continuous or normally distributed.
#' # X the exogenous regressor is binary (0 or 1).
#' # True values: alpha = 1 (P), beta = 1 (X), no intercept.
#' #------------------------------------------------------------------------
#' data("dataCopIMABinExo")
#' res3 <- copulaIMA(
#'    y ~ X + P - 1 | P,
#'    data = dataCopIMABinExo,
#'    cdf = "adj.ecdf",
#'    num.boots = 1000
#' )
#' summary(res3)
#' }
#'
#' @md
#' @export
#'
#' @importFrom stats coef terms
#' @importFrom Formula as.Formula
copulaIMA <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # Input checks  -------------------------------------------------------------------
  allowed.cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")
  check_err_msg(checkinput_copula2stage_data_basics(data))
  check_err_msg(checkinput_copulaima_formula_data(data = data, formula = formula))
  check_err_msg(checkinput_copula2stage_cdf(cdf, allowed.cdf = allowed.cdfs))
  check_err_msg(checkinput_copula2stage_numboots(num.boots))
  check_err_msg(checkinput_copula2stage_verbose(verbose))

  # Fit original data ---------------------------------------------------------------

  cdf <- match.arg(cdf, choices = allowed.cdfs)

  F.formula <- as.Formula(formula)
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  # Fitting with copula IMA model
  if (verbose) {
    message(
      "Fitting Haschka's copula-based IMA model with ",
      length(labels.endo),
      " continuous endogenous regressor(s)."
    )
  }
  fit <- copulaima_fit(
    F.formula = F.formula,
    data = data,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo
  )

  # Bootstrapping -------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    fit.b <- copulaima_fit(
      F.formula = F.formula,
      data = data.b,
      cdf = cdf,
      labels.endo = labels.endo,
      labels.exo = labels.exo
    )
    return(fit.b$res.augmented)
  }

  res.boots <- bootstrap_skip_degenerates(
    fn.fit = fn.fit.boots,
    data = data,
    num.boots = num.boots,
    coef.names = names(coef(fit$res.augmented)),
    verbose = verbose
  )

  # Return value ----------------------------------------------------------------------
  return(new_rendo_copula2stage(
    call = cl,
    F.formula = F.formula,
    res.lm.augmented = fit$res.augmented,
    res.boots = res.boots,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = fit$labels.pcop,
    subclass = "rendo.copula.ima"
  ))
}
