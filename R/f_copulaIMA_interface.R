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
#'
#' @template template_param_formuladataverbose
#'
#' @param cdf Character string specifying the method used to estimate the
#'   marginal distribution functions of the endogenous regressors.
#'   One of \code{"adj.ecdf"}, \code{"resc.ecdf"}, \code{"ecdf"}, or \code{"kde"}.
#' \describe{
#'     \item{\code{"adj.ecdf"}}{Adjusted empirical CDF with midrank correction
#'       (Liengaard et al., 2024). Keeps values strictly inside (0,1).}
#'     \item{\code{"resc.ecdf"}}{Rescaled empirical CDF via
#'       \code{copula::pobs} (Qian et al., 2024).}
#'     \item{\code{"ecdf"}}{Empirical CDF with boundary replacement
#'       (Becker et al., 2022).}
#'     \item{\code{"kde"}}{Kernel CDF via \code{ks::kcde}
#'       (Park and Gupta, 2012). Computationally slower.}
#'   }
#' @param num.boots Positive integer giving the number of bootstrap replications
#'   used for standard error estimation. A minimum of 1000 is recommended.
#'
#' @details
#'
#' IMA is an augmented OLS estimator. The estimation proceeds in three steps:
#'
#' \enumerate{
#'   \item For every explanatory variable \eqn{W \in \{X_1, \ldots, X_K,
#'         P_1, \ldots, P_L\}}, compute the normal score
#'         \eqn{W^* = \Phi^{-1}(\hat{F}_W(W))}, where \eqn{\hat{F}_W} is
#'         the estimated marginal CDF of \eqn{W} and \eqn{\Phi^{-1}} is the
#'         standard normal quantile function.
#'   \item For each endogenous regressor \eqn{P_l^*}, regress it on the normal
#'         scores of the exogenous regressors \eqn{X_1^*, \ldots, X_K^*}
#'         \emph{without intercept} and retain the residuals
#'         \eqn{\hat{\varepsilon}_l} as the copula correction terms. This
#'         first-stage regression exploits the dependence between endogenous
#'         and exogenous regressors in a way analogous to instrumental variable
#'         identification.
#'   \item Augment the structural model with \eqn{\hat{\varepsilon}_1, \ldots,
#'         \hat{\varepsilon}_L} as additional regressors and estimate by OLS.
#' }
#'
#' The method relies on estimating the marginal distribution functions of
#' the endogenous regressors using either empirical distribution functions
#' or kernel density estimation. These are transformed via the inverse
#' normal distribution and used to construct copula-based control terms.
#'
#' Bootstrap inference is performed by resampling the data with replacement.
#' Degenerate bootstrap samples (e.g. singular design matrices or failed
#' model estimation) are discarded and resampled until the requested number
#' of valid bootstrap replications is obtained. The percentage of discarded
#' samples is reported as a warning. Confidence intervals are computed
#' using only successful bootstrap replications.
#'
#' Haschka (2025) reported that simulation results showed that copula-based IMA
#' estimator may exhibit a slightly larger bias than alternative two-stage
#' approaches when the sample size is very small (e.g., n = 100) and with intercept.
#' However, the bias decreases rapidly as sample size increases and become
#' negligible for moderate sample sizes (around n >= 600). The estimator appears
#' asymptotically unbiased.
#'
#' @references
#' Haschka, R. E. (2025). Robustness of copula-correction models in causal
#' analysis: Exploiting between-regressor correlation.IMA Journal of Management
#' Mathematics, 36, 161–180
#' \doi{10.1093/imaman/dpae018}
#'
#' Park, S. and Gupta, S. (2012). Handling endogenous regressors by joint
#' estimation using copulas. \emph{Marketing Science}, 31(4), 567--586.
#' \doi{10.1287/mksc.1120.0718}
#'
#' @eval doc_rendocopulaima_return()
#'
#' @examples
#' #------------------------------------------------------------------------
#' # Example 1: Single endogenous regressor with continuous
#' # and normal exogenous regressor (Haschka 2025, Section 4.1 Scenario 1)
#' # True values: alpha = 1 (P), beta = 1 (X), no intercept
#' #------------------------------------------------------------------------
#' data(dataCopIMAContExo)
#' res <- copulaIMA(
#'   y ~ X + P | continuous(P),
#'   data = dataCopIMAContExo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res)
#'
#' #------------------------------------------------------------------------
#' # Example 2: Two endogenous regressors
#' # True values: alpha1 = 1 (P1), alpha2 = 1 (P2), beta = 1 (X), no intercept
#' # Extension of the first example
#' #------------------------------------------------------------------------
#' data("dataCopIMAMultiEndo")
#' res2 <- copulaIMA(
#'   y ~ X + P1 + P2 | continuous(P1) + continuous(P2),
#'   data      = dataCopIMAMultiEndo,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res2)
#'
#' @export
#' @importFrom stats coef
#' @importFrom utils txtProgressBar setTxtProgressBar
copulaIMA <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # Input checks  -------------------------------------------------------------------

  check_err_msg(checkinput_copulaIMA_formula(formula))
  check_err_msg(checkinput_copulaIMA_data(data))
  check_err_msg(checkinput_copulaIMA_dataVSformula(data = data, formula = formula))
  check_err_msg(checkinput_copulaIMA_numboots(num.boots))
  check_err_msg(checkinput_copulaIMA_verbose(verbose))
  check_err_msg(checkinput_copulaIMA_cdf(cdf))

  # Fit original data ---------------------------------------------------------------

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)
  f.main <- formula(F.formula, lhs = 1, rhs = 1)
  names.endo.regs <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  # Fitting with copula IMA model
  if (verbose) {
    message(
      "Fitting Haschka's copula-based IMA model for ",
      length(names.endo.regs),
      " continuous endogenous regressor(s)."
    )
  }
  fit <- copulaIMA_fit(
    f.main = f.main,
    data = data,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  )

  # Bootstrapping -------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    return(
      copulaIMA_fit(
        f.main = f.main,
        data = data.b,
        cdf = cdf,
        names.endo.regs = names.endo.regs
      )
    )
  }

  res.boots <- bootstrap_skip_degenerates(
    fn.fit = fn.fit.boots,
    data = data,
    num.boots = num.boots,
    coef.names = names(coef(fit)),
    verbose = verbose
  )

  # Return value ----------------------------------------------------------------------

  return(new_rendo_copula_ima(
    call = cl,
    F.formula = F.formula,
    names.endo.regs = names.endo.regs,
    cdf = cdf,
    res.lm = fit,
    boots = res.boots$boots.params,
    n.boots.attempted = res.boots$n.attempted,
    n.boots.failed = res.boots$n.failed
  ))
}
