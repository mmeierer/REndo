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
#' endogenous regressors.
#'
#' @template template_param_formuladataverbose
#' @template template_param_numboots
#' @template template_param_cdf
#'
#' @details
#' \subsection{Model}{
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
#' }
#'
#' @template template_text_details_bootsdegenerates
#'
#' @details
#' \subsection{Formula interface}{
#'
#' The \code{formula} argument follows a two-part notation separated by
#' \code{|}. The first part specifies the structural model. The second part
#' identifies the continuous endogenous regressors using \code{continuous()}:
#'
#' \preformatted{y ~ X + P | continuous(P)                          # typical use: with intercept}
#' \preformatted{y ~ X + P1 + P2 | continuous(P1) + continuous(P2)  # two endogenous regressors}
#' \preformatted{y ~ X + P - 1 | continuous(P)                      # no intercept (in simulation settings)}
#'
#' In typical applied settings the model includes an intercept. The
#' no-intercept specification (\code{-1}) is primarily used in simulation
#' studies following the design of (Haschka (2025)  Section 4.1)
#' and is unlikely to be appropriate for real datasets.
#' }
#'
#'
#' @template template_references_haschka2025ima
#' @template template_references_parkgupta2012
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopulaima_return()
#'
#' @family copula-based methods
#'
#' @examples
#' #------------------------------------------------------------------------
#' # Example 1: Single endogenous regressor with continuous
#' # and normal exogenous regressor (Haschka 2025, Section 4.1 Scenario 1)
#' # True values: alpha = 1 (P), beta = 1 (X), no intercept
#' #------------------------------------------------------------------------
#' data(dataCopIMAContExo)
#' res <- copulaIMA(
#'   y ~ X + P - 1 | continuous(P),
#'   data = dataCopIMAContExo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res)
#'
#' #------------------------------------------------------------------------
#' # Example 2: Two endogenous regressors with intercept and no exogenous regressor
#' # True values: mu=10, alpha1 = 1 (P1), alpha2 = 1 (P2)
#' # Extension of the first example
#' #------------------------------------------------------------------------
#' data("dataCopIMAMultiEndo")
#' res2 <- copulaIMA(
#'   # Alternative: y ~ P1 + P2 | continuous(P1, P2)
#'   y ~ P1 + P2 | continuous(P1) + continuous(P2),
#'   data = dataCopIMAMultiEndo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res2)
#'
#' #------------------------------------------------------------------------
#' # Example 3: Single endogenous regressor with binary exogenous regressor
#' # (Haschka 2025, section 4.1 scenario 2)
#'
#' # This example shows one of the key example of IMA method, i.e., the exogenous
#' # regressor does not need to be continuous or normally distributed.
#' # X the exogenous regressor is binary (0 or 1).
#' # True values: alpha = 1 (P), beta = 1 (X), no intercept.
#' #------------------------------------------------------------------------
#' data("dataCopIMABinExo")
#' res3 <- copulaIMA(
#'    y ~ X + P - 1 | continuous(P),
#'    data = dataCopIMABinExo,
#'    cdf = "adj.ecdf",
#'    num.boots = 1000
#' )
#' summary(res3)
#'
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

  # Structural residuals --------------------------------------------------------------
  names.coefs.all <- names(coef(fit))
  names.coefs.cop <- grep("_cop$", names.coefs.all, value = TRUE)
  names.structural <- names.coefs.all[!names.coefs.all %in% names.coefs.cop]
  coefs.structural <- coef(fit)[names.structural]
  mm.structural <- model.matrix(fit)[, names.structural, drop = FALSE]

  fitted.values <- drop(mm.structural %*% coefs.structural)
  names(fitted.values) <- row.names(mm.structural)

  residuals <- drop(model.response(model.frame(fit)) - fitted.values)
  names(residuals) <- names(fitted.values)

  # Return value ----------------------------------------------------------------------

  return(new_rendo_copula_ima(
    call = cl,
    F.formula = F.formula,
    fitted.values = fitted.values,
    residuals = residuals,
    boots = res.boots$boots.params,
    n.boots.attempted = res.boots$n.attempted,
    n.boots.failed = res.boots$n.failed,
    res.lm.augmented = fit,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
