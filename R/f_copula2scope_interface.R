#' Two-Stage Copula Generated Regressor Approach (2sCOPE)
#'
#' @description
#' Fitting the two-stage copula generator regressor approach (2sCOPE) estimator of
#' Yang et al. (2025) to address endogeneity without any external instruments. This method
#' generalises the Park and Gupta (2012) copula correction method to allow for correlation
#' between endogenous and exogenous regressors, while also relaxing the required assumption that
#' endogenous regressors need to be non-normally distributed.
#'
#' Compared to Park and Gupta (2012), the 2sCOPE method does not require the endogenous and exogenous
#' regressors to be independent. Moreover, 2sCOPE remains consistent when
#' endogenous regressors are both normally or near-normally distributed, given
#' that at least one exogenous regressor is non-normally distributed. In that case, the identification
#' would then come from the non-normal exogenous regressor instead of the endogenous regressor itself.
#'
#' @template template_param_formuladataverbose
#' @template template_param_cdf
#' @template template_param_numboots
#'
#' @details
#' \strong{Model}
#'
#' Consider the structural regression model with \eqn{K} endogenous regressors:
#'
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i' \beta + \varepsilon_i}
#'
#' where \eqn{i = 1, \ldots, n} is the number of observations,
#' \eqn{Y_i} is the dependent variable,
#' \eqn{P_{i,k}} are continuous endogenous regressors that may be correlated with both the
#' structural error \eqn{\varepsilon_i} and the exogenous regressors \eqn{X_i},
#' \eqn{X_i} is a vector of exogenous regressors uncorrelated with \eqn{\varepsilon_i}, and
#' \eqn{\mu, \alpha_k, \beta} are the structural model parameters.
#'
#' \strong{Methodology}
#'
#' 2sCOPE is an augmented OLS estimator which can be described in the following 3 steps
#' (Yang et al., 2025):
#'
#' \enumerate{
#'   \item For every explanatory variable \eqn{W \in \{X_1, \ldots, X_J,
#'         P_1, \ldots, P_K\}}, compute the normal score
#'         \eqn{W^* = \Phi^{-1}(\hat{F}_W(W))}, where \eqn{\hat{F}_W} is
#'         the estimated marginal CDF of \eqn{W} and \eqn{\Phi^{-1}} is the
#'         standard normal quantile function.
#'   \item For each endogenous regressor \eqn{P_k^*}, regress it on the normal
#'         scores of the exogenous regressors \eqn{X_1^*, \ldots, X_J^*}
#'         with intercept and keep the residuals
#'         \eqn{\hat{\varepsilon}_k} as the copula correction terms. This
#'         first-stage regression exploits the informational content of the
#'         exogenous regressors in a similar way to instrumental variable
#'         identification.
#'   \item Augment the structural model with \eqn{\hat{\varepsilon}_1, \ldots,
#'         \hat{\varepsilon}_K} as additional regressors and estimate the following using OLS:
#'         \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i' \beta
#'               + \sum_{k=1}^{K} \hat{\varepsilon}_{i,k} \gamma_k + \xi_i}
#'         where \eqn{\gamma_k} is the coefficient of the copula correction
#'         term \eqn{\hat{\varepsilon}_{i,k}} and \eqn{\xi_i} is the new
#'         error term.
#' }
#'
#' @template template_text_details_bootsdegenerates
#'
#' @details
#' \strong{Formula interface}
#'
#' The \code{formula} argument follows a two-part notation separated by
#' \code{|}. The first part specifies the structural model. The second part
#' identifies the continuous endogenous regressors using \code{continuous()}:
#'
#' \preformatted{y ~ X + P | continuous(P)                          # one endo}
#' \preformatted{y ~ X + P1 + P2 | continuous(P1) + continuous(P2)  # two endo}
#'
#' At least one exogenous regressor should be present in the model.
#' When no exogenous regressor is present, the correction terms reduce to
#' the normal scores of each endogenous regressor directly, equivalent to
#' the approach of Park and Gupta (2012).
#'
#'
#' @template template_references_yang2025
#' @template template_references_parkgupta2012
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopula2scope_return()
#'
#' @family copula-based methods
#'
#' @examples
#' # -------------------------------------------------------------------
#' # Example 1: Non-normal endogenous and exogenous regressors
#' # (Yang et al. 2025, Table 5, Case 1, rho_px = 0.5)
#' #
#' # Demonstrates 2sCOPE's core contribution: consistent estimation when
#' # the endogenous regressor P and exogenous regressor X
#' # are correlated.
#' # True values: mu = 1, alpha = 1 (P), beta = -1 (X).
#' # -------------------------------------------------------------------
#' data("dataCopula2sCOPECase1")
#' res1 <- copula2sCOPE(
#'   y ~ P + X | continuous(P),
#'   data      = dataCopula2sCOPECase1,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res1)
#'
#' \donttest{
#' # -------------------------------------------------------------------
#' # Example 2: Non-normal endogenous P but normally distributed exogenous X
#' # (Yang et al. 2025, case 2)
#' #
#' # To demonstrate that 2sCOPE remains consistent when the exogenous
#' # regressor X is normally distributed.
#' # True values: mu = 1, alpha = 1 (P), beta = -1 (X).
#' # -------------------------------------------------------------------
#' data("dataCopula2sCOPECase2")
#' res2 <- copula2sCOPE(
#'   y ~ P + X | continuous(P),
#'   data      = dataCopula2sCOPECase2,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res2)
#'
#' # -------------------------------------------------------------------
#' # Example 3: Normally distributed endogenous P, nonnormal exogenous X
#' # (Yang et al. 2025, case 3)
#' #
#' # This example is used to demonstrate that one of 2sCOPE's advantage
#' # is that there is a consistent estimation even when
#' # the endogenous regressor P is normally distributed, because
#' # identification comes from the nonnormal exogenous regressor X.
#' # True values: mu = 1, alpha = 1 (P), beta = -1 (X).
#' # -------------------------------------------------------------------
#' data("dataCopula2sCOPECase3")
#' res3 <- copula2sCOPE(
#'   y ~ P + X | continuous(P),
#'   data      = dataCopula2sCOPECase3,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res3)
#' }
#'
#' @export
#' @importFrom stats coef terms
#' @importFrom Formula as.Formula
copula2sCOPE <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copulashared_formula(formula))
  check_err_msg(checkinput_copulashared_data(data))
  check_err_msg(checkinput_copulashared_dataVSformula(data = data, formula = formula))
  check_err_msg(checkinput_copulashared_cdf(
    cdf,
    allowed.cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde")
  ))
  check_err_msg(checkinput_copulashared_numboots(num.boots))
  check_err_msg(checkinput_copulashared_verbose(verbose))

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)
  names.endo.regs <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  # Warning the user to use copulaCorrection() (Park and Gupta 2012)
  # if there is no exo regressors. 2sCOPE method is designed for correlated
  # endo and exo regressors. Without an exo regressor, the correction term
  # is just equivalent to the copulaCorrection() approach

  # Use labels(terms()) to also trigger when exogenous regs are transformed (like ~a+log(b) | log(b))
  # because names.endo.regs="log(b)" but all.vars(formula)="b"
  rhs1.vars <- labels(terms(F.formula, rhs = 1, lhs = 0))
  exo.vars <- rhs1.vars[!rhs1.vars %in% names.endo.regs]

  if (length(exo.vars) == 0) {
    warning(
      "No exogenous regressors found. The 2sCOPE method is designed for ",
      "settings with correlated endogenous and exogenous regressors. ",
      "Without exogenous regressors the correction terms reduce to the ",
      "normal scores of each endogenous regressor, equivalent to the ",
      "copulaCorrection() approach of Park and Gupta (2012). ",
      "Consider using copulaCorrection() instead.",
      call. = FALSE
    )
  }

  # Fitting with 2sCOPE
  if (verbose) {
    message(
      "Fitting 2sCOPE model with ",
      length(names.endo.regs),
      " endogenous regressors."
    )
  }
  fit <- copula2sCOPE_fit(F.formula = F.formula, data = data, cdf = cdf)

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    return(copula2sCOPE_fit(F.formula = F.formula, data = data.b, cdf = cdf))
  }

  res.boots <- bootstrap_skip_degenerates(
    fn.fit = fn.fit.boots,
    data = data,
    num.boots = num.boots,
    coef.names = names(coef(fit)),
    verbose = verbose
  )

  # Structural residuals --------------------------------------------------------------

  l.fitted.resid <- copula_compute_structural_fitted_residuals(
    res.lm.aug = fit,
    names.aux.regs = grep("_cop$", names(coef(fit)), value = TRUE)
  )

  # Return object ----------------------------------------------------------------------

  return(new_rendo_copula2sCOPE(
    call = cl,
    F.formula = F.formula,
    res.lm.augmented = fit,
    fitted.values = l.fitted.resid$fitted.values,
    residuals = l.fitted.resid$residuals,
    boots.params = res.boots$boots.params,
    n.boots.attempted = res.boots$n.attempted,
    n.boots.failed = res.boots$n.failed,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
