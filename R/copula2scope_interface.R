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
#' @templateVar data.addition Endogenous regressors must be \code{numeric}. Exogenous regressors may be \code{numeric}, \code{factor}, \code{character}, or \code{logical}, but not \code{ordered}.
#' @template template_param_formuladataverbose
#' @template template_param_cdf
#' @template template_param_numboots
#'
#' @details
#' ## Model
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
#' ## Methodology
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
#' @template template_text_details_cdfmethods
#'
#' @details
#' ## Parameter \code{formula}
#' The \code{formula} argument follows a two part notation separated by \code{|}.
#' The first part specifies the structural model (e.g. \code{y ~ X + P}).
#' The second part identifies the continuous endogenous regressors:
#'
#' \preformatted{y ~ X + P | P                       # one}
#' \preformatted{y ~ X + P1 + log(P2) | P1 + log(P2) # multiple endo}
#'
#' At least one exogenous regressor should be present in the model.
#' When no exogenous regressor is present, the correction terms reduce to
#' the normal scores of each endogenous regressor directly, equivalent to
#' the approach of Park and Gupta (2012).
#'
#' @template template_text_details_bootsdegenerates
#'
#' @template template_references_yang2025
#' @template template_references_parkgupta2012
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopula2stage_return(subclass = "rendo.copula.2scope")
#'
#' @family copula-based methods
#'
#' @seealso \code{\link[REndo:dataCopula2sCOPECase1]{dataCopula2sCOPECase1},
#' \link[REndo:dataCopula2sCOPECase2]{dataCopula2sCOPECase2},
#' \link[REndo:dataCopula2sCOPECase3]{dataCopula2sCOPECase3}} for detailed
#' information about the simulated datasets
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
#'   y ~ P + X | P,
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
#'   y ~ P + X | P,
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
#'   y ~ P + X | P,
#'   data      = dataCopula2sCOPECase3,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res3)
#' }
#'
#' @md
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

  # Input checks -----------------------------------------------------------------------
  allowed.cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")
  check_err_msg(checkinput_copula2stage_data_basics(data))

  # - >= 1 endogenous regressor
  # - >= 0 exogenous regressor
  # - no interactions
  # - endogenous: numeric
  # - exogenous: any
  check_err_msg(checkinput_copula2stage_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 0, # may have no exo regressor, resulting in warning
    allow.interactions = FALSE,
    allow.constant.regressors = FALSE,
    warn.low.card.endo = TRUE,
    allowed.classes.endo = "numeric",
    # forbid ordered factors
    allowed.classes.exo = c("numeric", "factor", "character", "logical")
  ))

  check_err_msg(checkinput_copula2stage_cdf(cdf, allowed.cdf = allowed.cdfs))
  check_err_msg(checkinput_copula2stage_numboots(num.boots))
  check_err_msg(checkinput_copula2stage_verbose(verbose))

  # Fit original data ------------------------------------------------------------------

  cdf <- match.arg(cdf, choices = allowed.cdfs)

  F.formula <- as.Formula(formula)
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  # Warning the user to use copulaCorrection() (Park and Gupta 2012)
  # if there is no exo regressors. 2sCOPE method is designed for correlated
  # endo and exo regressors. Without an exo regressor, the correction term
  # is just equivalent to the copulaCorrection() approach

  if (length(labels.exo) == 0) {
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

  if (verbose) {
    message(
      "Fitting 2sCOPE model with ",
      length(labels.endo),
      " endogenous regressors."
    )
  }
  fit <- copula2scope_fit(
    F.formula = F.formula,
    data = data,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    cdf = cdf
  )

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    fit.b <- copula2scope_fit(
      F.formula = F.formula,
      data = data.b,
      labels.endo = labels.endo,
      labels.exo = labels.exo,
      cdf = cdf
    )
    return(fit.b$res.lm.augmented)
  }

  res.boots <- bootstrap_skip_degenerates(
    fn.fit = fn.fit.boots,
    data = data,
    num.boots = num.boots,
    coef.names = names(coef(fit$res.lm.augmented)),
    verbose = verbose
  )

  # Return object ----------------------------------------------------------------------

  return(new_rendo_copula2stage(
    call = cl,
    F.formula = F.formula,
    res.lm.augmented = fit$res.lm.augmented,
    res.boots = res.boots,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = fit$labels.pcop,
    subclass = "rendo.copula.2scope"
  ))
}
