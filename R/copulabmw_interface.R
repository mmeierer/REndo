#' Copula-based Endogeneity Correction with Asymptotic properties (BMW)
#'
#' @description
#' Fits a linear model with continuous endogenous regressors by using the nonparametric
#' control function approach of Breitung, Meyer and Wied (2024). This method
#' corrects endogeneity without any external instrumental variables. It is a
#' copula-based method with asymptotic theory. The method does not require a
#' Gaussian copula assumption on the joint distribution of the structural error
#' and the endogenous regressor error. The method also allows conditional
#' heteroskedasticity of the structural error.
#'
#' @templateVar data.addition Endogenous regressors must be \code{numeric}.
#' @template template_param_formuladataverbose
#' @templateVar resc.ecdf.addition The theoretical recommendation of Breitung et al. (2024), Eq 2.3.
#' @template template_param_cdf
#' @template template_param_numboots
#'
#' @details
#'
#' ## Model
#' Consider the structural regression with \eqn{K} endogenous regressors:
#'
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i' \beta + \varepsilon_i}
#'
#' where \eqn{i=1, \ldots, n} is the number of observations,
#' \eqn{Y_i} is the dependent variable,
#' \eqn{P_{i,k}} are the continuous endogenous regressors correlated with \eqn{\varepsilon_i}
#' \eqn{X_i} is a vector of exogenous regressors uncorrelated with \eqn{\varepsilon_i} and
#' \eqn{\mu, \alpha_k, \beta} are the structural model parameters.
#' The endogenous variables and the error can be decomposed as:
#' \deqn{P_{i,k} = \delta_k' X_i + e_{i,k}, \quad
#'       \varepsilon_i = \sum_{k=1}^{K} \rho_k f_k(e_{i,k}) + \xi_i}
#'
#' where \eqn{e_{i,k} \perp X_i}, \eqn{E[\xi_i | X_i, P_{i,k}] = 0},
#' and \eqn{f_k(\cdot)} is a strictly monotone nonlinear function such
#' that \eqn{f_k(e_{i,k}) \sim N(0,1)}.
#'
#' ## Methodology
#'
#' The estimator is done in two steps:
#' \enumerate{
#' \item First: Each endogenous regressor \eqn{P_{i,k}} is regressed on the
#' exogenous regressors \eqn{X_i} using ordinary least squares in the original
#' variable space to obtain residuals \eqn{\hat{e}_{i,k} = P_{i,k} - \hat{\delta}_k' X_i}.
#' \item Second: Augment the structural model with \eqn{P^*_1, \ldots, P^*_K} as generated
#' regressors and estimate using ordinary least squares. Obtain:
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i'\beta + \sum_{k=1}^{K} P^*_{i,k} \rho_k
#' + \xi_i}
#'  where \eqn{\rho_k} is the coefficient of the correction term \eqn{P^*_{i,k}}.
#'
#' }
#'
#' This method requires at least one exogenous regressor for the first-stage
#' regression and supports only continuous regressors.
#'
#' ## Important assumptions
#' The method requires the following assumptions (Assumption A, Breitung et al. 2024):
#' 1. The endogenous regressor has a linear decomposition
#' \eqn{P_k = \delta_k' X + e_k} where \eqn{e_k \perp X},
#' \eqn{E[e_k] = 0}, \eqn{V[e_k] = \sigma^2_e > 0}, and \eqn{E[e_k^4] < \infty}.
#'
#' 2. Conditions on the error:
#'
#'    -  (i) The error \eqn{e_k} has a differentiable CDF \eqn{F_e} that does not
#'       coincide with the normal distribution. This is the identification condition.
#'    -  (ii) The density \eqn{f_e} of \eqn{e} decays sufficiently fast at the tails.
#'       This is satisfied by distributions such as Gamma with shape parameter \eqn{\geq 2}
#'       or Chi-square with degrees of freedom \eqn{\geq 3}. It is a technical condition
#'       needed to control the estimation error of the normal scores from first-stage
#'       residuals.
#'
#'
#' @template template_text_details_cdfmethods
#'
#' @details
#' ## Parameter \code{formula}
#' The \code{formula} argument follows a two part notation separated by \code{|}.
#' The first part specifies the structural model (e.g \code{y ~ X + P}).
#' The second part identifies the continuous endogenous regressors:
#'
#' \preformatted{y ~ X + P | P                       # endogenous P}
#' \preformatted{y ~ X + P1 + log(P2) | P1 + log(P2) # multiple endogenous regressors}
#'
#' @template template_text_details_bootsdegenerates
#'
#' @references
#' Breitung, J., Meyer, M., Wied, D. (2024). Asymptotic properties of endogeneity
#' corrections using nonlinear transformations. \emph{The Econometrics Journal},
#' 27, 362--383. \doi{10.1093/ectj/utae002}
#'
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopula2stage_return(subclass = "rendo.copula.bmw")
#'
#' @family copula-based methods
#' @template template_seealso_copula2stage
#'
#' @seealso \code{\link[REndo:dataCopBMW]{dataCopBMW},
#' \link[REndo:dataCopBMWMultiEndo]{dataCopBMWMultiEndo}} for detailed
#' information about the simulated datasets
#'
#' @examples
#' \donttest{
#' #------------------------------------------------------------------------
#' # Example 1: Single endogenous regressor, correlated with
#' # exogenous regressor (Breitung, Meyer, Wied 2024, Section 4,
#' # Inspired by DGP1 with delta = 1 (correlation between x and P), rho = 0.9 (endogeneity),
#' # x ~ Gamma(1,1), e ~ lognormal(0,1) first stage error)
#' # n = 1000
#' # True Parameters: mu = 1 (intercept), beta = -1 (X), alpha = 1 (P).
#' #------------------------------------------------------------------------
#' data("dataCopBMW")
#' res_bmw <- copulaBMW(
#'   y ~ X + P | P,
#'   data      = dataCopBMW,
#'   cdf       = "ecdf",
#'   num.boots = 1000
#' )
#' summary(res_bmw)
#'
#' #------------------------------------------------------------------------
#' # Example 2: BMW with 2 endogenous regressors
#' # (Extension of DGP1 per Remark 2.1)
#' #
#' # e1, e2 ~ Gamma(3,2), DGP being closer to normal than Gamma(1,1)
#' # True parameters: mu = 1 (intercept), beta = -1 (X),
#' #              alpha1 = 1 (P1), alpha2 = 1 (P2).
#' # n = 5000
#' #------------------------------------------------------------------------
#' data("dataCopBMWMultiEndo")
#' res_bmw_multi <- copulaBMW(
#'   y ~ X + P1 + P2 | P1 + P2,
#'   data      = dataCopBMWMultiEndo,
#'   cdf       = "ecdf",
#'   num.boots = 1000
#' )
#' summary(res_bmw_multi)
#' }
#'
#' @md
#' @export
#'
#' @importFrom stats coef terms
#' @importFrom Formula as.Formula
copulaBMW <- function(
  formula,
  data,
  # Kimberly: resc.ecdf=rank(x)/(n+1) "as default as it is specifically mentioned in
  # the paper that they adopted this rescaled ecdf. The adj.ecdf comes later from the
  # JAMS method research paper"
  cdf = c("resc.ecdf", "adj.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # Input checks -----------------------------------------------------------------------
  allowed.cdfs <- c("resc.ecdf", "adj.ecdf", "ecdf", "kde")
  check_err_msg(checkinput_copula2stage_data_basics(data))

  # - >= 1 endogenous regressor
  # - >= 1 exogenous regressor (equation 2.2 & assumption A4)
  # - endogenous: numeric
  # - exogenous: any class
  check_err_msg(checkinput_copula2stage_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = "At least one exogenous regressor is required for the first-stage regression of each endogenous regressor P on X.",
    allow.interactions = TRUE,
    allow.constant.regressors = FALSE,
    warn.low.card.endo = TRUE,
    allowed.classes.endo = "numeric",
    allowed.classes.exo = NULL # allows any
  ))

  check_err_msg(checkinput_copula2stage_cdf(cdf = cdf, allowed.cdf = allowed.cdfs))
  check_err_msg(checkinput_copula2stage_numboots(num.boots))
  check_err_msg(checkinput_copula2stage_verbose(verbose))

  # Fit original data ------------------------------------------------------------------

  cdf <- match.arg(cdf, choices = allowed.cdfs)

  F.formula <- as.Formula(formula)
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  if (verbose) {
    message(
      "Fitting BMW copula model with ",
      length(labels.endo),
      " endogenous regressor(s)."
    )
  }

  fit <- copulabmw_fit(
    F.formula = F.formula,
    data = data,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    cdf = cdf
  )

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    fit.b <- copulabmw_fit(
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
    subclass = "rendo.copula.bmw"
  ))
}
