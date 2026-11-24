#' Adjusted Gaussian Copula Estimator (JAMS)
#'
#' @description Fitting adjusted Gaussian copula estimator to address endogeneity
#' without external instruments. This method shows analytically that the quality of
#' CDF estimator is the dominant factor of finite-sample bias in the Gaussian copula
#' approach. Hence, an adjusted ECDF estimator \code{adj.ecdf} was introduced to
#' reduce bias in models with an intercept. The method also extends the copula
#' framework to handle multiple continuous endogenous regressors, both contiuous and
#' discrete exogenous regressors, nonlinearities and interaction terms and
#' a copula structure that can change across categories of discrete exogenous variables.
#'
#' The method handles discrete or categorical exogenous regressors \eqn{Z_i} by
#' stratifying the data into subsets by \eqn{Z_i} level and then estimating the
#' copula correction terms separately within each subset.
#'
#' @template template_param_formuladataverbose
#' @template template_param_numboots
#' @template template_param_cdf
#'
#' @details
#'
#' ## Model
#'
#' let's consider the following structural regression model (Liengaard et al. 2024)
#' \deqn{Y_i = g(P_i, W_i, Z_i) + \varepsilon_i}
#'
#' where
#' \eqn{P_i \in \mathbb{R}^{d_P}} are continuous endogenous
#' regressors
#' \eqn{W_i \in \mathbb{R}^{d_W}} are continuous exogenous
#' regressors
#' \eqn{Z_i} are discrete exogenous regressors, and
#' \eqn{g(\cdot)} is the structural function which may include
#' interactions and nonlinear transformations:
#'
#' \deqn{g(P_i, W_i, Z_i) = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k +
#'       W_i' \beta_W + Z_i' \beta_Z + \text{interactions}}
#'
#' where
#' \eqn{i = 1, \ldots, n} indexes observations
#' \eqn{Y_i} is the dependent variable
#' \eqn{P_{i,k}} are continuous endogenous regressors correlated with
#' \eqn{\varepsilon_i}
#' \eqn{W_i} is a vector of continuous exogenous regressors,
#' \eqn{Z_i} is a vector of discrete exogenous regressors, and
#' \eqn{\mu, \alpha_k, \beta_W, \beta_Z} are the structural model
#' parameters.
#'
#' ## Methodology
#' The JAMS method is an augmented ordinary least-squares estimator in three steps:
#'
#' 1. First step: For each value \eqn{z} of the discrete exogenous
#'    \eqn{Z_i} (or unconditionally if no \eqn{Z_i} is present), the CDF estimator
#'    is applied to all continuous regressors \eqn{P_i} and \eqn{W_i}
#'    to obtain normal scores: \deqn{C(P_i) = \Phi^{-1}(\hat{F}_P(P_i)),
#'    \quad C(W_i) = \Phi^{-1}(\hat{F}_W(W_i))}
#'
#' 2. Second step: The copula correction terms are constructed through the
#'    inverse variance-covariance matrix transformation:
#'    \deqn{\hat{C}^z(P_i, W_i) = (\hat{C}^z(P_i)', \hat{C}^z(W_i)')
#'       \hat{\Sigma}^{-1}_{\hat{C}^z(P), \hat{C}^z(W)}
#'       \begin{pmatrix} I_{d_P} \\ 0_{d_W \times d_P} \end{pmatrix}}
#'    where
#'    \eqn{\hat{\Sigma}} is the estimated variance-covariance matrix
#'    of the normal scores \eqn{(\hat{C}(P), \hat{C}(W))} and the projection
#'    \eqn{(I_{d_P}, 0_{d_W \times d_P})'} only keeps the \eqn{d_P} columns
#'    corresponding to the endogenous regressors which provides  one correction term
#'    per endogenous regressor.
#'    If there is discrete \eqn{Z_i}, it is estimated separately within each subset
#'    where \eqn{Z_i = z}.
#'
#' 3. Third step: The structural model is then augmented with the estimated
#'    correction tersm and estimated using the ordinary least-squares.
#'    If there is \eqn{Z_i} the equation is:
#'    \deqn{Y_i = g(P_i, W_i, Z_i) + \sum_{z=z_1}^{z_J} \sum_{k=1}^{d_P}
#'       \gamma_k^z \hat{C}_k^z(P_i, W_i) \mathbf{1}(Z_i = z) + u_i}
#'    If there is no \eqn{Z_i}:
#'    \deqn{Y_i = g(P_i, W_i) + \sum_{k=1}^{d_P} \gamma_k \hat{C}_k(P_i, W_i) + u_i}
#'
#' ## Note on the choice of the \code{cdf}
#' The \code{adj.ecdf} is the recommended default of Liengaard et al. (2024). It
#' helps to reduce finite-sample bias, especially in models with an intercept.
#'
#' @template template_text_details_cdfmethods
#'
#' @details
#' ## Formula interface
#' The \code{formula} argument follows a two part notation separated by \code{|}.
#' The first part specifies the structural model (e.g \code{y ~ X + P}).
#' The second part identifies the continuous endogenous regressors:
#'
#' \preformatted{y ~ X + P | P                          # one endo}
#' \preformatted{y ~ X + P1 + log(P2) | P1 + log(P2)    # two endo}
#' \preformatted{y ~ W + Z + P + P:W | P                # interaction}
#'
#' Continuous exogenous regressors \eqn{W} enter the variance-covariance matrix
#' computation. The discrete exogenous regressors \eqn{Z} activate stratification
#' and must be of class \code{factor}. If  \eqn{Z} is numeric, it first needs to be
#' converted to factor: \code{data$Z <- as.factor(data$Z)}.
#'
#' @template template_text_details_bootsdegenerates
#'
#' @references
#' Liengaard, B. D., Becker, J.-M., Bennedsen, M., Heiler, P., Taylor, L. N.,
#' and Ringle, C. M. (2025). Dealing with regression models' endogeneity by means
#' of an adjusted estimator for the Gaussian copula approach.
#' \emph{Journal of the Academy of Marketing Science}, 53, 279--299.
#'  \doi{10.1007/s11747-024-01055-4}
#'
#' @template template_param_cdf_references
#'
#' @eval doc_rendocopula2stage_return(subclass = "rendo.copula.jams", extra.items = doc_rendocopulajams_return_extra_items())
#'
#' @family copula-based methods
#'
#' @examples
#'
#' #------------------------------------------------------------------------
#' # Example 1: Two endogenous regressors, continuous and binary exogenous,
#' # interactions, and varying copula structure by Z
#' # (Liengaard et al. 2024, Simulation Study 2)
#' #
#' # This example shows the following:
#' # 1) Multiple correlated endogenous regressors P1, P2
#' # 2) Continuous exogenous W enters variance-covariance matrix (eq. 17)
#' # 3) Binary exogenous Z triggers stratification (eq. 20-21)
#' # 4) Interaction terms P1:P2, P1:W, P2:Z in structural model (eq. 16)
#' # 5) Copula structure varies by Z: rho=0.4 (Z=0), rho=0.6 (Z=1)
#' #
#' # True values: mu=1, alpha1=-1 (P1), alpha2=1 (P2),
#' #              beta1=-1 (W), beta2=-2 (Z),
#' #              delta1=-1 (P1:P2), delta2=-1 (P1:W), delta3=-2 (P2:Z).
#' #------------------------------------------------------------------------
#' data("dataCopJAMS")
#' dat        <- dataCopJAMS
#' dat$Z      <- as.factor(dat$Z)  # Z must be factor for stratification
#'
#' res <- copulaJAMS(
#'   y ~ P1 + P2 + W + Z + P1:P2 + P1:W + P2:Z | P1 + P2,
#'   data      = dat,
#'   cdf       = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res)
#'
#'
#' @md
#'
#' @export
#' @importFrom stats coef
copulaJAMS <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  #Input checks
  allowed.cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")
  check_err_msg(checkinput_copula2stage_data_basics(data))

  # - >= 1 endogenous regressor
  # - >= 1 exogenous regressor
  # - endogenous: numeric
  # - exogenous: any class (factors use stratification)
  # - interactions allowed
  check_err_msg(checkinput_copula2stage_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = "At least one exogenous regressor is required.",
    allow.interactions = TRUE,
    allow.constant.regressors = FALSE,
    warn.low.card.endo = TRUE,
    allowed.classes.endo = "numeric",
    allowed.classes.exo = NULL # allows any
  ))

  check_err_msg(checkinput_copula2stage_cdf(cdf = cdf, allowed.cdf = allowed.cdfs))
  check_err_msg(checkinput_copula2stage_numboots(num.boots))
  check_err_msg(checkinput_copula2stage_verbose(verbose))

  cdf <- match.arg(cdf, choices = allowed.cdfs)
  F.formula <- as.Formula(formula)
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  #fitting the original data
  if (verbose) {
    message(
      "Fitting JAMS copula model for ",
      length(labels.endo),
      " continuous endogenous regressor(s)."
    )
  }

  fit <- copulajams_fit(
    F.formula = F.formula,
    data = data,
    cdf = cdf,
    verbose = verbose
  )

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    fit.b <- copulajams_fit(
      F.formula = F.formula,
      data = data.b,
      cdf = cdf,
      verbose = FALSE
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

  # Return object ----------------------------------------------------------------------

  return(new_rendo_copula2stage(
    call = cl,
    F.formula = F.formula,
    res.lm.augmented = fit$res.augmented,
    res.boots = res.boots,
    cdf = cdf,
    labels.endo = fit$labels.endo,
    labels.exo = fit$labels.exo,
    labels.pcop = fit$labels.pcop,
    subclass = "rendo.copula.jams",
    # JAMS-specific
    P = fit$P,
    W = fit$W,
    df.Z = fit$df.Z
  ))
}


doc_rendocopulajams_return_extra_items <- function() {
  return(c(
    P = "\\item{\\code{P}}{The matrix of (continuous) endogenous regressors.}",
    W = "\\item{\\code{W}}{The matrix of continuous exogenous regressors. Only main effects: Discrete (\\code{factor}) regressors and interaction terms are excluded.}",
    df.Z = "\\item{\\code{df.Z}}{\\code{data.frame} of discrete (\\code{factor}) exogenous regressors used to stratify. \\code{NULL} if there are none.}"
  ))
}
