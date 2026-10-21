#' Two-Stage Nonparametric Copula Control Function Approach (2sCOPEnp)
#'
#' @description
#' Fitting the two-stage nonparametric copula control function (2sCOPEnp)
#' estimator of Hu et al. (2025) to address endogeneity without external
#' instruments.
#'
#' The 2sCOPEnp method generalises existing copula correction methods by
#' replacing the marginal CDF transformation with a nonparametric kernel
#' estimate of the \emph{conditional} CDF of each endogenous regressor given
#' the exogenous regressors, \eqn{\hat{F}(P_k | X)}. This eliminates the need
#' for a first-stage regression (as in \code{\link{copula2sCOPE}}) and relaxes
#' the Gaussian copula assumption on the regressor-error dependence structure.
#' 2sCOPEnp also supports \emph{discrete} endogenous regressors by leveraging
#' the exogenous control variables to smooth the discrete conditional CDF.
#'
#' When the endogenous regressor \eqn{P} is independent of all exogenous regressors \eqn{X},
#' the conditional CDF collapses to the marginal CDF, \eqn{\hat{F})(P|X) = \hat{F}(P)} and the
#' method then reduces to the Park and Gupta (2012) copula correction.
#'
#' @templateVar data.addition Variables classes must be set correctly before calling the function. Endogenous regressors must be \code{numeric} or \code{ordered}. Exogenous regressors must be \code{numeric}, \code{factor}, or \code{ordered}.
#' @template template_param_formuladataverbose
#' @template template_param_numboots
#' @param npcdistbw.args A named list of arguments which are passed to
#' \code{\link[np]{npcdistbw}}. To tweak the bandwidth selection for the kernel
#' conditional CDF. Runs with the method's defaults if not specified.
#' See \code{\link[np]{npcdistbw}} for valid inputs.
#' @param bws A named list of estimated \code{condbandwidth} objects (outputs of
#' \code{np::npcdistbw()}) used as starting points for estimating the bandwidth
#' from \code{data}. See Details.
#'
#'
#' @details
#'
#' ## Model
#'
#' Consider the structural regression model with \eqn{K} endogenous regressors:
#'
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k} \alpha_k + X_i' \beta + \varepsilon_i}
#'
#' where \eqn{i=1, \ldots, n } indexes observations,
#' \eqn{Y_i} is the dependent variable,
#' \eqn{P_{i,k}} are endogenous regressors (continuous or discrete) that may be correlated
#' with the structural error \eqn{\varepsilon_i},
#' \eqn{X_i} is a vector of exogenous regressors uncorrelated with \eqn{\varepsilon_i}, and
#' \eqn{\mu, \alpha_k, \beta} are structural model paramters.
#'
#'
#' ## Methodology
#'
#' The estimation proceeds in 2 stages (Hu et al. 2025)
#'
#' \emph{Stage 1}: For each endogenous regressor \eqn{P_k}:
#' \enumerate{
#'   \item Estimate the conditional CDF \eqn{\hat{F}(P_k | X)} nonparametrically
#'         using kernel methods (Li and Racine 2008), conditioning on all
#'         exogenous regressors \eqn{X}. A generalised product kernel handles
#'         mixed continuous and discrete \eqn{X} automatically (Hayfield and Racine 2008).
#'   \item Compute the copula correction term
#'         \eqn{C_{i,pk} = \Phi^{-1}(\hat{F}(P_{i,k} | X_i))}
#'         (Equation 21, Hu et al. 2025) where \eqn{\Phi^{-1}} is the
#'         standard normal quantile function.
#'   \item Repeat for all \eqn{K} endogenous regressors.
#' }
#'
#' \emph{Stage 2}: Add \eqn{\{C_{i,pk}\}, k =1, \ldots, K} to the structural model as
#' generated regressors and estimate using OLS (Equation 20):
#' \deqn{Y_i = \mu + \sum_{k=1}^{K} P_{i,k}\alpha_k + X_i'\beta +
#'       \sum_{k=1}^{K} C_{i,pk}\gamma_k + \xi_i}
#' where \eqn{\gamma_k} is the coefficient of the copula correction term \eqn{C_{i, pk}} and
#' \eqn{\xi_i} is the new error term.
#'
#' Note that small but significant quadratic (\code{P.Q}) or cubic (\code{P.C}) coefficients
#' may appear for discrete ordered regressors under strong endogeneity. This is expected
#' of the augmented regression separating structural coefficients from the correction
#' term when the correction term takes few distinct values (one per ordered level).
#' It does not indicate misspecification.
#'
#' ## Bandwidth Selection
#'
#' The accuracy of the copula correction terms depend on the quality of the nonparametric
#' conditional CDF estimate, which depends on the choice of the bandwidth. If a bandwidth is
#' too small, this could lead to overfitting of the conditional CDF. Likewise, if it is too large,
#' this could result in oversmoothing and introducing bias, leading to underfitting (Hu et al. 2025).
#' However, no closed-form rule-of-thumb bandwidth exists for conditional CDF estimator because these rules
#' assume a known marginal distribution, yet this is an assumption that the 2sCOPEnp method relaxes. The default
#' bandwidth is therefore selected through least-squares cross-validation (Li and Racine 2013) which is
#' data-driven and distribution-free.
#'
#' The bandwidth is estimated using \link[np:npcdistbw]{\code{np::npcdistbw()}} with its
#' default settings. To customize this, you may pass your own arguments via
#' \code{npcdistbw.args}. Together with the \code{bws} parameter, this can be used to
#' iteratively refine the bandwidth. See examples.
#'
#' ## Parameter \code{bws}
#'
#' If given, the items in \code{bws} will serve as the starting points for the bandwidth
#' estimation of the respective endogenous regressor, by passing them into
#' \code{np::npcdistbw(bws=bws[[p]])}. This allows to iteratively refine the bandwidth
#' search by passing the result of a previous fit. See examples.
#'
#' For each endogenous term, \code{bws} must contain a
#' \code{condbandwidth} object for the conditional CDF \code{(single endo) ~ (all exo)}.
#' \code{bws} must be a named list, with each each element named after the corresponding
#' endogenous terms.
#' The bandwidths must be estimated using the default \code{np::npcdistbw(xdat=,ydat=)}
#' interface, not the formula interface because this will break downstream usage in
#' \code{np::npcdist()}.
#'
#' ## Parameter \code{formula}
#'
#' The \code{formula} argument follows a two part notation separated by \code{|}.
#' The first part specifies the structural model (e.g \code{y ~ X + P}).
#' The second part lists the endogenous regressors:
#'
#' \preformatted{y ~ X + P | P                       # endogenous P}
#' \preformatted{y ~ X + P1 + log(P2) | P1 + log(P2) # multiple endogenous regressors}
#'
#' At least one exogenous regressor must be present in the model for the
#' nonparametric conditional CDF estimation to be feasible.
#' The formula may not contain interaction term (\code{A:B}) in either part because these
#' are only expanded by \code{model.matrix()} but the method uses \code{model.frame()}
#' output to preserve factor structure for the kernel.
#'
#' @template template_text_details_bootsdegenerates
#'
#' @details
#' Note that the bandwidth is fit on the full \code{data} and then re-used across
#' bootstrap samples. This substantially lowers the computation time by avoiding
#' repeated cross-validation for the selection of the bandwidth for each bootstrap sample.
#' By fixing the bandwidth, the impact on inference for large samples is expected to be
#' low because the conditional CDF estimator enters the procedure as an auxiliary
#' nonparametric component. The resulting bootstrap distribution should be interpreted
#' as conditional on the estimated bandwidths.
#'
#' If the bootstrap standard errors of the endogenous regressor coefficients are more
#' than 6 times larger than the corresponding OLS standard errors, this may indicate
#' near-multicollinearity between the copula correction term and the original regressors,
#' which may suggest a potential identification issue. (Hu et al. 2025, section 3.5)
#'
#' @template template_references_hu2025
#'
#' @references
#' Li, Q. and Racine, J. S. (2008). Nonparametric estimation of conditional CDF and
#' quantile functions with mixed categorical and continuous data.
#' \emph{Journal of Business and Economic Statistics}, 26(4), 423-434
#'
#' Li, Q., Lin, J. and Racine, J. S. (2013).  Optimal Bandwidth Selection
#' for Nonparametric Conditional Distribution and Quantile Functions.
#' \emph{Journal of Business and Economic Statistics}, 31(1), 57-65
#' \doi{10.1080/07350015.2012.738955}
#'
#' Hayfield, T. and Racine, J. S. (2008). Nonparametric econometrics: The np package.
#' \emph{Journal of Statistical Software}, 27(5).
#' \doi{10.18637/jss.v027.i05}
#'
#' @eval doc_rendocopula2stage_return(subclass = "rendo.copula.2scope.np", extra.items = doc_rendocopula2scopenp_return_extra_items())
#'
#' @family copula-based methods
#' @seealso \code{\link[REndo:summary.rendo.copula.2scope.np]{summary}}
#' @seealso \code{\link[np:npcdistbw]{npcdistbw}} for possible elements of parameter
#'  \code{npcdistbw.args}.
#' @seealso \code{\link[REndo:dataCopula2sCOPEnpCont]{dataCopula2sCOPEnpCont},
#' \link[REndo:dataCopula2sCOPEnpBi]{dataCopula2sCOPEnpBi},
#' \link[REndo:dataCopula2sCOPEnpMulti]{dataCopula2sCOPEnpMulti}} for detailed
#' information about the simulated datasets
#'
#' @examples
#'
#' \dontrun{
#' # # Set a random number seed because NP procedure is random
#' # set.seed(42)
#'
#' #--------------------------------------------------------------
#' # Example 1: Continuous endogenous regressor
#' # (Hu et al. 2025, Section 4.3)
#' #
#' # Demonstrates 2sCOPEnp's unique advantage: It is the only method
#' # that fully eliminates bias when neither the Gaussian copula nor
#' # the mean-dependence assumption holds.
#' # True values: mu = 1, alpha = 1 (P), beta = 2 (X).
#' #--------------------------------------------------------------
#' data("dataCopula2sCOPEnpCont")
#' res1 <- copula2sCOPEnp(
#'  y ~ P + X | P,
#'   data = dataCopula2sCOPEnpCont)
#'
#' #--------------------------------------------------------------
#' # Example 2: Binary endogenous regressor
#' # (Hu et al. 2025, Section 4.5)
#' #
#' # To show 2sCOPEnp's ability to handle discrete
#' # endogenous regressors. 2sCOPEnp corrects endogeneity via
#' # the smooth conditional CDF.
#' # True values: mu = 0, alpha = 1 (P), beta = 2 (X).
#' #--------------------------------------------------------------
#' data("dataCopula2sCOPEnpBi")
#' res2 <- copula2sCOPEnp(
#'   y ~ P + X | P,
#'   data = dataCopula2sCOPEnpBi
#' )
#' summary(res2)
#'
#' #--------------------------------------------------------------
#' # Example 3: Multiple endogenous regressors with mixed variables.
#'
#' # To show the extension of 2sCOPEnp with a mixed types of variables
#' #P1 is continuous lognormal endogenous correlated with X1
#' #P2 is ordered factor endogenous (4 levels) correlated with X2
#' #X3 is ordered factor exogenous (3 levels)
#'
#' # True values: mu = 1, alpha1 = -1 (P1), alpha2 = 1 (P2),
#' #              beta1 = 2 (X1), beta2 = 0.5, beta3 = 1 (X3)
#' # note that P2.Q may have small significant coefficient. This is expected
#' #when there is stronger endogeneity with discreted ordered regressors.
#'
#' #--------------------------------------------------------------
#'
#' data("dataCopula2sCOPEnpMulti")
#' res3 <- copula2sCOPEnp(
#'   y ~ P1 + P2 + X1 + X2 + X3 | P1 + P2,
#'   data = dataCopula2sCOPEnpMulti
#' )
#' summary(res3)
#'
#'
#'
#' #--------------------------------------------------------------
#' # How to tweak the bandwidth selection for the kernel estimate
#' # (parameter npcdistbw.args)
#' #
#' # For a good one-shot fit, we adapt the parameters for the
#' # bandwidth selection to prioritize reliability
#' #   nmulti: Number of restarts at random points. High to
#' #           protect from local minima.
#' #   bwmethod: Bandwidth selection method. Cross-validation ("cv.ls")
#' #           is already default over rule-of-thumb ("normal-reference").
#' #   itmax: Max iterations before failing numerical optimization.
#' #          High to prevent silent early stop without convergence.
#' #   ftol,tol: Do not override defaults.
#' #
#' #--------------------------------------------------------------
#' res4 <- copula2sCOPEnp(
#'  y ~ P + X | P,
#'  npcdistbw.args = list(
#'   nmulti = 25,
#'   itmax = 50000
#'  ),
#'  data = dataCopula2sCOPEnpCont)
#'
#'
#' #--------------------------------------------------------------
#' # Tweak the bandwidth selection over multiple iterations
#' #
#' # This allows us to first search broadly using a cheap method
#' # and then further re-fine with a more comprehensive method.
#'
#'
#' # First: Explore (search loosely)
#' res.loosely <- copula2sCOPEnp(
#'  y ~ X + P | P,
#'  npcdistbw.args = list(
#'    # alternative: Directly calculate 'rule-of-thumb’ bandwidth.
#'    # Performs no search (other params irrelevant)
#'    # bwmethod = "normal-reference",
#'
#'    # depending on data:
#'    #  Get close quickly (low 'nmulti')
#'    #  Avoid local minimas (high 'nmulti')
#'    # np::npcdistbw() still recommends multiple restarts
#'    # for exploratory search
#'    nmulti = 3,
#'
#'    # accept convergence faster
#'    ftol=.01,
#'    tol=.01
#'  ),
#'  data = dataCopula2sCOPEnpCont,
#'  num.boots = 2
#' )
#'
#' # Inspect diagnostics....
#'
#' # Final: Refine
#' # Pass bandwidths from previous run as starting points
#' res.final <- copula2sCOPEnp(
#'  y ~ X + P | P,
#'  # pass bandwidth from previous fit
#'  bws = res.loosely$bws,
#'  npcdistbw.args = list(
#'    # leave default bwmethod (cross-validation)
#'    # leave default convergence tolerances
#'
#'    # low as starting close to optimum
#'    nmulti = 2
#'  ),
#'  data = dataCopula2sCOPEnpCont
#' )
#'
#' #--------------------------------------------------------------
#'
#' }
#'
#' @md
#' @export
#'
#' @importFrom Formula as.Formula
#' @importFrom stats coef terms formula
copula2sCOPEnp <- function(
  formula,
  data,
  npcdistbw.args = list(
    #to communicate to the user that bw estimation uses CV by default
    bwmethod='cv.ls'
  ),
  bws = NULL,
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # Input checks -----------------------------------------------------------------------
  check_err_msg(checkinput_copula2stage_data_basics(data))

  # - >= 1 endogenous regressor
  # - >= 1 exogenous regressor
  # - endogenous: numeric, ordered
  # - exogenous: numeric, factor, ordered
  # - Block interaction terms: Are only resolved in model.matrix() but only use model.frame()
  #    in this method. Specifying them would silently ignore them and NOT forward
  #    them into np conditional CDF
  check_err_msg(checkinput_copula2stage_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = "copula2sCOPEnp requires at least one exogenous regressor for the nonparametric conditional CDF estimation.",
    allow.interactions = FALSE,
    allow.constant.regressors = FALSE,
    warn.low.card.endo = FALSE,
    allowed.classes.endo = c("numeric", "ordered"),
    allowed.classes.exo = c("numeric", "factor", "ordered")
  ))

  check_err_msg(checkinput_copula2scopenp_npcdistbwargs(npcdistbw.args))
  check_err_msg(checkinput_copula2stage_numboots(num.boots))
  check_err_msg(checkinput_copula2stage_verbose(verbose))

  # Fit original data ------------------------------------------------------------------

  F.formula <- as.Formula(formula)
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  check_err_msg(checkinput_copula2scopenp_bws(bws = bws, labels.endo = labels.endo))



  if (verbose) {
    message(
      "Fitting 2sCOPEnp model with ",
      length(labels.endo),
      " endogenous regressor(s)."
    )
  }

  # precomputing the bws once on original data for bootstrap reuse
  # bws estimates are consistent and sampling variability has negligible effect
  # on the bootstrap distribution of the structural coefficients
  bws <- copula2scopenp_bandwidth(
    data = data,
    labels.exo = labels.exo,
    labels.endo = labels.endo,
    bws = bws,
    npcdistbw.args = npcdistbw.args,
    verbose = verbose
  )

  # TODO: warn if itnmax was hit or bws are at search boundaries (lower bound for
  #   continuous, or lambda at its max). Warn here and not in copula2scopenp_bandwidth
  #   because should also check the user-given bws
  # TODO: warn if the fit diagnostics of the bandwidths are bad?

  fit <- copula2scopenp_fit(
    F.formula = F.formula,
    data = data,
    labels.exo = labels.exo,
    labels.endo = labels.endo,
    verbose = verbose,
    bws = bws
  )

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    fit.b <- copula2scopenp_fit(
      F.formula = F.formula,
      data = data.b,
      labels.exo = labels.exo,
      labels.endo = labels.endo,
      verbose = FALSE,
      bws = bws
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
    cdf = NULL, # will be skipped when printing
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = fit$labels.pcop,
    subclass = "rendo.copula.2scope.np",
    # 2sCOPEnp-specific
    first.stage.frames = fit$mfs,
    bws = bws,
    condists = fit$condists
  ))
}


doc_rendocopula2scopenp_return_extra_items <- function() {
  return(c(
    cdf = "\\item{\\code{cdf}}{Always \\code{NULL}: The conditional CDFs are estimated nonparametrically (see \\code{condists}).}",
    bws = "\\item{\\code{bws}}{A named list with one \\code{np::condbandwidth} object per endogenous
    regressor (named accordingly), each used to estimate the conditional CDF
    \\eqn{\\hat{F}(P_k \\mid X)}.}",
    first.stage.frames = "\\item{\\code{first.stage.frames}}{List of first-stage model frames, one per endogenous regressor (named after the respective endo regressor).}",
    condists = "\\item{\\code{condists}}{A named list with one \\code{np::condistribution}
    object per endogenous regressor (named accordingly).
    Each item is the output of \\code{np::npcdist()}: An estimate of the conditional CDF \\eqn{\\hat{F}(P_k \\mid X)}.}"
  ))
}
