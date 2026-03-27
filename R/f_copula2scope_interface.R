#' Two-Stage Copula Generator Regressor Approach (2sCOPE)
#'
#' @description
#' Fitting the two-stage copula generator regressor approach (2sCOPE) estimator of
#' Yang et al. (2024) to address endogeneity.
#'
#' @template template_param_formuladataverbose
#'
#' @param num.boots Integer giving the number of bootstrap replications
#'   used for inference.
#'
#' @references
#' Yang, F., Qian, Y., and Xie, H. (2025). Addressing Endogeneity Using a
#' Two-Stage Copula Generated Regressor Approach. \emph{EXPRESS: Journal of
#' Marketing Research}, 62(4), 601-623.
#' \doi{10.1177/00222437241296453}
#'
#'
#' @export
#' @importFrom stats coef
#' @importFrom utils txtProgressBar setTxtProgressBar
copula2sCOPE <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copula2sCOPE_formula(formula))
  check_err_msg(checkinput_copula2sCOPE_data(data))
  check_err_msg(checkinput_copula2sCOPE_dataVSformula(data = data, formula = formula))
  check_err_msg(checkinput_copula2sCOPE_numboots(num.boots))
  check_err_msg(checkinput_copula2sCOPE_verbose(verbose))
  check_err_msg(checkinput_copula2sCOPE_cdf(cdf))

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)
  names.endo.regs <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  #warning the user to use copulaCorrection() (Park and Gupta 2012)
  #if there is no exo regressors. 2sCOPE method is designed for correlated
  # endo and exo regressors. Without an exo regressor, the correction term
  #is just equivalent to the copulaCorrection() approach

  rhs1.vars <- all.vars(formula(F.formula, rhs=1, lhs =0))
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

  # Return object ----------------------------------------------------------------------

  return(new_rendo_copula2sCOPE(
    call = cl,
    F.formula = F.formula,
    res.lm = fit,
    boots.params = res.boots$boots.params,
    n.boots.attempted = res.boots$n.attempted,
    n.boots.failed = res.boots$n.failed,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
