#' @export
#'
copula2sCOPE <- function(
    formula,
    data,
    cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
    num.boots = 1000,
    verbose = TRUE
) {
  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copula2scope_formula(formula))
  check_err_msg(checkinput_copula2scope_data(data))
  check_err_msg(checkinput_copula2scope_dataVSformula(data, formula))
  check_err_msg(checkinput_copula2scope_numboots(num.boots))
  check_err_msg(checkinput_copula2scope_verbose(verbose))
  check_err_msg(checkinput_copula2scope_cdf(cdf))

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)
  names.endo.regs <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  # Fitting with 2sCOPE
  if (verbose) {
    message(
      "Fitting 2sCOPE model with ",
      length(names.endo.regs),
      " endogenous regressors."
    )
  }
  fit <- copula2scope_fit(F.formula, data, cdf)

  # Bootstrapping ----------------------------------------------------------------------

  fn.fit.boots <- function(data.b) {
    return(copula2scope_fit(F.formula = F.formula, data = data.b, cdf = cdf))
  }

  res.boots <- bootstrap_skip_degenerates(
    fn.fit = fn.fit.boots,
    data = data,
    num.boots = num.boots,
    coef.names = names(coef(fit)),
    verbose = verbose
  )

  # Return object ----------------------------------------------------------------------

  return(new_rendo_copula2scope(
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
