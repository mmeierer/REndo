#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix terms qnorm
copula2scope_fit <- function(F.formula, data, cdf) {
  F.formula <- Formula::as.Formula(F.formula)

  names.vars.continuous <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  F.formula.main <- formula(F.formula, rhs = 1, lhs = 1)
  mf <- model.frame(F.formula.main, data = data)
  X.main <- model.matrix(F.formula.main, data = mf)

  endogenous.columns <- colnames(X.main)[
    colnames(X.main) %in% names.vars.continuous
  ]

  if (length(endogenous.columns) == 0) {
    stop(
      "No continuous endogenous regressors found in the design matrix.",
      call. = FALSE
    )
  }

  if (length(endogenous.columns) < length(names.vars.continuous)) {
    stop(
      "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling."
    )
  }

  #applying CDF to all regressors both endo and exo with no intercept column
  X.no.intercept <- X.main[, colnames(X.main) != "(Intercept)", drop = FALSE]
  P.star <- copula2scope_pstar(X.no.intercept, cdf = cdf)

  #first-stage residuals: regressing each endogenous P* on exogenous W*, Eq (9) from Yang et. al 2024
  cop.terms <- copula2scope_residuals(P.star, endo.cols = endogenous.columns)

  f.main <- formula(mf)

  f.pcop <- reformulate(
    termlabels = c(".", colnames(cop.terms)),
    response = NULL,
    intercept = TRUE
  )

  f.final <- update(old = f.main, new = f.pcop)

  return(lm(
    formula = f.final,
    data = cbind(data, cop.terms)
  ))
}
