#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response
CopulaIMA_fit <- function(F.formula, data, cdf) {
  F.formula <- Formula::as.Formula(F.formula)

  names.vars.continuous <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  #model frame
  F.formula.main <- formula(F.formula, rhs = 1, lhs = 1) #Haschka 2025 page 164 eq. 3.1
  mf <- model.frame(F.formula.main, data = data)
  y <- model.response(mf)

  X.main <- model.matrix(formula(F.formula, rhs = 1, lhs = 0), mf)

  #Continuous endogenous var
  endogenous.columns <- colnames(X.main)[colnames(X.main) %in% names.vars.continuous]

  if (length(endogenous.columns) == 0) {
    stop("No continuous endogenous regressors found in design matrix.")
  }
  if (length(endogenous.columns) < length(names.vars.continuous)) {
    stop(
      "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling."
    )
  }

  P <- X.main[, endogenous.columns, drop = FALSE]
  P.names <- colnames(P)

  P.star <- copulaIMA_pstar(P = P, cdf = cdf)

  #checking for exogeneous variables

  #Haschka 2025 page 164 Eq. 3.4
  cop.terms <- copulaIMA_residuals(P.star)

  X.final <- cbind(X.main, cop.terms)

  # X.main contains intercept if specified in the original formula.
  fit <- lm(y ~ X.final - 1) #intercept is removed to fit without an additional implicit intercept

  return(fit)
}
