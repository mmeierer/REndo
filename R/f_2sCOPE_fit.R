#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix terms qnorm
copula2scope_fit <- function(F.formula, data, cdf){

  F.formula <- Formula::as.Formula(F.formula)

  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous", from.rhs = 2, params.as.chars.only = TRUE)

  F.formula.main <- formula(F.formula, rhs = 1, lhs = 1)
  mf <- model.frame (F.formula.main, data = data)
  y <- model.response(mf)
  X.main <- model.matrix(formula(F.formula, rhs = 1, lhs = 0 ), mf)

  endogenous.columns <- colnames(X.main)[colnames(X.main) %in% names.vars.continuous]

  if (length(endogenous.columns) == 0 )
    stop("No continuous endogenous regressors found in the design matrix.", call. = FALSE)

  if (length(endogenous.columns) < length(names.vars.continuous)){
    stop("Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling.")}

  P <- X.main[, endogenous.columns, drop = FALSE]
  P.star <- copula2scope_pstar(P, cdf = cdf)

  cop.terms <- copula2scope_residuals(P.star)

  X.final <- cbind(X.main, cop.terms)

  fit <- lm(y ~ X.final - 1)

  return(fit)
}
