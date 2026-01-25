#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix terms qnorm
CopulaIMA_fit <- function(F.formula, data, cdf,...){

  F.formula <- Formula::as.Formula(F.formula)

  rhs1.vars <- all.vars(formula(F.formula, rhs = 1, lhs = 0))
  rhs2.vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))

  names.vars.continuous <- formula_readout_special( F.formula = F.formula,name.special = "continuous",from.rhs = 2,params.as.chars.only = TRUE)

  #model frame
  F.formula.main <- formula(F.formula, rhs = 1, lhs = 1)
  mf <- model.frame(F.formula.main, data = data)
  y <- model.response(mf)

  X.main <- model.matrix(formula(F.formula, rhs = 1, lhs = 0), mf)

  #checking if there are intercepts
  has.intercept <- attr(terms(F.formula, rhs = 1), "intercept") == 1

  #Continuous endogenous var
  endogenous.columns <- colnames(X.main)[colnames(X.main) %in% names.vars.continuous]

  if (length(endogenous.columns)==0) stop("No continuous endogenous regressors found in design matrix.")
  if (length(endogenous.columns) < length(names.vars.continuous)) {stop( "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling.")}


  P <- X.main[, endogenous.columns, drop = FALSE]
  P.names <- colnames(P)

  P.star <- copulaIMA_pstar(P, cdf = cdf)

  #checking for exogeneous variables

 if (ncol(P)==1){
   cop.terms <- qnorm(P.star)
   colnames(cop.terms) <- paste0 (P.names, "_cop")
 } else{
   cop.terms <- copulaIMA_residuals(P.star, P.names)
 }


  X.final <- cbind(X.main, cop.terms)

  # X.main contains intercept if specified in the original formula.
  fit <- lm(y ~ X.final - 1) #intercept is removed to fit without an additional implicit intercept

  .new_rendo_base(
    F.formula = F.formula,
    mf = mf,
    coefficients = coef(fit),
    names.main.coefs = colnames(X.main),
    fitted.values = fitted(fit),
    residuals = residuals(fit),
    subclass = "rendo.copula.ima",
    cdf = cdf
  )
}

