#' @importFrom stats coef fitted residuals
build_rendo_boots_ima <- function(
  call,
  F.formula,
  names.endo.regs,
  cdf,
  res.lm,
  boots,
  n.boots.attempted,
  n.boots.failed
) {
  return(.new_rendo_boots(
    # Stuff for rendo.boots class
    call = call,
    F.formula = F.formula,
    mf = res.lm$model,
    coefficients = coef(res.lm),
    names.main.coefs = rownames(boots),
    fitted.values = fitted(res.lm),
    residuals = residuals(res.lm),
    boots.params = boots,
    # Stuff specific to copula ima
    subclass = "rendo.copula.ima",
    cdf = cdf,
    names.endo.regs = names.endo.regs,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed
  ))
}
