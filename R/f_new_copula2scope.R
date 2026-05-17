#' @importFrom stats coef model.frame
new_rendo_copula2sCOPE <- function(
  call,
  F.formula,
  fitted.values,
  residuals,
  res.lm.augmented,
  boots.params,
  n.boots.attempted,
  n.boots.failed,
  cdf,
  names.endo.regs
) {
  return(.new_rendo_boots_degenerates_removed(
    # Stuff for rendo.boots.degenerates.removed class
    call = call,
    F.formula = F.formula,
    mf = model.frame(res.lm.augmented),
    coefficients = coef(res.lm.augmented),
    names.main.coefs = names(coef(res.lm.augmented)), # OR: row.names(boots.params)
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,

    # Stuff specific to 2sCOPE
    subclass = "rendo.copula.2sCOPE",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
