#' @importFrom stats coef model.frame
new_rendo_copula2scope <- function(
  call,
  F.formula,
  fitted.values,
  residuals,
  res.lm.augmented,
  boots.params,
  n.boots.attempted,
  n.boots.failed,
  cdf,
  labels.endo,
  labels.exo,
  labels.pcop
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
    subclass = "rendo.copula.2scope",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop
  ))
}
