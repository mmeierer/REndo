#' @importFrom stats coef model.frame
new_rendo_copulaima <- function(
  call,
  F.formula,
  fitted.values,
  residuals,
  boots.params,
  n.boots.attempted,
  n.boots.failed,
  res.lm.augmented,
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
    names.main.coefs = names(coef(res.lm.augmented)),
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,

    # Stuff specific to copula ima
    subclass = "rendo.copula.ima",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop
  ))
}
