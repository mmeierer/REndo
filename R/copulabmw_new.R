#' @importFrom stats coef model.frame
new_rendo_copulabmw <- function(
    call,
    F.formula,
    res.lm.augmented,
    fitted.values,
    residuals,
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

    # Stuff specific to copulaBMW
    subclass = "rendo.copula.bmw",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop
  ))
}
