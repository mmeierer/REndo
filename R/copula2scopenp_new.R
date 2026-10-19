#' @importFrom stats coef model.frame
new_rendo_copula2scopenp <- function(
    call,
    F.formula,
    fitted.values,
    residuals,
    res.lm.augmented,
    boots.params,
    n.boots.attempted,
    n.boots.failed,
    labels.endo,
    labels.exo,
    labels.pcop,
    first.stage.frames,
    bws,
    condists
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

    # 2sCOPEnp-specific
    subclass = "rendo.copula.2scope.np",
    res.lm.augmented = res.lm.augmented,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop,
    first.stage.frames = first.stage.frames,
    bws = bws,
    condists = condists
  ))
}
