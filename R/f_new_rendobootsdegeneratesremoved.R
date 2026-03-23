.new_rendo_boots_degenerates_removed <- function(
    call,
    F.formula,
    mf,
    coefficients,
    names.main.coefs,
    fitted.values,
    residuals,
    # rendo.boot specific
    boots.params,
    # rendo.boots.degenerates.removed specific
    n.boots.attempted,
    n.boots.failed,

    # Can be further extended
    subclass = character(),
    ...
) {
  return(.new_rendo_boots(
    # Stuff for rendo.boots
    call = call,
    F.formula = F.formula,
    mf = mf,
    coefficients = coefficients,
    names.main.coefs = names.main.coefs,
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,

    # Stuff specific to degenerates.removed
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,
    subclass = c(subclass, "rendo.boots.degenerates.removed"),

    # Anything else to add to the object
    ...
  ))
}
