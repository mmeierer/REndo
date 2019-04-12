# Extends rendo.base by bootstrapped vcov from bootstrapped params
#' @importFrom Formula as.Formula
.new_rendo_boots <- function(# rendo.base specific
                                call, F.formula, mf,
                                coefficients,
                                names.main.coefs,
                                fitted.values, residuals,
                                # rendo.boot specific
                                boots.params,
                                # Can be further extended
                                subclass = character(),
                                ...){

  return(.new_rendo_base(
          call = call,
          F.formula = as.Formula(F.formula), #to be sure its a Formula
          mf   = mf,
          coefficients = coefficients,
          names.main.coefs = names.main.coefs,
          residuals = residuals,
          fitted.values = fitted.values,

          # rendo.boots specific
          subclass = c(subclass, "rendo.boots"),
          boots.params=boots.params,

          # Anything else to add to the object
          ...))
}
