# This class allows for subclassing by stating the name(s) of additional subclass(es) and providing any
#   other data in ... for storing in the object
#' @importFrom Formula as.Formula
.new_rendo_base <- function(call, F.formula, mf,
                            coefficients, names.main.coefs,
                            fitted.values, residuals,
                            subclass = character(),
                            # Anything else to create subclass of standard base class
                            ...) {

  return(structure(
    class = c(subclass, "rendo.base"),
    .Data = list(call = call,
                 formula = as.Formula(F.formula), # to be sure its a Formula
                 model   = mf,
                 terms   = terms(mf),
                 coefficients = coefficients,
                 names.main.coefs = names.main.coefs,
                 fitted.values = fitted.values,
                 residuals = residuals,
                 # Add params for subclass
                 ...)))
}
