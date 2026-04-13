doc_rendobase_return_list <- function(){
  return(
      c(
      formula = "\\item{\\code{formula}}{The formula given to specify the fitted model.}",
      model = "\\item{\\code{model}}{The model.frame used for model fitting.}",
      terms = "\\item{\\code{terms}}{The terms object used for model fitting.}",
      coefficients = "\\item{\\code{coefficients}}{A named vector of all coefficients resulting from model fitting.}",
      names.main.coefs = "\\item{\\code{names.main.coefs}}{A vector specifying which coefficients are from the model. For internal usage.}",
      fitted.values = "\\item{\\code{fitted.values}}{Fitted values at the found solution.}",
      residuals = "\\item{\\code{residuals}}{The residuals at the found solution.}"
    )
  )
}


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
