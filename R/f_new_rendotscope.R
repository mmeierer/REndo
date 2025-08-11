#' @title Constructor for 2sCOPE Objects
#'
#' @description Constructs an object of class \code{rendo.tscope} and \code{rendo.base} that stores the results from \code{tscope()}.
#'
#' @param call The matched call.
#' @param F.formula The \code{Formula} object used.
#' @param mf The model frame.
#' @param coefficients A named vector of coefficients.
#' @param names.main.coefs Names of the main coefficients.
#' @param fitted.values Fitted values from the final model.
#' @param residuals Residuals from the final model.
#' @param tscope_model The final \code{lm} model object.
#' @param details A list with additional details.
#'
#' @return An object of class \code{rendo.tscope} and \code{rendo.base}.
new_rendo_tscope <- function(call, F.formula, mf, coefficients, names.main.coefs,
                              fitted.values, residuals, tscope_model, details) {
  .new_rendo_base(
    call = call,
    F.formula = as.Formula(F.formula),
    mf = mf,
    coefficients = coefficients,
    names.main.coefs = names.main.coefs,
    fitted.values = fitted.values,
    residuals = residuals,
    subclass = c("rendo.tscope"),
    tscope_model = tscope_model,
    details = details
  )
}

#' @title Constructor for 2sCOPE Objects with Bootstrapping
#'
#' @description Constructs an object of class \code{rendo.tscope}, \code{rendo.boots}, and \code{rendo.base} 
#' that stores the results from \code{tscope()} with bootstrapped parameter estimates.
#'
#' @param call The matched call.
#' @param F.formula The \code{Formula} object used.
#' @param mf The model frame.
#' @param coefficients A named vector of coefficients.
#' @param names.main.coefs Names of the main coefficients.
#' @param fitted.values Fitted values from the final model.
#' @param residuals Residuals from the final model.
#' @param boots.params Matrix of bootstrapped parameter estimates.
#' @param tscope_model The final \code{lm} model object.
#' @param details A list with additional details.
#'
#' @return An object of class \code{rendo.tscope}, \code{rendo.boots}, and \code{rendo.base}.
new_rendo_tscope_boots <- function(call, F.formula, mf, coefficients, names.main.coefs,
                                   fitted.values, residuals, boots.params, tscope_model, details) {
  .new_rendo_boots(
    call = call,
    F.formula = as.Formula(F.formula),
    mf = mf,
    coefficients = coefficients,
    names.main.coefs = names.main.coefs,
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,
    subclass = c("rendo.tscope"),
    tscope_model = tscope_model,
    details = details
  )
}