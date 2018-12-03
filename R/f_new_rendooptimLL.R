#' @importFrom Formula as.Formula
new_rendo_optim_LL <- function(call, F.formula, mf, start.params, estim.params, estim.params.se,
                               names.main.coefs,
                               res.optimx, log.likelihood, hessian,
                               fitted.values, residuals) {

  return(structure(
            class = "rendo.optim.LL",
            list(call = call,
                 formula = as.Formula(F.formula), #to be sure its a Formula
                 model   = mf,
                 terms   = terms(mf),
                 start.params = start.params,
                 estim.params = estim.params,
                 estim.params.se = estim.params.se,
                 names.main.coefs = names.main.coefs,
                 res.optimx = res.optimx,
                 log.likelihood = log.likelihood,
                 hessian = hessian,
                 fitted.values = fitted.values,
                 residuals = residuals)))
}
