#' @importFrom Formula as.Formula
new_rendo_latent_IV <- function(call, F.formula, mf, start.params, coefficients,
                               m.delta.diag, names.main.coefs,
                               res.optimx, hessian,
                               fitted.values, residuals) {

  return(.new_rendo_base(
          call = call,
          F.formula = as.Formula(F.formula), #to be sure its a Formula
          mf   = mf,
          coefficients = coefficients,
          names.main.coefs = names.main.coefs,
          residuals = residuals,
          fitted.values = fitted.values,

          subclass = c("rendo.latent.IV"),
          res.optimx = res.optimx,
          start.params = start.params,
          m.delta.diag = m.delta.diag,
          hessian = hessian))
}
