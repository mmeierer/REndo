#' @importFrom Formula as.Formula
new_rendo_copula_c1 <- function(call, F.formula, mf,
                                coefficients,
                                names.main.coefs,
                                fitted.values, residuals,
                                start.params,
                                boots.params,
                                res.optimx){

  # Create a subclass of the rendo.base and rendo.boots (for vcov()) classes
  #   additionally store the optimx result to report all the LL & convergence stuff
  return(.new_rendo_boots(
            call = call,
            F.formula = as.Formula(F.formula), # to be sure its a Formula
            mf   = mf,
            coefficients = coefficients,
            names.main.coefs = names.main.coefs,
            residuals = residuals,
            fitted.values = fitted.values,
            boots.params=boots.params,

            # copula C1 specific
            subclass = c("rendo.copula.c1"),
            start.params=start.params,
            res.optimx=res.optimx))
}
