#' @importFrom Formula as.Formula
#' @importFrom stats fitted
new_rendo_copula_c2 <- function(call, F.formula,
                                res.lm.real.data,
                                boots.params,
                                names.vars.continuous,
                                names.vars.discrete){

  # Create a subclass of the rendo.base and rendo.boots (for vcov()) classes
  return(.new_rendo_boots(
    call             = call,
    F.formula        = as.Formula(F.formula), # to be sure its a Formula
    mf               = model.frame(res.lm.real.data),
    names.main.coefs = names(coef(res.lm.real.data)),
    coefficients     = coef(res.lm.real.data),
    residuals        = residuals(res.lm.real.data),
    fitted.values    = fitted(res.lm.real.data),
    boots.params=boots.params,

    # copula C2 specific
    subclass = c("rendo.copula.c2"),
    res.lm.real.data      = res.lm.real.data, # never required by any method but might be useful to the user
    names.vars.continuous = names.vars.continuous,
    names.vars.discrete   = names.vars.discrete))
}
