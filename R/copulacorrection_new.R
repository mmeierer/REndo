#' @importFrom Formula as.Formula
new_rendo_copula_correction <- function(call, F.formula, mf,
                                coefficients,
                                names.main.coefs,
                                fitted.values, residuals,
                                boots.params,
                                # copula correction specific
                                names.vars.continuous,
                                names.vars.discrete,
                                copula.case,
                                # C2 specific
                                res.lm.real.data=NULL,
                                # C1 specific
                                start.params=NULL,
                                res.optimx=NULL){

  # Create a subclass of the rendo.base and rendo.boots (for vcov() and summary()) classes
  l.args.call <- alist(call = call,
                     F.formula = as.Formula(F.formula), # to be sure its a Formula
                     mf   = mf,
                     coefficients = coefficients,
                     names.main.coefs = names.main.coefs,
                     residuals = residuals,
                     fitted.values = fitted.values,
                     boots.params=boots.params,
                     # copula correction specific
                     subclass = c("rendo.copula.correction"),
                     copula.case = copula.case,
                     names.vars.continuous = names.vars.continuous,
                     names.vars.discrete   = names.vars.discrete)

  # additionally store the c1 stuff (optimx result to report all the LL & convergence) and c2 stuff (fitted lm)
  if(copula.case == 1){
    # copula C1 specific: create object also with start.params and res.optimx
    l.args.call <- c(l.args.call,
                     alist(start.params = start.params,
                           res.optimx   = res.optimx))
  }else{
    # copula C2 specific: create object also with res.lm.real.data
    l.args.call <- c(l.args.call,
                     # never required by any method but might be useful to the user
                     alist(res.lm.real.data = res.lm.real.data))
  }

  return(do.call(what = .new_rendo_boots, args = l.args.call))
}
