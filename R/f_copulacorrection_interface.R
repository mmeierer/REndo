#' @importFrom Formula as.Formula
#' @importFrom methods formalArgs
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- quote(match.call())
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))


  # Read out specials ------------------------------------------------------------------------------------------
  F.formula <- as.Formula(formula)
  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous", from.rhs=2)
  names.vars.discrete   <- formula_readout_special(F.formula = F.formula, name.special = "discrete",   from.rhs=2)

  # Warn if the data is binomial=only has to values=dummy
  checkinput_copulacorrection_warnbinomialendodata(data=data, names.vars.continuous=names.vars.continuous,
                                                   names.vars.discrete=names.vars.discrete)

  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE

  # Dispatch to either LL optimization or PStar+lm -------------------------------------------------------------
  if(optimizeLL){
    # Single continuous - copula method 1 (optimize LL)
    res <- do.call(what = copulaCorrection_optimizeLL,
                   args = c(list(F.formula=F.formula, data=data,
                                 name.var.continuous =names.vars.continuous,
                                 verbose=verbose, cl=cl), # cl supplied to create return object
                            l.ellipsis))
  }else{
    # All other cases use pstar data + lm
    res <- copulaCorrection_linearmodel(F.formula = F.formula, data = data,
                                        verbose=verbose,
                                        cl=cl, # cl supplied to create return object
                                        names.vars.continuous = names.vars.continuous,
                                        names.vars.discrete   = names.vars.discrete)
  }

  return(res)
}

