#' @importFrom Formula as.Formula
#' @importFrom methods formalArgs
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- match.call()
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

  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE

  # Dispatch to either LL optimization or PStar+lm -------------------------------------------------------------
  if(optimizeLL){
    # Single continuous - copula method 1 (optimize LL)
    res <- do.call(what = copulaCorrection_optimizeLL, args = c(list(data=data, formula=formula, verbose=verbose),
                                                                l.ellipsis))
  }else{
    # All other cases use pstar data + lm
    res <- copulaCorrection_linearmodel(F.formula = F.formula, data = data,
                                        verbose=verbose, cl=cl,
                                        names.vars.continuous = names.vars.continuous,
                                        names.vars.discrete   = names.vars.discrete)
  }

  return(res)
}

