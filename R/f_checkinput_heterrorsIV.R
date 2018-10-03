checkinput_heterrors_formula <- function(formula=formula){
  return(checkinputhelper_formula_IIVs(formula=formula))
}


checkinput_heterrors_data <- function(data){
  return(.checkinputhelper_data_basicstructure(data=data))
}

checkinput_heterrors_formulaVSdata <- function(formula, data){
  return(checkinputhelper_dataVSformula_IIV(formula=formula, data=data))
}

checkinput_heterrors_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}


#' @importFrom Formula as.Formula
checkinput_heterrors_iivregressors <- function(F.formula, names.exo.IIV){
  err.msg <- c()

  if(length(names.exo.IIV) == 0)
    return("Please specify the exogenous regressors to build the internal instruments from in the IIV() function.")

  # Check that all regressors are in the RHS1 (main model)
  if(!all(names.exo.IIV %in% labels(terms(F.formula, lhs=0,rhs=1))))
    err.msg <- c(err.msg, "Please specifiy in IIV() only regressors that are also present in the first right-hand side (main model) of the formula.")

  # Check that no regressors is in the RHS2 (endo)
  if(any(names.exo.IIV %in% labels(terms(F.formula, lhs=0,rhs=2))))
    err.msg <- c(err.msg, "Please specifiy only the exogenous but not the endogenous regressors in IIV().")

  # Formula has already been checked to have all regressors in data - therefore if it is in the formula it is also in the data

  return(err.msg)
}

