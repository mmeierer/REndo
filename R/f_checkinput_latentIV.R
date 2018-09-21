#' @importFrom Formula as.Formula
checkinput_latentIV_formula <- function(formula, data){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)
  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)
  # Check that formula has only 1 RHS
  if(length(F.formula)[2] !=1)
    err.msg <- c(err.msg, "Please only provide only a single right-hand side in the formula and do not use bars (|).")

  # Check length of only RHS is only 1
  if(length(all.vars(formula(F.formula, rhs=1, lhs=0))) > 1)
    err.msg <- c(err.msg, "Please only provide only a single variable in the right-hand-side of the formula.")

  return(err.msg)
}

checkinput_latentIV_data <- function(data){
  return(.checkinputhelper_data_basicstructure(data=data))
}

checkinput_latentIV_dataVSformula <- function(data, formula){
  err.msg <- .checkinputhelper_dataVSformula_basicstructure(formula=formula, data=data)
}


checkinput_latentIV_startparams <- function(start.params, formula){
  return(checkinputhelper_startparams(start.params=start.params, F.formula=as.Formula(formula),
                                      forbidden.names=c("pi1", "pi2", "theta5", "theta6", "theta7", "theta8")))
}
