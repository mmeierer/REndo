#' @importFrom Formula as.Formula
checkinput_latentIV_formula <- function(formula, data){
  err.msg <- checkinputhelper_formula_1RHS(formula=formula, data=data)
  if(length(err.msg)>0)
    return(err.msg)

  # Check length of only RHS is only 1
  F.formula <- as.Formula(formula)
  if(length(all.vars(formula(F.formula, rhs=1, lhs=0))) > 1)
    err.msg <- c(err.msg, "Please only provide only a single variable in the right-hand-side of the formula.")

  return(err.msg)
}

checkinput_latentIV_data <- function(formula, data){
  return(checkinputhelper_data_basicstructure(formula=formula, data=data))
}

checkinput_latentIV_startparams <- function(start.params, formula){
  return(checkinputhelper_startparams(start.params=start.params, formula=formula,
                                      forbidden.names=c("pi1", "pi2", "theta5", "theta6", "theta7", "theta8")))
}
