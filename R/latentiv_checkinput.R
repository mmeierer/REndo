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
  return(.checkinputhelper_dataVSformula_basicstructure(formula=formula, data=data, rhs.rel.regr=1,
                                                            num.only.cols = all.vars(formula)))
}

checkinput_latentIV_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}

checkinput_latentIV_startparams <- function(start.params, formula){

  F.formula <- as.Formula(formula)

  # Required names are RHS1 + "(Intercept)" if the formula has one
  f.rhs1.terms      <- terms(F.formula, lhs=0, rhs=1)
  required.names    <- labels(f.rhs1.terms)
  if(attr(f.rhs1.terms, "intercept") == 1)
    required.names <- c(required.names, "(Intercept)")

  return(checkinputhelper_startparams(start.params=start.params, F.formula=F.formula,
                                      forbidden.names = c("pi1", "pi2", "theta5", "theta6", "theta7", "theta8"),
                                      required.names  = required.names))
}



checkinput_latentIV_optimxargs <- function(optimx.args){
  return(checkinputhelper_optimxargs(optimx.args=optimx.args))
}

