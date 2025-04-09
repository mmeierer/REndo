#' @importFrom Formula as.Formula
checkinput_tscope_formula <- function(formula, data) {
  err.msg <- .checkinputhelper_formula_basicstructure(formula = formula)
  if(length(err.msg) > 0)
    return(err.msg)

  F.formula <- Formula::as.Formula(formula)
  err.msg <- character() # Initialize err.msg here

  # Check number of RHS parts FIRST
  if(length(F.formula)[2] != 2) {
    err.msg <- c(err.msg, "Formula needs to have two parts")
  } else {
    # Only try to access rhs=2 if it exists
    endo_vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))
    if(length(endo_vars) < 1) {
      err.msg <- c(err.msg, "Please provide at least one endogenous regressor in the second part of the formula.")
    }
  }

  # Check LHS/RHS=1 part
  outcome_pred <- all.vars(formula(F.formula, lhs = 1, rhs = 1))
  if(length(outcome_pred) < 2) {
    err.msg <- c(err.msg, "The outcome part must include an outcome and at least one predictor.")
  }

  return(err.msg)
}

checkinput_tscope_data <- function(data) {
  return(.checkinputhelper_data_basicstructure(data = data))
}

checkinput_tscope_dataVSformula <- function(formula, data) {
  return(.checkinputhelper_dataVSformula_basicstructure(formula = formula, data = data, rhs.rel.regr = 2, num.only.cols = all.vars(formula)))
}

checkinput_tscope_verbose <- function(verbose) {
  return(checkinputhelper_single_logical(logical = verbose, param.name = "verbose"))
}