#' @importFrom Formula as.Formula
#' @title Input validation for tscope formula
#' @description
#' Checks that the formula provided to tscope is valid and meets the requirements for the 2sCOPE method. This includes verifying that the formula has exactly two parts (main regression and endogenous regressors), that at least one endogenous regressor is specified, and that the main part of the formula (LHS/RHS=1) includes both a dependent variable and at least one predictor. The function returns a character vector of error messages; an empty vector means the input is valid.
#'
#' @param formula A formula or Formula object specifying the model, with two parts separated by '|'. The first part must include a dependent variable and at least one predictor. The second part must list at least one endogenous regressor.
#' @param data A data.frame or compatible object containing all variables referenced in the formula.
#' @details
#' This function performs several checks:
#' \itemize{
#'   \item Ensures the formula is syntactically valid.
#'   \item Ensures the formula has exactly two parts (main regression and endogenous regressors).
#'   \item Ensures at least one endogenous regressor is specified in the second part.
#'   \item Ensures the main part of the formula (LHS/RHS=1) includes both a dependent variable and at least one predictor. This is done by extracting all variables from the first part of the formula and checking that there are at least two: one outcome and at least one predictor. This prevents cases where the user supplies a formula with only an outcome or only a predictor, which would be invalid for model fitting.
#' }
#' @return A character vector of error messages. An empty vector means the input is valid.
#' @export
checkinput_tscope_formula <- function(formula, data) {
  err <- .checkinputhelper_formula_basicstructure(formula = formula)
  if(length(err) > 0) {
    return(err)
  }

  F.formula <- as.Formula(formula)

  # Check number of RHS parts FIRST
  if(length(F.formula)[2] != 2) {
    return("Formula needs to have exactly two parts")
  }
  # Only try to access rhs=2 if it exists
  endo_vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))
  if(length(endo_vars) < 1) {
    return("Please provide at least one endogenous regressor in the second part of the formula.")
  }

  # Ensure the main part of the formula (LHS/RHS = 1) specifies both:
  # - a dependent (outcome) variable
  # - at least one predictor.
  #
  # For tscope we require exactly two RHS parts. The first RHS part (rhs = 1)
  # represents the main regression whose left-hand side (lhs = 1) holds the outcome
  # variable. By extracting all variables that appear in this first part we can verify
  # that there are at least two unique symbols present, one for the outcome and at
  # least 1 for the predictors. A length smaller than two implies that the user either
  # omitted the outcome or the predictors, both of which would make model estimation impossible.
  main_part_vars <- all.vars(formula(F.formula, lhs = 1, rhs = 1))
  if (length(main_part_vars) < 2) {
    return("The outcome part must include the outcome and at least one predictor (intercept-only models are not sufficient).")
  }

  return(character())
}

checkinput_tscope_data <- function(data) {
  return(.checkinputhelper_data_basicstructure(data = data))
}

checkinput_tscope_dataVSformula <- function(formula, data) {
  return(.checkinputhelper_dataVSformula_basicstructure(formula = formula, data = data, rhs.rel.regr = 1, num.only.cols = all.vars(formula)))
}

checkinput_tscope_verbose <- function(verbose) {
  return(checkinputhelper_single_logical(logical = verbose, param.name = "verbose"))
}

checkinput_tscope_numboots <- function(num.boots) {
  return(checkinputhelper_singlepositivewholenumeric(num.param = num.boots, parameter.name = "num.boots"))
}