#' @importFrom Formula as.Formula
#'
checkinput_copula2sCOPE_formula <- function(formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula = formula)
  if (length(err.msg) > 0) return(err.msg)

  F.formula <- Formula::as.Formula(formula)

  if (length(F.formula)[2] < 2)
    err.msg <- c(err.msg,
                 "Please specify endogenous regressors on a second right-hand side using | (e.g. y ~ X + P | continuous(P)).")

  if (length(F.formula)[2] > 2)
    err.msg <- c(err.msg,
                 "Please specify endogenous regressors using only one | separator.")

  if (length(err.msg) > 0) return(err.msg)

  #For the variables
  rhs1.vars <- all.vars(formula(F.formula, rhs = 1, lhs = 0))
  rhs2.vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))

  # RHS2 must be in RHS1
  if (!all(rhs2.vars %in% rhs1.vars))
    err.msg <- c(err.msg,
                 "Please specify every endogenous regressor also in the first right-hand side of the formula.")


  names.vars.continuous <- formula_readout_special(
    F.formula         = F.formula,
    name.special      = "continuous",
    from.rhs          = 2,
    params.as.chars.only = TRUE
  )

  names.vars.discrete <- formula_readout_special(
    F.formula         = F.formula,
    name.special      = "discrete",
    from.rhs          = 2,
    params.as.chars.only = TRUE
  )

  # 2sCOPE only supports continuous endogenous regressors (same as IMA)
  if (length(names.vars.discrete) > 0)
    err.msg <- c(err.msg,
                 "2sCOPE only supports continuous endogenous regressors. Remove discrete() or use Copula Correction.")

  # At least one continuous endogenous regressor is required
  if (length(names.vars.continuous) == 0)
    err.msg <- c(err.msg,
                 "2sCOPE requires at least one continuous endogenous regressor.")

  # Every RHS2 variable must be wrapped in continuous()
  if (!all(rhs2.vars %in% names.vars.continuous))
    err.msg <- c(err.msg,
                 "Please wrap every endogenous regressor on the second RHS in continuous().")

  # Variable names in continuous() must match exactly as they appear in RHS1
  # (including any transformations such as log(x) or I(x^2))
  rhs1.labels <- labels(terms(F.formula, rhs = 1, lhs = 0))
  if (!all(names.vars.continuous %in% rhs1.labels))
    err.msg <- c(err.msg,
                 "Please name every endogenous regressor exactly as in the main model, including transformations if any.")

  # continuous() must not appear on RHS1 or LHS
  num.specials.rhs1 <- sum(sapply(
    attr(terms(F.formula, rhs = 1, lhs = 0, specials = "continuous"), "specials"),
    length
  ))

  num.specials.lhs <- sum(sapply(
    attr(terms(F.formula, rhs = 0, lhs = 1, specials = "continuous"), "specials"),
    length
  ))

  if (num.specials.rhs1 > 0)
    err.msg <- c(err.msg,
                 "No endogenous regressors should be on the first RHS.")

  if (num.specials.lhs > 0)
    err.msg <- c(err.msg,
                 "No endogenous regressors should be on the LHS.")

  return(err.msg)
}


#' @importFrom Formula as.Formula
#'
checkinput_copula2sCOPE_data <- function(data) {
  .checkinputhelper_data_basicstructure(data = data)
}


#' @importFrom Formula as.Formula
#'
checkinput_copula2sCOPE_dataVSformula <- function(data, formula) {

  F.formula <- Formula::as.Formula(formula)

  # Column names of the endogenous (continuous) regressors
  names.cols.endo <- all.vars(terms(F.formula, rhs = 2, lhs = 0))

  err.msg <- .checkinputhelper_dataVSformula_basicstructure(
    formula      = F.formula,
    data         = data,
    rhs.rel.regr = c(1, 2),
    num.only.cols = names.cols.endo
  )

  # "Pstar" is used internally as a column name prefix; forbid it in the data
  err.msg <- c(
    err.msg,
    checkinputhelper_data_notnamed(
      formula          = F.formula,
      data             = data,
      forbidden.colname = "Pstar"
    )
  )

  err.msg
}


checkinput_copula2sCOPE_numboots <- function(num.boots) {

  err.msg <- checkinputhelper_singlepositivewholenumeric(
    num.param      = num.boots,
    parameter.name = "num.boots",
    min.num        = 2
  )

  if (length(err.msg) > 0) return(err.msg)

  if (num.boots < 1000)
    warning(
      "It is recommended to run 1000 or more bootstraps.",
      call.     = FALSE,
      immediate. = TRUE
    )

  c()
}


checkinput_copula2sCOPE_verbose <- function(verbose) {
  checkinputhelper_single_logical(logical = verbose, param.name = "verbose")
}


checkinput_copula2sCOPE_cdf <- function(cdf) {

  cdf.allowed <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")

  if (!is.character(cdf) || length(cdf) != 1)
    return("The argument 'cdf' must be a single character string.")

  if (!(cdf %in% cdf.allowed))
    return(paste0(
      "Value for 'cdf' is invalid. The allowed values are: ",
      paste(cdf.allowed, collapse = ", ")
    ))

  return(c())

}
