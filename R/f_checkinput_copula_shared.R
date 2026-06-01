checkinput_copulashared_cdf <- function(cdf, allowed.cdf) {
  return(checkinputhelper_choice(
    choice = cdf,
    allowed = allowed.cdf,
    param.name = "cdf"
  ))
}

checkinput_copulashared_numboots <- function(num.boots) {
  return(checkinputhelper_numboots(num.boots))
}

checkinput_copulashared_data <- function(data) {
  return(.checkinputhelper_data_basicstructure(data))
}

checkinput_copulashared_verbose <- function(verbose) {
  checkinputhelper_single_logical(logical = verbose, param.name = "verbose")
}


#' @importFrom Formula as.Formula
checkinput_copulashared_dataVSformula <- function(data, formula) {
  F.formula <- Formula::as.Formula(formula)

  names.cols.endo <- all.vars(terms(F.formula, rhs = 2, lhs = 0))

  err.msg <- .checkinputhelper_dataVSformula_basicstructure(
    formula = F.formula,
    data = data,
    rhs.rel.regr = c(1, 2),
    num.only.cols = names.cols.endo
  )

  # Still relevant??
  err.msg <- c(
    err.msg,
    checkinputhelper_data_notnamed(
      formula = F.formula,
      data = data,
      forbidden.colname = "Pstar"
    )
  )

  return(err.msg)
}


checkinput_copulashared_formula <- function(formula) {
  err.msg <- .checkinputhelper_formula_basicstructure(formula = formula)
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  F.formula <- Formula::as.Formula(formula)

  # Check if exactly 2 part formula ---------------------------------------------------

  # check to see if the formula inputed has 2 RHS
  if (length(F.formula)[2] < 2) {
    err.msg <- c(
      err.msg,
      "Please specify endogenous regressors on a second right-hand side using '|' (e.g. y ~ X + P | continuous(P))."
    )
  }

  if (length(F.formula)[2] > 2) {
    err.msg <- c(
      err.msg,
      "Please specify only a two-part formula using only one separator '|'."
    )
  }

  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Check if endo are in structural model -----------------------------------------------

  #for the variables
  rhs1.vars <- all.vars(formula(F.formula, rhs = 1, lhs = 0))
  rhs2.vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))

  #RHS2 vars must also be in RHS1
  if (!all(rhs2.vars %in% rhs1.vars)) {
    err.msg <- c(
      err.msg,
      "Please specify every endogenous regressor also in the first right-hand side of the formula."
    )
  }

  # Checks related to RHS2 continuous() ------------------------------------------------

  # Checks for specials function `continuous()`
  names.vars.continuous <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  # At least 1
  if (length(names.vars.continuous) == 0) {
    err.msg <- c(
      err.msg,
      "The method requires at least one continuous endogenous regressor, specified using `continuous()`."
    )
  }

  # Every RHS2 variable must be wrapped in continuous()
  rhs2.calls <- as.list(attr(terms(F.formula, rhs = 2, lhs = 0), "variables"))[-1]
  if (!all(vapply(rhs2.calls, function(e){
    is.call(e) && identical(e[[1]], as.name("continuous"))
  }, logical(1)))) {
    err.msg <- c(
      err.msg,
      "Please wrap every endogenous regressor on the second RHS in continuous()."
    )
  }

  # Variable names in continuous() must match exactly as they appear in RHS1
  # (including any transformations such as log(x) or I(x^2))
  rhs1.labels <- labels(terms(F.formula, rhs = 1, lhs = 0))
  if (!all(names.vars.continuous %in% rhs1.labels)) {
    err.msg <- c(
      err.msg,
      "Please name every endogenous regressor exactly as in the main model, including transformations if any."
    )
  }

  # Check if continuous() in LHS or first RHS ------------------------------------------

  num.specials.rhs1 <- sum(sapply(
    attr(terms(F.formula, rhs = 1, lhs = 0, specials = "continuous"), "specials"),
    length
  ))

  num.specials.lhs <- sum(sapply(
    attr(terms(F.formula, rhs = 0, lhs = 1, specials = "continuous"), "specials"),
    length
  ))

  if (num.specials.rhs1 > 0) {
    err.msg <- c(err.msg, "No endogenous regressors should be on the first RHS.")
  }

  if (num.specials.lhs > 0) {
    err.msg <- c(err.msg, "No endogenous regressors should be on the LHS.")
  }

  return(err.msg)
}
