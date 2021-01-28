#' @importFrom Formula as.Formula
checkinput_copulacorrection_formula <- function(formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)
  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)

  # Check that formula has 2 RHS
  if(length(F.formula)[2] < 2)
    err.msg <- c(err.msg, "Please indicate the endogenous regressors in the formula in a second right-hand-side separaed by a single vertical bar (|). (ie: y~X+P|continuous(P)")
  if(length(F.formula)[2] > 2)
    err.msg <- c(err.msg, "Please indicate only the endogenous regressors in the formula by separating it with a single vertical bar (|). Multiple endogenous regressors can be specified additively. (ie: y~X1+P1+P2|continuous(P1)+continuous(P2)")
  # Cannot proceed if not 2 RHS are available
  if(length(err.msg)>0)
    return(err.msg)

  # Check that every RHS2 (endo) is in RHS1(complete model)
  names.rhs1 <- all.vars(formula(F.formula, rhs=1, lhs=0))
  names.rhs2 <- all.vars(formula(F.formula, rhs=2, lhs=0))
  # RHS2 not in RHS1
  if(!all(names.rhs2 %in% names.rhs1))
    err.msg <- c(err.msg, "Please specify every endogenous regressors also in the first right-hand side (main model) of the formula.")

  # Read out the special functions
  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous",
                                                   from.rhs=2, params.as.chars.only=TRUE)
  names.vars.discrete   <- formula_readout_special(F.formula = F.formula, name.special = "discrete",
                                                   from.rhs=2, params.as.chars.only=TRUE)

  # Check that every endo regressor is also in the RHS1 exactly like this
  if(!all(c(names.vars.continuous, names.vars.discrete) %in% labels(terms(F.formula, rhs=1, lhs=0))))
    err.msg <- c(err.msg, "Please name every endogenous exactly as it is in the main model, incl. transformations (ie y~X+log(P)|continuous(log(P))).")

  # Same regressor cannot be continuous and discrete at the same time
  if(any(names.vars.continuous %in% names.vars.discrete))
    err.msg <- c(err.msg, "Please specify each regressors only as either continuous or discrete but not both at the same time.")

  # Check that any given
  if(length(names.vars.discrete) == 0 & length(names.vars.continuous) ==0)
    return("Please indicate for every endogenous regressor in the second right-hand side if it is either continuous or discrete by using the respective specification.")

  # Process specials for checks
  F.terms.rhs2      <- terms(F.formula, rhs=2, lhs=0, specials = c("continuous", "discrete"))
  num.specials.rhs1 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=1, specials = c("continuous", "discrete")), "specials"), length))
  num.specials.rhs2 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=2, specials = c("continuous", "discrete")), "specials"), length))
  num.specials.lhs  <- sum(sapply(attr(terms(F.formula, lhs=1, rhs=0, specials = c("continuous", "discrete")), "specials"), length))

  # Check if any special is empty
  if(any(labels(F.terms.rhs2) %in% c("continuous()", "discrete()")))
    err.msg <- c(err.msg, "Please specify a variable in every function call on the second right-hand side of the formula.")

  # Check that no other/misspelled/not indicated function
  if( length(labels(F.terms.rhs2)) != num.specials.rhs2)
    err.msg <- c(err.msg, "Please indicate for every endogenous regressor in the second right-hand side if it is either continuous or discrete by using the respective specification.")

  # Check that there are no special functions in the first RHS, LHS
  if(num.specials.rhs1 > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the first righ-hand side of the formula.")
  if(num.specials.lhs > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the left-hand side of the formula.")


  # Check that the specials contains no transformations / functions (ie discrete(log(P))) and are specified additively
  # allowed.names.rhs2 <- c("~", "+", "discrete", "continuous", all.vars(formula(F.formula, rhs=1, lhs=0)))
  # if(length(setdiff(all.names(F.terms.rhs2),allowed.names.rhs2))>0)
  #   err.msg <- c(err.msg, "Please specify endogenous regressor additively and without transformations.")

  # Pluses (+) in the specials escape the previous check. Count the number of pluses to check
  # num.pluses <- sum(all.names(F.terms.rhs2) == "+")
  # if(num.pluses > length(labels(F.terms.rhs2)) - 1)
  #   err.msg <- c(err.msg, "Please specify multiple endogenous regressors with commas instead of with pluses (ie. continuous(P1, P2) instead of continuous(P1+P2)).")


  # Check that every regressor is in the data is done in separate function

  return(err.msg)
}

checkinput_copulacorrection_data <- function(data){
  return(.checkinputhelper_data_basicstructure(data=data))
}

#' @importFrom Formula as.Formula
checkinput_copulacorrection_dataVSformula <- function(data, formula, names.cols.endo){
  F.formula <- as.Formula(formula)

  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous",
                                                   from.rhs=2, params.as.chars.only=TRUE)
  names.vars.discrete   <- formula_readout_special(F.formula = F.formula, name.special = "discrete",
                                                   from.rhs=2, params.as.chars.only=TRUE)

  # Need names of column in data, not as they are in specials (potentially transformed names)
  names.cols.endo <- all.vars(terms(F.formula, rhs=2, lhs=0))

  err.msg <- .checkinputhelper_dataVSformula_basicstructure(formula=F.formula, data=data,
                                                            rhs.rel.regr=c(1,2),
                                                            num.only.cols = names.cols.endo)

  err.msg <- c(err.msg, checkinputhelper_data_notnamed(formula=F.formula, data=data,
                                                       forbidden.colname="PStar"))

  return(err.msg)
}

checkinput_copulacorrection_numboots <- function(num.boots){
  err.msg <- checkinputhelper_singlepositivewholenumeric(num.param=num.boots,
                                              parameter.name = "num.boots", min.num=2)
  if(length(err.msg)>0)
    return(err.msg)

  if(num.boots < 1000)
    warning("It is recommended to run 1000 or more bootstraps.", call. = F, immediate. = T)

  return(c())
}


checkinput_copulacorrection_startparams <- function(start.params, F.formula, m.endo.exo){
  # The required names have to match the cols of model.matrix
  #   exo data may contain non-numeric data that results in multiple specific names

  return(checkinputhelper_startparams(start.params=start.params, F.formula=F.formula,
                                      forbidden.names = c("rho", "sigma"),
                                      required.names  = colnames(m.endo.exo)))
}


checkinput_copulacorrection_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}


checkinput_copulacorrection_warnbinomialendodata <- function(data, names.vars.continuous, names.vars.discrete){

  # Warn if the data is binomial
  fails <- c()

  # Check for every endogenous regressor if its binomial/dummy
  for(name in c(names.vars.discrete, names.vars.continuous))
    if(length(unique(data[, name])) == 2)
      fails <- c(fails, name)

  if(length(fails)>0)
    warning("Note that the endogenous regressor(s) ",paste(name, collapse=",")," may not be binomial!",
              call. = FALSE, immediate. = TRUE)
  # Nothing to return
}


checkinput_copulacorrection_optimxargs <- function(optimx.args){
  return(checkinputhelper_optimxargs(optimx.args = optimx.args))
}
