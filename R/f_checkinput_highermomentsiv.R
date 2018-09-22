checkinput_highermomentsiv_g <- function(g){
  # g may be NULL/missing because its not needed for all IIVs
  if(is.null(g))
    return(c())

  err.msg <- checkinputhelper_charactervector(vec=g, parameter.name="g",
                                              allowed.inputs=c("x2", "x3", "lnx", "1/x"))
  if(length(err.msg)>0)
    return(err.msg)

  if(length(g) != 1)
    err.msg <- c(err.msg, "Please provide exactly one single element for \'g\'")
  return(err.msg)
}

checkinput_highermomentsiv_iiv <- function(iiv){
  return(checkinputhelper_charactervector(vec=iiv, parameter.name="iiv",
                                              allowed.inputs=c("g", "gp", "gy", "yp", "p2", "y2")))
}

checkinput_highermomentsiv_formula <- function(formula=formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)
  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)
  # Check that formula has exactly 3 RHS (cannot proceed if not 3 RHS are available)
  if(length(F.formula)[2] != 3)
    return("Please specify the formula with exactly 3 parts on the right-hand side")

  # Check that every RHS2 is in RHs
  names.rhs1 <- all.vars(formula(F.formula, rhs=1, lhs=0))
  names.rhs2 <- all.vars(formula(F.formula, rhs=2, lhs=0))
  # RHS2 not in RHS1
  if(!all(names.rhs2 %in% names.rhs1))
    err.msg <- c(err.msg, "Please specify every endogenous regressors also in the first right-hand side (main model) of the formula.")

  # Check that not all RHS1 are endogenous
  if(all(names.rhs1 %in% names.rhs2))
    err.msg <- c(err.msg, "Please do not specify all regressors as endogenous.")



  # Process special for checks
  F.terms.rhs3      <- terms(F.formula, rhs=3, lhs=0, specials = "IIV")
  num.specials.rhs1 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=1, specials = "IIV"), "specials"), length))
  num.specials.rhs2 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=2, specials = "IIV"), "specials"), length))
  num.specials.rhs3 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=3, specials = "IIV"), "specials"), length))
  num.specials.lhs  <- sum(sapply(attr(terms(F.formula, lhs=1, rhs=0, specials = "IIV"), "specials"), length))

  # Check that no other/misspelled/not indicated function
  if( length(labels(F.terms.rhs3)) != num.specials.rhs3)
    err.msg <- c(err.msg, "Please indicate how the IIV shall be constructed in the 3rd right-hand side of the formula")

  # Check that there are no special functions in the first & second RHS
  if(num.specials.rhs1 > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the 1st righ-hand side of the formula.")
  if(num.specials.rhs2 > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the 2nd righ-hand side of the formula.")
  if(num.specials.lhs > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the left-hand side of the formula.")

  # Check if any special is empty or there is none
  if(any(labels(F.terms.rhs3) == "IIV()") | num.specials.rhs3 == 0)
    err.msg <- c(err.msg, "Please specify a variable in every function call on the third right-hand side of the formula.")


  # Read out the special functions
  # Check if any special is empty

  return(err.msg)
}

checkinput_highermomentsiv_iivVSg <- function(g, iiv){
  # g from c("x2", "x3", "lnx", "1/x")
  # IIV from c("g","gp","gy","yp","p2","y2"))

  # need some g if iiv does something with
  if(any(iiv %in% c("g","gp","gy")))
    # g is required
    if(is.null(g))
      # but not given
      return(paste0("Please also specifiy the parameter \'g\' required for the given \'iiv\' parameter \'",
                                 iiv,"\'.", collapse=""))
    else
      # and is given
      return(c())
  else
    # No g required by iiv
    if(!is.null(g)){
      # but g is still given
        warning("The given parameter \'",g,"\' for \'g\' is ignored because the specified \'iiv\' parameter \'",
                iiv,"\' does not require it.", call. = FALSE, immediate. = TRUE)
        return(c()) #return nothing as it is not stop-worthy
    }else
        # and also not given
        return(c())
}

checkinput_highermomentsiv_iivregressors <- function(l.ellipsis, F.formula){
  # The regressors are specified in the ... arg
  err.msg <- c()
  if(length(l.ellipsis) == 0)
    return("Please specify the exogenous regressors to build the internal instruments from in the IIV() function.")

  # Check that all regressors are in the RHS1 (main model)
  if(!all(l.ellipsis %in% all.vars(formula(F.formula, lhs=0,rhs=1))))
    err.msg <- c(err.msg, "Please specifiy in IIV() only regressors that are also present in the first right-hand side (main model) of the formula.")

  # Check that no regressors is in the RHS2 (endo)
  if(any(l.ellipsis %in% all.vars(formula(F.formula, lhs=0,rhs=2))))
    err.msg <- c(err.msg, "Please specifiy only the exogenous but not the endogenous regressors in IIV().")

  # Formula has already been checked to have all regressors in data - therefore if it is in the formula it is also in the data

  return(err.msg)
}
