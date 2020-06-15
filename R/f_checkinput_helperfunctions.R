# helper function to to check if there are error messages and if so, print them and stop
check_err_msg <- function(err.msg){
  if(length(err.msg) > 0){
    # cat(err.msg, sep="\n")
    message(paste(err.msg, collapse = "\n"))
    stop("The above errors were encountered!", call. = FALSE)
  }
}



# Only checks the basic structure of the formula object:
# Missing, NULL, class, all regressors present in data, single LHS, LHS and RHS not mixed
#' @importFrom methods is
#' @importFrom stats formula terms
#' @importFrom Formula is.Formula as.Formula
.checkinputhelper_formula_basicstructure <- function(formula){
  err.msg <- c()

  if(missing(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  if(is.null(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")
  # Check if it is a formula
  if(!is(object = formula,class2 = "formula") && !is.Formula(formula))
    return("Please provide a valid formula object as \'formula\' parameter.")

  F.formula <- as.Formula(formula)

  # Check that formula has 1 LHS
  if(length(F.formula)[1] == 0)
    err.msg <- c(err.msg, "Please specify a single dependent variable.")
  if(length(F.formula)[1] > 1 || length(all.vars(formula(F.formula, rhs=0, lhs=1))) > 1)
    err.msg <- c(err.msg, "Please specify only a single dependent variable.")

  # Formula too badly structured, unable to do further tests
  if(length(err.msg)>0)
    return(err.msg)

  # Read out names as is
  name.lhs  <- all.vars(formula(F.formula, lhs=1, rhs=0))
  name.rhs  <- all.vars(formula(F.formula, lhs=0))

  if(any("." %in% c(name.lhs, name.rhs)))
    return("Please provide a formula without dot expression (.). This is not (yet) supported.")

  # Check if LHS are not also on RHS and vice-versa
  if(any(name.lhs %in% name.rhs))
    err.msg <- c(err.msg, "The dependent variable cannot also be an independent variable.")
  if(any(name.rhs %in% name.lhs))
    err.msg <- c(err.msg, "The independent variables cannot also be a dependent variable.")

  return(err.msg)
}



.checkinputhelper_data_basicstructure <- function(data){
  err.msg <- c()

  if(missing(data))
    return("Please provide a data parameter.")

  if(is.null(data))
    return("Please provide a data parameter.")

  # Check right data type
  if(!is.data.frame(data))
    return("Please provide the data as a data.frame.")

  if(ncol(data) < 1)
    err.msg <- c(err.msg, "Please provide a data object containing columns.")
  if(nrow(data) < 1)
    err.msg <- c(err.msg, "Please provide a data object containing observations.")

  # Checking if all columns as given in the formula are present in the dataVSformula methods

  return(err.msg)
}

#' @importFrom stats .MFclass
.checkinputhelper_dataVSformula_basicstructure <- function(formula, data, rhs.rel.regr,
                                                           num.only.cols){
  # when here, the basic structure of data and formula are guaranteed to be correct
  err.msg <- c()
  F.formula <- as.Formula(formula)
  # Do not need terms object to expand . (dot) because not yet allowed in formula input.

  # Check that every regressor is in the data
  if(!all(all.vars(formula(F.formula, rhs=rhs.rel.regr)) %in% colnames(data)))
    return("Please provide a data object that contains all the formula's variables.")

  # Only allow numeric (real & integer) values in the specified columns
  data.types <- vapply(X = data, FUN = .MFclass, FUN.VALUE = "")
  data.types.have.to.be.num <- data.types[num.only.cols]

  if(any(!(data.types.have.to.be.num %in% "numeric")))
    err.msg <- c(err.msg, paste0("Please only provide numeric data for the regressors ",
                                 paste(num.only.cols, collapse = ", "), "."))

  # do not allow non.finite values in any of the relevant columns
  rel.data <- data[, all.vars(formula(F.formula, rhs=rhs.rel.regr)), drop=FALSE]

  # Check for NA among all variables, incl character
  #  also check for NA first with anyNA first because much faster than !is.finite
  if(anyNA(rel.data)){
    err.msg <- c(err.msg, "Please do not provide any NA values in the data.")
    return(err.msg)
  }

  #  Do is.finite only do for numeric data and if dont have
  #   any NAs (alloc mem for whole dataset)
  #   !sapply same as sapply(,!is.finite)
  numeric.are.finite <- sapply(rel.data, function(col){
    if(is.numeric(col))
      return(is.finite(col))
    else
      # all other types (chars etc) are seen as not finite.
      # Checking for NA not needed because done before
      return(TRUE)})

  if(any(!unlist(numeric.are.finite)))
    err.msg <-c(err.msg, "Please do not provide any non-finite values in the data.")

  return(err.msg)
}


#' @importFrom Formula as.Formula
checkinputhelper_data_notnamed <- function(formula, data, forbidden.colname){
  err.msg <- c()
  # Check that no column is named forbiddencol.ENDO
  name.data.endo <- paste(forbidden.colname,
                          all.vars(formula(as.Formula(formula), lhs=0, rhs=2)), sep=".")
  if(any(name.data.endo %in% colnames(data)))
    err.msg <- c(err.msg, paste0("Please name no column in the data \'",forbidden.colname,".ENDOGENOUSREGRESSOR\"."))
  return(err.msg)
}


checkinputhelper_singlepositivewholenumeric <- function(num.param, parameter.name, min.num=1){
  if(missing(num.param))
    return(paste("Please provide a parameter for \'",parameter.name,"\'.", sep = ""))

  if(is.null(num.param))
    return(paste("Please provide a parameter for \'",parameter.name,"\'.", sep = ""))

  if(anyNA(num.param))
    return(paste("Please provide no NA(s) for \'",parameter.name,"\'.", sep = ""))

  if(!is.numeric(num.param))
    return(paste("Please provide a numeric parameter for \'",parameter.name,"\'.", sep = ""))

  if(length(num.param) != 1)
    return(paste("Please provide exactly one single number for \'",parameter.name,"\'.", sep = ""))

  if(num.param < min.num)
    return(paste("Please provide a number greater than ",min.num-1," for \'",parameter.name,"\'.", sep = ""))

  if(num.param %% 1 != 0)
    return(paste("Please provide a whole number for \'",parameter.name,"\'.", sep = ""))
  return(c())
}


checkinputhelper_single_logical <- function(logical, param.name){
  err.msg <- c()

  # Cannot be real missing as has default value
  # no if(missing(verbose))
  #  no return("Please provide the parameter \'verbose\'")

  if(is.null(logical))
    return(paste0("Please provide a single logical for \'",param.name,"\'"))

  if(!is.vector(logical, mode = "logical"))
    err.msg <- c(err.msg, paste0("Please provide a single logical for \'",param.name,"\'"))

  if(length(logical) != 1)
    err.msg <- c(err.msg, paste0("Please provide a single logical for \'",param.name,"\'"))

  if(anyNA(logical))
    err.msg <- c(err.msg, paste0("Please provide no NA(s) for \'",param.name,"\'"))

  return(err.msg)

}


checkinputhelper_startparams <- function(start.params, F.formula,
                                         forbidden.names, required.names){
  err.msg <- c()

  # Do not check anything more as missing/NULL indicates that start.params should be generated automatically
  if(is.null(start.params))
    return(c())

  # is numeric vector
  if(!is.vector(start.params, mode = "numeric"))
    return("Please provide a numeric vector for \'start.params\'")

  # has no NA
  if(anyNA(start.params))
    err.msg <- c(err.msg, "Please provide no NA(s) in \'start.params\'.")

  # is generally named
  if(is.null(names(start.params)))
    err.msg <- c(err.msg, "Please provide a named vector/list as \"start.params\".")

  # same number of params as required by formula
  if(length(start.params) != length(required.names))
    err.msg <- c(err.msg, paste0("Please provide exactly ", length(required.names), " start parameters named ", paste(required.names, collapse = ", "), "."))

  # check that every name is not only "" (ie some are unnamed)
  if(any(names(start.params) == ""))
    err.msg <- c(err.msg, "Please provide names for every parameter in \"start.params\".")

  # check that every required parameter is present as name
  for(n in required.names)
    if(!(n %in% names(start.params)))
      err.msg <- c(err.msg, paste0("Please provide the start parameter for ", n, "."))

    # check that no parameter is named a forbidden name
    if(any(names(start.params) %in% forbidden.names))
      err.msg <- c(err.msg, paste0("Please provide none of the following parameters:",paste(paste0("\'", forbidden.names, "\'"), collapse = ","),"."))


    # Exactly one for every formula var: Done with length check & that every is present
    return(err.msg)
}

checkinputhelper_charactervector <- function(vec, parameter.name, allowed.inputs){
  err.msg <- c()
  if(missing(vec))
    return(paste("Please provide a parameter for \'",parameter.name,"\'.", sep = ""))

  if(is.null(vec))
    return(paste("Please provide a parameter for \'",parameter.name,"\'.", sep = ""))

  if(anyNA(vec))
    err.msg <- c(err.msg, paste("Please provide no NA(s) for \'",parameter.name,"\'.", sep = ""))

  if(!is.character(vec))
    err.msg <- c(err.msg, paste("Please provide a character vector for \'",parameter.name,"\'.", sep = ""))

  if(length(vec) < 1)
    err.msg <- c(err.msg, paste("Please provide at least one element for \'",parameter.name,"\'.", sep = ""))

  if(length(allowed.inputs)>0)
    if(!all(vec %in% allowed.inputs))
      err.msg <- c(err.msg, paste("Please provide only elements from c(",paste(allowed.inputs, collapse = ", "),
                                  ") for \'",parameter.name,"\'.", sep = ""))

    return(err.msg)
}


checkinputhelper_formula_IIVs <- function(formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)
  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)
  # Check that formula has exactly 3 RHS (cannot proceed if not 3 RHS are available)
  if(length(F.formula)[2] != 3 && length(F.formula)[2] != 4)
    return("Please specify the formula with exactly 3 or 4 parts on the right-hand side")

  # Check that every RHS2 is in RHs
  # all.vars do not account for transformations, ie log(P) and P is the same. Use labels(terms) instead
  names.rhs1 <- labels(terms(formula(F.formula, rhs=1, lhs=0)))
  names.rhs2 <- labels(terms(formula(F.formula, rhs=2, lhs=0)))
  # RHS2 not in RHS1
  if(!all(names.rhs2 %in% names.rhs1))
    err.msg <- c(err.msg, "Please specify every endogenous regressors also in the first right-hand side (main model) of the formula.")

  # Check that not all RHS1 are endogenous
  if(all(names.rhs1 %in% names.rhs2))
    err.msg <- c(err.msg, "Please do not specify all regressors as endogenous.")

  # Check that only single endogenous is given
  if(length(names.rhs2) > 1)
    err.msg <- c(err.msg, "Please specify only a single regressor as endogenous.")

  # Process special for checks
  F.terms.rhs3      <- terms(F.formula, rhs=3, lhs=0, specials = "IIV")
  num.specials.rhs1 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=1, specials = "IIV"), "specials"), length))
  num.specials.rhs2 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=2, specials = "IIV"), "specials"), length))
  num.specials.rhs3 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=3, specials = "IIV"), "specials"), length))
  num.specials.lhs  <- sum(sapply(attr(terms(F.formula, lhs=1, rhs=0, specials = "IIV"), "specials"), length))

  # Check that no other/misspelled/not indicated function
  if( length(labels(F.terms.rhs3)) != num.specials.rhs3)
    err.msg <- c(err.msg, "Please indicate how the IIV shall be constructed in the 3rd right-hand side of the formula")

  # Check that there are no special functions in the first & second RHS (& EIV)
  if(num.specials.rhs1 > 0)
    err.msg <- c(err.msg, "Please specify no IIV() in the 1st righ-hand side of the formula.")
  if(num.specials.rhs2 > 0)
    err.msg <- c(err.msg, "Please specify no IIV() in the 2nd righ-hand side of the formula.")
  if(num.specials.lhs > 0)
    err.msg <- c(err.msg, "Please specify no IIV() in the left-hand side of the formula.")
  # check for EIV if present
  if(length(F.formula)[2] == 4)
    if(sum(sapply(attr(terms(F.formula, lhs=0, rhs=4, specials = "IIV"), "specials"), length)) > 0)
      err.msg <- c(err.msg, "Please specify no IIV in the 4th righ-hand side of the formula.")


  # Check if any special is empty or there is none
  if(any(labels(F.terms.rhs3) == "IIV()") | num.specials.rhs3 == 0)
    err.msg <- c(err.msg, "Please specify a variable in every function call on the third right-hand side of the formula.")

  return(err.msg)
}

#' @importFrom Formula as.Formula
checkinputhelper_dataVSformula_IIV <- function(formula, data){
  F.formula <- as.Formula(formula)
  relevant.cols.for.datacols <- if(length(F.formula)[[2]] == 4) c(1,2,4) else c(1,2)

  # Num only: Endogenous + exogenous in the IIV()s
  #   Have to use intersect() because params to IIVs (x2,gp,..) are recognized as vars
  exo.cols.IIV  <- intersect(all.vars(terms(F.formula, lhs=0, rhs=1)),
                             all.vars(terms(F.formula, lhs=0, rhs=3)))
  num.only.cols <- union(all.vars(terms(F.formula, lhs=0, rhs=2)), # endo
                         exo.cols.IIV)

  err.msg <- .checkinputhelper_dataVSformula_basicstructure(formula=F.formula, data=data,
                                                            rhs.rel.regr = relevant.cols.for.datacols,
                                                            num.only.cols = num.only.cols)

  # No need to check that columns are not named iiv or g because that is caught as given double

  return(err.msg)
}


#' @importFrom optimx optimx
#' @importFrom methods formalArgs is
#' @importFrom utils getFromNamespace
checkinputhelper_optimxargs <- function(optimx.args){
  err.msg <- c()
  if(is.null(optimx.args))
    return("The parameter \"optimx.args\" may not be NULL!")

  if(!is(object = optimx.args, class2 = "list"))
    return("Please provide \"optimx.args\" as a list!")

  # further checks only if not empty list (ie no argument passed)
  if(length(optimx.args) > 0){

    if(is.null(names(optimx.args)))
      err.msg <- c(err.msg, "Please provide a named list for \"optimx.args\"!")

    for(n in names(optimx.args))
      if(nchar(n) < 1){
        err.msg <- c(err.msg, "Please provide names for every element in \"optimx.args\"!")
        break
      }

    # the names need to match the inputs to optimx
    optimx.allowed <- formalArgs(getFromNamespace("optimx",ns="optimx"))
    # optimx.allowed <- c("gr", "hess", "lower", "upper", "method", "itnmax", "hessian")
    for(n in names(optimx.args))
      if(!(n %in% optimx.allowed))
        err.msg <- c(err.msg, paste0("The element ",n," in optimx.args is not a valid input to optimx()!"))

    # only one method allowed
    if("method" %in% names(optimx.args))
      if(length(optimx.args$method) > 1)
        err.msg <- c(err.msg, paste0("Only a single method can be used at a time!"))

  }
  return(err.msg)
}
