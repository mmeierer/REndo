# helper function to to check if there are error messages and if so, print them and stop
check_err_msg <- function(err.msg){
  if(length(err.msg) > 0){
    # cat(err.msg, sep="\n")
    message(paste(err.msg, collapse = "\n"))
    stop("The above errors were encountered!", call. = FALSE)
  }
}



#' Only checks the basic structure of the formula object:
#' Missing, NULL, class, all regressors present in data, single LHS, LHS and RHS not mixed
#' @importFrom methods is
#' @importFrom stats formula terms
#' @importFrom Formula is.Formula as.Formula
.checkinputhelper_formula_basicstructure <- function(formula){ #, data){
  err.msg <- c()
  # if(missing(data) || is.null(data))
  #   return("Please provide a valid data object as \'data\' parameter.")

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

  # **IMplmenet separately and remove here
  # # Check if all regressors are present in the data
  # if(!all(c(name.lhs, name.rhs) %in% colnames(data)))
  #   err.msg <- c(err.msg, "Please provide a data object that contains all the formula's variables.")

  return(err.msg)
}

#' @importFrom Formula as.Formula
checkinputhelper_formula_2RHS <- function(formula, data){
  err.msg <- .checkinputhelper_formula_basicstructure(formula = formula, data=data)
  if(length(err.msg)>0)
    return(err.msg)
  F.formula <- as.Formula(formula)

  # Check that formula has 2 RHS
  if(length(F.formula)[2] < 2)
    err.msg <- c(err.msg, "Please indicate the endogenous regressors in the formula by separating it with a single vertical bar (|). (ie: y~X+P|P)")
  if(length(F.formula)[2] > 2)
    err.msg <- c(err.msg, "Please indicate only the endogenous regressors in the formula by separating it with a single vertical bar (|). Multiple endogenous regressors can be specified additively. (ie: y~X1+P1+P2|P1+P2)")
  # Cannot read out names if not 2 RHS are available
  if(length(err.msg)>0)
    return(err.msg)

  # Read out names as is
  name.lhs       <- all.vars(formula(F.formula, lhs=1, rhs=0))
  name.rhs.model <- all.vars(formula(F.formula, lhs=0, rhs=1))
  name.rhs.endo  <- all.vars(formula(F.formula, lhs=0, rhs=2))

  # Not all endogenous are in main model regressors
  if(!all(name.rhs.endo %in% name.rhs.model))
    err.msg <- c(err.msg, "Please also specify all endogenous regressors also in the main model.")
  if(all(name.rhs.model %in% name.rhs.endo))
    err.msg <- c(err.msg, "Not all regressors can be endogenous.") # **?? really?

  return(err.msg)
}

#' @importFrom Formula as.Formula
checkinputhelper_formula_1RHS <- function(formula, data){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula, data=data)
  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)
  # Check that formula has only 1 RHS
  if(length(F.formula)[2] !=1)
    err.msg <- c(err.msg, "Please only provide only a single right-hand-side in the formula and do not use bars (|).")


  return(err.msg)
}





checkinputhelper_data_basicstructure <- function(formula, data){
  err.msg <- c()

  if(missing(formula) || is.null(formula) || !inherits(formula, "formula"))
    return("Please provide a valid formula as \'formula\' parameter.")

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

  # Check
  # Do not need terms object to expand . (dot) because not yet allowed in formula input.
  # all.vars only returns only names of 1st RHS but 2nd RHS are all in 1stRHS
  # Only allow numeric (real & integer) values in the data
  data.types <- vapply(X = data, FUN = .MFclass, FUN.VALUE = "")
  data.types <- data.types[all.vars(formula)]
  if(any(!(data.types %in% "numeric")))
    err.msg <- c(err.msg, "For all regressors, only numeric data is currently supported.")

  # Checking if all columns as given in the formula are present is done in the formula check

  return(err.msg)
}


#' @importFrom Formula as.Formula
checkinputhelper_data_notnamed <- function(formula, data, forbidden.colname){
  err.msg <- c()
  # Check that no column is named forbiddencol.ENDO
  name.data.endo <- paste(forbidden.colname, all.vars(formula(as.Formula(formula), lhs=0, rhs=2)), sep=".")
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



checkinputhelper_startparams <- function(start.params, formula, forbidden.names){
  err.msg <- c()

  # Do not check anything more as missing/NULL indicates that start.params should be generated with a linear model
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

  # Required names are RHS1 + "(Intercept)" if the formula has one
  f.rhs1 <- formula(as.Formula(formula), lhs=0, rhs=1)
  names.model <- all.vars(f.rhs1)
  if(attr(terms(f.rhs1), "intercept") == 1)
    names.model <- c(names.model, "(Intercept)")

  # same number of params as required by formula
  if(length(start.params) != length(names.model))
    err.msg <- c(err.msg, paste0("Please provide exactly ", length(names.model), " start parameters named ", paste(names.model, collapse = ", "), "."))

  # check that every name is not only "" (ie some are unnamed)
  if(any(names(start.params) == ""))
    err.msg <- c(err.msg, "Please provide names for every parameter in \"start.params\".")

  # check that every required parameter is present as name
  for(n in names.model)
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
      err.msg <- c(err.msg, paste("Please provide only elements from c(",paste(allowed.inputs, collapse = ", "),") for \'",parameter.name,"\'.", sep = ""))

  return(err.msg)
}
