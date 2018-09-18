checkinput_copulacorrectioncontinuous1_formula <- function(formula, data){
  return(checkinputhelper_formula_2RHS(formula=formula, data=data))
}


checkinput_copulacorrectioncontinuous1_data <- function(formula, data){
  err.msg <- checkinputhelper_data_basicstructure(formula=formula, data=data)

  if(length(err.msg)>0)
    return(err.msg)

  err.msg <- checkinputhelper_data_notnamed(formula = formula, data=data, forbidden.colname="PStar")
  # Check length of second RHS is only 1
  F.formula <- as.Formula(formula)
  if(length(all.vars(formula(F.formula, rhs=2, lhs=0))) > 1)
    err.msg <- c(err.msg, "Please only provide a single endogenous regressor for this method.")

  return(err.msg)

  # Checking if all columns as given in the formula are present is done in the formula check
}


checkinput_copulacorrectioncontinuous1_numboots <- function(num.boots){
  err.msg <- checkinputhelper_singlepositivewholenumeric(num.param=num.boots, parameter.name = "num.boots", min.num=2)
  if(length(err.msg)>0)
    return(err.msg)

  if(num.boots < 10)
    warning("It is recommended to run more than 10 bootstrappings.", call. = F, immediate. = T)

  return(c())
}


checkinput_copulacorrectioncontinuous1_startparams <- function(start.params, formula){
  return(checkinputhelper_startparams(start.params=start.params, formula=formula, forbidden.names=c("rho", "sigma")))
}

checkinput_copulacorrectioncontinuous1_verbose <- function(verbose){
  err.msg <- c()

  # Cannot be missing as has default value
  # no if(missing(verbose))
  #  no return("Please provide the parameter \'verbose\'")

  if(is.null(verbose))
    return("Please provide a single logical for \'verbose\'")

  if(!is.vector(verbose, mode = "logical"))
    err.msg <- c(err.msg, "Please provide a single logical for \'verbose\'")

  if(length(verbose) != 1)
    err.msg <- c(err.msg, "Please provide a single logical for \'verbose\'")

  if(anyNA(verbose))
    err.msg <- c(err.msg, "Please provide no NA(s) for \'verbose\'")

  return(err.msg)
}
