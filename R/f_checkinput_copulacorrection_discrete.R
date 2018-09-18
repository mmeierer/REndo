
checkinput_copulacorrectiondiscrete_formula <- function(formula, data){
  return(checkinputhelper_formula_2RHS(formula=formula, data=data))
}


#' @importFrom methods is
#' @importFrom stats .MFclass
#' @importFrom Formula as.Formula
checkinput_copulacorrectiondiscrete_data <- function(data, formula){
  err.msg <- checkinputhelper_data_basicstructure(formula=formula, data=data)

  if(length(err.msg)>0)
    return(err.msg)

  return(checkinputhelper_data_notnamed(formula=formula, data=data, forbidden.colname="PStar"))

  # Checking if all columns as given in the formula are present is done in the formula check
}


checkinput_copulacorrectiondiscrete_numsimulations <- function(num.simulations){
  err.msg <- checkinputhelper_singlepositivewholenumeric(num.param=num.simulations, parameter.name = "num.simulations", min.num=2)
  if(length(err.msg)>0)
    return(err.msg)

  if(num.simulations < 100)
    warning("It is recommended to run more than 100 simulations", call. = F, immediate. = T)

  return(c())
}
