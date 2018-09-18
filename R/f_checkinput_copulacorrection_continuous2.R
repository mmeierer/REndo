checkinput_copulacorrectioncontinuous2_formula <- function(formula, data){
  return(checkinputhelper_formula_2RHS(formula=formula, data=data))
}



checkinput_copulacorrectioncontinuous2_data <- function(formula, data){
  err.msg <- checkinputhelper_data_basicstructure(formula=formula, data=data)
  if(length(err.msg) > 0)
    return(err.msg)
  return(checkinputhelper_data_notnamed(formula = formula, data=data, forbidden.colname="PStar"))
}
