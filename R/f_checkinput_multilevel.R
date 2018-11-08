checkinput_multilevel_formula <- function(formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)

  # ** TODO: more advanced tests **
}

checkinput_multilevel_data <- function(data){
  err.msg <- .checkinputhelper_data_basicstructure(data=data)
}

checkinput_multilevel_dataVSformula <- function(formula,data){
  # F.formula <- as.Formula(formula)
  num.only.cols <- all.vars(formula)
  return(.checkinputhelper_dataVSformula_basicstructure(formula=formula, data=data,
                                                        rhs.rel.regr = 1, ## ** TODO: adapt!**
                                                        num.only.cols = num.only.cols))
}

checkinput_multilevel_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}

checkinput_multilevelsummary_model <- function(model){
  err.msg <- c()
  # Check it is something
  if(missing(model))
    return("Please provide a character string to specify the model.")
  if(is.null(model))
    return("Please provide a character string to specify the model.")

  if(anyNA(model))
    err.msg <- c(err.msg, "Please provide no NA to specify the model.")

  # Check its a character string
  if(!is.vector(x=model, mode = "character"))
    err.msg <- c(err.msg, "Please provide a character string to specify the model.")

  # Check its only a single string
  if(length(model) != 1)
    err.msg <- c(err.msg, "Please provide exactly a single character string to specify the model.")

  # Return now as could break later checks
  if(length(err.msg)>0)
    return(err.msg)

  # Check its one of the allowed strings
  allowed.models <- c("REF", "FE_L2", "GMM_L2", "FE_L3","GMM_L3")
  if(!model %in% allowed.models)
    err.msg <- c(err.msg, paste0("Please specify one of the following models:",paste(allowed.models,collapse = ","),"."))

  return(err.msg)
}
