
checkinput_highermomentsiv_formula <- function(formula=formula){
  return(checkinputhelper_formula_IIVs(formula=formula))
}

checkinput_highermomentsiv_data <- function(data){
  return(.checkinputhelper_data_basicstructure(data=data))
}

checkinput_highermomentsiv_formulaVSdata <- function(formula, data){
  return(checkinputhelper_dataVSformula_IIV(formula=formula, data=data))
}

checkinput_highermomentsiv_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}

checkinput_highermomentsiv_iiv <- function(iiv){
  return(checkinputhelper_charactervector(vec=iiv, parameter.name="iiv",
                                          allowed.inputs=c("g", "gp", "gy", "yp", "p2", "y2")))
}

checkinput_highermomentsiv_docalllist <- function(l.args){

  l.err.msg <- lapply(l.args, FUN = function(sublist){
    err.msg <- c()
    # Check that the list containing the parsed args and is used to do.call the IIV function is fine
    if(sum(names(sublist) == "iiv") > 1)
      err.msg <- c(err.msg, "Please specify the parameter \'iiv\' in each IIV() only exactly once.")

    if(sum(names(sublist) == "g") > 1)
      err.msg <- c(err.msg, "Please specify the parameter \'g\' in each IIV() only exactly once.")
    return(err.msg)
  })

  return(unique(unlist(l.err.msg)))
}

checkinput_highermomentsiv_g <- function(g){
  # g may be missing because its not needed for all IIVs

  # Do separately as could be NA for which == fails
  if(length(g) == 0 || nchar(g, keepNA = FALSE)==0)
      return(c())

  err.msg <- checkinputhelper_charactervector(vec=g, parameter.name="g",
                                              allowed.inputs=c("x2", "x3", "lnx", "1/x"))
  if(length(err.msg)>0)
    return(err.msg)

  if(length(g) != 1)
    err.msg <- c(err.msg, "Please provide exactly one single element for \'g\'")
  return(err.msg)
}

checkinput_highermomentsiv_iivVSg <- function(g, iiv){
  # g from c("x2", "x3", "lnx", "1/x")
  # IIV from c("g","gp","gy","yp","p2","y2"))

  # need some g if iiv does something with
  if(any(iiv %in% c("g","gp","gy")))
    # g is required
    if(length(g)==0 || nchar(g, keepNA = FALSE)==0) #is.null(g) alone not enough because can be given, but length==0
      # but not given
      return(paste0("Please also specifiy the parameter \'g\' required for the given \'iiv\' parameter \'",
                                 iiv,"\'.", collapse=""))
    else
      # and is given
      return(c())
  else
    # No g required by iiv
    if(length(g) != 0 && nchar(g, keepNA = FALSE)>0){   #!is.null(g) alone not enough because can be given, but length==0
      # but g is still given
        warning("The given parameter \'",g,"\' for \'g\' is ignored because the specified \'iiv\' parameter \'",
                iiv,"\' does not require it.", call. = FALSE, immediate. = TRUE)
        return(c()) #return nothing as it is not stop-worthy
    }else
        # and also not given
        return(c())
}

checkinput_highermomentsiv_iivregressors <- function(l.iivregressors, F.formula, iiv){
  # The regressors are specified in the ... arg
  err.msg <- c()

  # All IIVs need exo data, except y2, p2,yp
  if(iiv %in% c("y2", "p2", "yp")){
    # No exo regressors needed
    if(length(l.iivregressors) != 0)
      warning("The specified exogenous regressors are ignored because they are not needed to built the internal instruments.",
              call. = FALSE, immediate. = TRUE)
    return(c())
  }
  # Exo regressors needed

  if(length(l.iivregressors) == 0)
    return("Please specify the exogenous regressors to build the internal instruments from in the IIV() function.")

  # Check that all regressors are in the RHS1 (main model)
  if(!all(l.iivregressors %in% labels(terms(F.formula, lhs=0,rhs=1))))
    err.msg <- c(err.msg, "Please specifiy in IIV() only regressors that are also present in the first right-hand side (main model) of the formula.")

  # Check that no regressors is in the RHS2 (endo)
  if(any(l.iivregressors %in% labels(terms(F.formula, lhs=0,rhs=2))))
    err.msg <- c(err.msg, "Please specifiy only the exogenous but not the endogenous regressors in IIV().")

  # Formula has already been checked to have all regressors in data - therefore if it is in the formula it is also in the data

  return(err.msg)
}
