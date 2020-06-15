# # remove as.character() from formula_readout_special and can reuse
# IIV <- function(...){ return(match.call()[-1])}
# as.list()
# l.args <- lapply(as.list(IIV(g=x2, iiv=gy, X1, X2, P)), as.character)
#
#
#
# exo.names <- l.args[which(names(l.args) == "")]
# g <- l.args[["g"]]
# iiv <- l.args[["iiv"]]




# params.as.chars.only=TRUE returns a vector of chars with the parameter names in it.
#                           Used to readout given param name
#                           Example:  continuous(X1, X2)
#                                   => c("X1", "X2")
# params.as.chars.only=FALSE returns a list of list. The inner list is named after the arg
#                            and the element contains the actual input as char. Elements in sub-lists have
#                            to be named so that they do not get matched to unintended args because of their
#                            position (ie X2 to g)
#                            Example:  IIV(g=x2, iiv=gp, X1, X2) + IIV()
#                                   => list(list(g="x2",iiv="gp", X1="X1",X2="X2"), list(..),..)
#' @importFrom stats terms
formula_readout_special <- function(F.formula, name.special, from.rhs, params.as.chars.only = TRUE){

  F.terms   <- terms(formula(F.formula, lhs=0, rhs=from.rhs), specials = name.special)

  # Read out positions in formula
  # NULL if there are no specials
  ind.special <- attr(F.terms, "specials")[[name.special]]

  # End if there are no such special in the formula
  if(is.null(ind.special))
    return(NULL)

  # Read out commands
  # +1 because first is "list"
  lang.special <- attr(F.terms, "variables")[1L+ind.special]

  # Obtain column name is indicated in the function
  #   create function that is exectuted and simply returns the specified inputs as strings

  # create new function named after special which simply returns the input given
  assign(x = paste0("fct.", name.special), value = function(...){ return(match.call()[-1])})

  # Execute this function for each special by adding "fct." at the beginning and evaluating
  names.special  <- lapply(paste0("fct.",lang.special), FUN=function(str){
                              eval(expr=parse(text=str))})

  if(params.as.chars.only){
    # Return vector with (unique) parameter names only
    names.special        <- lapply(names.special, function(call){as.character(as.list(call))})
    names.special        <- unique(unlist(names.special))
    names(names.special) <- names.special
  }else{
    # return list with names=args, entry = arg data
    # here:list of language object
    names.special <- lapply(names.special, as.list)
    # here: nested list of arg-lists
    names.special <- lapply(names.special, lapply, deparse) # make char representation
    # replace the "", "NULL" as actuall NULLs
    names.special <- lapply(names.special, lapply, function(char.arg){if(char.arg%in%c("NULL", "", "NA")) NULL else char.arg})
    # Name unnamed elements (=args) in sublists after the elements
    names.special <- lapply(names.special, function(subl){
                                            unnamed.ind <- which(names(subl)=="")
                                            names(subl)[unnamed.ind] <- subl[unnamed.ind]
                                            return(subl)})
    # Does not need be unique per list, will not mixup entries if regressors are named same as g/iiv
    #   rather it is caught that they may not be twice
  }

  return(names.special)
}


# Builds a formula consisiting of response ~ main model (RHS1) + data columns
#' @importFrom stats reformulate update.formula
formula_build_mainmodel_data <- function(F.formula, data){
  # intercept=T to not remove if there is one. Will not add one if there is none.
  F.return <- update(formula(F.formula, lhs=1, rhs=1),
                     reformulate(termlabels = c(".", colnames(data)),
                                 response = NULL,
                                 intercept = TRUE))
  return(F.return)
}


lme4formula_get_numberoflevels <- function(l4.form){
  return(length(l4.form$reTrms$flist)+1)
}

lme4formula_get_namesslopes <- function(l4.form){
  return(unique(unname(unlist(l4.form$reTrms$cnms))))
}

lme4formula_get_namesgroups <- function(l4.form){
  return(unique(names(l4.form$reTrms$cnms)))
}


# Builds a formula as input to ivreg
# In:     response ~ complete model | single endogenous | instructions for IV (| EIV)
# Out:    response ~ complete model | all exogenous from complete model + internal IVs from data (+ EIVs)
formula_build_ivreg <- function(F.formula, df.data.iv, vec.desc.IVs){

  # 1st part:
  # DV ~ complete model
  F.part.1 <- formula(F.formula, lhs = 1, rhs = 1)

  # 2nd part:
  # ~ | all exogenous from complete model + IIVs (+ external IVs)

  # All exogenous: complete - endogenous
  labels.all.exo <- setdiff(labels(terms(F.formula, rhs=1, lhs=0)),
                            labels(terms(F.formula, rhs=2, lhs=0)))

  labels.ivs     <- colnames(df.data.iv)
  labels.ext.iv  <- if(length(F.formula)[[2]] == 4)
    labels(terms(F.formula, lhs=0, rhs=4))
  else
    NULL # include external IVs only if present

  # unique() reduces instruments to appear only exactly once
  #
  # For highermomentsIV's IIV data:
  # As the instruments are built from colnames, this applies also to specifed IIV()s, even partial:
  #   IIV(g=x2,iiv=g,X1,X2) + IIV(g=x2,iiv=g,X1, X2) is reduced to a single IIV(g=x2,iiv=g,X1, X2)
  #   IIV(g=x2,iiv=g,X1,X3) + IIV(g=x2,iiv=g,X1, X2) is reduced to IIV(g=x2,iiv=g,X1, X2, X3)
  F.part.2 <- reformulate(termlabels = unique(c(labels.all.exo, labels.ivs, labels.ext.iv)),
                          response   = NULL,
                          intercept  = TRUE)

  # Add 1st and 2nd part to formula
  F.ivreg <- as.Formula(F.part.1, F.part.2)

  # Print the 2nd part of the formula "by hand" because the real formula is ugly (cannot use special chars)
  desc.2nd.part <- paste0(c(labels.all.exo,vec.desc.IVs,labels.ext.iv),collapse = " + ")
  model.desc <- paste0(format(formula(F.ivreg, lhs=1, rhs=1)), "|", desc.2nd.part)

  # Return formula and pretty printed description (because of IVs)
  return(list(F.ivreg=F.ivreg, model.desc = model.desc))

}
