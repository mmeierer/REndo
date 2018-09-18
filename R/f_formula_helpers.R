#' @importFrom stats terms
formula_readout_special <- function(F.formula, name.special, from.rhs=2){
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
  assign(x = paste0("fct.", name.special), value = function(...){ return(as.character(match.call()[-1]))})

  # Execute this function for each special by adding "fct." at the beginning and evaluating
  names.special  <- lapply(paste0("fct.",lang.special), FUN=function(str){
    eval(expr=parse(text=str))})

  names.special        <- unique(unlist(names.special))
  names(names.special) <- names.special
  return(names.special)
}


#' Builds a formula consisiting of response ~ main model (RHS1) + data columns
#' @importFrom stats reformulate update.formula
formula_build_mainmodel_data <- function(F.formula, data){
  # intercept=T to not remove if there is one. Will not add one if there is none.
  F.return <- update(formula(F.formula, lhs=1, rhs=1),
                     reformulate(termlabels = c(".", colnames(data)),
                                 response = NULL,
                                 intercept = T))
  return(F.return)
}
