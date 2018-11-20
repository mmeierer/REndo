checkinput_multilevel_formula <- function(formula){
  err.msg <- .checkinputhelper_formula_basicstructure(formula=formula)

  if(length(err.msg)>0)
    return(err.msg)

  F.formula <- as.Formula(formula)

  # Formula should be of structure: y~X1+X2+(1+X3|CID)+(1+X1) | endo(X2)

  # as Formula, this is exactly 2 parts
  if(length(F.formula)[[2]] != 2)
    err.msg <- c(err.msg, "Please provide exactly two parts in the formula.")

  # Cannot proceed if not 2 RHS are available
  if(length(err.msg)>0)
    return(err.msg)

  # Check that every RHS2 (endo) is in RHS1 (complete model)
  names.rhs1 <- all.vars(formula(F.formula, rhs=1, lhs=0))
  names.rhs2 <- all.vars(formula(F.formula, rhs=2, lhs=0))
  if(!all(names.rhs2 %in% names.rhs1))
    err.msg <- c(err.msg, "Please specify every endogenous regressors also in the first right-hand side (main model) of the formula.")

  # Read out the special functions
  names.vars.endo <- formula_readout_special(F.formula = F.formula, name.special = "endo",
                                             from.rhs=2, params.as.chars.only=TRUE)

  # Check that any given
  if(length(names.vars.endo) == 0)
    return("Please indicate at least one endogenous regressor in the second right-hand side.")

  # Check that every endo regressor is also in the RHS1 exactly like this
  if(!all(names.vars.endo %in% labels(terms(F.formula, rhs=1, lhs=0))))
    err.msg <- c(err.msg, "Please name every endogenous exactly as it is in the main model, incl. transformations.")

  # Process specials for checks
  F.terms.rhs2      <- terms(F.formula, rhs=2, lhs=0, specials = "endo")
  num.specials.rhs1 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=1, specials = "endo"), "specials"), length))
  num.specials.rhs2 <- sum(sapply(attr(terms(F.formula, lhs=0, rhs=2, specials = "endo"), "specials"), length))
  num.specials.lhs  <- sum(sapply(attr(terms(F.formula, lhs=1, rhs=0, specials = "endo"), "specials"), length))

  # Check if any special is empty
  if(any(labels(F.terms.rhs2) == "endo()"))
    err.msg <- c(err.msg, "Please specify a variable in endo() on the second right-hand side of the formula.")

  # Check that no other/misspelled/not indicated function
  if( length(labels(F.terms.rhs2)) != num.specials.rhs2)
    err.msg <- c(err.msg, "Please indicate endogenous regressors only using endo().")

  # Check that there are no special functions in the first RHS, LHS
  if(num.specials.rhs1 > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the first righ-hand side of the formula.")
  if(num.specials.lhs > 0)
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the left-hand side of the formula.")

  # Check that not all RHS1 are endogenous
  if(all(all.vars(formula(F.formula, rhs=1, lhs=0)) %in%  names.vars.endo))
    err.msg <- c(err.msg, "Please do not specify all regressors as endogenous.")



  # Check that every regressor is in the data is done in separate function

  return(err.msg)
}

checkinput_multilevel_data <- function(data){
  err.msg <- .checkinputhelper_data_basicstructure(data=data)
}

#' @importFrom methods is
#' @importFrom Formula as.Formula
#' @importFrom lme4 lFormula
checkinput_multilevel_dataVSformula <- function(formula,data){
  F.formula     <- as.Formula(formula)
  num.only.cols <- all.vars(F.formula)
  err.msg <- .checkinputhelper_dataVSformula_basicstructure(formula=formula, data=data,
                                                            rhs.rel.regr = 1,
                                                            num.only.cols = num.only.cols)
  if(length(err.msg) > 0)
    return(err.msg)

  # lmer specific function part
  f.lmer <- formula(F.formula, lhs = 1, rhs = 1)

  # check that it can be interpreted by lmer..??
  l4.form <- tryCatch(lme4::lFormula(formula = f.lmer, data = data),
                  error = function(e)return(e))
  if(is(object = l4.form, class2 = "simpleError"))
    if(grepl(pattern = "No random effects terms", x = l4.form$message)){
      # Known error
      return("Please specify a random effects terms in the formula.")
    }else{
      # Some other error
      return("Please provide a formula interpretable by lme4::lmer.")
    }

  # check number of levels
  if(!(lme4formula_get_numberoflevels(l4.form = l4.form) %in% c(2,3)))
    err.msg <- c(err.msg, "Please specify exactly 2 or 3 levels in the formula.")

  # Check that no endogenous is in (|) part
  # Read out the special functions
  names.vars.endo <- formula_readout_special(F.formula = F.formula, name.special = "endo",
                                             from.rhs=2, params.as.chars.only=TRUE)

  l.bars <- lme4::findbars(term = f.lmer) #read out brackets (x|s)
  l.bars <- lapply(l.bars, function(b){as.Formula(paste0("~", deparse(b)))}) # make char, add ~, make Formula
  l.bars <- lapply(l.bars, function(F.f){all.vars(formula(F.f, rhs=1, lhs=0))})
  if(any(names.vars.endo %in% unlist(l.bars)))
    err.msg <- c(err.msg, "Please specify no endogenous regressor in the brackets part \'(x|x)\' of the formula.")


  return(err.msg)
}


checkinput_multilevel_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}


checkinput_multilevel_model <- function(object, model){
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
    return(paste0("Please specify one of the following models:",paste(allowed.models,collapse = ","),"."))


  # Check that the named model is actually in the fitted model (ie no L3 name in L2 fitted)
  #   check by names and also grepl, just to be sure
  if(!any(grepl(pattern = model, x = colnames(object$coefficients))) ||
     !any(grepl(pattern = model, x = names(object$l.ovt))) ||
     !any(grepl(pattern = model, x = names(object$l.fitted))) ||
     !any(grepl(pattern = model, x = names(object$l.residuals))))
    err.msg <- c(err.msg, "Please only specify a model that is present in the fitted object.")

  return(err.msg)
}
