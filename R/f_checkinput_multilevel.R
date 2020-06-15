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
#' @importFrom lme4 lFormula nobars
checkinput_multilevel_dataVSformula <- function(formula,data){

  err.msg <- c()

  F.formula       <- as.Formula(formula)
  names.vars.endo <- formula_readout_special(F.formula = F.formula, name.special = "endo",
                                             from.rhs=2, params.as.chars.only=TRUE)

  # lmer specific function part
  f.lmer <- formula(F.formula, lhs = 1, rhs = 1)

  l4.form <- tryCatch(lme4::lFormula(formula = f.lmer, data = data),
                  error = function(e)return(e))
  if(is(object = l4.form, class2 = "simpleError"))
    if(grepl(pattern = "No random effects terms", x = l4.form$message)){
      # Known error
      return("Please specify a random effects terms in the formula.")
    }else{
      # Some other error
      return(paste0("Please provide a formula interpretable by lme4::lmer().\nError: ",
                    sQuote(l4.form$message)))
    }

  # check number of levels
  if(!(lme4formula_get_numberoflevels(l4.form = l4.form) %in% c(2,3)))
    err.msg <- c(err.msg, "Please specify exactly 2 or 3 levels in the formula.")

  # rely on lformula processing
  names.slopes <- lme4formula_get_namesslopes(l4.form = l4.form)
  names.groups <- lme4formula_get_namesgroups(l4.form = l4.form)
  names.model  <- colnames(l4.form$X)

  # Check that no endogenous is in (|) part
  if(any(names.vars.endo %in% c(names.slopes, names.groups)))
    err.msg <- c(err.msg, "Please specify no endogenous regressor as slopes nor as group id.")

  # Check that no level grouping Id is in model
  if(any(names.groups %in% all.vars(lme4::nobars(f.lmer))))
    err.msg <- c(err.msg, "Please specify no level grouping Id in the model part of the formula.")

  # Check that no level grouping Id is in slope
  if(any(names.groups %in% names.slopes))
    err.msg <- c(err.msg, "Please specify no level grouping Id in the slopes part of the formula.")

  # Check that slopes are exactly like this in model
  if(!all(names.slopes %in% names.model))
    err.msg <- c(err.msg, "Please specify all slopes as well in the model part of the formula.")

  # data contains no NA
  #   cannot rely on lmer as its works with NA (but will block any other non-finites)
  rel.data <- data[, all.vars(f.lmer), drop=FALSE]

  if(anyNA(rel.data))
    err.msg <- c(err.msg, "Please provide no NA values in the data.")

  # Not needed, because lmer() already catches this
  # if(any(!sapply(rel.data[numeric.cols], is.finite)))
    # return("Please provide no non-finite values in the data.")

  return(err.msg)
}


checkinput_multilevel_verbose <- function(verbose){
  return(checkinputhelper_single_logical(logical=verbose, param.name="verbose"))
}


checkinput_multilevel_model <- function(object, model){
  # needs this order for match.arg in this function
  allowed.models <- c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3")

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

  # Check its not empty vector
  if(length(model) < 1)
    err.msg <- c(err.msg, "Please provide exactly one character string to specify the model.")

  # Return now as could break later checks if length()=0
  if(length(err.msg)>0)
    return(err.msg)

  # Check that only one model is given, except if it is the default arguments (ie non given)
  #   which match.arg will reduce to first arg (this is common R behavior)
  if(length(model) > 1 & !isTRUE(all.equal(model,target = allowed.models)))
    err.msg <- c(err.msg, "Please provide exactly one character string to specify the model.")

  # Check its one of the allowed strings
  #   To make sure that match.arg does not fail ungracefully
  #     "FE_L2", "GMM_L2", "FE_L3","GMM_L3" have to match exactly, also in match.arg because
  #       they only differ by the last char
  #   "REF" can be (abreviated with) "R", "RE", "REF"
  allowed.models.all <- c("R", "RE", allowed.models)
  if(!all(model %in% allowed.models.all))
    err.msg <- c(err.msg, paste0("Please (partially) specify one of the following models:",paste(allowed.models,collapse = ","),"."))

  # Could crash the follwing match.arg
  if(length(err.msg)>0)
    return(err.msg)

  # Check that the named model is actually in the fitted model (ie no L3 name in L2 fitted)
  #   check by names and also grepl, just to be sure
  model <- match.arg(arg = model, choices = allowed.models, several.ok = FALSE) # in case all default args given (ie >1 model)
  if(!any(grepl(pattern = model, x = colnames(object$coefficients))) ||
     !any(grepl(pattern = model, x = names(object$l.ovt))) ||
     !any(grepl(pattern = model, x = names(object$l.fitted))) ||
     !any(grepl(pattern = model, x = names(object$l.residuals))))
    err.msg <- c(err.msg, "Please only specify a model that is present in the fitted object.")

  return(err.msg)
}


#' @importFrom methods is
checkinput_multilevel_lmercontrol <- function(lmer.control){
  # Not checked in detail, expect it to be an output from lmerControl

  # Check it is something
  if(missing(lmer.control))
    return("Please provide an ouput from lmerControl() for lmer.control.")
  if(is.null(lmer.control))
    return("Please provide an ouput from lmerControl() for lmer.control.")

  if(!is(lmer.control, "lmerControl"))
    return("Please provide an ouput from lmerControl() for lmer.control.")

  return(c())
}
