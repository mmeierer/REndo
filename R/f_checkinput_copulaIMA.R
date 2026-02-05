#' @importFrom Formula as.Formula
#'
checkinput_copulaIMA_formula <- function(formula){

  err.msg <- .checkinputhelper_formula_basicstructure(formula =formula)
  if(length(err.msg) >0) return(err.msg)

  F.formula <- Formula::as.Formula(formula)

  # check to see if the formula inputed has 2 RHS
  if (length(F.formula)[2] < 2)
    err.msg <- c(err.msg,"Please specify endogenous regressors on a second right-hand side using | (e.g. y ~ X + P | continuous(P)).")

  if(length(F.formula)[2] > 2)
    err.msg <- c(err.msg, "Please specify endogenous regressors using only one | separator.")

  if (length(err.msg) >0) return(err.msg)

  #for the variables
  rhs1.vars <- all.vars(formula(F.formula, rhs =1 , lhs = 0))
  rhs2.vars <- all.vars(formula(F.formula, rhs = 2, lhs = 0))

  #RHS2 vars must also be in RHS1

  if(!all(rhs2.vars %in% rhs1.vars))
    err.msg <- c(err.msg,"Please specify every endogenous regressor also in the first right-hand side of the formula.")

  #Continuous and discrete variables (no discrete should appear for IMA)
  #Reused the same definition as was in REndo - unsure about this

  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous", from.rhs =2, params.as.chars.only =  TRUE)

  #Defining the discrete variables in case user decides to use Copula IMA on discrete too

  names.vars.discrete <- formula_readout_special(F.formula = F.formula, name.special = "discrete", from.rhs = 2, params.as.chars.only =  TRUE)

  # IMA only works with continuous endogenous regressors
  if(length(names.vars.discrete)>0)
    err.msg <- c(err.msg, "IMA only supports continuous endogenous regressors. Remove discrete() or use Copula Correction.")

  #IMA in case there is no continuous variables
  if(length(names.vars.continuous) ==0)
    err.msg <- c(err.msg, "IMA requires at least one continuous endogenous regressor.")

  #RHS2 being in 'continuous()'
  if(!all(rhs2.vars %in% names.vars.continuous))
    err.msg <- c(err.msg, "Please wrap every endogenous regressor on the second RHS in continuous().")

  #checking matching in RHS1
  rhs1.labels <- labels(terms(F.formula, rhs =1 , lhs =0))
  if(!all(names.vars.continuous %in% rhs1.labels))
    err.msg <- c(err.msg, "Please name every endegenous regressor exactly as in the main model, including transformations if any.")

  #checking for specials on RH1 or LHS

  num.specials.rhs1 <- sum(sapply(attr(terms(F.formula, rhs =1, lhs =0, specials = "continuous"), "specials"), length))

  num.specials.lhs <- sum(sapply(attr(terms(F.formula, rhs =0, lhs =1, specials = "continuous"), "specials"), length))

  if (num.specials.rhs1 >0)
    err.msg <- c(err.msg, "No endegenous regressors should be on the first RHS.")

  if(num.specials.lhs >0)
    err.msg <- c(err.msg, "No endegenous regressors should be on the first LHS.")

  return(err.msg)

}

#Checking for data
#used the same as copula correction
checkinput_copulaIMA_data <- function(data){
  .checkinputhelper_data_basicstructure(data = data)
}

#Checking data VS formula
#' @importFrom Formula as.Formula
#'
#'

checkinput_copulaIMA_dataVSformula <- function(data, formula){

  F.formula <- Formula::as.Formula(formula)

  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous", from.rhs = 2, params.as.chars.only = TRUE)

  names.cols.endo <- all.vars(terms(F.formula, rhs = 2, lhs =0))

  err.msg <- .checkinputhelper_dataVSformula_basicstructure(
    formula = F.formula,
    data = data,
    rhs.rel.regr = c(1,2),
    num.only.cols = names.cols.endo
  )

  err.msg <- c(
    err.msg,
    checkinputhelper_data_notnamed(
      formula = F.formula,
      data = data,
      forbidden.colname = "Pstar"
    )
  )

  err.msg
}

#for num.boots and verbose

checkinput_copulaIMA_numboots <- function(num.boots){
  err.msg <- checkinputhelper_singlepositivewholenumeric(
    num.param = num.boots,
    parameter.name = "num.boots",
    min.num = 2
  )

  if(length(err.msg)>0) return(err.msg)

  if(num.boots < 1000)
    warning(
      "It is recommended to run 199 or more bootstraps.",
      call. = FALSE,
      immediate. = TRUE
    )

  c()
}

#checking input for verbose
checkinput_copulaIMA_verbose <- function(verbose){
  checkinputhelper_single_logical(verbose, "verbose")
}

#checking input for cdf

checkinput_copulaIMA_cdf <- function(cdf){

  err.msg <- c()

  cdf.allowed <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")

  if (!is.character(cdf) || length(cdf) !=1){
    err.msg <-c(err.msg, "The argument 'cdf' must be a single character string." )
  }

  if (length(err.msg)==0 && !(cdf %in% cdf.allowed)){

    err.msg <- c(err.msg, paste0("Value for 'cdf' is invalid. The allowed values are: ", paste(cdf.allowed, collapse = ", ")))

  }

  return(err.msg)
}


