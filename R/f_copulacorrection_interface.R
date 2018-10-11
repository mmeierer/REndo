#' @title  Fitting Linear Models Endogeneous Regressors using Gaussian Copula
#' @description
#' Fits linear models with continuous or discrete endogeneous regressors using Gaussian copulas, method presented in Park and Gupta (2012).
#' This is a statistical technique to address the endogeneity problem, where no external instrumental variables are needed.
#' The important assumption of the model is that the endogeneous variables should NOT be normally distributed.
#'
#' @details
#' For the case of a single continuous.
#'
#' Something about how the formula is given.
#'
#' Something about the confidence interval for discrete only the performed.
#'
#' @template template_param_formuladataverbose
#' @param ... Arguments for the log-likleihood optimization function in the case of a single continuous endogenous
#'  regressor. Ignored with a warning otherwise.
#' \describe{
#' \item{start.params}{A named vector containing a set of parameters to use in the first optimization iteration.
#' The names have to correspond exactly to the names of the components specified in the formula parameter.
#' If not provided, a linear model is fitted to derive them.}
#' \item{num.boots}{Number of bootstrapping iterations. Defaults to 1000.}
#' }
#'
#' @return
#'
#'
#' @references   Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86.
#'
#' @examples
#' data("dataCopC1")
#' data("dataCopC2")
#' data("dataCopDis")
#' data("dataCopDisCont")
#'
#' \notrun{
#' # Single continuous: log-likelihood optimization
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), data=dataCopC1)
#' # same as above, with start.parameters and number of bootstrappings
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), num.boots=500, data=dataCopC1,
#'                        start.params = c("(Intercept)"=1, X1=1, X2=-2, P=-1))
#' }
#'
#' # All following examples fit linear model with Gaussian copulas
#'
#' # 2 continuous endogenous regressors
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2), data=dataCopC2)
#' # same as above
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1)+continuous(P2), data=dataCopC2)
#'
#' # single discrete endogenous regressor
#' d1 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1), data=dataCopDis)
#' # two discrete endogenous regressor
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+discrete(P2), data=dataCopDis)
#' # same as above
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1, P2), data=dataCopDis)
#'
#' # single discrete, single continuous
#' cd <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+continuous(P2), data=dataCopDisCont)
#'
#' @importFrom Formula as.Formula
#' @importFrom methods formalArgs
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- quote(match.call())
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))

  # **?? CHECK ASSUMPTION THAT ENDOGENOUS IS NOT NORMAL ??

  # Read out specials ------------------------------------------------------------------------------------------
  F.formula <- as.Formula(formula)
  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous",
                                                   from.rhs=2, params.as.chars.only=TRUE)
  names.vars.discrete   <- formula_readout_special(F.formula = F.formula, name.special = "discrete",
                                                   from.rhs=2, params.as.chars.only=TRUE)


  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE

  # Dispatch to either LL optimization or PStar+lm -------------------------------------------------------------
  if(optimizeLL){
    # Single continuous - copula method 1 (optimize LL)
    res <- do.call(what = copulaCorrection_optimizeLL,
                   args = c(list(F.formula=F.formula, data=data,
                                 name.var.continuous =names.vars.continuous,
                                 verbose=verbose, cl=cl), # cl supplied to create return object
                            l.ellipsis))
  }else{
    # All other cases use pstar data + lm
    res <- do.call(copulaCorrection_linearmodel,
                   c(list(F.formula = F.formula, data = data,verbose=verbose,
                          cl=cl, # cl supplied to create return object
                          names.vars.continuous = names.vars.continuous,
                          names.vars.discrete   = names.vars.discrete),
                   l.ellipsis))
  }

  return(res)
}

