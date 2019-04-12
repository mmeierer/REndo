#' @title Confidence Interval for copula correction models fitted with augmented OLS.
#' @description
#' In the case of only discrete endogenous regressors, the model is re-fitted multiple times
#' and the confidence interval is obtained for each fitted model. The mean of all simulated confidence
#' intervals per parameter is reported.
#'
#' In all other cases, the standard method for a fitted linear model \code{\link[stats]{lm}} is applied.
#'
#' @inheritParams stats::confint
#' @param num.simulations the number of times the model is re-fitted to obtain confidence intervals in case of discrete endogenous regressors only. Ignored with a warning otherwise.
#' @param ... ignored, for consistency with the generic function.
#'
#' @seealso \code{\link[stats]{confint}} for the standard method for linear models
#' @seealso \code{\link{copulaCorrection}} for more information about the model's background
#'
#' @importFrom stats confint confint.lm
#' @export
confint.rendo.pstar.lm <- function(object, parm, level=0.95, num.simulations=250L, ...){

  # Read out needed stuff -------------------------------------------------------------------------------
  # All if parm is missing (needed because parm is unknown in lapply)
  if(missing(parm))
    parm <- names(coef(object))

  names.vars.continuous <- object$names.vars.continuous
  names.vars.discrete   <- object$names.vars.discrete

  # Determine case --------------------------------------------------------------------------------------
  # Forward to lm's confint function for all except "discrete only" which needs simulation
  if(length(names.vars.discrete)>0 & length(names.vars.continuous) == 0){

    # Simulations: Discrete only case

    # Run simulation ------------------------------------------------------------------------------------
    # Run .run.simulation.copulacorrection.discrete num.simulation times and extract the coefs and CI
    # for each regressor (and intercept)
    l.simulated.coefs.and.confints <-
      lapply(X = seq(num.simulations), FUN = function(i){
        res.lm <- copulaCorrection_linearmodel(F.formula             = object$formula,
                                               data                  = object$original.data,
                                               names.vars.continuous = names.vars.continuous,
                                               names.vars.discrete   = names.vars.discrete,
                                               verbose = FALSE, cl=quote(match.call()))
        # Be sure to call lm's confint per fit, otherwise Inf loop
        return(confint.lm(res.lm, parm = parm, level = level))
      })
    single.lm.ci <- l.simulated.coefs.and.confints[[1]]

    # If there are no results (ie all removed) cannot do calculations
    if(nrow(single.lm.ci) == 0)
      return(single.lm.ci)

    # Separate tables for confints
    #   return depends on number of parm given. if only 1, the results is not a matrix
    simulated.confints.lower  <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,1]),
                                        nrow=nrow(single.lm.ci))
    simulated.confints.higher <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,2]),
                                        nrow=nrow(single.lm.ci))

    # Mean of coefs and confints ------------------------------------------------------------------------
    confint.mean           <- cbind(rowMeans(simulated.confints.lower),
                                    rowMeans(simulated.confints.higher))
    dimnames(confint.mean) <- dimnames(single.lm.ci)

    return(confint.mean)
  }else{

    if(!missing(num.simulations))
      warning("The specified parameter for \'num.simulations\' is ignored because this model does not use discrete endogenous regressors only.",
                call. = FALSE, immediate. = TRUE)

    # No discrete only case: Return lm's confint
    return(stats::confint.lm(object = object, parm = parm, level = level))
  }
}
