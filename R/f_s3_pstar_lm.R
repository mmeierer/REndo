#' @importFrom stats confint confint.lm
#' @export
confint.rendo.pstar.lm <- function(object, parm, level=0.95, num.simulations=250L, ...){

  # -----------------------------------------------------------------------------------------------------
  # check_err_msg() **TODO: Check or not?

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
                                               verbose = FALSE, cl=match.call())
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
    # No discrete only case: Return lm's confint
    return(confint.lm(object = object, parm = parm, level = level))
  }
}
