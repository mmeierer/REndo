#' @importFrom stats confint
#' @export
confint.rendo.pstar.lm <- function(object, parm, level=0.95, num.simulations=250, ...){

  # -----------------------------------------------------------------------------------------------------
  # check_err_msg()

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

    # Separate tables for confints
    #   return depnds on number of parm given. if only 1, the results is not a matrix
    simulated.confints.lower  <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,1]),
                                        nrow=length(parm))
    simulated.confints.higher <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,2]),
                                        nrow=length(parm))

    # Mean of coefs and confints ------------------------------------------------------------------------
    confint.mean           <- cbind(rowMeans(simulated.confints.lower),
                                    rowMeans(simulated.confints.higher))
    dimnames(confint.mean) <- dimnames(l.simulated.coefs.and.confints[[1]])

    return(confint.mean)
  }else{
    # No discrete only case: Return lm's confint
    return(confint.lm(object = object, parm = parm, level = level))
  }
}
