#' @export
confint.rendo.copulacorrection.lm.discrete <- function(object, parm, level=0.95, num.simulations=250, ...){

  # Run simulation ------------------------------------------------------------------------------------
  # Run .run.simulation.copulacorrection.discrete num.simulation times and extract the coefs and CI
  # for each regressor (and intercept)
  l.simulated.coefs.and.confints <-
    lapply(X = seq(num.simulations), FUN = function(i){
      res.lm <- copulaCorrection_linearmodel(F.formula = object$formula, data = model.frame(object),
                                             data.is.model.frame = TRUE,
                                             names.vars.continuous = object$names.vars.continuous,
                                             names.vars.discrete   = object$names.vars.discrete)
      return(confint(res.lm, parm = parm, level = level))
    })
  print(head(l.simulated.coefs.and.confints))
  str(l.simulated.coefs.and.confints)

  # Separate tables for confints
  simulated.confints.lower  <- sapply(l.simulated.coefs.and.confints, function(x) x[,1])
  simulated.confints.higher <- sapply(l.simulated.coefs.and.confints, function(x) x[,2])

  # Mean of coefs and confints ------------------------------------------------------------------------
  confint.mean        <- matrix(c(apply(simulated.confints.lower,  1, mean),
                                  apply(simulated.confints.higher, 1, mean)),
                                ncol=2, dimnames = dimnames(l.simulated.coefs.and.confints[[1]]))

  return(confint.mean)
}
