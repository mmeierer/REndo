#' @importFrom Formula as.Formula
#' @importFrom stats ecdf runif coef lm confint qnorm model.frame update formula reformulate
#' @export
copulaCorrectionDiscrete <- function(formula, data, num.simulations=250){

  cl <- match.call()

  # Check user input ----------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrectiondiscrete_formula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrectiondiscrete_data(data=data, formula=formula))
  check_err_msg(checkinput_copulacorrectiondiscrete_numsimulations(num.simulations=num.simulations))


  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- as.Formula(formula)
  # Need endogenous data for calculating PStar
  df.data.endo      <- model.frame(formula = F.formula, data = data, lhs=0, rhs = 2)


  # Definition: simulation to run ---------------------------------------------------------------------
  # The following function is run for simulation and calculates p.star and then fits meth.2 (lm)
  # The fitted linear model is returned which can then be further used

  .run.simulation.copulacorrection.discrete <- function(){

    # Calculate p.star for each endogenous regressor --------------------------------------------------
    p.star <- copulaCorrectionDiscrete_pstar(df.data.endo=df.data.endo)
    colnames(p.star) <- paste("PStar", colnames(df.data.endo), sep=".")

    # Calculate meth.2 --------------------------------------------------------------------------------
    # use all user input data (endo, exo) and p.star
    df.data.copula <- cbind(data, p.star)

    # use the formula second part for fitting lm and also include P.star
    f.lm.relevant <- update(formula(F.formula, lhs=1, rhs=1),
                            reformulate(termlabels = c(".", colnames(p.star))))
    return(lm(formula = f.lm.relevant, data = df.data.copula))

  }#end sim function definition

  # Single fit ---------------------------------------------------------------------------------------
  single.estimate <- .run.simulation.copulacorrection.discrete()


  # Run simulation ------------------------------------------------------------------------------------
  # Run .run.simulation.copulacorrection.discrete num.simulation times and extract the coefs and CI
  # for each regressor (and intercept)
  l.simulated.coefs.and.confints <-
    lapply(X = seq(num.simulations), FUN = function(i){
      meth.2.i <- .run.simulation.copulacorrection.discrete()
      return(list(coef = coef(meth.2.i), confint = confint(meth.2.i, level = 0.95)))
    })

  # Separate tables for coef and confints
  simulated.coefs           <- sapply(l.simulated.coefs.and.confints, function(x) x[["coef"]])
  simulated.confints.lower  <- sapply(l.simulated.coefs.and.confints, function(x) x[["confint"]][,1])
  simulated.confints.higher <- sapply(l.simulated.coefs.and.confints, function(x) x[["confint"]][,2])

  # Mean of coefs and confints ------------------------------------------------------------------------
  # coefs.mean          <- apply(simulated.coefs, 1, mean)
  confint.mean        <- matrix(c(apply(simulated.confints.lower,  1, mean),
                                  apply(simulated.confints.higher, 1, mean)),
                                ncol=2, dimnames = dimnames(l.simulated.coefs.and.confints[[1]]$confint))


  # Return ---------------------------------------------------------------------------------------------
  l.res <- structure(class= "rendo.copulacorrection.discrete",
                    list(call          = cl,
                         formula       = formula,
                         CI            = confint.mean, #reg = df.data.copula, reg not needed?
                         coefficients  = coef(single.estimate),
                         fitted.lm     = single.estimate))

  return(l.res)
}

