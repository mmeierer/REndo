#' @importFrom Formula as.Formula
#' @importFrom stats ecdf runif coef lm confint qnorm model.frame update
#' @export
copulaCorrectionDiscrete <- function(formula, use.intercept = FALSE, data, num.simulations=250){

  cl <- match.call()

  # Check user input ----------------------------------------------------------------------------------
  # tbd

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- as.Formula(formula)
  # df.data.all       <- model.part(object = F.formula, data = data, lhs=NULL, rhs = NULL) #NULl for all
  # df.data.exo.endo  <- model.part(object = F.formula, data = data, lhs=0, rhs = c(1,2))
  df.data.endo      <- model.frame(formula = F.formula, data = data, lhs=0, rhs = 2)

  # Definition: P.star for discrete data --------------------------------------------------------------
  fct.p.star.discrete <- function(single.col.endo){

    # Fit empirical CDF to data
    H.p  <- stats::ecdf(single.col.endo)

    # Caluclate H.pX
    H.p1 <- H.p(single.col.endo)
    H.p0 <- H.p(single.col.endo-1)
    H.p0[H.p0 == 0] <- 0.0000001
    H.p0[H.p0 == 1] <- 0.9999999
    H.p1[H.p1 == 0] <- 0.0000001
    H.p1[H.p1 == 1] <- 0.9999999

    # Calculate U.p
    U.p <- stats::runif(n = NROW(single.col.endo), min=H.p0, max=H.p1)

    # Calculate p.star
    return(qnorm(U.p))
  }


  # Definition: simulation to run ---------------------------------------------------------------------
  # The following function is run for simulation and calculates p.star and then fits meth.2 (lm)
  # The fitted linear model is returned which can then be further used

  .run.simulation.copulacorrection.discrete <- function(){

    # Calculate p.star for each endogenous regressor --------------------------------------------------
    p.star <- apply(X=df.data.endo, MARGIN = 2, FUN = fct.p.star.discrete)
    colnames(p.star) <- paste("PStar", colnames(df.data.endo), sep=".")

    # Calculate meth.2 --------------------------------------------------------------------------------
    # use all user input data (endo, exo) and p.star
    df.data.copula <- cbind(data, p.star)
    # use the formula first part for fitting lm and also include P.star
    f.lm.relevant <- update(formula(F.formula, lhs=1, rhs=1),
                            paste0(".~.+", paste(colnames(p.star), collapse = "+")))

    return(stats::lm(formula = f.lm.relevant, data = df.data.copula))

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
  coefs.mean          <- apply(simulated.coefs, 1, mean)
  confint.mean        <- matrix(c(apply(simulated.confints.lower,  1, mean),
                                  apply(simulated.confints.higher, 1, mean)),
                                ncol=2, dimnames = dimnames(l.simulated.coefs.and.confints[[1]]$confint))


  # Return ---------------------------------------------------------------------------------------------
  l.res <- structure(list(call = cl, coefficients = coef(single.estimate), CI=confint.mean, #reg = df.data.copula, reg not neeced?
                          resid = single.estimate$residuals, fitted.values=single.estimate$fitted.values),
                     class= "rendo.copulacorrection.discrete")

  return(l.res)
}

