#' @importFrom Formula as.Formula model.part
#' @importFrom stats ecdf runif coef lm confint qnorm
copulaCorrectionDiscrete_new <- function(formula, use.intercept = FALSE, data, num.simulations=250){

  # Check user input ----------------------------------------------------------------------------------
  # tbd

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- Formula::as.Formula(formula)
  df.data.all       <- Formula::model.part(object = F.formula, data = data, lhs=NULL, rhs = NULL) #NULl for all
  df.data.exo.endo  <- Formula::model.part(object = F.formula, data = data, lhs=0, rhs = c(1,2))
  df.data.endo      <- Formula::model.part(object = F.formula, data = data, lhs=0, rhs = 2)

  # Definition: Empirical cumulative distribution function --------------------------------------------
  fct.ecdf <- function(x){
    H.p  <- stats::ecdf(x)
    H.p1 <- H.p(x)
    H.p0 <- H.p(x-1)
    H.p0 <- ifelse(H.p0==0,0.0000001,H.p0)
    H.p0 <- ifelse(H.p0==1,0.9999999,H.p0)
    H.p1 <- ifelse(H.p1==0,0.0000001,H.p1)
    H.p1 <- ifelse(H.p1==1,0.9999999,H.p1)
    return(list(H.p = H.p, H.p1=H.p1, H.p0 = H.p0))
  }


  # Definition: simulation to run
  # The following function is run for simulation and calculates p.ecdf, U.p, p.star, and then meth.2(lm)
  # The fitted linear model meth.2 is returned
  .run.simulation.copulacorrection.discrete <- function(){

    # Calculate p.ecdf --------------------------------------------------------------------------------
    # For every row, calculate H.p, H.Hp1, and Hp0
    p.ecdf <- apply(df.data.endo, 2, fct.ecdf)


    # Calculate U.p for each endogenous regressor -----------------------------------------------------
    U.p <- matrix(data = NA_real_, nrow = nrow(df.data.endo), ncol = ncol(df.data.endo))
    for(k in seq(ncol(df.data.endo))){
      U.p[,k] <- stats::runif(n = nrow(df.data.endo),min=p.ecdf[[k]]$H.p0, max=p.ecdf[[k]]$H.p1)
    }


    # Calculate p.star for each endogenous regressor --------------------------------------------------
    p.star <- apply(U.p, 2, qnorm)
    colnames(p.star) <- paste("PStar", seq(ncol(df.data.endo)), sep=".")


    # Calculate meth.2 --------------------------------------------------------------------------------
    # use endo, exo and p.star data
    df.data.copula <- cbind(df.data.all, p.star)
    f.meth.2       <- if(use.intercept){ y~.}else{y~.-1}
    meth.2         <- stats::lm(formula = f.meth.2, data = df.data.copula)
    return(meth.2)

    # NOT USED Calculate b.hat ------------------------------------------------------------------------
    # bhat           <- stats::coef(stats::lm(formula = f.b.hat, data = df.data.copula))
    # Set names?

  }#end sim function definition


  # Run simulation ------------------------------------------------------------------------------------
  # Run .run.simulation.copulacorrection.discrete num.simulation times and
  # extract the CI for each regressor (and intercept)
  l.simulated.confints <-
    lapply(X = seq(num.simulations), FUN = function(i){
      meth.2.i <- .run.simulation.copulacorrection.discrete()
      return(stats::confint(meth.2.i, level = 0.95))
    })


  # Calculate mean CI of each regressor ----------------------------------------------------------------
  m.CI <- apply(l.simulated.confints, 2, mean)
  names(m.CI) <- c("Lower95%", "Upper95%")

  # Return results from last simulation run and mean CIs -----------------------------------------------
  l.res <- list(coefficients = meth.2.i$coefficients, CI=CI, reg = dataCopula, resid = meth.2.i$residuals, fitted.values=meth.2.i$fitted.values)
  # class(l.res) <- "rendo.copula.correction.discrete" #not sure yet
  return(l.res)
}

