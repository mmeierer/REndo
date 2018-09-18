

copulaCorrection_optimizeLL <- function(data,  start.params = NULL, names.main.model, name.var.continuous, num.bootstraps){
  # optimize LL
  # Create start parameters for optimx ----------------------------------------------------------------

  # Definition: Optimization function -----------------------------------------------------------------

  # Run once for coef estimates -----------------------------------------------------------------------

  # bootstrap num.boots times
  # Bootstrapping for SD ------------------------------------------------------------------------------

  # Return --------------------------------------------------------------------------------------------
}


# To reuse the function in the discrete only confint simulation, also a model.frame object can be passed as
# the data parameter instead of a regular data.frame. Reason is that the fitted lm contains only the
# model.frame but not the original data. The MF can be re-used by using the lm.fit function directly.
copulaCorrection_linearmodel <- function(F.formula, data, data.is.model.frame = FALSE,
                                         names.vars.continuous, names.vars.discrete){

  # Generate PStar data ---------------------------------------------------------------------------------------
  l.pstar.data.continuous <- NULL
  l.pstar.data.discrete   <- NULL

  if(length(names.vars.continuous) > 0){
    l.pstar.data.continuous <- lapply(names.vars.continuous, function(n){
                                                      copulaCorrectionContinuous_pstar(vec.data.endo=data[[n]]) })
    names(l.pstar.data.continuous) <- paste0("PStar.", names.vars.continuous, sep="")
  }

  if(length(names.vars.discrete) > 0){
    l.pstar.data.discrete   <- lapply(names.vars.discrete, function(n){
                                                    copulaCorrectionDiscrete_pstar(vec.data.endo=data[[n]])})
    names(l.pstar.data.discrete) <- paste0("PStar.", names.vars.discrete, sep="")
  }
  df.data.pstar <- do.call(cbind, c(l.pstar.data.continuous, l.pstar.data.discrete))


  if(!data.is.model.frame){
    # Update formula to include PStar data ----------------------------------------------------------------------
    # use the formula RHS1 and also include P.star to fit lm
    F.lm  <- formula_build_mainmodel_data(F.formula = F.formula, data=df.data.pstar)

    # Run linear model ------------------------------------------------------------------------------------------
    df.data.copula <- cbind(data, df.data.pstar)
    lm.res <- lm(formula = F.lm, data = df.data.copula)
  }else{
    print(head(df.data.pstar))
    # str(data)
    print(head(data))
    # Update PStar data in model.frame
    data[, colnames(df.data.pstar)] <- df.data.pstar
    print(head(data))
    lm.res <- lm.fit(x = model.matrix(terms(data), data=data), y=model.response(data))
    str(lm.res)
  }



  # Return object --------------------------------------------------------------------------------------
  return(lm.res)
}

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

#' @importFrom Formula as.Formula
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- match.call()
  print(cl)
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))


  # Read out specials ------------------------------------------------------------------------------------------
  F.formula <- as.Formula(formula)
  names.vars.continuous <- fct.readout.special(F.formula = F.formula, name.special = "continuous", from.rhs=2)
  names.vars.discrete   <- fct.readout.special(F.formula = F.formula, name.special = "discrete",   from.rhs=2)

  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE

  if(optimizeLL){
    # Further checks required for the parameters in ...
    check_err_msg(checkinput_copulacorrection_singlecontinuous(l.ellipsis=l.ellipsis))

    if(verbose){
      # Tell what is done
      message("For XXXXXX ** TO DO *** ")
      if(any(!(names(l.ellipsis) %in% c("start.params", "num.boots"))))
        warning("Additional parameters besides \'start.params\' and \'num.boots\' given in the ... argument are ignored.", call. = FALSE, immediate. = FALSE)
    }

    # Single continuous - copula method 1 (optimize LL)
    res <- copulaCorrection_optimizeLL(data = data, names.main.model = , start.params =,
                                          name.var.continuous = names.vars.continuous)
  }else{
    if(verbose){
      # Tell what is done
      message("For XXXXXX ** TO DO *** ")
      # Warn if unnecessary parameters (anything in ...) were given because user seemingly doesnt understand what hes doing
      if(length(l.ellipsis)>0)
        warning("Additional parameters given in the ... argument are ignored because they are not needed.",
                call. = FALSE, immediate. = TRUE)
    }

    # All other cases use pstar data + lm
    res <- copulaCorrection_linearmodel(F.formula = F.formula, data = data,
                                           names.vars.continuous = names.vars.continuous,
                                           names.vars.discrete   = names.vars.discrete)
  }

  # discrete only gets an additional class for its own confidence interval simulation function
  if(length(names.vars.continuous) == 0 & length(names.vars.discrete) > 0)
    class(res) <- c("rendo.copulacorrection.lm.discrete",class(res))

  # Adapt return -----------------------------------------------------------------------------------------------
  res$formula <- F.formula
  res$call    <- cl
  res$names.vars.continuous <- names.vars.continuous
  res$names.vars.discrete   <- names.vars.discrete
  return(res)
  # if(length(names.vars.discrete)>0){
  #   # Single/multiple discrete and "mixed" (continuous+discrete) all use pstar
  # }else{
  #   # Only continuous
  #   if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0){
  #     # Single continuous - copula method 1 (optimize LL)
  #
  #   }else{
  #     # Multiple continuous - pstar continuous
  #   }
  # }
  # lm with pstar
  # or
  # optimize LL
}

