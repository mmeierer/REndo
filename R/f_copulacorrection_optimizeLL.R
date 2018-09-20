copulaCorrection_optimizeLL <- function(F.formula, data, name.var.continuous, verbose,
                                        start.params=NULL, num.boots=10, cl, ...){

  # Further checks required for the parameters in ... -------------------------------------------------
  l.ellipsis <- list(...)
  check_err_msg(checkinput_copulacorrection_singlecontinuous(l.ellipsis=l.ellipsis))

  # Tell what is done ---------------------------------------------------------------------------------
  if(verbose){
    # Tell what is done
    message("Optimizing LL for the single continuous endogenous regressor ",name.var.continuous,".")
    if(any(!(names(l.ellipsis) %in% c("start.params", "num.boots"))))
      warning("Additional parameters besides \'start.params\' and \'num.boots\' given in the ... argument are ignored.",
              call. = FALSE, immediate. = TRUE)
  }

  # Read out needed data ------------------------------------------------------------------------------
  # Using model.part() directly does not work if there are transformations in the DV
  mf                      <- model.frame(formula = F.formula, data = data, rhs=1, lhs=1)
  vec.data.y              <- as.vector(model.part(object = F.formula, data = mf, lhs=1, rhs = 0, drop=TRUE))
  # rhs1 contains exo and endo, as per input-checks
  m.model.data.exo.endo   <- model.matrix(object = F.formula, data = mf, lhs=0, rhs = 1)
  vec.data.endo           <- m.model.data.exo.endo[, name.var.continuous, drop=TRUE]


  # Create start parameters for optimx ----------------------------------------------------------------
  if(is.null(start.params)){
    # Generate with lm if they are missing
    start.params <- coef(lm(formula = formula(F.formula, lhs=1, rhs=1), data = data))
    str.brakets  <- paste0("(", paste(names(start.params), "=", round(start.params,3), collapse = ", ", sep=""), ")")
    if(verbose)
      message("No start parameters were given. The linear model ",deparse(formula(F.formula, lhs=1, rhs=1)),
              " was fitted and start parameters c",str.brakets," are used.")
  }

  # Add rho and sigma with defaults
  start.params <- c(start.params, rho=0.0, sigma=1)


  # Definition: Optimization function -----------------------------------------------------------------
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, vec.data.endo, hessian=F){
    # Bounds for rho (0,1): LL returns Inf if outside as NelderMead cannot deal with bounds
    return(optimx(par = optimx.start.params, fn = copulaCorrection_LL,
                  method="Nelder-Mead", control=list(trace=0),
                  hessian = hessian,
                  vec.y=vec.data.y, m.data.exo.endo=m.model.data.exo.endo, vec.data.endo=vec.data.endo))
  }


  # Run once for coef estimates with real data---------------------------------------------------------
  res.real.data.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y,
                                          m.model.data.exo.endo = m.model.data.exo.endo,
                                          vec.data.endo = vec.data.endo, hessian = T)

  # bootstrap num.boots times
  # Bootstrapping for SD ------------------------------------------------------------------------------
  if(verbose)
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)

  res.boots <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      if(verbose)
        setTxtProgressBar(pb, i)

      indices                 <- sample(x = length(vec.data.y), size=length(vec.data.y)*0.8, replace = TRUE)
      i.y                     <- vec.data.y[indices]
      i.m.model.data.exo.endo <- m.model.data.exo.endo[indices, ,drop=FALSE]
      i.vec.data.endo         <- vec.data.endo[indices]
      # return as vector / first row (only 1 method used). As matrix it cannot be used again as input to optimx
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y, m.model.data.exo.endo = i.m.model.data.exo.endo, vec.data.endo = i.vec.data.endo))[1,])
    })

  if(verbose)
    close(pb)

  # Calculate data --------------------------------------------------------------------------------------------
  # Prepare data to create a rendo LL optim object

  # Parameter and sd
  # Boots results: Rows = per parameter,  Columns = for each boots run
  coefficients          <- coef(res.real.data.optimx)[1,] # extract parameters from single fit
  parameter.sd          <- apply(res.boots, 1, sd)   # SD of bootstrapped parameters
  names(coefficients)   <- names(parameter.sd) <- names(start.params)

  names.params.exo.endo <- setdiff(names(coefficients), c("rho", "sigma"))

  hessian               <- attr(res.real.data.optimx, "details")[,"nhatend"][[1]]
  rownames(hessian)     <- colnames(hessian) <- names(start.params)

  fitted.values         <- as.vector(coefficients[names.params.exo.endo] %*% t(m.model.data.exo.endo))
  names(fitted.values)  <- rownames(m.model.data.exo.endo)
  residuals             <- vec.data.y - fitted.values
  names(residuals)      <- rownames(m.model.data.exo.endo)

  # Return data as object -------------------------------------------------------------------------------------
  return(new_rendo_optim_LL(call = cl, F.formula = F.formula, start.params = start.params,
                            estim.params = coefficients, estim.params.sd = parameter.sd,
                            names.main.coefs = names.params.exo.endo, hessian = hessian,
                            res.optimx = res.real.data.optimx,
                            fitted.values = fitted.values, residuals = residuals,
                            log.likelihood = res.real.data.optimx$value,
                            vcov.error = matrix(NA_real_)))
}