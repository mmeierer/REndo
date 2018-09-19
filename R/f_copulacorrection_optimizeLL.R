copulaCorrection_optimizeLL <- function(data, formula, verbose, start.params=NULL, num.boots=10, ...){

  # Further checks required for the parameters in ... -------------------------------------------------
  l.ellipsis <- list(...)
  check_err_msg(checkinput_copulacorrection_singlecontinuous(l.ellipsis=l.ellipsis))

  # Tell what is done ---------------------------------------------------------------------------------
  if(verbose){
    # Tell what is done
    message("Optimizing LL for single continuous endogenous regressor.")
    if(any(!(names(l.ellipsis) %in% c("start.params", "num.boots"))))
      warning("Additional parameters besides \'start.params\' and \'num.boots\' given in the ... argument are ignored.",
              call. = FALSE, immediate. = TRUE)
  }

  # Create start parameters for optimx ----------------------------------------------------------------
  if(is.null(start.params)){
    # Generate with lm if they are missing
    start.params <- coef(lm(formula = formula(F.formula, lhs=1, rhs=1), data = data))
    str.brakets <- paste0("(", paste(names(start.params), "=", round(start.params,3), collapse = ", ", sep=""), ")")
    if(verbose)
      warning("No start parameters were given. The linear model ",deparse(formula(F.formula, lhs=1, rhs=1)),
              " was fitted and start parameters c",str.brakets," are used.", call. = F, immediate. = T)
  }

  # Add rho and sigma with 0.001 and 0.9998 as defaults
  start.params <- c(start.params, rho=0.0, sigma=1)


  # Definition: Optimization function -----------------------------------------------------------------
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, m.data.endo, hessian=F){
    # Bounds for rho (0,1): LL returns Inf if outside as NelderMead cannot deal with bounds
    return(optimx(par = optimx.start.params, fn = copulaCorrection_LL,
                  method="Nelder-Mead", control=list(trace=0),
                  hessian = hessian,
                  vec.y=vec.data.y, m.data.exo.endo=m.model.data.exo.endo, m.data.endo=m.data.endo))
  }


  # Run once for coef estimates -----------------------------------------------------------------------
  res.once.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y,
                                      m.model.data.exo.endo = m.model.data.exo.endo, m.data.endo = m.data.endo,
                                      hessian = T)

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
      i.m.data.endo           <- m.data.endo[indices, ,drop=FALSE]
      # return as vector / first row (only 1 method used). As matrix it cannot be used again as input to optimx
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y, m.model.data.exo.endo = i.m.model.data.exo.endo, m.data.endo = i.m.data.endo))[1,])
    })

  if(verbose)
    close(pb)

  # Return --------------------------------------------------------------------------------------------
}
