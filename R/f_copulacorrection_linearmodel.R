#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
copulaCorrection_linearmodel <- function(F.formula, data, names.vars.continuous, names.vars.discrete,
                                         verbose, cl, num.boots, ...){
  l.ellipsis <- list(...)

  # Tell what is done -----------------------------------------------------------------------------------------
  if(verbose){
    message("Fitting linear model with additional PStar copula data for ", length(names.vars.continuous),
            " continuous and ", length(names.vars.discrete), " discrete endogenous regressors.")
  }
  # Warn if unnecessary parameters except for lm are given because user seemingly doesnt understand what he is doing
  if(length(l.ellipsis)>0)
    warning("Additional parameters given in the ... argument are ignored because they are not needed.",
            call. = FALSE, immediate. = TRUE)


  # Function that fits lm with additional pstar data ------------------------------------------------------------
  fct.fit.lm.pstar <- function(F.formula, data, names.vars.continuous, names.vars.discrete){

    # Generate PStar data ---------------------------------------------------------------------------------------
    l.pstar.data.continuous <- NULL # init so that they exists for cbind
    l.pstar.data.discrete   <- NULL

    # MF is only used for the (numeric) endovars. The full data then is used again in lm()
    mf.data <- model.frame(F.formula, rhs=1, lhs=0, data=data)

    # Warn if the data is binomial=only has to values=dummy
    #   can only be done after the data was transformed
    checkinput_copulacorrection_warnbinomialendodata(data = mf.data,
                                                     names.vars.continuous = names.vars.continuous,
                                                     names.vars.discrete   = names.vars.discrete)

    if(length(names.vars.continuous) > 0){
      l.pstar.data.continuous <- lapply(names.vars.continuous, function(n){
        copulaCorrectionContinuous_pstar(vec.data.endo=mf.data[[n]]) })
      names(l.pstar.data.continuous) <- paste0("PStar.", names.vars.continuous, sep="")
    }

    if(length(names.vars.discrete) > 0){
      l.pstar.data.discrete   <- lapply(names.vars.discrete, function(n){
        copulaCorrectionDiscrete_pstar(vec.data.endo=mf.data[[n]])})
      names(l.pstar.data.discrete) <- paste0("PStar.", names.vars.discrete, sep="")
    }

    df.data.pstar <- do.call(cbind, c(l.pstar.data.continuous, l.pstar.data.discrete))
    colnames(df.data.pstar) <- make.names(colnames(df.data.pstar))

    # Update formula to include PStar data ----------------------------------------------------------------------
    # use the formula RHS1 and also include P.star to fit lm
    F.lm  <- formula_build_mainmodel_data(F.formula = F.formula, data=df.data.pstar)


    # Run linear model ------------------------------------------------------------------------------------------
    df.data.copula <- cbind(data, df.data.pstar)

    return(lm(formula = F.lm, data = df.data.copula))
  }



  # Fit on the original data ------------------------------------------------------------------------------------
  res.lm.real.data <- fct.fit.lm.pstar(F.formula = F.formula, data = data,
                                       names.vars.continuous=names.vars.continuous, names.vars.discrete=names.vars.discrete)

  # Bootstrap params --------------------------------------------------------------------------------------------
  if(verbose){
    message("Running ",num.boots," bootstraps.")
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  }

  boots.params <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      if(verbose)
        setTxtProgressBar(pb, i)

      boot.indices <- sample.int(n = NROW(data), replace = TRUE)
      boot.data    <- data[boot.indices, ,drop=FALSE]
      return(coef(fct.fit.lm.pstar(F.formula = F.formula, data = boot.data,
                                   names.vars.continuous=names.vars.continuous, names.vars.discrete=names.vars.discrete)))
      })

  if(verbose)
    close(pb)

  # Return object ------------------------------------------------------------------------------------------------
  # Coefs to calculate fitted and residuals are withouth pstar
  #   however, pstar names are part of names.main.coefs so that they are shown in the coef table in summary()
  names.main.coefs <- setdiff(names(coef(res.lm.real.data)),
                              make.names(c(paste0("PStar.", names.vars.discrete,   sep=""),
                                           paste0("PStar.", names.vars.continuous, sep=""))))

  # Only coefs and data without pstar
  X <- model.matrix(res.lm.real.data)[, names.main.coefs, drop=FALSE]
  main.coefs <- coef(res.lm.real.data)[names.main.coefs]

  res.lm.mf  <- model.frame(res.lm.real.data)
  y <- model.response(res.lm.mf)

  fitted.vals   <- drop(X %*% main.coefs)
  residual.vals <- drop(y - fitted.vals)

  return(new_rendo_copula_correction(call             = cl,
                                     F.formula        = F.formula,
                                     mf               = res.lm.mf,
                                     names.main.coefs = names(coef(res.lm.real.data)),
                                     coefficients     = coef(res.lm.real.data),
                                     residuals        = residual.vals,
                                     fitted.values    = fitted.vals,

                                     boots.params     = boots.params,
                                     copula.case      = 2,
                                     names.vars.continuous = names.vars.continuous,
                                     names.vars.discrete   = names.vars.discrete,

                                     res.lm.real.data  = res.lm.real.data))
}
