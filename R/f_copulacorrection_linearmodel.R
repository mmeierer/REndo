#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
copulaCorrection_linearmodel <- function(F.formula, data, names.vars.continuous, names.vars.discrete,
                                         verbose, cl, ...){
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

  res.lm <- lm(formula = F.lm, data = df.data.copula)

  # Adapt return -----------------------------------------------------------------------------------------------
  return(new_rendo_pstar_lm(res.lm=res.lm, F.formula=F.formula, cl=cl, data=data,
                            names.vars.continuous = names.vars.continuous,
                            names.vars.discrete   = names.vars.discrete))
}
