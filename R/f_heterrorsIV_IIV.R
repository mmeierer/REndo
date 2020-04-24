# For each exogen. hetero. variables, derive the instruments:
#   residuals * [x-mean(x)] where residuals from
#     lm(endo P ~ all exogenous) and x are the single exo.het.vars
#' @importFrom stats cov as.formula
#' @importFrom lmtest bptest
hetErrorsIV_IIV <- function(F.formula, data, verbose){

  # Check inputs ---------------------------------------------------------------------------------------------
  names.exo.IIV <- formula_readout_special(F.formula = F.formula, name.special = "IIV",
                                           from.rhs = 3, params.as.chars.only = TRUE)

  check_err_msg(checkinput_heterrors_iivregressors(F.formula=F.formula, names.exo.IIV=names.exo.IIV))


  # Calculate residuals for internal instruments -------------------------------------------------------------
  # Cannot use labels/character as it is internally converted to symbols instead
  #  of language objects. Transformations in symbols then get backticks what fails
  #  any further processing in lm and model.frame.
  #   Alternative:
  #   attr(delete.response(terms(f, rhs=2)), "variables")[-1]
  #   response   = labels(terms(F.formula, rhs=formula.rhs.endo, lhs=0)),
  F.residuals <-
    reformulate(response = formula(F.formula, rhs=2, lhs=0)[[2]],
                # all exo regs: complete model - endogenous reg
                termlabels = setdiff(labels(terms(F.formula, lhs=0, rhs=1)),
                                     labels(terms(F.formula, lhs=0, rhs=2))),
                intercept  = TRUE)

  res.lm.resid <- lm(F.residuals, data=data)

  if(any(!is.finite(coef(res.lm.resid))))
    stop(paste0("The residuals of the linear model (",format(F.residuals),
                ") to build the internal instruments could not be derived because some of the model coefficient were non-finite."),
         call. = FALSE)

  internal.instr.residuals <- residuals(res.lm.resid)

  if(any(!is.finite(internal.instr.residuals)))
    stop(paste0("The residuals of the linear model (",format(F.residuals),
                ") to build the internal instruments could not be derived because some of the residuals were non-finite."),
         call. = FALSE)


  if(verbose)
    message("Residuals were derived by fitting ",format(F.residuals),".")


  # IV assumption check ----------------------------------------------------------------------------------------------
  names.endo        <- labels(terms(F.formula, lhs = 0, rhs = 2))
  mf.model          <- model.frame(F.formula, lhs=0, rhs=1, data = data)
  df.data.endo      <- mf.model[, names.endo,    drop=FALSE]
  df.data.exohetero <- mf.model[, names.exo.IIV, drop=FALSE]

  # For every exogenous variable separately, test whether a weak instrument is built from it
  #   Regress endo ~ exo to test whether the heteroscedasticity assumptions hold

  for(n.exo in colnames(df.data.exohetero)){

    # Create the formula directly from the names in the data
    #   This adds transformations in column names to the formula with backticks, ie avoids that the
    #     transformation is applied again on the df.data.bp which is already transformed (being output from model.frame)
    #   Place endo in first column to include it as formula response
    df.data.bp <- cbind(df.data.endo, df.data.exohetero[, n.exo, drop=FALSE])
    F.bp       <- as.formula(df.data.bp)

    # Breusch-Pagan tests the null hypothesis of homoscedasticity
    #   If p-value >= 0.05 the null hypothesis of homoscedasticity is not rejected and the
    #   assumption of heteroscedasticity for the variables is not satisfied (= instrument is weak).
    # Therefore p-value:
    #   <  0.05: ok
    #   >= 0.05: warning
    res.bp <- bptest(formula    = F.bp,
                     studentize = TRUE,
                     data       = df.data.bp)
    if(res.bp$p.value >= 0.05){
      warning(paste0("A studentized Breusch-Pagan test (",format(F.bp),") ",
                     "indicates at a 95% confidence level that the assumption of heteroscedasticity for the variable is not satisfied (p-value: ",
                     round(res.bp$p.value, 4),"). The instrument built from it therefore is weak."),
              call. = FALSE, immediate. = FALSE)
    }
  }

  # Build internal instruments -----------------------------------------------------------------------------------
  # For every IIV() regressor (column): demean * resid

  df.data.internal.instr <- as.data.frame(apply(df.data.exohetero, MARGIN = 2, FUN = function(col){
    (col - mean(col)) * internal.instr.residuals }))

  # Naming stuff
  vec.desc.hetii.s                 <- paste0("IIV(",names.exo.IIV , ")")
  colnames(df.data.internal.instr) <- make.names(paste0("IIV.", names.exo.IIV))


  # Return ------------------------------------------------------------------------------------------------
  # Return list with readable description and IIV as data.frame
  return(list(desc.IIV = vec.desc.hetii.s, df.IIV = df.data.internal.instr))
}
