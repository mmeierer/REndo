# For each exogen. hetero. variables, derive the instruments:
#   residuals * [x-mean(x)] where residuals from
#     lm(endo P ~ all exogenous) and x are the single exo.het.vars
#' @importFrom stats cov
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
  # response   = labels(terms(F.formula, rhs=formula.rhs.endo, lhs=0)),
  F.residuals <-
    reformulate(response = formula(F.formula, rhs=2, lhs=0)[[2]],
                # all exo regs: complete model - endogenous reg
                termlabels = setdiff(labels(terms(F.formula, lhs=0, rhs=1)),
                                     labels(terms(F.formula, lhs=0, rhs=2))),
                intercept  = TRUE)

  res.lm.resid <- lm(F.residuals, data=data)
  if(anyNA(coef(res.lm.resid)))
    stop("The residuals of the linear model to build the internal instruments could not be derived.", call. = FALSE)

  internal.instr.residuals <- residuals(res.lm.resid)

  if(verbose)
    message("Residuals were derived by fitting ",format(F.residuals),".")


  # Calculate internal instruments -----------------------------------------------------------------------------------
  df.data.exohetero <- model.frame(F.formula, lhs=0, rhs=1, data = data)[, names.exo.IIV, drop=FALSE]

  # For every IIV() regressor (column): demean * resid

  # df.data.internal.instr.2 <- de.mean(df.data.exohetero) * internal.instr.residuals
  df.data.internal.instr <- as.data.frame(apply(df.data.exohetero, MARGIN = 2, FUN = function(col){
    (col - mean(col)) * internal.instr.residuals }))

  # Naming stuff
  # labels.het.exo   <- labels(terms(F.formula, lhs = 0, rhs = formula.rhs.exo))
  vec.desc.hetii.s <- paste0("IIV(",names.exo.IIV , ")")
  colnames(df.data.internal.instr) <- make.names(paste0("IIV.", names.exo.IIV))

  # IV assumption check ----------------------------------------------------------------------------------------------
  # The covariance between the squared residuals and the instruments should be different from 0.
  # If the assumption does not hold, the model assumption are not met and the instrument is weak.

  assumption.cov <- cov(internal.instr.residuals, df.data.internal.instr)

  if(any(assumption.cov >= 0.1))
    warning("The model assuption are not met:\nThe covariance between the instruments and the residuals are ",
            paste(names.exo.IIV,"=",round(assumption.cov,4), collapse = ", "),". ",
            "\nThe instruments therefore are weak.", immediate. = TRUE, call. = FALSE)


  # Return ------------------------------------------------------------------------------------------------
  # Return list with readable description and IIV as data.frame

  return(list(desc.IIV = vec.desc.hetii.s, df.IIV = df.data.internal.instr))
}
