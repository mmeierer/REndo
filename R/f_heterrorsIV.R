#' @importFrom Formula as.Formula
#' @importFrom stats model.frame residuals reformulate update.formula terms
#' @importFrom AER ivreg
#' @export
hetErrorsIV <- function(formula, data, verbose=TRUE){

  y~X1+X2+P|P|X1|EIV
  cl <- match.call()

  formula.rhs.endo <- 2
  formula.rhs.exo  <- 3
  formula.rhs.eiv  <- 4

  F.formula <- Formula::as.Formula(formula)

  # For each exogen. hetero. variables, derive the instruments:
  #   residuals * [x-mean(x)] where residuals from
  #     lm(endo P ~ all exogenous(:=RHS2)) and x are the single exo.het.vars

  # Calculate residuals for internal instruments --------------------------------------------------------------------

  F.residuals <-
    reformulate(response   = attr(terms(formula(F.formula, rhs=formula.rhs.endo, lhs=0)), "term.labels"),
                termlabels = attr(terms(formula(F.formula, rhs=formula.rhs.exo,  lhs=0)), "term.labels"),
                intercept  = attr(terms(formula(F.formula, rhs=formula.rhs.exo,  lhs=0)), "intercept"))

  # Fit lm and obtain residuals
  internal.instr.residuals <- residuals(lm(F.residuals, data=data))


  # Calculate internal instruments -----------------------------------------------------------------------------------
  df.data.exohetero <- model.frame(F.formula, lhs=0, rhs=formula.rhs.exo, data = data)

  # For every exo heter regressor (column): demean * resid
  df.data.internal.instr <- as.data.frame(
    apply(df.data.exohetero, MARGIN = 2, FUN = function(col){
                    (col - mean(col)) * internal.instr.residuals }))

  # Naming stuff
  labels.het.exo <- labels(terms(F.formula, lhs = 0, rhs = formula.rhs.exo))
  vec.desc.hetii.s <- paste0("HETIV(",labels.het.exo , ")")
  colnames(df.data.internal.instr) <- make.names(paste0("HET.internal.instruments.", labels.het.exo))

  # IV assumption check ----------------------------------------------------------------------------------------------
  # The covariance between the squared residuals and the instruments should be different from 0.
  # If the assumption does not hold, the model assumption are not met and the instrument is weak.

  assumption.cov <- cov(internal.instr.residuals, df.data.internal.instr)
  if(isTRUE(all.equal.numeric(assumption.cov, 0, tolerance = sqrt(.Machine$double.eps))))
      warning("The model assuption are not meet. The instrument is weak.", immediate. = TRUE, call. = FALSE)

  # Instrumental variable regression ---------------------------------------------------------------------------------

  # Get formula and pretty model description
  l.F        <- formula_build_ivreg(F.formula=F.formula, df.data.iv=df.data.internal.instr,
                                    vec.desc.IVs = vec.desc.hetii.s)
  F.ivreg    <- l.F[["F.ivreg"]]
  model.desc <- l.F[["model.desc"]]

  # Put data together: user data +instruments
  df.data.ivreg <- cbind(data, df.data.internal.instr)

  if(verbose){
    message("The following internal instruments were built: ", paste0(vec.desc.hetii.s, collapse = ", "),".")
    message("Fitting an instrumental variable regression with model ", model.desc, ".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula=F.formula, call = cl,
                         res.ivreg = res.ivreg))
}


# lm for residuals: RHS1 ~ RHS2
# instruments x for x-mean(x): x from RHS3
# IVreg formula: LHS1 ~ RHS1 + RHS2 | RHS2 + instruments (+ RHS4, if present)
