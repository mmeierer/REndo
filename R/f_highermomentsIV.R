#' @importFrom Formula as.Formula
#' @importFrom stats update.formula reformulate model.response
#' @importFrom AER ivreg
#' @export
higherMomentsIV <- function(formula, data, verbose=TRUE){

# *** TODO: fail raluca check: underlying assumptions not satisfied - stop()
  cl <- match.call()

  # Input checks -------------------------------------------------------------------------------------
  check_err_msg(checkinput_highermomentsiv_formula(formula=formula))
  check_err_msg(checkinput_highermomentsiv_data(data=data))
  # **TODO: Inputcheck verbose
  check_err_msg(checkinput_highermomentsiv_formulaVSdata(formula=formula, data=data))

  # Read out the args from all IIV functions in the formula ------------------------------------------
  F.formula  <- as.Formula(formula)
  l.IIV.args <- formula_readout_special(F.formula = F.formula, name.special = "IIV",
                                        from.rhs=3, params.as.chars.only = FALSE)
  # str(l.IIV.args)

  # Tell what is done --------------------------------------------------------------------------------
  # if(verbose){
  #   message("Creating instrumental variables:")
  # }

  # Execute the IIV function and pass in the read out arguments --------------------------------------

  # check first that iiv and g are not present more than once
  check_err_msg(checkinput_highermomentsiv_docalllist(l.args = l.IIV.args))

  l.data.IIVs <- lapply(l.IIV.args, function(iiv.args){
                        # Add data and formula to the args here instead before to every sublist to avoid copy
                        do.call(what = higherMomentsIV_IIV,
                                args = c(alist(data=data, F.formula=F.formula),
                                         iiv.args))})

  # Bind results in list together to single data.frame
  df.data.IIVs <- do.call(cbind, l.data.IIVs)

  # Build formula for IVreg --------------------------------------------------------------------------
  # In:         response ~ complete model | single endogenous | IIV()s (| EIV)
  # Needed:     response ~ complete model | all exogenous from complete model + interal IVs (+ EIVs)

  # 1st part:
  # DV ~ complete model
  F.part.1 <- formula(F.formula, lhs = 1, rhs = 1)

  # 2nd part:
  # ~ | all exogenous from complete model + IIVs (+ external IVs)

  # All exogenous: complete - endogenous
  labels.all.exo <- setdiff(labels(terms(F.formula, rhs=1, lhs=0)),
                            labels(terms(F.formula, rhs=2, lhs=0)))

  labels.iivs    <- colnames(df.data.IIVs)
  labels.ext.iv  <- if(length(F.formula)[[2]] == 4)
                        labels(terms(F.formula, lhs=0, rhs=4))
                      else
                        NULL # include external IVs only if present

  # unique() reduces instruments to appear only exactly once
  # As the instruments are built from colnames, this applies also to specifed IIV()s, even partial:
  #   IIV(g=x2,iiv=g,X1,X2) + IIV(g=x2,iiv=g,X1, X2) is reduced to a single IIV(g=x2,iiv=g,X1, X2)
  #   IIV(g=x2,iiv=g,X1,X3) + IIV(g=x2,iiv=g,X1, X2) is reduced to IIV(g=x2,iiv=g,X1, X2, X3)
  F.part.2 <- reformulate(termlabels = unique(c(labels.all.exo, labels.iivs, labels.ext.iv)),
                          response   = NULL,
                          intercept  = TRUE)

  # Add 1st and 2nd part to formula
  F.ivreg <- as.Formula(F.part.1, F.part.2)

  # Call ivreg ---------------------------------------------------------------------------------------
  # Put data together: user data + internal instruments
  df.data.ivreg <- cbind(data, df.data.IIVs)

  if(verbose){
    print(head(df.data.ivreg))
    message("The following internal instruments were built: ")
    message("Fitting an instrumental variable regression with model ", format(F.ivreg), ".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula = F.formula,
                         res.ivreg = res.ivreg,
                         call = cl))
}
