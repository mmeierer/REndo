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
  check_err_msg(checkinput_highermomentsiv_formulaVSdata(formula=formula, data=data))
  check_err_msg(checkinput_highermomentsiv_verbose(verbose=verbose))

  # Read out the args from all IIV functions in the formula ------------------------------------------
  F.formula  <- as.Formula(formula)
  l.IIV.args <- formula_readout_special(F.formula = F.formula, name.special = "IIV",
                                        from.rhs = 3, params.as.chars.only = FALSE)
  # str(l.IIV.args)


  # Execute the IIV function and pass in the read out arguments --------------------------------------

  # check first that iiv and g are not present more than once
  check_err_msg(checkinput_highermomentsiv_docalllist(l.args = l.IIV.args))

  l.res.IIVs <- lapply(l.IIV.args, function(iiv.args){
                        # Add data and formula to the args here instead before to every sublist to avoid copy
                        do.call(what = higherMomentsIV_IIV,
                                args = c(alist(data=data, F.formula=F.formula),
                                         iiv.args))})

  # Read out IV data
  l.data.IIVs   <- lapply(l.res.IIVs, "[[","df.IIV")
  vec.desc.IIVs <- unique(unlist(lapply(l.res.IIVs, "[[","desc.IIV")))

  # Bind results in list together to single data.frame
  df.data.IIVs <- do.call(cbind, l.data.IIVs)


  # Call ivreg ---------------------------------------------------------------------------------------

  # Get formula
  l.F <- formula_build_ivreg(F.formula=F.formula, df.data.iv=df.data.IIVs, vec.desc.IVs = vec.desc.IIVs)
  F.ivreg     <- l.F[["F.ivreg"]]
  model.desc  <- l.F[["model.desc"]]

  # Put data together: user data + internal instruments
  df.data.ivreg <- cbind(data, df.data.IIVs)

  if(verbose){
    # print(head(df.data.ivreg))
    message("The following internal instruments were built: ", paste0(vec.desc.IIVs, collapse = ", "),".")
    message("Fitting an instrumental variable regression with model ", model.desc,".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula = F.formula,
                         res.ivreg = res.ivreg,
                         call = cl))
}
