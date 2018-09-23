higherMomentsIV_IIV <- function(F.formula, data, g=NULL, iiv,  ...){

  # Catch
  l.ellipsis <- list(...)

  # Check inputs ---------------------------------------------------------------------------------------------
  check_err_msg(checkinput_highermomentsiv_g(g=g))
  check_err_msg(checkinput_highermomentsiv_iiv(iiv=iiv))
  check_err_msg(checkinput_highermomentsiv_iivVSg(g=g, iiv=iiv))
  check_err_msg(checkinput_highermomentsiv_iivregressors(l.ellipsis=l.ellipsis, F.formula=F.formula, iiv=iiv))

  # Read out needed data (regressors) ------------------------------------------------------------------------

  # Calculate internal IVs -----------------------------------------------------------------------------------
  # g function
  # iiv calculation

  # Return IIV as single data.frame column

  return(data.frame("iiv" = 1:10))
}
