# add class rendo.ivre to regular ivreg object to
#   catch when the user calls summary() and forward to ivreg's summary () with diagnostic=TRUE
new_rendo_ivreg <- function(F.formula, res.ivreg, call){

  class(res.ivreg)     <- c("rendo.ivreg",class(res.ivreg))
  res.ivreg$call       <- call
  return(res.ivreg)
}
