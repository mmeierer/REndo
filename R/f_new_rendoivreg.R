new_rendo_ivreg <- function(F.formula, res.ivreg, call){

  class(res.ivreg)     <- c("rendo.ivreg",class(res.ivreg))
  res.ivreg$call       <- call
  res.ivreg$rendo.Form <- F.formula
  return(res.ivreg)
}
# HetErrorsIV:
#   res <- structure(class="rendo.heterrorsiv",
#                    list(call          = cl,
#                         formula       = F.formula,
#                         coefficients  = coef(res.ivreg),
#                         res.ivreg     = res.ivreg))
# # jtest         = as.matrix(unclass(l.res$j.test[[2]])),
# # ftest         = l.res$f.test.stats))

# Highermoments:
# Return ------------------------------------------------------------------------------------------
# res <- structure(class="rendo.highermomentsiv",
#                  list(call          = cl,
#                       formula       = formula,
#                       coefficients  = coef(res.ivreg),
#                       res.ivreg     = res.ivreg))
