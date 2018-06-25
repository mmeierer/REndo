#' @importFrom ivlewbel lewbel
#' @export
hetErrorsIV <- function(formula, data, clustervar = NULL, robust = TRUE){

  cl <- match.call()

  # Lewbel est
  l.res <- lewbel(formula = formula, data = data, clustervar = clustervar, robust = robust)

  # Return ------------------------------------------------------------------------------------------
  res <- structure(class="rendo.heterrorsiv",
                   list(call          = cl,
                        formula       = formula,
                        # order with intercept first
                        coefficients  = l.res$coef[c(2,1,seq(from=3,to=NROW(l.res$coef.est))), ],
                        nobs          = l.res$num.obs,
                        jtest         = as.matrix(unclass(l.res$j.test[[2]])),
                        ftest         = l.res$f.test.stats
                        ))
}
