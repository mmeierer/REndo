#' @importFrom Formula as.Formula
#' @importFrom stats model.frame residuals
#' @export
hetErrorsIV <- function(formula, data){

  cl <- match.call()

  F.formula <- as.Formula(formula)

#  y ~ P| X1 + X2 | X1+X2

  # Calculate internal instruments --------------------------------------------------------------------
  # For each exogen. hetero. variables (3rd RHS), derive the instruments:
  #   residuals * [x-mean(x)] where  residuals from lm(endo P ~ all exogenous(:=RHS2)) and x are the single exo.het.vars

  F.residuals.intinstr <- P ~ X1 + X2
    # all.vars(formula(F.formula, lhs = 0, rhs = 1))
                            # formula(F.formula, lhs = 0, rhs = 2))

  print(F.residuals.intinstr)
  intinstr.residuals <- residuals(lm(F.residuals.intinstr, data=data))

  df.data.exohetero <- model.frame(~ X1 + X2, data=data) #F.formula, lhs=0, rhs=3, data = data)
  print(head(df.data.exohetero))

  df.data.intinstr <- apply(X = df.data.exohetero, MARGIN = 2, FUN = function(single.exohetero){
    (single.exohetero - mean(single.exohetero)) * intinstr.residuals
    })

  colnames(df.data.intinstr) <- paste0("internal.instrument", seq(NCOL(df.data.intinstr)))

  print(head(df.data.intinstr))


  # First try with lm only ----------------------------------------------------------------------------
  # Basic parts
  # Add internal.instruments

  # General formula building
  # F.lm.only <-  update(formula(F.formula,lhs=1, rhs=2),
  #                      paste0(".~.+", paste(colnames(df.data.intinstr), collapse = "+")))

  F.lm.only <- y~X1 + X2+internal.instrument1+internal.instrument2
  df.data.lm.only <- cbind(data, df.data.intinstr)

  print(head(df.data.lm.only))
  print(F.lm.only)

  return(lm(F.lm.only, data=df.data.lm.only))





  # Lewbel est
  # l.res <- lewbel(formula = formula, data = data, clustervar = clustervar, robust = robust)


#
#   # Return ------------------------------------------------------------------------------------------
#   res <- structure(class="rendo.heterrorsiv",
#                    list(call          = cl,
#                         formula       = formula,
#                         # order with intercept first
#                         coefficients  = l.res$coef[c(2,1,seq(from=3,to=NROW(l.res$coef.est))), ],
#                         jtest         = as.matrix(unclass(l.res$j.test[[2]])),
#                         ftest         = l.res$f.test.stats
#                         ))
}
