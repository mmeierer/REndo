#'
#with bootstrap

build_rendo_boots_ima <- function(call, F.formula, mf, estimated, boots) {
  .new_rendo_boots(
    call = call,
    F.formula = F.formula,
    mf = mf,
    coefficients = estimated$coefficients,
    names.main.coefs = estimated$names.main.coefs,
    fitted.values = estimated$fitted.values,
    residuals = estimated$residuals,
    boots.params = boots,
    subclass = "rendo.copula.ima"
  )
}

