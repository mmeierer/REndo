#' @importFrom Formula as.Formula
#'

#without bootstrap
build_rendo_base_ima <- function(call, F.formula, mf, estimated) {
  .new_rendo_base(
    call = call,
    F.formula = F.formula,
    mf = mf,
    coefficients = estimated$coefficients,
    names.main.coefs = estimated$names.main.coefs,
    fitted.values = estimated$fitted.values,
    residuals = estimated$residuals,
    subclass = "rendo.copula.ima"
  )
}

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

