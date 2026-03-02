#'
#with bootstrap

build_rendo_boots_ima <- function(call, F.formula, res.lm, boots) {
  .new_rendo_boots(
    call = call,
    F.formula = F.formula,
    mf = res.lm$model,
    coefficients = coef(res.lm),
    names.main.coefs = rownames(boots),
    fitted.values = fitted(res.lm),
    residuals = resid(res.lm),
    boots.params = boots,
    subclass = "rendo.copula.ima"
  )
}

