#
# - >= 1 endogenous regressor
# - >= 1 exogenous regressor (equation 2.2 & assumption A4)
# - exogenous: any class
# - endogenous: numeric
#
checkinput_copulabmw_formula_data <- function(formula, data) {
  return(checkinput_copulashared_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = "At least one exogenous regressor is required for the first-stage regression of each endogenous regressor P on X.",
    allow.interactions = TRUE,
    allow.constant.regressors = FALSE,
    warn.low.card.endo = TRUE,
    allowed.classes.endo = "numeric",
    allowed.classes.exo = NULL # allows any
  ))
}
