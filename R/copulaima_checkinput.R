# - no interactions
# - >= 1 endogenous regressor
# - >= 1 exogenous regressor
# - exogenous: numeric
# - endogenous: numeric
# - no constant regressors
#
checkinput_copulaima_formula_data <- function(formula, data) {
  return(checkinput_copulashared_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = paste0(
      "At least one exogenous regressor is required for the first-stage regression. ",
      "Without exogenous regressors, IMA is equivalent to Park and Gupta (2012): ",
      "Use copulaCorrection() instead."
    ),
    allow.interactions  = FALSE,
    allow.constant.regressors = FALSE,
    allowed.classes.endo= "numeric",
    allowed.classes.exo = "numeric"
  ))
}


