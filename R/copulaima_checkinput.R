# - no interactions
# - >= 1 endogenous regressor
# - >= 1 exogenous regressor
# - exogenous: numeric
# - endogenous: numeric
#
checkinput_copulaima_formula_data <- function(formula, data) {
  return(checkinput_copulashared_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    allow.interactions  = FALSE,
    allowed.classes.endo= "numeric",
    allowed.classes.exo = "numeric"
  ))
}


