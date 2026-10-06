# - no interactions
# - >= 1 endogenous regressor
# - >= 0 exogenous regressor
# - exogenous: any class except ordered
# - endogenous: numeric
#
checkinput_copulaima_formula_data <- function(formula, data) {
  return(checkinput_copulashared_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 0, # may have no exo
    allow.interactions  = FALSE,
    allowed.classes.endo= "numeric",
    allowed.classes.exo = c("numeric", "logical", "character", "factor")
  ))
}


