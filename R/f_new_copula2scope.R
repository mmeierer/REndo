doc_rendocopula2scope_return_list <- function() {
  doc_boots <- doc_rendobootsdegeneratesremoved_return_list()

  doc_boots[['residuals']] <- "\\item{\\code{residuals}}{The structural residuals.}"
  doc_boots[[
    'fitted.values'
  ]] <- "\\item{\\code{fitted.values}}{Fitted values of the structural model.}"

  doc_copula2scope <- c(
    cdf = "\\item{\\code{cdf}}{The used cdf function.}",
    names.endo.regs = "\\item{\\code{names.endo.regs}}{The names of the continuous endogenous regressors.}",
    res.lm.augmented = "\\item{\\code{res.lm.augmented}}{The fitted augmented regression model, including the control function terms.}"
  )

  return(c(doc_boots, doc_copula2scope))
}


doc_rendocopula2scope_return <- function() {
  doc_intro <- c(
    return = "@return An object of class \\code{rendo.copula.2sCOPE} which is a list that contains:"
  )

  return(c(doc_intro, doc_rendocopula2scope_return_list()))
}

#' @importFrom stats coef model.frame
new_rendo_copula2sCOPE <- function(
  call,
  F.formula,
  fitted.values,
  residuals,
  res.lm.augmented,
  boots.params,
  n.boots.attempted,
  n.boots.failed,
  cdf,
  names.endo.regs
) {
  return(.new_rendo_boots_degenerates_removed(
    # Stuff for rendo.boots.degenerates.removed class
    call = call,
    F.formula = F.formula,
    mf = model.frame(res.lm.augmented),
    coefficients = coef(res.lm.augmented),
    names.main.coefs = names(coef(res.lm.augmented)), # OR: row.names(boots.params)
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,

    # Stuff specific to 2sCOPE
    subclass = "rendo.copula.2sCOPE",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
