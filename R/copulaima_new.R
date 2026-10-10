doc_rendocopulaima_return_list <- function() {
  doc_boots <- doc_rendobootsdegeneratesremoved_return_list()

  doc_boots[['residuals']] <- "\\item{\\code{residuals}}{The structural residuals.}"
  doc_boots[[
    'fitted.values'
  ]] <- "\\item{\\code{fitted.values}}{Fitted values of the structural model.}"

  doc_copulaima <- c(
    cdf = "\\item{\\code{cdf}}{The used cdf function.}",
    res.lm.augmented = "\\item{\\code{res.lm.augmented}}{The fitted augmented regression model, including the control function terms.}",
    labels.endo = "\\item{\\code{labels.endo}}{The term labels of the endogenous regressors.}",
    labels.exo = "\\item{\\code{labels.exo}}{The term labels of the exogenous regressors.}",
    labels.pcop = "\\item{\\code{labels.pcop}}{The term labels of the generated, auxiliary regressors.}"
  )

  return(c(doc_boots, doc_copulaima))
}


doc_rendocopulaima_return <- function() {
  doc_intro <- c(
    return = "@return An object of class \\code{rendo.copula.ima} which is a list that contains:"
  )

  return(c(doc_intro, doc_rendocopulaima_return_list()))
}

#' @importFrom stats coef model.frame
new_rendo_copulaima <- function(
  call,
  F.formula,
  fitted.values,
  residuals,
  boots.params,
  n.boots.attempted,
  n.boots.failed,
  res.lm.augmented,
  cdf,
  labels.endo,
  labels.exo,
  labels.pcop
) {
  return(.new_rendo_boots_degenerates_removed(
    # Stuff for rendo.boots.degenerates.removed class
    call = call,
    F.formula = F.formula,
    mf = model.frame(res.lm.augmented),
    coefficients = coef(res.lm.augmented),
    names.main.coefs = names(coef(res.lm.augmented)),
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,

    # Stuff specific to copula ima
    subclass = "rendo.copula.ima",
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop
  ))
}
