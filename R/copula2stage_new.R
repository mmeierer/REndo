doc_rendocopula2stage_return_list <- function() {
  doc_boots <- doc_rendobootsdegeneratesremoved_return_list()

  doc_boots[['residuals']] <- "\\item{\\code{residuals}}{The structural residuals.}"
  doc_boots[[
    'fitted.values'
  ]] <- "\\item{\\code{fitted.values}}{Fitted values of the structural model.}"

  doc_copula2stage <- c(
    cdf = "\\item{\\code{cdf}}{The used cdf function.}",
    res.lm.augmented = "\\item{\\code{res.lm.augmented}}{The fitted augmented regression model, including the control function terms.}",
    labels.endo = "\\item{\\code{labels.endo}}{The term labels of the endogenous regressors.}",
    labels.exo = "\\item{\\code{labels.exo}}{The term labels of the exogenous regressors.}",
    labels.pcop = "\\item{\\code{labels.pcop}}{The term labels of the generated, auxiliary regressors.}"
  )

  return(c(doc_boots, doc_copula2stage))
}


doc_rendocopula2stage_return <- function(subclass, extra.items = NULL) {
  doc_intro <- c(
    return = paste0(
      "@return An object of class \\code{",
      subclass,
      "} which is a list that contains:"
    )
  )

  doc_items <- doc_rendocopula2stage_return_list()
  if (length(extra.items) > 0) {
    doc_items[names(extra.items)] <- extra.items
  }

  return(c(doc_intro, doc_items))
}


# Shared constructor for the two-stage copula methods.
# Method-specific fields are passed in `...`.
#' @importFrom stats coef model.frame
new_rendo_copula2stage <- function(
  call,
  F.formula,
  res.lm.augmented,
  res.boots,
  cdf,
  labels.endo,
  labels.exo,
  labels.pcop,
  subclass,
  # Anything else to add to the object
  ...
) {
  # Fitted values and residuals of the structural model (without the correction terms)
  l.fitted.resid <- copula2stage_compute_structural_fitted_residuals(
    res.lm.augmented = res.lm.augmented,
    labels.pcop = labels.pcop
  )

  return(.new_rendo_boots_degenerates_removed(
    # Stuff for rendo.boots.degenerates.removed class
    call = call,
    F.formula = F.formula,
    mf = model.frame(res.lm.augmented),
    coefficients = coef(res.lm.augmented),
    # WHat to show in the coef table: All coefs incl the correction terms
    names.main.coefs = names(coef(res.lm.augmented)),
    fitted.values = l.fitted.resid$fitted.values,
    residuals = l.fitted.resid$residuals,
    boots.params = res.boots$boots.params,
    n.boots.attempted = res.boots$n.attempted,
    n.boots.failed = res.boots$n.failed,

    # Stuff specific to rendo.copula.2stage
    subclass = c(subclass, "rendo.copula.2stage"),
    res.lm.augmented = res.lm.augmented,
    cdf = cdf,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.pcop = labels.pcop,

    # Method specific stuff
    ...
  ))
}
