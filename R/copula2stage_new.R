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


#' @importFrom stats coef model.frame model.matrix model.response
copula2stage_get_structural_parts <- function(
  res.lm.augmented,
  labels.pcop
) {
  if (
    is.null(labels.pcop) ||
      length(labels.pcop) == 0 ||
      anyNA(labels.pcop) ||
      any(nchar(labels.pcop) == 0)
  ) {
    stop(
      "Internal error: 'labels.pcop' is empty. This should not happen - please report as a bug!",
      call. = FALSE
    )
  }

  names.coefs.all <- names(coef(res.lm.augmented))

  # Check if all aux regs are here
  missing.aux <- setdiff(labels.pcop, names.coefs.all)
  if (length(missing.aux)) {
    stop(
      "Internal error: Auxiliary regressor(s) not found in coefficients: ",
      toString(missing.aux),
      ". This should not happen - please report as a bug!",
      call. = FALSE
    )
  }

  names.structural <- names.coefs.all[!names.coefs.all %in% labels.pcop]
  coefs.structural <- coef(res.lm.augmented)[names.structural]
  mm.structural <- model.matrix(res.lm.augmented)[, names.structural, drop = FALSE]

  fitted.values <- drop(mm.structural %*% coefs.structural)
  names(fitted.values) <- row.names(mm.structural)

  residuals <- drop(model.response(model.frame(res.lm.augmented)) - fitted.values)
  names(residuals) <- names(fitted.values)

  return(list(fitted.values = fitted.values, residuals = residuals))
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
  l.structural <- copula2stage_get_structural_parts(
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
    fitted.values = l.structural$fitted.values,
    residuals = l.structural$residuals,
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
