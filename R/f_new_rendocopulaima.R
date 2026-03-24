doc_rendocopulaima_return_list <- function() {
  doc_boots <- doc_rendoboots_return_list()
  doc_copulaima <- c(
    "\\item{\\code{cdf}}{The used cdf function.}",
    "\\item{\\code{names.endo.regs}}{The names of the continuous endogenous regressors.}",
    "\\item{\\code{n.boots.attempted}}{The number of bootstrap iterations that were attempted.}",
    "\\item{\\code{n.boots.failed}}{The number of bootstrap iterations resulting in a failed fit.}"
  )

  return(c(doc_boots, doc_copulaima))
}


doc_rendocopulaima_return <- function() {
  doc_intro <- c(
    "@return An object of classes \\code{rendo.copula.ima}, \\code{rendo.boots},",
    "  and \\code{rendo.base}, which is a list that contains:"
  )

  return(c(doc_intro, doc_rendocopulaima_return_list()))
}

#' @importFrom stats coef fitted residuals
new_rendo_copula_ima <- function(
  call,
  F.formula,
  names.endo.regs,
  cdf,
  res.lm,
  boots,
  n.boots.attempted,
  n.boots.failed
) {
  return(.new_rendo_boots_degenerates_removed(
    # Stuff for rendo.boots.degenerates.removed class
    call = call,
    F.formula = F.formula,
    mf = res.lm$model,
    coefficients = coef(res.lm),
    names.main.coefs = rownames(boots),
    fitted.values = fitted(res.lm),
    residuals = residuals(res.lm),
    boots.params = boots,
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,

    # Stuff specific to copula ima
    subclass = "rendo.copula.ima",
    cdf = cdf,
    names.endo.regs = names.endo.regs
  ))
}
