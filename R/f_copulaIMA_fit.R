#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response
copulaIMA_fit <- function(F.formula, data, cdf) {
  F.formula <- Formula::as.Formula(F.formula)

  names.vars.continuous <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  # Haschka 2025 page 164 eq. 3.1
  mf <- model.frame(F.formula, rhs = 1, lhs = 1, data = data)
  X.main <- model.matrix(F.formula, rhs = 1, lhs = 0, data = mf)

  #Continuous endogenous var
  endogenous.columns <- colnames(X.main)[colnames(X.main) %in% names.vars.continuous]

  if (length(endogenous.columns) == 0) {
    stop("No continuous endogenous regressors found in design matrix.")
  }
  if (length(endogenous.columns) < length(names.vars.continuous)) {
    stop(
      "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling."
    )
  }

  # Create Pstar
  P <- X.main[, endogenous.columns, drop = FALSE]
  P.star <- copulaIMA_pstar(P = P, cdf = cdf)

  # Haschka 2025 page 164 Eq. 3.4
  cop.terms <- copulaIMA_residuals(P.star)

  # Fit:
  #   - formula: Main model as given by the user + additional Pcop regressors
  #   - data: As given by the user (or bootstrapped)
  #
  # Extending the formula with the additional regressors will ensure all coefficients
  # names etc are preserved as the user gave them. This will just let `lm()` do the magic.
  #
  # Build final formula: Main model + Pcop regressors
  # Build by updating the original formula (adding regressors):
  #   update(y ~ X + P,  ~ . + Pcop1 + Pcop2)

  # main formula has to be expanded to replace dot '.' with actual columns first.
  # Take the already expanded forumla from the model frame
  f.main <- formula(mf)

  # Build copula terms (~ . + Pcop1 + Pcop2)
  f.pcop <- reformulate(
    termlabels = c(".", colnames(cop.terms)),
    response = NULL, # empty response (lhs)
    intercept = TRUE # Keep intercept. FALSE would remove it when updating
  )

  # Add copula regressors to existing formula
  f.final <- update(old = f.main, new = f.pcop)

  # Alternative: Build from scratch using the "names" of each regressor
  # f.cop.terms <- reformulate(colnames(cop.terms))
  # f.combined <- reformulate(
  #   c(labels(terms(mf)), colnames(cop.terms)),
  #   response = all.vars(f.main)[1]
  # )

  return(lm(
    formula = f.final,
    data = cbind(data, cop.terms)
  ))
}
