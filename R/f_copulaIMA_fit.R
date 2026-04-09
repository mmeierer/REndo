#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response
copulaIMA_fit <- function(f.main, data, cdf, names.endo.regs) {

  # Prepare additional regressors -----------------------------------------------------

  # Haschka 2025 page 164 eq. 3.1
  mf <- model.frame(f.main, data = data)
  X.main <- model.matrix(f.main, data = mf)

  # Continuous endogenous var
  endogenous.columns <- colnames(X.main)[colnames(X.main) %in% names.endo.regs]

  if (length(endogenous.columns) == 0) {
    stop("No continuous endogenous regressors found in design matrix.")
  }
  if (length(endogenous.columns) < length(names.endo.regs)) {
    stop(
      "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling."
    )
  }

  # Create Pstar
  #Applying CDF to all regressors (both endo and exo), excluding the intercept. Haschka IMA 2024, section 3.2 step 1
  X.no.intercept <- X.main[, colnames(X.main) != "(Intercept)", drop = FALSE]
  P.star <- copulaIMA_pstar(P = X.no.intercept, cdf = cdf)

  # Haschka 2025 section 3.2 step 2: regressing each P*_l on exo X* only, without intercept
  cop.terms <- copulaIMA_residuals(P.star, endo.cols = endogenous.columns)


  # Fit final model -------------------------------------------------------------------

  # Fit:
  #   - formula: Main model as given by the user + additional Pcop regressors
  #   - data: As given by the user (or bootstrapped) + Pcop regressor data
  #
  # Extending the formula with the additional regressors will ensure all coefficients
  # names etc are preserved as the user gave them. This will just let `lm()` do the magic.
  #
  # Build final formula: Main model + Pcop regressors
  # Build by updating the original formula (adding regressors):
  #   update(y ~ X + P,  ~ . + Pcop1 + Pcop2)

  # main formula has to be expanded to replace dot '.' with actual columns first.
  # Take the already expanded formula from the model frame
  f.main <- formula(mf)

  # Build copula terms (~ . + Pcop1 + Pcop2)
  has_intercept <- attr(terms(f.main), "intercept") == 1 #to ensure that intercept is handled
  #consistently across all stages

  f.pcop <- reformulate(
    termlabels = c(".", colnames(cop.terms)),
    response = NULL, # empty response (lhs)
    intercept = has_intercept
  )

  # Add copula regressors to existing formula
  f.final <- update(old = f.main, new = f.pcop)

  # Alternative: Build final formula from scratch using the "names" of each regressor
  # f.combined <- reformulate(
  #   c(labels(terms(mf)), colnames(cop.terms)),
  #   response = all.vars(f.main)[1]
  # )

  return(lm(
    formula = f.final,
    data = cbind(data, cop.terms)
  ))
}
