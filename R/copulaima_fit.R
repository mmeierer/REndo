#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response na.fail qnorm residuals
copulaima_fit <- function(F.formula, data, labels.endo, labels.exo, cdf) {
  # Stage 1 --------------------------------------------------------------------

  # . Create Pstar -------------------------------------------------------------
  # Applying CDF to all regressors (both endo and exo), excluding the intercept.
  # Haschka IMA 2024, section 3.2 step 1
  # Haschka 2025 page 164 eq. 3.1

  mf.all <- model.frame(formula(F.formula, lhs = 1, rhs = 1), data = data, na.action = na.fail)

  f.endo <- reformulate(termlabels = labels.endo, response = NULL, intercept = FALSE)
  X.endo <- model.matrix(f.endo, data = mf.all)
  copula2stage_stop_if_constant(X.endo)

  # (constructing P star, from Haschka 2025, page 164)
  # copula2stage_pstar is applied column-wise
  P.star.endo <- copula2stage_pstar(P = X.endo, cdf = cdf)

  # NOTE: Having no intercept makes model.matrix() code first factor with k dummies
  # instead of the regular k-1 dummies! This is different from Haschkas
  # implementation which uses the whole formula.
  # But: All this doesnt matter as long as exo vars are only numeric.
  f.exo <- reformulate(termlabels = labels.exo, response = NULL, intercept = FALSE)
  X.exo <- model.matrix(f.exo, data = mf.all)
  copula2stage_stop_if_constant(X.exo)
  P.star.exo <- copula2stage_pstar(P = X.exo, cdf = cdf)

  # . Create copula terms ------------------------------------------------------
  # Haschka 2025 section 3.2 step 2: regressing each P*_l on exo X* only, without intercept

  Z.endo <- qnorm(P.star.endo)
  Z.exo <- qnorm(P.star.exo)

  # Exogenous normal scores: all the columns that are not endo
  # chapter 3.2 step 2: regressing P*l on X1*, ..., XK* (exogenous only)

  cop.terms <- copula2stage_get_1ststage_copterms(
    nrow = nrow(data),
    labels.endo = labels.endo
  )

  for (j in seq_len(ncol(Z.endo))) {

    # There are always exo regressors
    Z.j <- Z.endo[, j, drop = TRUE]
    lm.j <- lm(Z.j ~ Z.exo - 1)
    cop.terms[, j] <- residuals(lm.j)

    # if (has.exo) {
    #   lm.j <- lm(Z.j ~ Z.exo - 1)
    #   cop.terms[, j] <- residuals(lm.j)
    # } else {
    #   cop.terms[, j] <- Z.j # no exo regressors
    # }
  }

  # Stage 2 --------------------------------------------------------------------
  res.2nd.stage <- copula2stage_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.lm.augmented = res.2nd.stage$res.lm.augmented,
    labels.pcop = res.2nd.stage$labels.pcop
  ))
}
