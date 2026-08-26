#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response
copulaima_fit <- function(f.main, data, cdf, names.endo.regs) {

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
  P.star <- copula_pstar(P = X.no.intercept, cdf = cdf) # (constructing P star, from Haschka 2025, page 164)

  # Haschka 2025 section 3.2 step 2: regressing each P*_l on exo X* only, without intercept
  cop.terms <- copulaima_residuals(P.star, endo.cols = endogenous.columns)


  # Stage 2 --------------------------------------------------------------------
  res.2nd.stage <- copula_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.augmented = res.2nd.stage$res.augmented,
    labels.pcop = res.2nd.stage$labels.pcop
  ))
}


#' @importFrom stats qnorm lm residuals
copulaima_residuals <- function(P.star, endo.cols) {
  Z <- qnorm(P.star)

  if (!is.matrix(Z)) {
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if (is.null(P.names)) {
    stop("P.star must have column names")
  }

  # Exogenous normal scores: all the columns that are not endo
  # chapter 3.2 step 2: regressing P*l on X1*, ..., XK* (exogenous only)
  exo.cols <- colnames(Z)[!colnames(Z) %in% endo.cols]

  res <- matrix(NA, nrow = nrow(Z), ncol = length(endo.cols))
  colnames(res) <- paste0(endo.cols, "_cop")

  for (j in seq_along(endo.cols)) {
    Z.j <- Z[, endo.cols[j], drop = TRUE]

    if (length(exo.cols) ==0){
      res[, j] <- Z.j # no exo regressors
    } else{
      Z.exo <- Z[, exo.cols, drop = FALSE]
      lm.j <- lm(Z.j ~ Z.exo - 1)
      res[, j] <- residuals(lm.j)
    }
  }
  return(res)
}

