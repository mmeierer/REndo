#' @importFrom Formula as.Formula
#' @importFrom stats formula lm model.frame model.matrix terms reformulate update
copula2sCOPE_fit <- function(F.formula, data, cdf) {
  F.formula <- Formula::as.Formula(F.formula)

  names.vars.continuous <- formula_readout_special(
    F.formula = F.formula,
    name.special = "continuous",
    from.rhs = 2,
    params.as.chars.only = TRUE
  )

  F.formula.main <- formula(F.formula, rhs = 1, lhs = 1)
  mf <- model.frame(F.formula.main, data = data)
  X.main <- model.matrix(F.formula.main, data = mf)

  endogenous.columns <- colnames(X.main)[
    colnames(X.main) %in% names.vars.continuous
  ]

  if (length(endogenous.columns) == 0) {
    stop(
      "No continuous endogenous regressors found in the design matrix.",
      call. = FALSE
    )
  }

  if (length(endogenous.columns) < length(names.vars.continuous)) {
    stop(
      "Bootstrap sample dropped at least one endogenous regressor. This happened when a regressor becomes constant in the resampling."
    )
  }

  #applying CDF to all regressors both endo and exo with no intercept column
  X.no.intercept <- X.main[, colnames(X.main) != "(Intercept)", drop = FALSE]
  P.star <- copula_pstar(P = X.no.intercept, cdf = cdf)

  #first-stage residuals: regressing each endogenous P* on exogenous W*, Eq (9) from Yang et. al 2024
  cop.terms <- copula2sCOPE_residuals(P.star = P.star, endo.cols = endogenous.columns)


  f.main <- formula(mf)
  # colnames(cop.terms) may contain chars illegal for formula labels
  colnames(cop.terms) <- make.names(colnames(cop.terms))
  f.pcop <- reformulate(
    termlabels = c(".", colnames(cop.terms)),
    response = NULL,
    intercept = TRUE
  )

  f.final <- update(old = f.main, new = f.pcop)

  return(lm(
    formula = f.final,
    data = cbind(data, cop.terms)
  ))
}


#' @importFrom stats qnorm lm residuals
copula2sCOPE_residuals <- function(P.star, endo.cols) {
  Z <- qnorm(P.star)

  if (!is.matrix(Z)) {
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if (is.null(P.names)) {
    stop("P.star must have column names")
  }

  #Exogenous columns are columns which are not endo
  exo.cols <- P.names[!P.names %in% endo.cols]

  res <- matrix(NA, nrow(Z), length(endo.cols))
  colnames(res) <- paste0(endo.cols, "_cop")

  for (j in seq_along(endo.cols)) {
    Z.j <- Z[, endo.cols[j], drop = TRUE]

    if (length(exo.cols) == 0) {
      if (length(endo.cols) == 1) {
        # when there is no exogenous regressors, correction term is just qnorm(F(P_j))
        # This matches the single regressor case in Park & Gupta (2012)
        res[, j] <- Z.j # I tried replicating the steps in Yang et al. 2025
        #but when it comes to the intercept, since when there is no exo regressors,it should just be
        #correction term qnorm(F(P_j)), this code will still remove the intercept first, before
        # doing the CDF and then applying the P*, then implicitly add the intercept at the end
        #it works well when the dataset has exo regressors as well but when there is a single
        #endo regressor and nothing else, because of the intercept, it may not be the same. So
        #better use the copulaCorrection() method directly
      } else {
        #multiple endogenous regressors without exogenous ones
        #projecting every P_j* on all other P* with intercept
        other.endo.cols <- endo.cols[-j]
        Z.others <- Z[, other.endo.cols, drop = FALSE]
        lm.j <- lm(Z.j ~ Z.others) #with intercept
        res[, j] <- residuals(lm.j)
      }
    } else {
      #situation when there are exogenous regressors
      #projecting on exogenous W* with intercept
      Z.exog <- Z[, exo.cols, drop = FALSE] # W_t* exogenous only
      lm.j <- lm(Z.j ~ Z.exog) # With intercept included from 2sCOPE table 1,
      #from ADDRESSING ENDOGENEITY USING A TWO-STAGE COPULA GENERATED REGRESSOR APPROACH (From Yang et al. 2025, page 7 )
      res[, j] <- residuals(lm.j)
    }
  }
  return(res)
}
