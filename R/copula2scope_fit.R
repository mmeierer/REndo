#' @importFrom stats formula lm model.frame model.matrix na.fail reformulate qnorm residuals
copula2scope_fit <- function(F.formula, data, labels.endo, labels.exo, cdf) {
  # 1st stage ------------------------------------------------------------------

  # . prepare data -------------------------------------------------------------

  mf.all <- model.frame(
    formula(F.formula, rhs = 1, lhs = 1),
    data = data,
    na.action = na.fail
  )

  # endo: only numeric, no intercept
  X.endo <- model.matrix(reformulate(labels.endo, intercept = FALSE), data = mf.all)
  copula2stage_stop_if_constant(X.endo)

  # exo: in order to get k-1 dummies for each factor, there needs to be an
  # intercept. Otherwise model.matrix creates the first factor with k dummies.
  has.exo <- length(labels.exo) > 0
  if (has.exo) {
    # create with intercept for k-1 dummies, then immediately drop again
    X.exo <- model.matrix(reformulate(labels.exo, intercept = TRUE), data = mf.all)
    X.exo <- X.exo[, colnames(X.exo) != "(Intercept)", drop = FALSE]
    copula2stage_stop_if_constant(X.exo)
  }

  # . Pstar --------------------------------------------------------------------

  # apply CDF to endo and exo regressors, neither of which has an intercept
  P.star.endo <- copula2stage_pstar(P = X.endo, cdf = cdf)
  Z.endo <- qnorm(P.star.endo)
  if (has.exo) {
    P.star.exo <- copula2stage_pstar(P = X.exo, cdf = cdf)
    Z.exo <- qnorm(P.star.exo)
  }

  # . 1st stage residuals ------------------------------------------------------
  #first-stage residuals: regressing each endogenous P* on exogenous W*, Eq (9) from Yang et. al 2024

  cop.terms <- copula2stage_get_1ststage_copterms(
    nrow = nrow(X.endo),
    labels.endo = labels.endo
  )

  for (j in seq_len(ncol(X.endo))) {
    Z.endo.j <- Z.endo[, j, drop = TRUE]

    if (!has.exo) {
      if (ncol(Z.endo) == 1) {
        # when there is no exogenous regressors, correction term is just qnorm(F(P_j))
        # This matches the single regressor case in Park & Gupta (2012)
        cop.terms[, j] <- Z.endo.j # I tried replicating the steps in Yang et al. 2025
        #but when it comes to the intercept, since when there is no exo regressors,it should just be
        #correction term qnorm(F(P_j)), this code will still remove the intercept first, before
        # doing the CDF and then applying the P*, then implicitly add the intercept at the end
        #it works well when the dataset has exo regressors as well but when there is a single
        #endo regressor and nothing else, because of the intercept, it may not be the same. So
        #better use the copulaCorrection() method directly
      } else {
        #multiple endogenous regressors without exogenous ones
        #projecting every P_j* on all other P* with intercept
        Z.endo.others <- Z.endo[, -j, drop = FALSE]
        lm.j <- lm(Z.endo.j ~ Z.endo.others) #with intercept
        cop.terms[, j] <- residuals(lm.j)
      }
    } else {
      #situation when there are exogenous regressors
      #projecting on exogenous W* with intercept
      # W_t* exogenous only
      lm.j <- lm(Z.endo.j ~ Z.exo) # With intercept included from 2sCOPE table 1,
      #from ADDRESSING ENDOGENEITY USING A TWO-STAGE COPULA GENERATED REGRESSOR APPROACH (From Yang et al. 2025, page 7 )
      cop.terms[, j] <- residuals(lm.j)
    }
  }

  # 2nd stage --------------------------------------------------------------------
  l.2nd.stage <- copula2stage_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.lm.augmented = l.2nd.stage$res.lm.augmented,
    labels.pcop = l.2nd.stage$labels.pcop
  ))
}
