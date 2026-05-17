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
      if (length(endo.cols) ==1){
        # when there is no exogenous regressors, correction term is just qnorm(F(P_j))
        # This matches the single regressor case in Park & Gupta (2012)
        res[, j] <- Z.j # I tried replicating the steps in Yang et al. 2025
        #but when it comes to the intercept, since when there is no exo regressors,it should just be
        #correction term qnorm(F(P_j)), this code will still remove the intercept first, before
        # doing the CDF and then applying the P*, then implicitly add the intercept at the end
        #it works well when the dataset has exo regressors as well but when there is a single
        #endo regressor and nothing else, because of the intercept, it may not be the same. So
        #better use the copulaCorrection() method directly
      } else{
        #multiple endogenous regressors without exogenous ones
        #projecting every P_j* on all other P* with intercept
        other.endo.cols <- endo.cols[-j]
        Z.others <- Z[, other.endo.cols, drop =FALSE]
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
