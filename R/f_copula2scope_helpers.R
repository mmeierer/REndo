#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
#' @importFrom stats ecdf qnorm
#' @importFrom Matrix rankMatrix
pobs_adj_2sCOPE <- function(x) {
  ##will maybe need a simpler form?
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  n <- nrow(x)

  U <- apply(x, 2, rank, na.last = "keep", ties.method = "average") *
    ((n - 1) / n^2) +
    1 / (2 * n)

  return(U)
}


#' @importFrom copula pobs
#' @importFrom ks kcde
copula2sCOPE_pstar <- function(P, cdf) {
  if (cdf == "kde") {
    P.star <- apply(P, 2, function(x) {
      Fhat <- ks::kcde(x)
      predict(Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf") {
    P.star <- apply(P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf") {
    P.star <- apply(P, 2, pobs_adj_2sCOPE)
  } else {
    ecdf0 <- apply(P, 2, ecdf)
    P.star <- sapply(seq_along(ecdf0), function(i) {
      u <- ecdf0[[i]](P[, i])
      u[u == min(u)] <- 10e-7
      u[u == max(u)] <- 1 - 10e-7
      u
    })
    P.star <- as.matrix(P.star)
  }

  colnames(P.star) <- colnames(P)
  return(P.star)
}


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
        res[, j] <- Z.j # I tried replicating the steps in Yang et al. 2024
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
      #from ADDRESSING ENDOGENEITY USING A TWO-STAGE COPULA GENERATED REGRESSOR APPROACH (From Yang et al. 2024, page 7 )
      res[, j] <- residuals(lm.j)
    }
  }
  return(res)
}
