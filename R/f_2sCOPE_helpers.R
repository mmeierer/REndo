#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
#' @importFrom stats ecdf qnorm
#' @importFrom Matrix rankMatrix
pobs_adj_2scope <- function(x){ ##will maybe need a simpler form?
  if (!is.matrix(x)){
    x <- matrix(x, ncol = 1)
  }

  n <- nrow(x)

  U <- apply(x, 2, rank, na.last = "keep", ties.method = "average")*((n-1)/ n^2) + 1/ (2*n)

  return(U)
}


#' @importFrom copula pobs
#' @importFrom ks kcde
copula2scope_pstar <- function(P, cdf){

  if (cdf == "kde"){
    P.star <- apply(P, 2, function(x){
      Fhat <- ks::kcde(x)
      predict (Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf"){
    P.star <- apply (P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf"){
    P.star <- apply (P, 2, pobs_adj_2scope)
  } else {
    ecdf0 <- apply(P, 2, ecdf)
    P.star <- sapply(seq_along(ecdf0), function(i){
      u <- ecdf0[[i]](P[,i])
      u[u == min(u)] <- 10e-7
      u[u == max(u)] <- 1 - 10e-7
      u
    })
  }

  return (P.star)
}


copula2scope_residuals <- function (P.star, endo.col){

  Z <- qnorm(P.star)

  if(!is.matrix(Z)){
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if(is.null(P.names)){
    stop("P.star must have column names")
  }

  #Exogenous columns are columns which are not endo
  exo.cols <- P.names[!P.names %in% endo.cols]

  res <- matrix(NA, nrow(Z), length(endo.cols))
  colnames(res) <- paste0(endo.cols, "_cop")


  for (j in seq_along(endo.cols)){
    Z.j <- Z [, endo.cols[j], drop = FALSE]

    if (length(exog.cols) == 0) { #when there is no exogenous regressors, correction term is just qnorm(F(P_j))
      # This matches the single-regressor case in Park & Gupta (2012)
      res[, j] <- Z.j
    } else {
      Z.exog <- Z[, exog.cols, drop = FALSE]   # W_t* exogenous only
      lm.j   <- lm(Z.j ~ Z.exog)              # With intercept included from 2sCOPE table 1,
      #from ADDRESSING ENDOGENEITY USING A TWO-STAGE COPULA GENERATED REGRESSOR APPROACH (From Yang et al. 2024, page 7 )
      res[, j] <- residuals(lm.j)
    }
  }
  return(res)

}
