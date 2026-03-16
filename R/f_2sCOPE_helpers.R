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
      u[u == min(u)] <- 1e-7
      u[u == max(u)] <- 1 - 1e-7
      u
    })
  }

  return (P.star)
}


copula2scope_residuals <- function (P.star){

  Z <- qnorm(P.star)

  if(!is.matrix(Z)){
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if(is.null(P.names)){
    stop("P.star must have column names")
  }

  # only one endogenous regressor (should be same behavious as IMA)
  if (length(P.names) ==1){
    res <- matrix(Z[,P.names], ncol=1)
    colnames(res) <- paste0(P.names, "_cop")
    return(res)
  }

  # Having more than one endo regressor
  res <- matrix (NA, nrow(Z), length(P.names))
  colnames(res) <- paste0(P.names, "_cop")

  for (j in seq_along(P.names)){
    Z.j <- Z [, j, drop = FALSE]
    Z.others <- Z[, -j, drop = FALSE]
<<<<<<< Updated upstream
    lm.j <- lm(Z.j ~ Z.others) #intercept included
=======
    lm.j <- lm(Z.j ~ Z.others) #intercept included by default
>>>>>>> Stashed changes
    res[, j] <- residuals(lm.j)
  }

  return(res)

}
