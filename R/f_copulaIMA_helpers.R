copulaIMA_adjecdf <- function(x){

  if(!is.matrix(x)){
    x <- matrix(x, ncol = 1)
  }

  n <- nrow(x)

  #Adjusted empirical CDF
  #U_i = rank_i/n * (n-1/n) + (1/2n) and simplifies to U_i = (rank_i-1/2)/ n.
  #This is a midrank adjustment used in nonparametric statistics to handle tied data points/identical values
  #when calculating the empirical CDF or assigning ranks.
  #This keeps the observations strictly inside (0,1) and avoids infinities when
  #applying qnorm().
  U <- apply(x, 2, rank, na.last = "keep", ties.method = "average")*((n-1)/ n^2) + 1/ (2*n)

  return(U)
}

#' @importFrom copula pobs
#' @importFrom ks kcde
#' @importFrom stats ecdf predict
copulaIMA_pstar <- function(P, cdf){

  if (cdf == "kde"){
    P.star <- apply(P, 2, function(x){
      Fhat <- ks::kcde(x)
      predict (Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf"){
    P.star <- apply (P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf"){
    P.star <- apply (P, 2, copulaIMA_adjecdf)
  } else {
    ecdf0 <- apply(P, 2, ecdf)
    P.star <- sapply(seq_along(ecdf0), function(i){
      u <- ecdf0[[i]](P[,i])
      u[u == min(u)] <- 1e-7
      u[u == max(u)] <- 1 - 1e-7
      u
    })

    P.star <- as.matrix(P.star)
    colnames(P.star) <- colnames(P)
  }

  return (P.star) #constructing P star, from Haschka 2025, page 164
}


#' @importFrom stats qnorm lm residuals
copulaIMA_residuals <- function (P.star){

  Z <- qnorm(P.star)

  if(!is.matrix(Z)){
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if(is.null(P.names)){
    stop("P.star must have column names")
  }

  # only one endeogenous regressor
  if (length(P.names) ==1){
    res <- matrix(Z[,P.names], ncol=1)
    colnames(res) <- paste0(P.names, "_cop")
    return(res)
  }

  # Having more than one endo regressor
  res <- matrix(data=NA, nrow=nrow(Z), ncol=length(P.names))
  colnames(res) <- paste0(P.names, "_cop")

  for (j in seq_along(P.names)){
    Z.j <- Z [, j, drop = FALSE]
    Z.others <- Z[, -j, drop = FALSE]
    lm.j <- lm(Z.j ~ Z.others) #Haschka 2025 Eq. 3.3
    res[, j] <- residuals(lm.j)
  }

  return(res)

}


