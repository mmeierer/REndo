copula_adj_ecdf <- function(x) {
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  n <- nrow(x)

  #Adjusted empirical CDF
  #U_i = rank_i/n * (n-1/n) + (1/2n) and simplifies to U_i = (rank_i-1/2)/ n.
  #This is a midrank adjustment used in nonparametric statistics to handle tied data points/identical values
  #when calculating the empirical CDF or assigning ranks.
  #This keeps the observations strictly inside (0,1) and avoids infinities when
  #applying qnorm().
  U <- apply(x, 2, rank, ties.method = "average") * ((n - 1) / n^2) + 1 / (2 * n)

  return(U)
}


#' @importFrom copula pobs
#' @importFrom ks kcde
#' @importFrom stats ecdf predict
copula_pstar <- function(P, cdf) {
  if (cdf == "kde") {
    P.star <- apply(P, 2, function(x) {
      Fhat <- ks::kcde(x)
      predict(Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf") {
    P.star <- apply(P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf") {
    P.star <- apply(P, 2, copula_adj_ecdf)
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
