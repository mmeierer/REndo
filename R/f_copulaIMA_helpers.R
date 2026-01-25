#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
#' @importFrom stats ecdf qnorm
#' @importFrom Matrix rankMatrix
pobs_adj <- function(x, na.last = "keep", ties.method= "average", lower.tail = TRUE){

  ties.method <- match.arg(ties.method)
  U <- if (is.matrix(x)){ ##rank or 2 for a matrix?
    U <- apply(x, 2, rank, na.last = na.last, ties.method = ties.method)*((nrow(x) -1)/(nrow(x)^2)) + 1/(2*nrow(x))
  } else{
    U <- rank(x, na.last = na.last, ties.method = ties.method)*((length(x) - 1)/(length(x)^2)) + 1/(2*length(x))
  }

  if (lower.tail){
    return(U)
  } else {
    return (1- U)
  }
}


#' @importFrom copula pobs
#' @importFrom ks kcde
copulaIMA_pstar <- function(P, cdf){

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  if (cdf == "kde"){
    P.star <- apply(P, 2, function(x){
      Fhat <- ks::kcde(x)
      predict (Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf"){
    P.star <- apply (P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf"){
    P.star <- apply (P, 2, pobs_adj)
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


copulaIMA_residuals <- function (P.star, P.names){

  Z <- qnorm(P.star)

  if(!is.matrix(Z)){
    Z <- as.matrix(Z)
  }
  if(is.null(colnames(Z))){
    stop("P.star must have column names")
  }

  # only one endeogenous regressor
  if (length(P.names) ==1){
    res <- matrix(Z[,P.names], ncol=1)
    colnames(res) <- paste0(P.names, "_cop")
    return(res)
  }

  # Having more than one endo regressor
  res <- matrix (NA, nrow(Z), length(P.names))
  colnames(res) <- paste0(P.names, "_cop")

  for (j in seq_along(P.names)){
    Pj <- P.names[j]
    Z.j <- Z [, Pj, drop = FALSE]

    others <- P.names[which(P.names != Pj)]
    Z.others <- Z[, others, drop = FALSE]

    lm.j <- lm(Z.j ~ Z.others)
    res[, j] <- residuals(lm.j)
  }

  res

}


