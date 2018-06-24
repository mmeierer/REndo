#' @importFrom stats ecdf qnorm
copulaPStar <- function(data.endo){

  # Helper function to generate the p.star values for each single
  fct.single.col.pstar <- function(single.col.P){
    H.p <- stats::ecdf(single.col.P)
    U.p <- H.p(single.col.P)

    U.p[U.p == 0] <- 0.0000001
    U.p[U.p == 1] <- 0.9999999

    p.star <- stats::qnorm(U.p)
    p.star[p.star == 0] <- 0.0000001
    return(p.star)
  }

  # Apply the P.star generating function on each column of the given data
  p.star <- apply(X = data.endo, MARGIN = 2, FUN = fct.single.col.pstar)
  colnames(p.star) <- paste("PStar", colnames(data.endo), sep=".")
  return(p.star)
}
