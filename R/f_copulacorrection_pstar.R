#' @importFrom stats ecdf qnorm
copulaPStar <- function(P){
# ** ?? What if P has > 1 column (ie regressor)-ecdf per regressor or all together. Also considering the usage in method 2! ??
  H.p <- stats::ecdf(P)
  H.p <- H.p(P)

  H.p <- ifelse(H.p==0,0.0000001,H.p)
  H.p <- ifelse(H.p==1,0.9999999,H.p)

  U.p <- H.p
  p.star <- stats::qnorm(U.p)
  p.star <- ifelse(p.star==0,0.0000001,p.star)
  return(p.star)
}
