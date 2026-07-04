#' @importFrom stats ecdf qnorm
copulaCorrectionContinuous_pstar <- function(vec.data.endo){

  # Helper function to generate the p.star values for each single
  # fct.single.col.pstar.continuous <- function(single.col.P){
  H.p <- stats::ecdf(vec.data.endo)
  U.p <- H.p(vec.data.endo)
  n <- length(vec.data.endo)

  U.p[U.p == 0] <- 1 - n/(n+1) #0.0000001
  U.p[U.p == 1] <- n/(n+1)#0.9999999

  p.star <- stats::qnorm(U.p)
  p.star[p.star == 0] <- 1 - n/(n+1) #0.0000001
  return(p.star)
}
