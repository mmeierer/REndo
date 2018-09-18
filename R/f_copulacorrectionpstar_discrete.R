copulaCorrectionDiscrete_pstar <- function(vec.data.endo){

  # Definition: P.star for discrete data --------------------------------------------------------------
  # fct.single.col.p.star.discrete <- function(single.col.endo){

    # Fit empirical CDF to data
    H.p  <- stats::ecdf(vec.data.endo)

    # Caluclate H.pX
    H.p1 <- H.p(vec.data.endo)
    H.p0 <- H.p(vec.data.endo-1)
    H.p0[H.p0 == 0] <- 0.0000001
    H.p0[H.p0 == 1] <- 0.9999999
    H.p1[H.p1 == 0] <- 0.0000001
    H.p1[H.p1 == 1] <- 0.9999999

    # Calculate U.p
    U.p <- runif(n = length(vec.data.endo), min=H.p0, max=H.p1)

    # Calculate p.star
    return(qnorm(U.p))
  # }
#

  # return(apply(X=df.data.endo, MARGIN = 2, FUN = fct.single.col.p.star.discrete))
}
