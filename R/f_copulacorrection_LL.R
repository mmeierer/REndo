#' @importFrom stats pnorm qnorm dnorm
copulaCorrection_LL <- function(params, vec.y, m.data.exo.endo, vec.data.endo){

  # Extract params from optimx inputs --------------------------------------------------------
  params.endo.exo <- params[setdiff(names(params), c("sigma", "rho"))]
  sigma           <- params["sigma"]
  rho             <- params["rho"]

  # Constrain rho to [0,1]
  # (incl bound because can be very large which flips to 0 and 1)
  rho <- exp(rho)
  rho <- rho / (1+rho)

  # P.star -----------------------------------------------------------------------------------
  p.star <- copulaCorrectionContinuous_pstar(vec.data.endo = vec.data.endo)

  # epsilon, incl. endo regressor ------------------------------------------------------------

  # Reorder params to fit matrix col order
  params.endo.exo <- params.endo.exo[colnames(m.data.exo.endo)]

  eps.1 <- vec.y - m.data.exo.endo %*% params.endo.exo

  # PPnorm -----------------------------------------------------------------------------------
  # residulas - epsilon should be normally distributed
  ppnorm <- pnorm(eps.1, mean=0, sd=sigma)
  ppnorm[ppnorm >= 0.999998]   <- 0.999888
  ppnorm[ppnorm <= 0.0001]     <- 0.0001

  # epsilon star -----------------------------------------------------------------------------
  eps.star <- stats::qnorm(ppnorm)

  # l.eps ------------------------------------------------------------------------------------
  # l.eps <- sum(log(dnorm(eps.1,mean=0,sd=sigma)))
  l.eps <- sum(dnorm(eps.1,mean=0,sd=sigma, log = TRUE))

  # s
  s <- sum((p.star^2 + eps.star^2)/(2*(1-rho^2)) - (rho*p.star*eps.star)/(1-rho^2))
  # mm
  mm <- (length(vec.y)/2)* log(1-rho^2)
  # LL
  log.LL <- -mm - s + l.eps
  return(-1 * log.LL)

}
