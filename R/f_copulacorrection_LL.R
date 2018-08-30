#' @importFrom stats pnorm qnorm dnorm
copulaCorrection_LL <- function(params, vec.y, m.data.exo.endo, m.data.endo){
# print(params)
  # Extract params from optimx inputs --------------------------------------------------------
  params.endo.exo <- params[setdiff(names(params), c("sigma", "rho"))]
  sigma           <- params["sigma"]
  rho             <- params["rho"]

  # Bounds for rho: [0,1]. Return Inf as NelderMead cannot handle bounds
  if(rho < 0 || rho > 1)
    return(Inf)

  # P.star -----------------------------------------------------------------------------------
  p.star <- copulaCorrectionContinuous_pstar(data.endo = m.data.endo)

  # epsilon, incl. endo regressor ------------------------------------------------------------
  eps.1 <- vec.y - m.data.exo.endo %*% params.endo.exo

  # PPnorm -----------------------------------------------------------------------------------
  # residulas - epsilon should be normally distributed
  ppnorm <- stats::pnorm(eps.1, mean=0, sd=sigma)
  ppnorm[ppnorm >= 0.999998]   <- 0.999888
  ppnorm[ppnorm <= 0.0001]     <- 0.0001

  # epsilon star -----------------------------------------------------------------------------
  eps.star <- stats::qnorm(ppnorm)

  # l.eps ------------------------------------------------------------------------------------
  l.eps <- sum(log(stats::dnorm(eps.1,mean=0,sd=sigma)))

  # s
  s <- sum((p.star^2 + eps.star^2)/(2*(1-rho^2)) - (rho*p.star*eps.star)/(1-rho^2))
  # mm
  mm <- (length(vec.y)/2)* log(1-rho^2)
  # LL
  log.LL <- -mm - s + l.eps
  return(-1 * log.LL)

}
