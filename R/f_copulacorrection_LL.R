#' @importFrom stats pnorm qnorm dnorm
copulaCorrection_LL <- function(params, vec.y, m.data.exo.endo, vec.data.endo.pstar){

  # Extract params from optimx inputs --------------------------------------------------------
  params.endo.exo <- params[setdiff(names(params), c("sigma", "rho"))]
  sigma           <- params["sigma"]
  rho             <- params["rho"]

  # Constrain rho to [0,1] and sigma to [0, +Inf]
  #   Because the vcov is not derived from the hessian, the hessian is not caluclated and
  #     returning NA is not a problem
  if(rho > 1 | rho < 0)
    return(NA_real_)

  if(sigma < 0)
    return(NA_real_)

  # epsilon, incl. endo regressor ------------------------------------------------------------

  # Reorder params to fit matrix col order
  params.endo.exo <- params.endo.exo[colnames(m.data.exo.endo)]

  eps.1 <- vec.y - m.data.exo.endo %*% params.endo.exo

  # PPnorm -----------------------------------------------------------------------------------
  # residulas - epsilon should be normally distributed
  ppnorm <- pnorm(eps.1, mean=0, sd=sigma)
  ppnorm[ppnorm >= 0.999998]   <- 0.999998
  ppnorm[ppnorm <= 0.0001]     <- 0.0001

  # epsilon star -----------------------------------------------------------------------------
  eps.star <- stats::qnorm(ppnorm)

  # l.eps ------------------------------------------------------------------------------------
  # l.eps <- sum(log(dnorm(eps.1,mean=0,sd=sigma)))
  l.eps <- sum(dnorm(eps.1,mean=0,sd=sigma, log = TRUE))

  # s
  s <- sum((vec.data.endo.pstar^2 + eps.star^2)/(2*(1-rho^2)) -
             (rho*vec.data.endo.pstar*eps.star)/(1-rho^2))
  # mm
  mm <- (length(vec.y)/2)* log(1-rho^2)
  # LL
  log.LL <- -mm - s + l.eps
  return(-1 * log.LL)

}
