#' @importFrom mvtnorm dmvnorm
latentIV_LL<- function(params, m.data.mvnorm){ # vec.data.resonse, m.data.endo){

  # Data for dmvnorm quantiles -------------------------------------------------
  # m.data.mvnorm <- cbind(vec.data.resonse, m.data.endo) # single matrix needed

  # Sigma ----------------------------------------------------------------------
  m.sigma.tmp <- matrix(c(params["theta5"], 0,
                          params["theta6"], params["theta7"]),
                        byrow = T, ncol = 2, nrow = 2)
  m.sigma     <- m.sigma.tmp %*% t(m.sigma.tmp)

  # Varcov matrix reduced form -------------------------------------------------
  a1      <- params["a1"]
  s2e     <- m.sigma[1,1]
  s2v     <- m.sigma[2,2]
  sev     <- m.sigma[1,2]

  varcov      <-  matrix(0,2,2)
  varcov[1,1] <- a1*a1*s2v+2*a1*sev+s2e
  varcov[2,1] <- a1*s2v+sev
  varcov[1,2] <- varcov[2,1]
  varcov[2,2] <- s2v


  # Group 1 contribution -------------------------------------------------------
  pi1 <- params["pi1"]
  if("b00" %in%names(params))
  b00 <- params["b00"]

  mu1 <- matrix(data = c(b00+a1*pi1, pi1), nrow=2, ncol=1)

  pdf1 <- dmvnorm(m.data.mvnorm, mean=mu1, sigma=varcov)

  # Group 2 contribution -------------------------------------------------------
  pi2 <- params["pi2"]
  mu2 <- matrix(c(b00+a1*pi2, pi2), nrow=2,ncol=1)

  pdf2 <- dmvnorm(m.data.mvnorm, mean=mu2, sigma=varcov)

  # Log Likelihood -------------------------------------------------------------

  # Probability for group 1
  pt <- exp(params["theta8"])
  pt <- pt / (1+pt)

  logLL <-  sum(log(pt*pdf1 + (1-pt)*pdf2))

  return(-1*logLL)
}
