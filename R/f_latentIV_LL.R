#' @importFrom mvtnorm dmvnorm
latentIV_LL<- function(params, m.data.mvnorm, use.intercept,
                       name.intercept, name.endo.param){

  # Extract main model coefficients --------------------------------------------
  # Number of coefs depends on if there is an intercept or not.
  # Set b00 to 0 if there is no intercept
  if(use.intercept){
    # With intercept (2 coefs from lm)
    b00     <- params[name.intercept]
    a1      <- params[name.endo.param]
  }else{
    # No intercept (only 1 coef in lm)
    b00     <- 0
    a1      <- params[name.endo.param]
  }

  # Probability for group 1 - constraint to [0,1]
  # (incl bound because can be very large which flips to 0 and 1)
  pt <- exp(params["theta8"])
  pt <- pt / (1+pt)


  # Sigma ----------------------------------------------------------------------
  m.sigma.tmp <- matrix(c(params["theta5"], 0,
                          params["theta6"], params["theta7"]),
                        byrow = TRUE, ncol = 2, nrow = 2)
  m.sigma     <- m.sigma.tmp %*% t(m.sigma.tmp)

  # Varcov matrix reduced form -------------------------------------------------
  s2e     <- m.sigma[1,1]
  s2v     <- m.sigma[2,2]
  sev     <- m.sigma[1,2]

  varcov      <- matrix(0,2,2)
  varcov[1,1] <- a1*a1*s2v+2*a1*sev+s2e
  varcov[2,1] <- a1*s2v+sev
  varcov[1,2] <- varcov[2,1]
  varcov[2,2] <- s2v


  # Group 1 contribution -------------------------------------------------------
  pi1 <- params["pi1"]

  mu1 <- matrix(data = c(b00+a1*pi1, pi1), nrow=2, ncol=1)

  pdf1 <- dmvnorm(m.data.mvnorm, mean=mu1, sigma=varcov)

  # Group 2 contribution -------------------------------------------------------
  pi2 <- params["pi2"]
  mu2 <- matrix(c(b00+a1*pi2, pi2), nrow=2,ncol=1)

  pdf2 <- dmvnorm(m.data.mvnorm, mean=mu2, sigma=varcov)

  # Log Likelihood -------------------------------------------------------------

  logLL <-  sum(log(pt*pdf1 + (1-pt)*pdf2))

  return(-1*logLL)
}
