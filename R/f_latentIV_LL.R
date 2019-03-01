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
  pt <- exp(params["theta5"])
  pt <- pt / (1+pt)


  # Sigma ----------------------------------------------------------------------
  # Previous code:
  # m.sigma.tmp <- matrix(c(params["theta8"], 0,
  #                         params["theta6"], params["theta7"]),
  #                       byrow = TRUE, ncol = 2, nrow = 2)
  # m.sigma     <- m.sigma.tmp %*% t(m.sigma.tmp)

  m.sigma  <- matrix(c(params["theta6"], params["theta7"],
                       params["theta7"], params["theta8"]),
                        byrow = TRUE, ncol = 2, nrow = 2)


  # Varcov matrix reduced form -------------------------------------------------
  s2e     <- m.sigma[1,1] # theta6
  s2v     <- m.sigma[2,2] # theta8
  sev     <- m.sigma[1,2] # theta7

  varcov      <- matrix(0,2,2)
  varcov[1,1] <- a1*a1*s2v+2*a1*sev+s2e
  varcov[2,1] <- a1*s2v+sev
  varcov[1,2] <- varcov[2,1]
  varcov[2,2] <- s2v


  # Group 1 contribution -------------------------------------------------------
  pi1 <- params["pi1"]

  mu1 <- matrix(data = c(b00+a1*pi1, pi1), nrow=2, ncol=1)

  # Use log probs after LL explanations
  # pdf1 <- dmvnorm(m.data.mvnorm, mean=mu1, sigma=varcov)

  # Group 2 contribution -------------------------------------------------------
  pi2 <- params["pi2"]
  mu2 <- matrix(c(b00+a1*pi2, pi2), nrow=2,ncol=1)

  # Use log probs after LL explanations
  # pdf2 <- dmvnorm(m.data.mvnorm, mean=mu2, sigma=varcov)

  # Log Likelihood -------------------------------------------------------------
  # original: sum(log(pt*pdf1 + (1-pt)*pdf2))
  #
  # log(A+B)
  # log(exp(log(A)) + exp(log(B)))
  # -> LogSumExp trick:
  # max(log(A), log(B)) + log(exp(log(A)-max) + exp(log(B)-max))
  # A = pt*pdf1    B = (1-pt)*pdf2
  # Step 1:
  # max(log(pt*pdf1), log((1-pt)*pdf2))
  # max(log(pt)+log(pdf1), log(1-pt)+log(pdf2))
  # Step 2:
  # log(exp(log(pt*pdf1)-max) + exp(log(1-pt)*pdf2()-max))
  # log(exp(log(pt)+log(pdf1)-max) + exp(log(1-pt)+log(pdf2)-max))
  # log(pt*exp(log(pdf1)-max)) + (1-pt)*exp(log(pdf2)-max)))
  # -> log(pdf1)&log(pdf2) = dmvnorm(..., log=TRUE)
  log.pdf1 <- dmvnorm(m.data.mvnorm, mean=mu1, sigma=varcov, log = TRUE)
  log.pdf2 <- dmvnorm(m.data.mvnorm, mean=mu2, sigma=varcov, log = TRUE)

  max.AB   <- pmax(log(pt)+log.pdf1, log(1-pt)+log.pdf2)
  logLL.lse<- sum(max.AB+log(pt*exp(log.pdf1-max.AB) + (1-pt)*exp(log.pdf2-max.AB)))

  # logLL <-  sum(log(pt*pdf1 + (1-pt)*pdf2))
  # print(isTRUE(all.equal(logLL, logLL.lse))) # TRUE
  return(-1*logLL.lse)
}
