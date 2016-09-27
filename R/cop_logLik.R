#'@title Log-likelihood 
#
#'@description Computes the log-likelihood function 
#'
# Arguments
#'@param    theta  initial values for the parameters to be optimized over.
#'@param    y  a vector containing the dependent variable. 
#'@param    X  a matrix containing the regressors, with the endogeneous variable being on the last column.
#'@param    P  a vector containing the discrete endogeneous regressor.
#
# return value
#'@return returns the value of the negative log-likelihood.
#'@keywords internal
logLL<- function(theta,y,X,P){       # Log-likelihood  Function
 
  k <- ncol(X)
  k1 <- k+2                           # add 2 for sigma and rho
  beta <- theta[1:k]		             # parameters
  rho1      <-  theta[k+1]
  sig.eps1  <-  theta[k+2]
 
  X <- as.matrix(X)
 
  
  eps1 <- y - X %*% beta		# epsilon, X should contain the endogeneous regressor as well
  
  p.star <- copulaPStar(P)			# function that computes ecdf(P) and P.star
  
  # residulas - epsilon should be normally distributed
    
   ppnorm <- suppressWarnings(stats::pnorm(eps1,mean=0,sd=sig.eps1))
   
   ppnorm <- ifelse(ppnorm >=0.999998, 0.999888, ppnorm)
   ppnorm1 <- ifelse(ppnorm <= 0.0001, 0.00001, ppnorm)
 
   # epsilon star 
   e.star <- stats::qnorm(ppnorm1)
   
  s <- sum((p.star^2 + e.star^2)/(2*(1-rho1^2)) - (rho1*p.star*e.star)/(1-rho1^2))
  
  
  l.eps <- suppressWarnings(sum(log(stats::dnorm(eps1,mean=0,sd=sig.eps1))))
  
  mm <- (length(y)/2)* suppressWarnings(log(1-rho1^2))
  logll <- -mm - s + l.eps
  
  ll <- -1* logll
 return(ll)
}

