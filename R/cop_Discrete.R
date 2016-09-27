#'@title Fitting Linear Models with Endogeneous Discrete Regressors using Internal Instrumental Variables
# Description
#'@description  Fits linear models with discrete, endogeneous regressors using the approach described in Park and Gupta (2012).
#
# Arguments
#'@param  y  the vector containing the dependent variable. 
#'@param  X  the matrix containing the regressors, with the endogenous variables occupying the last columns.
#'@param  P  the matrix containing the discrete endogeneous regressors.
#'@param  intercept  by deault the model is estimated adding an intercept. If no intercept is required, intercept should be set to FALSE.
# Return Value
#'@return  Returns an object of class "lm".
#'@references Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86.

#'@keywords internal

copulaDiscrete <- function(y,X,P, intercept = NULL) {

 sim <- 250  
 k1 <- ncol(X)   
# P is the endogeneous regressor
 P1 <- as.matrix(P)
 colnames(P1) <- colnames(X)[-c(1:(ncol(X)-ncol(P1)))]

#  empirical cumulative distribution function
	 getEcdf <- function(x){
    H.p <- stats::ecdf(x)
    H.p1 <- H.p(x)
    H.p0 <- H.p(x-1)
    H.p0 <- ifelse(H.p0==0,0.0000001,H.p0)
    H.p0 <- ifelse(H.p0==1,0.9999999,H.p0)
    H.p1 <- ifelse(H.p1==0,0.0000001,H.p1)
    H.p1 <- ifelse(H.p1==1,0.9999999,H.p1)
        
    gecdf <- list(H.p = H.p, H.p1=H.p1, H.p0 = H.p0)
    
    return(gecdf)
  }


# each element of pecdf had 3 levels - H.p, H.p0 and H.p1. 
# Nr. of elements of pecdf = Nr. columns P1
 pecdf <- apply(P1,2,getEcdf)

  a <- matrix(0,sim,ncol(P1))
  sd.a <- matrix(0,sim,ncol(P1))
  if (is.null(intercept)) {
  b <- matrix(0,sim,ncol(X)-ncol(P1)+1)
  sd.b <- matrix(0,sim,ncol(X)-ncol(P1)+1)
  } else 
  {
    b <- matrix(0,sim,ncol(X)-ncol(P1))
    sd.b <- matrix(0,sim,ncol(X)-ncol(P1))
  }
  ps <- matrix(0,sim,ncol(P1))
  sd.ps <- matrix(0,sim,ncol(P1))

  U.p <- matrix(NA, dim(P1)[1], dim(P1)[2])
  
for (i in 1:sim){
  
  for (k in 1:dim(P1)[2]){
    
    U.p[,k] <- stats::runif(dim(P1)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
    
  }
  
  p.star <- apply(U.p, 2,stats::qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P1),sep=".")
     
  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- X1

  if (!is.null(intercept)){  # no intercept 
  meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))
  a[i,] <- meth.2$coefficients[-c(1:(ncol(X)-ncol(P1)),(k2-ncol(P1)+1):k2)]  # coef of endogenous variables
  b[i,] <- meth.2$coefficients[1:(ncol(X)-ncol(P1))]  # coef of exogenous variables
  ps[i,] <- meth.2$coefficients[(k2-ncol(P1)+1):k2]  # pstar-s
  } else {
    meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))  # with intercept
    ps[i,] <- meth.2$coefficients[-c(1:(ncol(X)-ncol(P1)+1),(k2-ncol(P1)+1):k2)]
    b[i,] <- meth.2$coefficients[1:(ncol(X)-ncol(P1)+1)]
    a[i,] <- meth.2$coefficients[(k2-ncol(P1)+1):k2]
  }
}
a.est <- apply(a,2,mean)
b.est <- apply(b, 2,mean)
ps.est <- apply(ps,2,mean)
sd.a <- apply(a,2,stats::sd)
sd.b <- apply(b,2,stats::sd)
sd.ps <- apply(ps,2,stats::sd)


res <- list(a.est = a.est, b.est = b.est, ps.est = ps.est, se.a=sd.a, se.b = sd.b, se.ps = sd.ps, reg=X1)
class(res) <- c("copulaEndo")
return(res)
}