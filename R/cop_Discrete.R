#'@title Fitting Linear Models with Endogeneous Discrete Regressors using Internal Instrumental Variables
# Description
#'@description  Fits linear models with discrete, endogeneous regressors using the approach described in Park and Gupta (2012). Due to the variablility 
#'in pStar, a simulation was needed in order to obtain the coefficient estimates. Then, in order to get the Z-scores and p-values the sum of the Z method was used
#'as described in  Zaykin, D V.(2011). "Optimally weighted Z-test is a powerful method for combining probabilities in meta-analysis". Journal of Evolutionary Biology, 24:1836-1841.

#
# Arguments
#'@param  formula  the model formula, e.g. \code{y ~ X1 + X2 + P}.
#'@param  endoVar  a string with the name of the endogenous variable/s, in quotation marks.
#'@param  intercept  an optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "FALSE", otherwise the value "TRUE".
#'@param data a data frame or matrix containing the variables of the model.


#
# Return Value
#'@return  Returns an object of class "lm".
#'@references Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86.

#'@keywords internal

copulaDiscrete <- function(formula, endoVar, intercept = NULL, data) {

  mf <- model.frame(formula = formula, data = data)
  y <- mf[,1]
  regs <- unlist(strsplit(as.character(formula)[3], "\\(")) 
  predictors <- unlist(strsplit(regs[1], " [+] "))

  regressors <- mf[,2:dim(mf)[2]]
  X <- regressors # both endogenous and exogenous
  P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  colnames(P) <- endoVar
 
 sim <- 250  
 k1 <- ncol(X)   
# P is the endogeneous regressor
# P1 <- as.matrix(P)
# colnames(P1) <- colnames(X)[-c(1:(ncol(X)-ncol(P1)))]

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
 pecdf <- apply(P,2,getEcdf)


  U.p <- matrix(NA, dim(P)[1], dim(P)[2])
  ifelse (intercept==TRUE, nc <- ncol(X) + ncol(P) +1, nc <- ncol(X) + ncol(P))
  # create a list with nc elements (number of regressors + Pstar) , for each regressor save the confint for each lm out of sim simulations
  conf.int <- rep(list(matrix(0,sim,2)), nc)

  for (k in 1:dim(P)[2]){
  
    U.p[,k] <- stats::runif(dim(P)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
  }  
  p.star <- apply(U.p, 2,qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P),sep=".")
  
  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- X1
  
  
  if (intercept==FALSE){  # no intercept 
    meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))
    bhat <-   stats::coef(summary(meth.2))[,1] # estimated coefficients
    
  } else {
    meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))  # with intercept
    bhat <- stats::coef(summary(meth.2))[,1]   # estimated coefficients
  } 
  names(bhat) <- colnames(dataCopula)
  
  
  for (i in 1:sim){
  for (k in 1:dim(P)[2]){
    
    U.p[,k] <- stats::runif(dim(P)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
  }  
  p.star <- apply(U.p, 2,qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P),sep=".")
     
  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- X1

  if (intercept==TRUE){  # intercept 
  meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))
  for (s in 1:nc) {
           conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]
           }
    } else {
    meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))  # with intercept
   for (s in 1:nc) {
      conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]
      
    }
  }
}

CI <- matrix(0,nc,2)
colnames(CI) <- c("Lower95%", "Upper95%")
for (i in 1:nc)
  CI[i,] <- apply(conf.int[[i]],2,mean)


res <- list(coefficients = meth.2$coefficients, CI=CI, reg = dataCopula, resid = meth.2$residuals, fitted.values=meth.2$fitted.values)
return(res)
}