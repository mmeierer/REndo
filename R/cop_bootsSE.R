#'@title Bootstrapping Standard Errors
#'
#'@description Performs bootstrapping to obtain the standard errors of the estimates of the model with one continuous endogenous regressor estimated via maximum likelihood
#'using the \code{\link{copulaEndo}} function.
#
# Arguments
#'@param    bot  number of bootstrap replicates.
#'@param    y  the vector or matrix containing the dependent variable. 
#'@param    X  the data frame or matrix containing the regressors of the model, both exogeneous and endogeneous. The last column/s should contain the endogeneous variable/s.
#'@param    P  the vector containing the continuous, non-normally distributed endogeneous variable.
#'@param    param  initial values for the parameters to be optimized over. See \code{\link{copulaEndo}} for more details.  
#'@param    intercept  an optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "no".  
# Return Value
#'@return Returns the standard errors of the estimates of the model using the copula method 1 described in Park and Gupta (2012). See Details section of \code{\link{copulaEndo}}.
#'@details The function could be used only when there is a single endogenous regressor and method one is selected in \code{\link{copulaEndo}}.
#'of the \code{copulaEndo} function is used for estimation.
#'@seealso \code{\link{copulaEndo}}
boots <- function(bot,y,X,P,param,intercept = NULL){
   
  if (!is.null(intercept)) {
    k <- ncol(X)
  }
  
  if (is.null(intercept)) {
    X <- cbind(rep(1,nrow(X)),X)
    k <- ncol(X) 
  }
  
  k1 <- k+2    # +2 = rho and sigma  
  cop.sd <- matrix(,nrow=bot, ncol=k1)
 
  
for (j in 1:bot) {

boot.se <- sample(length(y), length(P), replace=TRUE) 

y <- y[boot.se]
X <- X[boot.se, ]
P <- as.matrix(P)
P <- P[boot.se, ]
 
cop.opt <- copulaCont1(y,X,P,param, intercept="no")
# put results in a matrix
# col 1- alfa, col2-n -beta, last 2 cols - rho and sigma
matrix.out <- matrix(unlist(cop.opt), byrow=TRUE)  # 1 column matrix with the elements returned by optimx

cop.sd[j,1:ncol(X)] <- matrix.out[1:ncol(X),]
cop.sd[j,(ncol(X)+1)] <- matrix.out[ncol(X)+1,]
cop.sd[j,(ncol(X)+2)] <- matrix.out[ncol(X)+2,]
}

sd.b <- rep(0, ncol(X)+2)
 for (i in 1:length(sd.b)){
   sd.b[i] <- stats::sd(cop.sd[,i])
 }
 
res.sd <- list(se.a = sd.b[1], se.betas = sd.b[2:ncol(X)], se.rho = sd.b[length(sd.b)-1], se.sigma = sd.b[length(sd.b)])
return(res.sd)
}


	