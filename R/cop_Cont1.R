#'@title Fitting Linear Models with One Enedogenous Regressor using Gaussian Copula
#'
#'@description  Estimates a linear model with one endogenous variable using Gaussian copula. The optimization is done via maximum likelihood, using the BFLG algorithm.
#
# Arguments

#'@param    y  a vector or matrix containing the dependent variable. 
#'@param    X  a data frame or matrix containing the regressors of the model, both exogeneous and endogeneous. The last column should contain the endogeneous variable.
#'@param    P  a vector containing the continuous, non-normally distributed endogeneous variable.
#'@param    param  Initial values for the parameters to be optimized over.  
#'@param    intercept  Optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "no".  

#
# Return Value
#'@return    Returns a list with the best set of parameters found. 
#'@keywords
#'internal
#'copula
#'instrumental variables
copulaCont1 <- function(y,X,P, param = NULL, intercept = NULL){

    if (is.null(intercept)) {
      X <- cbind(rep(1,nrow(X)),X)
      k <- ncol(X) 
    } else {
        k <- ncol(X)
     }

    k1 <- k+2
    
    if (is.null(param)){
      datalm <- data.frame(X)
      if (!is.null(intercept)) {
      l <- stats::lm(y ~ .-1, data=datalm)
      } else
        {
      l <- stats::lm(y ~ ., data=datalm)
      }
      par1 <- rep(0,ncol(datalm))
      for (i in 1:(ncol(datalm))){
        par1[i] <- l$coefficients[[i]]
      }
      #parameter are composed of the OLS estimates, while for rho and sigma the default value of 0.5 is given
      param <- c(par1,0.2,1)
    }
   b <- optimx::optimx(par=param,fn=logLL,y=y, X=X, P=P,method="BFGS",control=list(trace=0))
   
  res <- list(coefExoVar = b[,1:(k-1)], coefEndoVar = b[,k], rho = b[,k1-1], sigma = b[,k1], value = b$fevals, convCode = b$convcode, reg=X )
  class(res) <- c("copulaEndo")
   return(res)
  
  }

