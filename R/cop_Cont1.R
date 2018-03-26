#'@title Fitting Linear Models with One Enedogenous Regressor using Gaussian Copula
#'
#'@description  Estimates a linear model with one endogenous variable using Gaussian copula. The optimization is done via maximum likelihood, using the BFLG algorithm.
#
# Arguments

#'@param    formula  the model formula, e.g. \code{y ~ X1 + X2 + X3}. 
#'@param    endoVar  a string with the name of the endogenous variable/s, in quotation marks.
#'@param    param  initial values for the parameters to be optimized over.  
#'@param    intercept  an optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "FALSE", otherwise the value "TRUE".
#'@param    data    a data frame or matrix containing the variables of the model.
#
# Return Value
#'@return    Returns a list with the best set of parameters found. 
#'@keywords
#'internal
#'copula
#'instrumental variables
copulaCont1 <- function(formula, endoVar, param = NULL, intercept =NULL, data){

  mf <- model.frame(formula = formula, data = data)
  y <- mf[,1]
  regs <- unlist(strsplit(as.character(formula)[3], "\\(")) 
  predictors <- unlist(strsplit(regs[1], " [+] "))
  regressors <- mf[,2:dim(mf)[2]]
  X <- regressors # both endogenous and exogenous
  P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  colnames(P) <- endoVar
  
  k <- ncol(X)
  k1 <- k+2
    
  if (is.null(param)){
      
      datalm <- data.frame(X)
      if (intercept==TRUE){
        l <- stats::lm(y ~ ., data=datalm)
      } else {
        l <- stats::lm(y ~ .-1, data=datalm)
        }
      par1 <- rep(0,length(l$coefficients))
      for (i in 1:(length(l$coefficients))){
        par1[i] <- l$coefficients[[i]]
      }
      #parameter are composed of the OLS estimates, while for rho and sigma the default value are 0.5 and 0.9 
      param <- c(par1,0,1)
  }
   
  if (intercept==TRUE)  {
      I <- as.matrix(rep(1,length(y)))
      colnames(I) <- c("I")
      X <- cbind(I, X) 
      b <- optimx::optimx(par=param,fn=logLL,y=y, X=X, P=P, method="BFGS", control=list(trace=0)) 
      k <- ncol(X)
      k1 <- k+2
  } else { 
     b <- optimx::optimx(par=param,fn=logLL,y=y, X=X, P=P, method="BFGS", control=list(trace=0)) 
    }
   results <- list(k=k,k1=k1,coef_cop=b, reg=as.matrix(X), param = param) 
    return(results)
  
  }

