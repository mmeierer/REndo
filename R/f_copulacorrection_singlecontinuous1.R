#' @importFrom stats lm
#' @importFrom optimx optimx
copulaCorrection_singleContinuous1 <- function(y, X, P, intercept, param){

  # ** Only changes after here: fix datalm <- c(y, X), and setting return object right------------------------------

  k <- ncol(X)
  k1 <- k+2

  if (is.null(param)){

    datalm <- cbind(y, data.frame(X))
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
    # **As in email: change values to 0.001 and 0.9998
    param <- c(par1,0.001,0.9998)
  }

  if (intercept==TRUE)  {
    I <- as.matrix(rep(1,length(y)))
    colnames(I) <- c("I")
    X <- cbind(I, X)
    b <- optimx::optimx(par=param,fn=copulaCorrection_LL,y=y, X=X, P=P, method="BFGS", control=list(trace=0))
    k <- ncol(X)
    k1 <- k+2
  } else {
    b <- optimx::optimx(par=param,fn=copulaCorrection_LL,y=y, X=X, P=P, method="BFGS", control=list(trace=0))
  }


  results <- list(k=k,k1=k1,coef_cop=b, reg=as.matrix(X), param = param)
  return(results)


}
