#'@title Fitting Linear Models with Endogeneous Regressors using Gaussian Copula - Method 2
#'
# Description
#'@description    It fits linear models with several endogeneous regressors using Gaussian Copula proposed by Park and Gupta (2012).
#
# Arguments
#'@param   formula  the model formula, e.g. \code{y ~ X1 + X2 + P}. 
#'@param   endoVar  a string with the name of the endogenous variable/s, in quotation marks.
#'@param   intercept   an optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "FALSE", otherwise the value "TRUE".
#'@param  data a data frame or matrix containing the variables of the model.
#
# Return Value
#'@return  Returns an object of class "lm".
#'@keywords internal instrumental variables
#'@keywords copula 
#'@keywords endogenous

copulaMethod2  <- function(formula, endoVar,intercept=NULL,data){

  mf <- model.frame(formula = formula, data = data)
  y <- mf[,1]
  regs <- unlist(strsplit(as.character(formula)[3], "\\(")) 
  predictors <- unlist(strsplit(regs[1], " [+] "))
 
  regressors <- mf[,2:dim(mf)[2]]
  X <- regressors # both endogenous and exogenous
  P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  colnames(P) <- endoVar
  k <- ncol(X) 

# create pStar for all endogenous regressors provided in P
PS <- apply(P, 2, copulaPStar)
colnames(PS) <- paste("PS", 1:ncol(P), sep=".")
#X <- as.matrix(X)
#add the pStar variables to the set of regressors 
dataCopula <- data.frame(cbind(X, PS))
datCop <- cbind(X,PS)
if (intercept==TRUE) {
f.lm <- stats::lm(y ~., dataCopula)
} else {f.lm <- stats::lm(y ~.-1, dataCopula)}
f.lm$call <- match.call()
lm_stats <- list(residSE = summary(f.lm)$sigma,r2 = summary(f.lm)$r.squared,adjr2 = summary(f.lm)$adj.r.squared,fstat = summary(f.lm)$fstatistic, df=summary(f.lm)$df)
res <- list(f=f.lm, reg=datCop, resid = f.lm$residuals, fitted.values = f.lm$fitted.values, stats = lm_stats)
return(res)
}