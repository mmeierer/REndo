#'@title Fitting Linear Models with Endogeneous Regressors using Gaussian Copula - Method 2
#'
# Description
#'@description    It fits linear models with several endogeneous regressors using Gaussian Copula proposed by Park and Gupta (2012).
#
# Arguments
#'@param   y  a vector or matrix containing the dependent variable.
#'@param   X  a data frame or matrix containing the regressors of the model, both exogeneous and endogeneous. The last column/s should contain the endogeneous variable/s.
#'@param   P  a matrix containing the endogeneous variables. They should be continuous and non-normally distributed.
# Return Value
#'@return  Returns an object of class "lm".
#'@keywords internal
#'@keywords copula 

copulaMethod2  <- function(y,X,P, intercept=NULL){

k <- ncol(X) 
P <- as.matrix(P)
# create pStar for all endogenous regressors provided in P
PS <- apply(P, 2, copulaPStar)
colnames(PS) <- paste("PS", 1:ncol(P), sep=".")
X <- as.matrix(X)
#add the pStar variables to the set of regressors 
dataCopula <- data.frame(cbind(X, PS))
datCop <- as.matrix(cbind(X,PS))
if (!is.null(intercept)) {
f.lm <- stats::lm(y ~.-1, data=dataCopula)
} else {f.lm <- stats::lm(y ~., data=dataCopula)}
f.lm$call <- match.call()
res <- list(results = f.lm, reg=datCop)
return(res)
}