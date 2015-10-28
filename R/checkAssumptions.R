#'@title Checks Assumptions for Constructing Internal Instruments 
#'@description The internal instruments are constructed as in Lewbel(1997). See \code{\link{hmlewbel}}.
#'@keywords internal
#'@keywords lewbel 
checkAssumptions <- function(formula,IIV,EIV=NULL, data=NULL)
{
  mf <- as.matrix(model.frame(formula = formula, data = data))
  
 # y <- mf[,1]
#  P <- mf[,ncol(mf)]
 # X <- mf[,c(-1,-ncol(mf))]
  # check if model error is symetrically distributed
  
  e1 <- RcppEigen::fastLm(mf[,1] ~ mf[,c(-1,-ncol(mf))] + mf[,ncol(mf)])$residuals
  
  if (abs(e1071::skewness(e1)) > 0.10 & IIV == "y2")  
    warning(gettextf("Model error not symetrically distributed"), domain = NA)
 

# check assumption E(qp) !=0
# save residuals of P regressed on 1 and X
  p1 <- RcppEigen::fastLm(mf[,ncol(mf)] ~ mf[,c(-1,-ncol(mf))])$residuals

 IV <- internalIV(formula,IIV)

 IVS <- cbind(mf[,c(-1,-ncol(mf))],IV)
# compute the mean of the product btw residuals and Instruments
 A12 <- round(apply(IVS*p1,2,mean),3)

for (i in length(A12)){
      if (abs(A12[i])<=0.1) 
       warning(gettextf("Assumptions E(qp)!=0 is not met"), domain = NA)
   }
}
#  
#  # check assumption E(qe)==0
#  # r1 are the residuals from the strucutral equation
#  r1 <- lm(y ~ X[,-1] + P)$residuals 
#  A11 <- round(mean(IVS*r1),3)
#  
#  # if the residuals are greater then 0.4 or if the mean A12 is 
#  #smaller than 0.1, the assumptions are not met
#  
#   
#  
