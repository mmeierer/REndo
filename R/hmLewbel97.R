
hmlewbel <- function(formula = formula ,IIV=c("g","gp","gy","yp","p2","y2"),EIV, data=NULL){

# check to see if any external instruments were provided
if (missing(EIV)) {
  EIV <- NULL} else
{
  EIV <- as.matrix(EIV)
}
   
mf <- model.frame(formula = formula, data = data)

# X should contain an intercept in the first column
X <- as.matrix(X)
  
# The endogenous variable, for this version of the package only 1 endog. var supported
P <- as.matrix(P)

# computes the internal IVs proposed by Lewbel 97 - g, gp, gy, yp, y2, p2, depending on 
# the values provided by the user in IIV
IV <- InternalIV(y,X,P,IIV)

# check if external IVs or additional IVs were provided. If, yes, add them to the IIV
if (missing(EIV)){IV <- IV} else {
  IV = cbind(IV,EIV)
}  

# function that checks whether the model error is symmetrically distributed
# if yes, then IIV6 can be used as instrument
checkAssumptions(y, X, P,EIV, IIV)

# uses ivreg function from \pkg{AER} to be able to use package ivpack 
model <- ivreg(y ~ X[,-1] + P | X[,-1] + IV)
return(model)

}
