#'@title Constructs internal instrumental variables from available data
#'@description Function constructing the internal instrumental variables proposed by Lewbel(1997).
#' The function can be used to construct additional instruments to be supplied to hmlewbel() as additional instruments
#' in "EIV" argument.
#' @param formula - an object of type 'formula': a symbolic description of the model to be fitted. Example \code{var1 ~ var2 + var3}, where \code{var1} is a vector
#' containing the dependent variable, while \code{var2, var3}, etc are vectors containing the regressors. The endogenous variable should be the last regressor
#' in the formula.
#' @param IIV - the internal instrumental variable to be constructed. It can take six values, \code{"g","gp","gy","yp","p2"or"y2"}. See the "Details" section 
#' of \code{\link{hmlewbel}} for a description of the internal instruments.
#' @param data -  optional data.frame or list containing the variables in the model.
#' @references Lewbel, A. (1997). "Lewbel, A. (1997). 'Constructing Instruments for Regressions with Measurement Error when No Additional Data Are Available,
#' with An Application to Patents and R&D'. Econometrica, 65(5), 1201-1213."
#' @keywords endogeneity
#' @keywords instruments
#' @keywords lewbel
#' @seealso \code{link{hmlewbel}}

# the endogenous variable should be on the last position in the formula
internalIV <- function(formula, IIV=c("g","gp","gy","yp","p2","y2"), data=NULL){

  
mf <- model.frame(formula = formula, data = data)  

# function computing the de-meanned variable
s <- function(t){
    s1 <- t - mean(t)
    return(s1)
}   

y <- mf[,1]
P <- mf[,ncol(mf)]
X <- mf[,c(-1,-ncol(mf))]
# here G = ()^2 
IIV1 <- s(X^2)    # q1i = s(X^2)
IIV2 <- s(X^2) * as.vector(s(P))  # q2i
IIV3 <- s(X^2) * s(y)  # q3i
IIV4 <- s(y) * s(P)   # q41, 
IIV5 <- s(P)^2  # q5i
IIV6 <- s(y)^2  # q6i  
    
AIV <- NULL  
 
if (IIV=="g") {AIV <- IIV1} else {
    if (IIV == "gp") {AIV <- IIV2} else {
      if (IIV == "gy") {AIV <- IIV3} else {
        if (IIV == "yp") {AIV <- IIV4} else {
          if (IIV == "p2") {AIV <- IIV5} else {
            if (IIV == "y2") {AIV <- IIV6}
          }
        }
      }
    }
  }
  
return(AIV)

}

