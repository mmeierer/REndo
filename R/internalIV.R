#'@title Constructs internal instrumental variables from available data
#'@aliases internalIV
#'@description The function can be used to construct additional instruments to be supplied to \code{\link{hmlewbel}} as additional instruments
#' in the "EIV" argument.
#' @param    y   the vector or matrix containing the dependent variable.
#' @param    X   the data frame or matrix containing the exogenous regressors of the model.
#' @param    P   the endogenous variables of the model as columns of a matrix or dataframe.
#' @param    IIV   the internal instrumental variable to be constructed. It can take six values, \code{"g","gp","gy","yp","p2"or"y2"}. See the "Details" section 
#' of \code{\link{hmlewbel}} for a description of the internal instruments.
#' @param    data  optional data frame or list containing the variables in the model.
#' @references Lewbel, A. (1997). "Lewbel, A. (1997). 'Constructing Instruments for Regressions with Measurement Error when No Additional Data Are Available,
#' with An Application to Patents and R&D'. Econometrica, 65(5), 1201-1213."
#' @keywords endogeneity
#' @keywords instruments
#' @keywords lewbel
#' @seealso \code{\link{hmlewbel}}
#'@export
# the endogenous variable should be on the last position in the formula
internalIV <- function(y,X,P, IIV=c("g","gp","gy","yp","p2","y2"), data=NULL){

# function computing the de-meanned variable
s <- function(t){
    s1 <- t - mean(t)
    return(s1)
}   

# here G = ()^2
IIV1 <- s(X^2)    # q1i = s(X^2)
IIV2 <- s(X^2) * s(P)  # q2i
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

