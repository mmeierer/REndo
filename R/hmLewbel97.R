#'@title  Fitting Linear Models with one Endogenous Regressor using Lewbel's higher moments approach
#'@aliases hmlewbel
# Description
#'@description  Fits linear models with one endogenous regressor using internal instruments built using the approach described in 
#' Lewbel A. (1997). This is a statistical technique to address the endogeneity problem where no external instrumental
#' variables are needed. The implementation allows the incorporation of external instruments if available. 
#' An important assumption for identification is that the endogenous variable has a skewed distribution.  
#
# Arguments
#'@param formula an object of type 'formula': a symbolic description of the model to be fitted. Example \code{var1 ~ var2}, where \code{var1} is a vector
#'containing the dependent variable, while \code{var2} is a vector containing the endogenous variable.
#'@param IIV  stands for "internal instrumental variable". It can take six values: \code{g,gp,gy,yp,p2} or \code{y2}. Tells the function 
#'which internal instruments to be constructed from the data. See "Details" for further explanations.
#'@param EIV  stands for "external instrumental variable". It is an optional argument that lets the user specify any external variable(s) to be used as instrument(s).
#'@param data  optional data frame or list containing the variables in the model.
#

#'@details 
#'Consider the model below: 
#'\deqn{ Y_{t} = \beta_{0}+ \gamma^{'} W_{t} + \alpha P_{t}+\epsilon_{t} \hspace{0.3cm} (1) } 
#' \deqn{ P_{t} = Z_{t}+\nu_{t} \hspace{2.5 cm} (2)} 
#' 
#'The observed data consist of \eqn{Y_{t}}, \eqn{W_{t}} and \eqn{P_{t}}, while \eqn{Z_{t}}, \eqn{\epsilon_{t}}, and \eqn{\nu_{t}} are unobserved. The endogeneity problem 
#'arises from the correlation of \eqn{P_{t}} with the structural error, \eqn{\epsilon_{t}}, since \eqn{E(\epsilon \nu)\neq 0}. 
#'The requirement for the structural and measurement error is to have mean zero, but no restriction is imposed on their distribution. 
#'
#'Let \eqn{\bar{S}} be the sample mean of a variable \eqn{S_{t}} and \eqn{G_{t}=G(W_{t})} for any given function \eqn{G} that has finite third 
#'own and cross moments. Lewbel(1997) proves that the following instruments can be constructed and used with 2SLS to obtain consistent estimates:
#' \deqn{ q_{1t}=(G_{t} - \bar{G})  \hspace{1.6 cm}(3a)}
#' \deqn{ q_{2t}=(G_{t} - \bar{G})(P_{t}-\bar{P}) \hspace{0.3cm} (3b) }
#' \deqn{ q_{3t}=(G_{t} - \bar{G})(Y_{t}-\bar{Y}) \hspace{0.3cm} (3c)}
#' \deqn{ q_{4t}=(Y_{t} - \bar{Y})(P_{t}-\bar{P}) \hspace{0.3cm} (3d)}
#' \deqn{ q_{5t}=(P_{t}-\bar{P})^{2} \hspace{1.5 cm} (3e) }
#' \deqn{ q_{6t}=(Y_{t} - \bar{Y})^{2}\hspace{1.5 cm} (3f)}
#' 
#'Instruments in equations \code{3e} and \code{3f} can be used only when the measurement and the structural errors are symmetrically distributed.
#'Otherwise, the use of the instruments does not require any distributional assumptions for the errors. Given that the regressors \eqn{G(W)=W} 
#'are included as instruments, \eqn{G(W)} should not be linear in \eqn{W} in equation \code{3a}.
#'
#'Let small letter denote deviation from the sample mean: \eqn{s_{i} = S_{i}-\bar{S}}. Then, using as instruments the variables presented in
#'equations \code{3} together with \code{1} and \eqn{W_{t}}, the two-stage-least-squares estimation will provide consistent estimates for the parameters
#'in equation \code{1} under the assumptions exposed in Lewbel(1997).
#'

#Return Value
#'@return Returns: 
#'\item{coefficients}{returns the value of the parameters for the intercept and the endogenous regressor as computed with maximum likelihood.}

#'@keywords endogenous
#'@keywords latent
#'@keywords instruments
#'@author The implementation of the model formula by Raluca Gui based on the paper of Lewbel (1997).
#'@references  Lewbel, A. (1997). 'Constructing Instruments for Regressions with Measurement Error when No Additional Data Are Available,
#' with An Application to Patents and R&D'. \emph{Econometrica}, \bold{65(5)}, 1201-1213.

#\code{\link[AER]{ivreg}}, \code{\link{liv}}
# make availble to the package users
#'@export


hmlewbel <- function(formula, IIV = c("g","gp","gy","yp","p2","y2"), EIV=NULL, data=NULL){
  
  # in formula, the last variable should be the endogenous one!
  
#   warning("Attention! Be sure to provide the endogenous variable as the last regressor in the formula")

  
  # check to see if any external instruments were provided
  if (!is.null(EIV)) {
        EIV <- as.matrix(EIV)
    }
  
  mf <- model.frame(formula = formula, data = data)
  
  name.vars <- all.vars(formula)  #get the variable names
  
  X <- mf[,c(-1,-ncol(mf))]  # all vars except the first and the last
  names(X) <- name.vars[-c(1,dim(mf)[2])]
  # The endogenous variable, for this version of the package only 1 endog. var supported
  # The endogenous variable to be the last one provided in the formula
  P <- mf[,ncol(mf)]
  y <- mf[,1]
  
  # computes the internal IVs proposed by Lewbel 97 - g, gp, gy, yp, y2, p2, depending on 
  # the values provided by the user in IIV
  IV <- internalIV(formula,IIV, data)
  
  # check if external instruments were provided. If, yes, add them to the IIV
  if (is.null(EIV)){IV <- IV} else {
    IV <- cbind(IV,EIV)
  }  
  
  # function that checks whether the model error is symmetrically distributed
  # if yes, then IIV6 can be used as instrument
  checkAssumptions(formula,IIV,EIV, data)
  
  # uses ivreg function from \pkg{AER} for users to be able to use afterwards the package ivpack 
  # hmlewbel should return an object of class "ivreg"
  res <- AER::ivreg(y ~ X + P|X  + IV, x=TRUE )
  
  return(res)
  
}
