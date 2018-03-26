#'@title  Fitting Linear Models with Endogenous Regressors using Heteroskedastic Covariance Restictions
#'@aliases hetErrorsIV
# Description
#'@description  This function estimates the model parameters and associated standard errors for a 
#'linear regression model with one or more endogenous regressors. Identification is achieved 
#'through heteroscedastic covariance restrictions within the triangular system as proposed in Lewbel(2012). The function \code{hetErrorsIV}
#'builds on the \code{lewbel} function form the \code{ivlewbel} package. Changes have been made only to the printing and the summary of
#'the function, as well as the name. 
#
# Arguments
#'@param    formula   the model formula, e.g.\code{y ~ X1 + X2 + P}.
#'@param    data   the data frame containing the dataset. This argument is mandatory.
#'@param    clustervar   a character value naming the cluster on which to adjust the standard errors and test statistics. 
#'@param    robust   if TRUE the function reports standard errors and test statistics that have been corrected for the presence heteroscedasticity using White's method.

#'@details 
#'The formula follows a four-part specification. The following formula is an example: y2 ~ y1 | x1 + x2 + x3 | x1 + x2 | z1. 
#' Here, y2 is the dependent variable and y1 is the endogenous regressor. The code x1 + x2 + x3 
#' represents the exogenous regressors whereas the third part x1 + x2 specifies the exogenous 
#' heteroscedastic variables from which the instruments are derived. The final part z1 is optional,
#' allowing the user to include tradtional instrumental variables. If both robust=TRUE and clustervar=TRUE,
#' the function overrides the robust command and computes clustered standard errors and test statistics adjusted to
#' account for clustering. The function also computes partial F-statistics that indicate potentially weak identification.
#' In cases where there is more than one endogenous regressor the Angrist-Pischke (2009) method for multivariate
#' first-stage F-statistics is employed.

#Return Value
#'@return Returns an object of class \code{hetREndo}, with the following components:
#'\item{coefficients}{ a coefficient matrix with columns containing the estimates, associated standard errors, test statistics and p-values..}
#'\item{call}{the matched call.}
#'\item{obs}{ the number of observations.}
#'\item{jtest}{ J-test for overidentifying restrictions.}
#'\item{ftest}{ Partial F-test statistics for weak IV detection.}
#'@keywords endogenous
#'@keywords instruments
#'@keywords lewbel
#'@keywords heteroskedastic
#'@author The implementation of the model formula by based on the paper of Lewbel (2012).
#'@references  Lewbel, A. (2012). Using Heteroskedasticity to Identify and Estimate Mismeasured and Endogenous Regressor Models,\emph{Journal of Business \& Economic Statistics}, \bold{30(1)}, 67-80.
#'@references Angrist, J. and Pischke, J.S. (2009). Mostly Harmless Econometrics: An Empiricists Companion, Princeton University Press.
#'@references  Fernihough, A. (2014). \bold{ivlewbel} package: Uses heteroscedasticity to estimate mismeasured and endogenous regressor models.
#'@seealso \code{\link[ivlewbel]{lewbel}}, \code{\link{higherMomentsIV}}, \code{\link{latentIV}},  \code{\link{copulaCorrection}}, \code{\link[AER]{ivreg}}.
#'@examples 
#'data(dataHetIV)
#'resultsHetIV <- hetErrorsIV(y ~ P| X1 + X2 | X1 + X2, data = dataHetIV)
#'summary(resultsHetIV)
#' @export
hetErrorsIV <- function(formula, data, clustervar = NULL, robust = TRUE){
    
    object <- methods::new("hetREndo")
    
    res <- lewbel.est(formula, data, clustervar, robust)
    
    mf <- model.frame(formula = formula, data = data)

    nreg <- nrow(res$coefs)    # number of regressors
    o <- c(2,1,seq(3,nreg,1))  # put intercept as first row
    resCoef <- res$coef[o,]
    object@coefficients <- resCoef
    object@formula <- res$formula
    object@call <- match.call()
    object@obs <- res$num.obs
    object@jtest <- as.matrix(unclass(res$j.test[[2]]))
    object@ftest <- res$f.test.stats
    object
  }