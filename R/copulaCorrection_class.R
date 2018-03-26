#' @title generics for copulaREndo-class
#' @description This class is used to store and further analyze the results of the copulaCorrection function when method=1 is applied
#' @slot copCall - call fo the copulaCorrection function.
#' @slot coefficients - estimated coefficients.
#' @slot coefEndoVar - estimated coefficients of the variables considered endogenous.
#' @slot coefExoVar - estimated coefficients of the variables considered exogenous.
#' @slot coefPStar - estimated coefficient of the additional variable, p*.
#' @slot seCoefficients - standard errors of the coefficients.
#' @slot residuals - the residuals.
#' @slot fitted.values - fitted values.
#' @slot stats - model fit statistics such as R2, Adj-R2, Fstatistic.
#' @slot rho - the estimate of the correlation between the endogenous variable and the error.
#' @slot sigma - standard deviation of the error.
#' @slot logLik - value of the maximum likelihood.
#' @slot param - initial values of the parameter. THese are either chosen by the user or are the OLS estimates, with value 0.5 for rho and 0.9 for sigma.
#' @slot AIC - Akaike Information Criterion.
#' @slot BIC - Bayesian Information Criterion.
#' @slot convCode - the convergence code. If is equal to zero, the model converged.
#' @slot regressors - returns the regressors matrix.
#' @slot confint - returns the lower and upper limits of the 95\% confidence interval for the estimates.

#' @name copulaREndo-class
#' @exportClass copulaREndo
#' @keywords internal
#' @rdname copulaRendo-class
#' @examples 
#' getSlots("copulaREndo")
#'
#' @importFrom methods setClass
#
setClass(
  
  #Class name
  "copulaREndo",
    
  #Slots / member vars
  slots = c(
    copCall = "call",
    coefficients = "numeric",
    coefEndoVar = "numeric",
    coefExoVar = "numeric",
    coefPStar = "numeric",
    seCoefficients = "matrix",
    residuals = "matrix",
    fitted.values = "matrix",
    stats = "list",
    rho = "numeric",
    sigma = "numeric",
    logLik = "numeric",
    param = "numeric",
    AIC = "numeric",
    BIC = "numeric",
    convCode = "numeric",
    regressors = "matrix",
    confint = "matrix"  
    ),
  
  prototype = list(
                  # copCall = "call",
                   coefficients = NA_real_,
                   coefEndoVar = NA_real_,
                   coefExoVar = NA_real_,
                   coefPStar = NA_real_,
                   seCoefficients = matrix(NA),
                   residuals = matrix(NA),
                   fitted.values = matrix(NA),
                   stats = list(NA),
                   rho = NA_real_,
                   sigma = NA_real_,
                   param = NA_real_,
                   logLik = NA_real_,
                   AIC = NA_real_,
                   BIC = NA_real_,
                   convCode = NA_real_,
                   regressors = matrix(NA),
                   confint = matrix(NA)
                  
  )
)




