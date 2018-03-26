#'@title  mixedREndo S4 Object
#'@description defines the slots of class mixedREndo.

#' @slot coefficients - the estimated coefficients. 
#' @slot coefSdErr - standard errors of the estimates.
#' @slot mixedCall - function call.
#' @slot vcovMat - variance-covariance matrix.
#' @slot weightMat - the weight matrix used for estimation.
#' @slot omittedVarTest - Omitted Variable Test statistic and p-value.

#' @name mixedREndo-class
#' @exportClass mixedREndo
#' @rdname mixedREndo
#' @export
#' @keywords internal
#' @examples
#' getSlots("mixedREndo")
#'
#' @importFrom methods setClass

setClass("mixedREndo",

  #Slots / member vars
  slots = c(
    
    coefficients = "matrix",
    coefSdErr = "list",
    mixedCall= "call",
    vcovMat = "matrix",
    weightMat = "matrix",
    omittedVarTest = "list"
    
  ),

  prototype = list(
                  
                   coefficients = matrix(NA),
                   coefSdErr = list(NA),
                   vcovMat = matrix(NA),
                   weightMat = matrix(NA),
                   omittedVarTest = list(NA)
                  
  )
)


