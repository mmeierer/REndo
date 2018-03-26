#'@title hetErrorsIV Object
#'
#' @description This class is used to store and further analyze the results of the hetErrorsIV function
#' @slot call - function call.
#' @slot formula - model formula.
#' @slot coefficients - estimated coefficients.
#' @slot obs - number of observations.
#' @slot jtest - results of the J test. 
#' @slot ftest - results of the f-test.

#' @name hetREndo-class
#' @exportClass hetREndo
#' @rdname hetREndo-class
#'
#' @examples
#' getSlots("hetREndo")
#'
#' @importFrom methods setClass
#' @export
#' @keywords internal
setClass(
  
  #Class name
  "hetREndo",
  
  #Slots / member vars
  slots = c(
    call = "call",
    formula = "formula",
    coefficients = "matrix",
    obs = "numeric",
    jtest = "matrix",
    ftest = "numeric"
    
  ),
  
  prototype = list(
    formula = y~x,
    coefficients = matrix(NA),
    obs = NA_real_,
    jtest = matrix(NA),
    ftest = NA_real_
  )
)



