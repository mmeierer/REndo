#' copulaEndo S4 Object
#'
#' This class is used to store and further analyze the results of the copulaEndo function when method=1 is applied
#' @slot coefEndoVar
#' @slot coefExoVar
#' @slot coefPStar
#' @slot seCoefficients
#' @slot rho
#' @slot sigma
#' @slot value
#' @slot convCode


#' @name copulaEndo-class
#' @rdname copulaEndo - class
#' @exportClass copulaEndo
#' @examples
#' getSlots("copulaEndo")
#'
#' @importFrom methods setClass
#' @export
setClass(
  
  #Class name
  "copulaEndo",
  
  
  #Slots / member vars
  slots = c(
    coefEndoVar = "numeric",
    coefExoVar = "numeric",
    coefPStar = "numeric",
    seCoefficients = "numeric",
    sigma = "numeric",
    rho = "numeric",
    value = "numeric",
    convCode = "integer"
    
  ),
  
  prototype = list(
                   coefEndoVar = NA_real_,
                   coefExoVar = NA_real_,
                   coefPStar = NA_real_,
                   seCoefficients = NA_real_,
                   rho = NA_real_,
                   sigma = NA_real_,
                   value = NA_real_,
                   convCode = NA_integer_
                   
  )
)


#' S3 Method for copulaEndo object for generic "coef"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo(...,method =1).
#'@param ... further arguments passed to or from other methods.
#'@export
coef.copulaEndo <- function(object, ...)
{print(object@coefEndoVar,object@coefExoVar, object@coefPStar,object@seCoefficients,object@rho,object@sigma)}


#' S3 Method for copulaEndo object for generic "summary"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo(...,method = 1).
#'@param ... further arguments passed to or from other methods.
#'@export
summary.copulaEndo <- function(object, ...)
{
  z <- object
  estEnd <- z@coefEndoVar  # estimates value
  estEx <- z@coefExoVar 
  coef.table <- cbind(estEnd, estEx)
  colnames(coef.table) <- c("Estimates")
  #rownames(coef.table) <- c("Intercept")
  
  cat("\nCoefficients:\n")
  printCoefmat(coef.table) # print the coefficient 
  
  cat("\nThe Value of the log likelihood function:\n", z@value)  # print logLik values
  cat("\n")
  cat("\nConvergence Code:\n", z@convCode)  # print comvergence code
}


#' S3 Method for copulaEndo object for generic "print"
#'@param x an object of class "copulaEndo", usually, a result of a call to copulaEndo(...,method = 1).
#'@param ... further arguments passed to or from other methods.
#'@export
print.copulaEndo <- function(x, ...)
{
  str(x)
  # str(object)
}


