#' @title generics for copulaREndo-class
#' @description This class is used to store and further analyze the results of the copulaEndo function when method=1 is applied
#' @slot coefficients - estimated coefficients.
#' @slot coefEndoVar - estimated coefficients of the variables considered endogenous.
#' @slot coefExoVar - estimated coefficients of the variables considered exogenous.
#' @slot coefPStar - estimated coefficient of the additional variable, p*.
#' @slot seCoefficients - standard errors of the coefficients.
#' @slot rho - the estimate of the correlation between the endogenous variable and the error.
#' @slot sigma - standard deviation of the error.
#' @slot value - value of the maximum likelihood.
#' @slot convCode - the convergence code. If is equal to zero, the model converged.
#' @slot reg - returns the regressors matrix.



#' @name copulaEndo-class
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
    
    coefficients = "numeric",
    coefEndoVar = "numeric",
    coefExoVar = "numeric",
    coefPStar = "numeric",
    seCoefficients = "numeric",
    rho = "numeric",
    sigma = "numeric",
    value = "numeric",
    convCode = "numeric",
    reg = "matrix"
  ),
  
  prototype = list(
    
    coefficients = NA_real_,
    coefEndoVar = NA_real_,
    coefExoVar = NA_real_,
    coefPStar = NA_real_,
    seCoefficients = NA_real_,
    rho = NA_real_,
    sigma = NA_real_,
    value = NA_real_,
    convCode = NA_real_,
    reg = matrix(NA)
  )
)


#' S3 Method for copulaEndo object for generic "coef"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo().
#'@param ... further arguments passed to or from other methods.
#'@export

coef.copulaEndo <- function(object, ...)
  {
   mcall <- match.call()
   if ("discrete" %in% mcall$type){
     print(object$a.est, object$b.est,object$ps.est,object$se.a,object$se.b,object$se.ps)
   } 
   if (("continuous" %in% mcall$type) || ("1" %in% mcall$method)){
     
     print(object@coefExoVar, object@coefEndoVar)
   }
    if (mcall$method == 2){
      print(object@coefficients)
    }
}


#' S3 Method for copulaEndo object for generic "summary"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo().
#'@param ... further arguments passed to or from other methods.
#'@export
summary.copulaEndo <- function(object, ...)
{
  mcall <- match.call()     
  if ("1" %in% mcall$method) {
    
    z <- object
    estEnd <- z@coefEndoVar  # estimates value
    estEx <- z@coefExoVar 
    coef.table <- estEx
    coef.table <- append(coef.table, estEnd)
    coef.table <- matrix(coef.table, , ncol=1) 
    rownames(coef.table) <- colnames(z@reg)
    colnames(coef.table) <- c("Estimate")
    
    cat("\nCoefficients:\n")
    stats::printCoefmat(coef.table) # print the coefficient 
    
    cat("\nThe Value of the log likelihood function:\n", z@value)  # print logLik values
    cat("\n")
    cat("\nConvergence Code:\n", z@convCode)  # print comvergence code
  }
  if ("2" %in% mcall$method){
    z <- object
    cat("\nCoefficients:\n")
    stats::printCoefmat(z@coefficients) # print the coefficient 
  }
  if ("discrete" %in% mcall$type){
    z <- object
    estEnd <- z@coefEndoVar  # estimates value
    estEx <- z@coefExoVar 
    coef.table <- estEx
    coef.table <- append(coef.table, estEnd)
    coef.table <- matrix(coef.table, , ncol=1) 
    coef.table <- append(coef.table, z@coefPStar)
    coef.table <- matrix(coef.table, , ncol=1)
    rownames(coef.table) <- append(colnames(z@reg),"pStar")
    colnames(coef.table) <- c("Estimate")
    cat("\nCoefficients:\n")
    stats::printCoefmat(coef.table) # print the coefficient 
  }
  
}

#' S3 Method for copulaEndo object for generic "print"
#@param x an object of class "copula", usually, a result of a call to copula().
#@param ... further arguments passed to or from other methods.
#@export
#print.copulaEndo <- function(x, ...)
#{
#    cat("Coefficients Exogenous Variables:", x$coefExoVar, "\n")
#    cat("Coefficients Endogenous Variable(s):", x$coefEndoVar, "\n")
#}


