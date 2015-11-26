#' liv S4 Object
#'
#' This class is used to store and further analyze the results of the liv function
#' @slot formula
#' @slot coefficients
#' @slot seCoefficients
#' @slot groupMeans
#' @slot seMeans
#' @slot sigma
#' @slot probG1
#' @slot seProbG1
#' @slot initValues
#' @slot value
#' @slot convCode
#' @slot hessian

#' @name liv-class
#' @rdname liv-class
#' @exportClass liv
#'
#' @examples
#' getSlots("liv")
#'
#' @importFrom methods setClass
#' @export
setClass(

  #Class name
  "liv",


  #Slots / member vars
  slots = c(
    formula = "formula",
    coefficients = "numeric",
    seCoefficients = "numeric",
    groupMeans = "numeric",
    seMeans = "numeric",
    sigma = "matrix",
    probG1 = "numeric",
    seProbG1 = "numeric",
    initValues = "numeric",
    value = "numeric",
    convCode = "integer",
    hessian = "matrix"

  ),

  prototype = list(formula = NA,
                   coefficients = NA_real_,
                   seCoefficients = NA_real_,
                   groupMeans = NA_real_,
                   seMeans = NA_real_,
                   sigma = matrix(NA),
                   probG1 = NA_real_,
                   initValues = NA_real_,
                   value = NA_real_,
                   convCode = NA_integer_,
                   hessian = matrix(NA)
  )
)


#' S3 Method for LIV object for generic "coef"
#'@param object an object of class "liv", usually, a result of a call to liv().
#'@param ... further arguments passed to or from other methods.
#'@export
coef.liv <- function(object, ...)
  {print(object@coefficients)}


#' S3 Method for liv object for generic "summary"
#'@param object an object of class "liv", usually, a result of a call to liv().
#'@param ... further arguments passed to or from other methods.
#'@export
summary.liv <- function(object, ...)
{
    z <- object
    est <- z@coefficients  # estimates value
    se <- z@seCoefficients  # standard errors
    names.coef <- all.vars(z@formula[[3]])

    coef.table <- cbind(est,se)
    colnames(coef.table) <- c("Estimate","Std. Error")
    rownames(coef.table) <- c("Intercept",names.coef)

    cat("\nCoefficients:\n")
    printCoefmat(coef.table) # print the coefficient and std errors

    cat("\nInitial Parameter Values:\n", z@initValues)  # print initial param values
    cat("\n")
    cat("\nThe Value of the log likelihood function:\n", z@value)  # print logLik values
    cat("\n")
    cat("\nConvergence Code:\n", z@convCode)  # print comvergence code
}


#' S3 Method for liv object for generic "print"
#'@param x an object of class "liv", usually, a result of a call to liv().
#'@param ... further arguments passed to or from other methods.
#'@export
print.liv <- function(x, ...)
{
  str(x)
  # str(object)
}


