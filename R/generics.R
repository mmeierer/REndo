#' liv S4 Object
#'
#' This class is used to store and further analyze the results of the liv function
#' @slot formula
#' @slot coefficients
#' @slot se_coefficients
#' @slot group_means
#' @slot se_means
#' @slot sigma
#' @slot prob_G1
#' @slot se_probG1
#' @slot value
#' @slot convergence
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
    se_coefficients = "numeric",
    group_means = "numeric",
    se_means = "numeric",
    sigma = "matrix",
    prob_G1 = "numeric",
    se_probG1 = "numeric",
    value = "numeric",
    convergence = "integer",
    hessian = "matrix"

  ),

  prototype = list(formula = NA,
                   coefficients = NA_real_,
                   se_coefficients = NA_real_,
                   group_means = NA_real_,
                   se_means = NA_real_,
                   sigma = matrix(NA),
                   prob_G1 = NA_real_,
                   se_probG1 = NA_real_,
                   value = NA_real_,
                   convergence = NA_integer_,
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
  str(object)
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


