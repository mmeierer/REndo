#' @title  methods for the hetErrorsIV function
#' @description prints the coefficients for the model's parameters 
#' @param object an object of class "hetREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod( f = "coef", signature= c(object="hetREndo"), definition=function(object, ...)
  {
    z <- object   
    cat("Call:\n")
    print(z@call)
    cat("\nCoefficients\n")
    printCoefmat(z@coefficients, P.values=T, has.Pvalue=T)
    }
)

#' @title  methods for the hetErrorsIV function
#' @description prints summary for the model's parameters 
#' @param object an object of class "hetREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod( f = "summary", signature= c(object="hetREndo"), definition=function(object, ...)
{
  z <- object   
  cat("Call:\n")
  print(z@call)
  cat("\nCoefficients:\n")
  printCoefmat(z@coefficients, P.values=T, has.Pvalue=T)
  cat("\nOveridentification Test:\n")
  print(z@jtest)
  cat("\nPartial F-test Statistics for Weak IV Detection:\n")
  print(z@ftest)
  cat("\nNumber of Observations:", z@obs, "\n")
}
)