
setClass(
  #Class name
  "copulaEndo"
)


#' S3 Method for copulaEndo object for generic "coef"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo().
#'@param ... further arguments passed to or from other methods.
#'@export

coef.copulaEndo <- function(object, ...)
  {
   if ("discrete" %in% type){
     print(object$a.est, object$b.est,object$ps.est,object$se.a,object$se.b,object$se.ps)
   } else if (("continuous" %in% type) || (method == 1)){
     
     print(object$coefExoVar, object$coefEndoVar)
   }
    if (method == 2){
      print(object$coefficients)
    }
}


#' S3 Method for copulaEndo object for generic "summary"
#'@param object an object of class "copulaEndo", usually, a result of a call to copulaEndo().
#'@param ... further arguments passed to or from other methods.
#'@export
summary.copulaEndo <- function(object, ...)
{
    z <- object
    est <- z$coefficients  # estimates value
    se <- z$seCoefficients  # standard errors
    names.coef <- all.vars(z$formula[[3]])

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


#' S3 Method for copulaEndo object for generic "print"
#'@param x an object of class "liv", usually, a result of a call to liv().
#'@param ... further arguments passed to or from other methods.
#'@export
print.copulaEndo <- function(obj, ...)
{
    cat("Coefficients Exogenous Variables:", obj$coefExoVar, "\n")
    cat("Coefficients Endogenous Variable(s):",obj$coefEndoVar, "\n")
    cat("GPA:", obj$, "\n")
  
}


