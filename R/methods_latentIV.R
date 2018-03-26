#' @title  latentIV methods
#' @description prints the coefficients of the estimated model
#' @param object an object of class "liv"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "coef", signature= c(object="livREndo"), definition=function(object, ...) {
               
               names.coef <- c("Intercept", object@formula[[3]])
               est <- t(object@coefficients)   # estimates value
               colnames(est) <- names.coef
               est
}
)

#' @title  latentIV methods
#' @description returns the summary of the estimated model
#' @param object an object of class "liv"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "summary", signature= c(object="livREndo"), definition=function(object, ...) {
          
            z <- object
            est <- z@coefficients   # estimates value
            se <- z@seCoefficients    # standard errors
            t_val <- est/(se/sqrt(length(z@dataset[,1])))  # t-score
            pval <- 2*pt(q=(-abs(t_val)), df=(length(z@dataset[,1])-1))    # p-values
            names.coef <- all.vars(z@formula[[3]])
            
            coef.table <- cbind(est,se, t_val, pval)
            colnames(coef.table) <- c("Estimate","Std. Error", "t-value","Pr(>|t|)")
            rownames(coef.table) <- c("Intercept",names.coef)
            
            cat("\nCall:\n")
            print(z@call)
            cat("\nCoefficients:\n")
            printCoefmat(coef.table) # print the coefficient and std errors
            
            cat("\nInitial Parameter Values:\n", z@initValues)  # print initial param values
            cat("\n")
            cat("\nThe value of the log likelihood function:\n", z@value)  # print logLik values
            cat("\n")
            cat("\nAIC:", z@AIC, ",", "BIC:", z@BIC)  # AIC and BIC
            cat("\n")
            cat("\nConvergence Code:\n", z@convCode)  # print convergence code
      }
)
