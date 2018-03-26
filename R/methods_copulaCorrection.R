# Raluca Gui 02.06.2016
# methods for copulaCorrection

#' @title  coefficients for the copulaCorrection method
#' @description prints the estimated coefficients for the model's parameters
#' @param  object     an object of class "copulaREndo"
#' @param ...     extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "coef", signature= c(object="copulaREndo"), definition=function(object, ...)
  {
             mcl <- object@copCall

            if ("1" %in% mcl$method){
              z <- object
              estEnd <- z@coefEndoVar  # estimates value
              estEx <- z@coefExoVar
              coef.table <- round(estEx,5)
              coef.table <- append(coef.table, round(estEnd,5))
              coef.table <- append(coef.table, round(z@rho,5))
              coef.table <- append(coef.table, round(z@sigma,5))
              coef.table <- matrix(coef.table,,ncol=1)
              # z=score
              z_val_table <- coef.table[,1]/z@seCoefficients  # z-score endogenous variable
              # p-values
              pval <- 2*stats::pt(q=(-abs(z_val_table)), df=(length(z@regressors[,1])-1))  
              rownames(z@seCoefficients) <- NULL    # remove the row names not to appear twice
              colnames(z@seCoefficients) <- NULL    # remove the row names not to appear twice
              coef.table <- cbind(coef.table,round(z@seCoefficients,5))
              if (mcl$intercept == TRUE){
                rownames(coef.table) <- append(c("Intercept"),colnames(z@regressors),c("rho","sigma"))
              } else {
                rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
                }
              coef.table <- cbind(coef.table, round(z_val_table,3))
              coef.table <- cbind(coef.table, round(pval,3))
             # rownames(coef.table) <- colnames(z@regressors)
              colnames(coef.table) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")
              invisible(coef.table)
            }else
            if ("2" %in% mcl$method) {
              z <- object
              coef.table <- matrix(z@coefficients,ncol=1)
              if (mcl$intercept==TRUE){
              rownames(coef.table) <- append(c("Intercept"),colnames(z@regressors))
              }
              colnames(coef.table) <- c("Estimate")
             #cat("\nCoefficients:\n"))
              invisible(coef.table)


              } else
                if ("discrete" %in% mcl$type) {
                    z <- object
                    est <- z@coefficients # estimates value
                    coef.table <- est
                    coef.table <- matrix(coef.table, , ncol=1)
                    if (mcl$intercept==TRUE){
                       rownames(coef.table) <- append(c("Intercept"),colnames(z@regressors))
                    }
                    colnames(coef.table) <- c("Estimate")
                    # cat("\nCoefficients:\n")
                    invisible(coef.table) # print the coefficient


                }
           
            return(coef.table)
}

)

#' @title  summary for the copulaCorrection method
#' @description returns the summary of the estimated model
#' @param  object an object of class "copulaREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "summary", signature= c(object="copulaREndo"), definition = function(object, ...)            
    {         
            mcl <- object@copCall    
            
            if ("1" %in% mcl$method) {
              z <- object
              estEnd <- z@coefEndoVar  # estimates value
              estEx <- z@coefExoVar 
              coef.table <- round(estEx,5)
              coef.table <- append(coef.table, round(estEnd,5))
              coef.table <- append(coef.table, round(z@rho,5))
              coef.table <- append(coef.table, round(z@sigma,5))
              coef.table <- matrix(coef.table, , ncol=1) 
              
              #rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
              # z=score
              z_val_table <- coef.table[,1]/z@seCoefficients  # z-score endogenous variable
              # p-values
              pval <- 2*stats::pt(q=(-abs(z_val_table)), df=(length(z@regressors[,1])-1))  
              rownames(z@seCoefficients) <- NULL    # remove the row names not to appear twice
              colnames(z@seCoefficients) <- NULL    # remove the row names not to appear twice
              coef.table <- cbind(coef.table,round(z@seCoefficients,5))
              #if (mcl$intercept==TRUE){
              rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
              #} else {
              #  rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
              #}
              coef.table <- cbind(coef.table, round(z_val_table,3))
              coef.table <- cbind(coef.table, round(pval,3))
              # rownames(coef.table) <- colnames(z@regressors)
              colnames(coef.table) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")
              cat("\nCall:\n")
              print(z@copCall)
              cat("\nCoefficients:\n")
              print(coef.table) # print the coefficient 
              cat("\n Initial parameter values:\n", round(z@param,3))  # print initial param values
              cat("\n")
              cat("\n The Value of the log likelihood function:\n", z@logLik)  # print logLik values
              cat("\n")
              cat("\n AIC:", z@AIC,", ", "BIC: ",z@BIC, "\n")
              cat("\n Convergence Code:\n", z@convCode)  # print comvergence code
            } else 
              if ("2" %in% mcl$method){
                      z <- object
                      coef.table <- z@coefficients
                      coef.table <- cbind(coef.table, round(z@seCoefficients,5))
                      # z=score
                      z_val_table <- z@coefficients/z@seCoefficients  # z-score endogenous variable
                      coef.table <- cbind(coef.table, round(z_val_table,3))
                      # p-values
                      pval <- 2*stats::pt(q=(-abs(z_val_table)), df=(length(z@regressors[,1])-1))  
                      coef.table <- cbind(coef.table, round(pval,3))
                      if (mcl$intercept==TRUE){
                       rownames(coef.table) <- append(c("Intercept"),colnames(z@regressors))
                      } else{ 
                        rownames(coef.table) <- colnames(z@regressors)
                      }
                      colnames(coef.table) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")
                      cat("\nCall:\n")
                      print(z@copCall)
                      cat("\nCoefficients:\n")
                      printCoefmat(coef.table)
                      cat("Residual standard error: ",z@stats$residSE, "on", z@stats$df[2],"degrees of freedom \n")
                      cat("Multiple R-squared: ",z@stats$r2, " Adjusted R-squared: ", z@stats$adjr2,"\n")
                      pval_fstats <- pf(length(z@regressors[,1]), as.numeric(z@stats$fstat[2]), as.numeric(z@stats$fstat[3]), lower.tail=F)  # compute pvalue for Fstat
                      cat("F-statistic: ",z@stats$fstat[1], "on", z@stats$df[1], "and",  z@stats$df[2], "degrees of freedom,", "p-value: ", pval_fstats,"\n")
                    } else 
                       if ("discrete" %in% mcl$type){
                              z <- object
                              est <- z@coefficients  # coef. estimates 
                              coef.table <- round(est,5)
                              coef.table <- cbind(coef.table, round(z@confint,3)) 
                              if (mcl$intercept==TRUE){
                                rownames(coef.table) <- append(c("Intercept"),colnames(z@regressors))
                              } else{
                                rownames(coef.table) <- colnames(z@regressors)
                              }
                              colnames(coef.table) <- c("Estimate","Low_95%CI","Up_95%CI")
                              cat("\nCall:\n")
                              print(z@copCall)
                              cat("\nCoefficients:\n")
                              printCoefmat(coef.table) # print the coefficient 
                             
                       }
          
}           

)





