 #' @title  methods for the multilevelIV function
#' @description prints the coefficients for the model's parameters 
#' @param object an object of class "mixedREndo"
#' @param ... extra named arguments 
#' @export
#' @docType methods
#' @aliases coef-methods
#' @aliases coef, ANY-method
#' @aliases coef, mixedREndo-method
#' @keywords internal
setMethod(f = "coef", signature= c(object="mixedREndo"), definition=function(object, ...){
            
            z <- object
            coef.table <- z@coefficients
            print(coef.table)
        
     }
)

#' @title  methods for the multilevelIV function
#' @description prints the summary for the model's parameters 
#' @param object    object of class multilevelIV
#' @param model     for which of the estimated models should the coefficients be displayed. If \code{model="all"}, the coefficients of all estimated models will be printed.
#' @export
#' @docType methods
#' @aliases summary-methods
#' @aliases summary, ANY-method
#' @aliases summary, mixedREndo-method
#' @keywords internal
setMethod(f = "summary", signature= c(object="mixedREndo"), definition=function(object, model=NULL){
 
             z <- object
             mcl <- match.call() 
             
             # if a model is not provided, print all
            if (is.null(mcl$model)) { 
              ans <- z@coefSdErr 
              cat("\nCall:\n")
              print(z@mixedCall)
              coef.table <- z@coefficients
              print(coef.table)
              } else {
            
             if ("REF" %in% mcl$model){
               ans <-  z@coefSdErr$REF 
               OmittedVarTest <- z@omittedVarTest$REF 
             }
             if ("GMM_L3" %in% mcl$model){
               ans <- z@coefSdErr$GMM_L3 
               OmittedVarTest <- z@omittedVarTest$GMM_L3
             }
             if ("GMM_L2" %in% mcl$model){
               ans <- z@coefSdErr$GMM_L2 
               OmittedVarTest <- z@omittedVarTest$GMM_L2
             }
             if ("FE_L3" %in% mcl$model){
               ans <-  z@coefSdErr$FE_L3
               OmittedVarTest <- z@omittedVarTest$FE_L3
             } 
             if ("FE_L2" %in% mcl$model){
              ans <-  z@coefSdErr$FE_L2
              OmittedVarTest <- z@omittedVarTest$FE_L2
              }
          
            cat("\nCall:\n")
            print(z@mixedCall)
            cat("\nCoefficients:\n")
            print(ans)
            cat("\n")
            cat("Omitted variable test:\n") 
            cat("\n")
            print(OmittedVarTest) 
            cat("\n alternative hypothesis: one model is inconsistent")
            #return(OmittedVarTest)  
            }
}  
)


  
