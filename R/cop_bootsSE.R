#'@title Bootstrapping Standard Errors
#'@aliases boots
#'@description Performs bootstrapping to obtain the standard errors of the estimates of the model with one continuous endogenous regressor estimated via maximum likelihood
#'using the \code{\link{copulaCorrection}} function.
#
# Arguments
#'@param    bot  number of bootstrap replicates.
#'@param    formula  the model formula, e.g. \code{y ~ X1 + X2 + P}. 
#'@param    endoVar  a string with the name of the endogenous variable/s, in quotation marks.
#'@param    param  initial values for the parameters to be optimized over. See \code{\link{copulaCorrection}} for more details.  
#'@param    intercept  an optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired in model estimation, intercept should be given the value "FALSE", otherwise the value "TRUE".  
#'@param    data a data frame or matrix containing the variables of the model.
# Return Value
#'@return Returns the standard errors of the estimates of the model using the copula method 1 described in Park and Gupta (2012). See Details section of \code{\link{copulaCorrection}}.
#'@details The function could be used only when there is a single endogenous regressor and method one is selected in \code{\link{copulaCorrection}}.
#'of the \code{copulaCorrection} function is used for estimation.
#'@seealso \code{\link{copulaCorrection}}
#'@export
boots <- function(bot,formula, endoVar,param,intercept = NULL, data){
   
  mf <- model.frame(formula = formula, data = data)
  y <- mf[,1]
  regs <- unlist(strsplit(as.character(formula)[3], "\\(")) 
  predictors <- unlist(strsplit(regs[1], " [+] "))
 
  regressors <- mf[,2:dim(mf)[2]]
  X <- regressors # both endogenous and exogenous
  P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  colnames(P) <- endoVar
  if (intercept==TRUE) {
    cop.sd <- matrix(,nrow=bot, ncol=ncol(X)+2+1)
  } else {
    cop.sd <- matrix(,nrow=bot, ncol=ncol(X)+2)
  }
   
  for (j in (1:bot)) {

  boot.se <- sample(length(y), length(P), replace=TRUE) 

  y <- y[boot.se]
  X <- X[boot.se, ]
  dataBoot <- cbind.data.frame(y=scale(y, scale=TRUE), scale(X, scale=TRUE))
 
# I <- matrix(rep(1,nrow(X)),nrow(X),1)
# colnames(I) <- c("I")
# dataBoot <- cbind(I, dataBoot)    # add intercept I to matrix X

cop.opt <- suppressWarnings(copulaCont1(formula,endoVar, param, intercept, data=dataBoot))
# put results in a matrix
# col 1- alfa, col2-n -beta, last 2 cols - rho and sigma
matrix.out <- matrix(unlist(cop.opt$coef_cop), byrow=TRUE)  # 1 column matrix with the elements returned by optimx


cop.sd[j,1:cop.opt$k1] <- matrix.out[1:cop.opt$k1]
}

sd.b <- rep(0, cop.opt$k1)
 for (i in 1:length(sd.b)){
   sd.b[i] <- sd(cop.sd[,i])
 }
 
#if (intercept==TRUE)
#{ I <- matrix(rep(1,nrow(X)))
#  colnames(I) <- c("I")
#  X <- cbind(I,X)
#}
k <- cop.opt$k
res.sd <- list(se.a = sd.b[k], se.betas = sd.b[1:(cop.opt$k-1)], se.rho = sd.b[length(sd.b)-1], se.sigma = sd.b[length(sd.b)], reg=as.matrix(X))

coef.table <- res.sd$se.a
coef.table <- append(coef.table, res.sd$se.betas)
coef.table <- append(coef.table, res.sd$se.rho)
coef.table <- append(coef.table, res.sd$se.sigma)
coef.table <- matrix(coef.table,, ncol=1)
if (intercept==TRUE){
  r1 <- append(colnames(res.sd$reg),c("rho","sigma"))
  rownames(coef.table)  <- append(c("Intecept"),r1)
} else {
  rownames(coef.table) <- append(colnames(res.sd$reg),c("rho","sigma"))
}
colnames(coef.table) <- c("Std.Error")
return(coef.table)
}


	