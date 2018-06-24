#' @importFrom Formula as.Formula model.part
#' @importFrom stats lm coef terms
#' @importFrom optimx optimx
#' @export
copulaCorrectionContinuous1_old  <- function(formula, start.params = c(), num.boots=10, data){
  message("The endogeneous regressor should be continuous and NOT normally distributed")

  cl <- match.call()
  param <- start.params # "backward compatibility" to old code

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- as.Formula(formula)
  df.y              <- model.part(object = F.formula, data = data, lhs=1, rhs = 0)
  df.data.exo.endo  <- model.part(object = F.formula, data = data, lhs=0, rhs = c(1,2))
  df.data.endo      <- model.part(object = F.formula, data = data, lhs=0, rhs = 2)

  intercept <- as.logical(attr(x = terms(x = F.formula), which = "intercept"))

  # * Replace old stuff with new ------------------------------------------------------------------------

  # mf <- model.frame(formula = formula, data = data)
  # y <- mf[,1]
  y <- df.y[,1]
  # regs <- unlist(strsplit(as.character(formula)[3], "\\("))
  # predictors <- unlist(strsplit(regs[1], " [+] "))

  # regressors <- mf[,2:dim(mf)[2]]
  # X <- regressors # both endogenous and exogenous
  X <- df.data.exo.endo
  # P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  # colnames(P) <- endoVar
  P <- as.matrix(df.data.endo)
  colnames(P) <- colnames(df.data.endo)
  k <- ncol(X)
  k1 <- k+2
  # ** Only changes after here: fix datalm <- c(y, X), and setting return object right------------------------------

  cC1 <- copulaCorrection_singleContinuous1(y=y, X=X, P=P, intercept=intercept, param=param)
  # function(bot, y, X, P, intercept,formula, endoVar,param,intercept = NULL, data)
  seC1 <- copulaCorrection_bootstrappingSE(bot = num.boots, param=cC1$param, y=y, X=X, P=P, intercept=intercept)


  k <- cC1$k    # position of the endogenous regressor - always on the last column of X
  k1 <- cC1$k1    # k1=k+2

  res <- list(call = cl)
  res$coefEndoVar  <- cC1$coef_cop[,k]
  res$coefExoVar <- as.numeric(cC1$coef_cop[,1:(k-1)])
  res$seCoefficients <- seC1
  res$rho   <- cC1$coef_cop[,k1-1]
  res$sigma <- cC1$coef_cop[,k1]
  res$param <- cC1$param
  # res$logLik <- (-1)*cC1$coef_cop[,"fevals"] *** WRONG**
  res$logLik <- (-1)*cC1$coef_cop[,"value"]
  res$AIC <- (-2)*(res$logLik) + 2*length(cC1$param)  # Akaike Information Criterion
  res$BIC <- (-2)*(res$logLik) + length(y)*length(cC1$param)
  res$convCode <- cC1$coef_cop[,"convcode"]
  res$regressors <- cC1$reg
  res$fitted.values <- as.numeric(cC1$coef_cop[,1:k]) * cC1$reg
  res$residuals <- as.matrix(y-res$fitted.values)

  class(res) <- "rendo.copulacorrection.continuous1"

  return(res)
}
