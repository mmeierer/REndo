#' @importFrom Formula as.Formula model.part
#' @importFrom stats lm coef terms
#' @export
copulaCorrectionContinuous2  <- function(formula, data){

  cl <- match.call()

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- Formula::as.Formula(formula)
  df.y              <- Formula::model.part(object = F.formula, data = data, lhs=1, rhs = 0)
  df.data.exo.endo  <- Formula::model.part(object = F.formula, data = data, lhs=0, rhs = c(1,2))
  df.data.endo      <- Formula::model.part(object = F.formula, data = data, lhs=0, rhs = 2)

  intercept <- as.logical(attr(x = terms(x = F.formula), which = "intercept"))

  # * Replace old stuff with new ------------------------------------------------------------------------


  # mf <- model.frame(formula = formula, data = data)
  # y <- mf[,1]
  y <- df.y
  # regs <- unlist(strsplit(as.character(formula)[3], "\\("))
  # predictors <- unlist(strsplit(regs[1], " [+] "))

  # regressors <- mf[,2:dim(mf)[2]]
  # X <- regressors # both endogenous and exogenous
  X <- df.data.exo.endo
  # P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  # colnames(P) <- endoVar
  P <- df.data.endo
  # k <- ncol(X)

  # ** Only changes after here: fix dataCopula <- c(y, X), and setting return object right------------------------------

  # create pStar for all endogenous regressors provided in P
  PS <- apply(P, 2, copulaPStar)
  colnames(PS) <- paste("PS", 1:ncol(P), sep=".")
  #X <- as.matrix(X)
  #add the pStar variables to the set of regressors
  dataCopula <- data.frame(cbind(y, X, PS))
  datCop <- cbind(X,PS)
  if (intercept==TRUE) {
    f.lm <- stats::lm(y ~., dataCopula)
  } else {f.lm <- stats::lm(y ~.-1, dataCopula)}

  # ** Set return object for printing etc --------------------------------------------------------------
  # f.lm$call <- match.call()
  # res <- list(f=f.lm, reg=datCop, resid = f.lm$residuals, fitted.values = f.lm$fitted.values, stats = lm_stats)
  f.lm.summary <- summary(f.lm)
  lm_stats <- list(residSE = f.lm.summary$sigma,r2 = f.lm.summary$r.squared,adjr2 = f.lm.summary$adj.r.squared,fstat = f.lm.summary$fstatistic, df=f.lm.summary$df)
  res <- list(call = cl,
              coefficients = coef(f.lm),
              seCoefficients = as.matrix(f.lm.summary$coefficients[,2]),
              regressors = datCop,
              residuals = as.matrix(as.numeric(f.lm$residuals)),
              fitted.values = as.matrix(f.lm$fitted.values),
              lm_stats = lm_stats)

  class(res) <- "rendo.copulacorrection.continuous2"

  return(res)
}
