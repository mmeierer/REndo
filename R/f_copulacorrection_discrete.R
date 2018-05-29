#' @importFrom Formula as.Formula model.part
#' @importFrom stats ecdf runif coef lm confint qnorm terms
#' @export
copulaCorrectionDiscrete <- function(formula, data) {

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
  P <- as.matrix(df.data.endo)
  # colnames(P) <- endoVar
  colnames(P) <- colnames(df.data.endo)


  # ** Only changes after here: fix dataCopula <- c(y, X) and set class of return ------------------------------

  sim <- 250
  k1 <- ncol(X)
  # P is the endogeneous regressor
  # P1 <- as.matrix(P)
  # colnames(P1) <- colnames(X)[-c(1:(ncol(X)-ncol(P1)))]

  #  empirical cumulative distribution function
  getEcdf <- function(x){
    H.p <- stats::ecdf(x)
    H.p1 <- H.p(x)
    H.p0 <- H.p(x-1)
    H.p0 <- ifelse(H.p0==0,0.0000001,H.p0)
    H.p0 <- ifelse(H.p0==1,0.9999999,H.p0)
    H.p1 <- ifelse(H.p1==0,0.0000001,H.p1)
    H.p1 <- ifelse(H.p1==1,0.9999999,H.p1)

    gecdf <- list(H.p = H.p, H.p1=H.p1, H.p0 = H.p0)

    return(gecdf)
  }


  # each element of pecdf had 3 levels - H.p, H.p0 and H.p1.
  # Nr. of elements of pecdf = Nr. columns P1
  pecdf <- apply(P,2,getEcdf)


  U.p <- matrix(NA, dim(P)[1], dim(P)[2])
  ifelse (intercept==TRUE, nc <- ncol(X) + ncol(P) +1, nc <- ncol(X) + ncol(P))
  # create a list with nc elements (number of regressors + Pstar) , for each regressor save the confint for each lm out of sim simulations
  conf.int <- rep(list(matrix(0,sim,2)), nc)

  for (k in 1:dim(P)[2]){

    U.p[,k] <- stats::runif(dim(P)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
  }
  p.star <- apply(U.p, 2,qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P), sep=".")

  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- cbind(y,X1)

  if (intercept==FALSE){  # no intercept
    meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))
    bhat <-   stats::coef(summary(meth.2))[,1] # estimated coefficients

  } else {
    meth.2 <- stats::lm(y~.,data=data.frame(dataCopula))  # with intercept
    bhat <- stats::coef(summary(meth.2))[,1]   # estimated coefficients
  }
  # names(bhat) <- colnames(dataCopula) # ** fails w/o intecept! ??


  for (i in 1:sim){
    for (k in 1:dim(P)[2]){

      U.p[,k] <- stats::runif(dim(P)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
    }
    p.star <- apply(U.p, 2,qnorm)
    colnames(p.star) <- paste("PStar",1:ncol(P),sep=".")

    X1 <- cbind(X,p.star)
    X1 <- as.matrix(X1)
    k2 <- ncol(X1)
    dataCopula <- cbind(y, X1)

    if (intercept==TRUE){  # intercept
      meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))
      for (s in 1:nc) {
        conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]
      }
    } else {
      meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))  # with intercept
      for (s in 1:nc) {
        conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]

      }
    }
  }

  CI <- matrix(0,nc,2)
  colnames(CI) <- c("Lower95%", "Upper95%")
  for (i in 1:nc)
    CI[i,] <- apply(conf.int[[i]],2,mean)


  res <- list(call = cl, coefficients = meth.2$coefficients, CI=CI, reg = dataCopula, resid = meth.2$residuals, fitted.values=meth.2$fitted.values)

  # ** Set class of returned object for printing etc ------------------------------------------------
  class(res) <- "rendo.copulacorrection.discrete"

  return(res)
}
