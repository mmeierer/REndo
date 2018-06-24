#' @importFrom stats sd
copulaCorrection_bootstrappingSE <- function(bot, y, X, P, intercept, param){


  # * Replace old stuff with given input already extracted before --------------------------------------------------

  # mf <- model.frame(formula = formula, data = data)
  # y <- mf[,1]
  # regs <- unlist(strsplit(as.character(formula)[3], "\\("))
  # predictors <- unlist(strsplit(regs[1], " [+] "))

  # regressors <- mf[,2:dim(mf)[2]]
  # X <- regressors # both endogenous and exogenous
  # P <- as.matrix(X[,which(colnames(X) %in% endoVar)])
  # colnames(P) <- endoVar
  # ** After here: call to copulaCont adapted ----------------------------------------------------------------------

  if (intercept==TRUE) {
    cop.sd <- matrix(,nrow=bot, ncol=ncol(X)+2+1)
  } else {
    cop.sd <- matrix(,nrow=bot, ncol=ncol(X)+2)
  }

  for (j in (1:bot)) {

    # *** PERCENTAGE 80%
    # *** length(P) should be nrow()
    boot.se <- sample(length(y), length(P), replace=TRUE)

    y <- y[boot.se]
    X <- X[boot.se, ]
    # *** Do together
    dataBoot <- cbind.data.frame(y=scale(y, scale=TRUE), scale(X, scale=TRUE))

    # I <- matrix(rep(1,nrow(X)),nrow(X),1)
    # colnames(I) <- c("I")
    # dataBoot <- cbind(I, dataBoot)    # add intercept I to matrix X

    # **** New LL will be on GIT
    # cop.opt <- suppressWarnings(copulaCont1(formula,endoVar, param, intercept, data=dataBoot))
    cop.opt <- suppressWarnings(copulaCorrection_singleContinuous1(y=y, X=X, P=P, intercept=intercept, param=param))
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

