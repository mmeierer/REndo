#' @importFrom corpcor make.positive.definite
#' @importFrom Matrix rankMatrix t solve bdiag
#' @importFrom stats pchisq
multilevel_ommitedvartest <- function(IV1, IV2,
                                      coef.IV1, coef.IV2,
                                      gammaH.IV1, gammaH.IV2,
                                      W,
                                      l.L2.groups.X, l.L2.groups.y){


  # LambdaHats ---------------------------------------------------------------------------

  # Proposition 6
  # lambdaHat = (1/n)H_i'WVhatWH_j, i,j = 1,2
  #  where  Vhat = blkdiag(e1e′1, . . . , ene′n)
  #   and residuals e_i = y_i − X_ib_1

  # residuals in blocks in bdiag
  # bd.b1      <- Matrix::bdiag(rep(list(coef.IV1), times=num.groups))
  # bd.resid.e <- bd.y - bd.X %*% bd.b1
  m.coef.IV1 <- matrix(coef.IV1, ncol=1)
  l.L2.groups.resid.e <- mapply(g.y=l.L2.groups.y, g.x=l.L2.groups.X, FUN=function(g.y,g.x){
    g.y - g.x %*% m.coef.IV1})


  # resid * resid' in blocks
  V.hat <- Matrix::tcrossprod(Matrix::bdiag(l.L2.groups.resid.e))
  print(paste0("V.hat done:", class(V.hat)))

  # to reduce computations
  WVhatW <- W %*% V.hat %*% W
  print(paste0("WvhatW done:", class(WVhatW)))

  # ** What is n here really? it is e_n for residuals -> num groups! But what for L3 case?
  n <- nrow(IV1) # num.groups?? num.groups <- length(l.Xgroups)
  LambdaHat11 <- Matrix::t(IV1) %*% WVhatW %*% IV1 / n
  LambdaHat22 <- Matrix::t(IV2) %*% WVhatW %*% IV2 / n
  LambdaHat12 <- Matrix::t(IV1) %*% WVhatW %*% IV2 / n

  print(paste0("lambdahat done:", class(LambdaHat12)))

  # Proposition 6.
  # *** WHERE IS THE F ??
  OmegaHat <- gammaH.IV2 %*% LambdaHat22 %*% Matrix::t(gammaH.IV2) +
                gammaH.IV1 %*% LambdaHat11 %*% Matrix::t(gammaH.IV1) -
                gammaH.IV1 %*% LambdaHat12 %*% Matrix::t(gammaH.IV2) -
                gammaH.IV1 %*% LambdaHat12 %*% Matrix::t(gammaH.IV2)
  colnames(OmegaHat) <- rownames(OmegaHat) <- names(coef.IV1)

  print(paste0("OmegaHat done:", class(OmegaHat)))

  # Results ------------------------------------------------------------------------------------
  names.common  <- intersect(names(coef.IV1), names(coef.IV2))

  # diff of coefs
  diff.coef    <- coef.IV1[names.common] - coef.IV2[names.common]

  # vcov diff
  diff.vcov <- corpcor::make.positive.definite(OmegaHat[names.common, names.common]/n)

  # x2 diff
  # Proposition 6:
  # The test statistic TS_robust = (b_2gmm-b_1gmm)'F'inv(OmegaHat)F(b_2gmm-b_1gmm)
  # *** Is stat.ht this TS_robust ??
  stat.ht    <- as.numeric(Matrix::t(diff.coef) %*% corpcor::pseudoinverse(diff.vcov, diff.coef))
  names(stat.ht) <- "chisq"

  df      <- Matrix::rankMatrix(OmegaHat[names.common, names.common])
  p.val   <- stats::pchisq(q = stat.ht, df = df, lower.tail = FALSE)

  return(list(stat=stat.ht, df = df, p.val = p.val))
}
