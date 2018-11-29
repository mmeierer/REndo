#' @importFrom corpcor make.positive.definite
#' @importFrom Matrix rankMatrix t bdiag
#' @importFrom stats pchisq
multilevel_omittedvartest <- function(IV1, IV2,
                                      res.gmm.IV1, res.gmm.IV2,
                                      W,
                                      # Have to be highest level blocks (L3:s, L2:c)
                                      l.Lhighest.X, l.Lhighest.y){
  # Read data from GMM results -----------------------------------------------------------
  coef.IV1 <- res.gmm.IV1$coef
  coef.IV2 <- res.gmm.IV2$coef
  gammaH.IV1 <- res.gmm.IV1$Gamma.H
  gammaH.IV2 <- res.gmm.IV2$Gamma.H

  # n = number of groups at highest level (L3:num schools, L2:num schools)
  num.groups <- length(l.Lhighest.X)


  # Proposition 6 ------------------------------------------------------------------------
  #  The Test-statistic TS_robust = (b_2 - b_1)' inv(OmegaHat) (b_2 - b_1)'
  #   A robust estimator of the asymptotic variance of b_{2,GMM} - b_{1,GMM} is
  #     OmegaHat = ....
  #       with
  #     lambdaHat = (1/n) H_i' W Vhat W H_j,   i,j = 1,2
  #       where  Vhat = blkdiag(e1e′1, . . . , ene′n), n=number highest level (school)
  #         and residuals e_i = y_i − X_ib_1



  # Residuals ----------------------------------------------------------------------------
  m.coef.IV1 <- matrix(data = coef.IV1, ncol=1)
  l.Lhighest.resid.e <- mapply(l.Lhighest.y, l.Lhighest.X, FUN=function(g.y,g.x){
                                  g.y - g.x %*% m.coef.IV1})
  # resid * resid' in blocks
  V.hat <- Matrix::tcrossprod(Matrix::bdiag(l.Lhighest.resid.e))

  # Alternative:
  # bd.b1      <- Matrix::bdiag(rep(list(coef.IV1), times=num.groups))
  # bd.resid.e <- Matrix::bdiag(l.Lhighest.y) - Matrix::bdiag(l.Lhighest.X) %*% bd.b1
  # V.hat.bd <- Matrix::tcrossprod(bd.resid.e)
  # print(all.equal(V.hat.bd, V.hat)) # TRUE


  # LambdaHats + OmegaHat -------------------------------------------------------------------------
  #   lambdaHat = (1/n) H_i' W Vhat W H_j,   i,j = 1,2
  WVhatW <- W %*% V.hat %*% W # to reduce computations
  LambdaHat11 <- Matrix::t(IV1) %*% WVhatW %*% IV1 / num.groups
  LambdaHat12 <- Matrix::t(IV1) %*% WVhatW %*% IV2 / num.groups
  LambdaHat21 <- Matrix::t(IV2) %*% WVhatW %*% IV1 / num.groups
  LambdaHat22 <- Matrix::t(IV2) %*% WVhatW %*% IV2 / num.groups

  # OmegaHat = GH2 L22 GH2' + GH1 L11 GH1' - GH2 L21 GH1' - GH1 L12 GH2'
  OmegaHat <- gammaH.IV2 %*% LambdaHat22 %*% Matrix::t(gammaH.IV2) +
                gammaH.IV1 %*% LambdaHat11 %*% Matrix::t(gammaH.IV1) -
                gammaH.IV2 %*% LambdaHat21 %*% Matrix::t(gammaH.IV1) -
                gammaH.IV1 %*% LambdaHat12 %*% Matrix::t(gammaH.IV2)

  # Proposition 3 and example code by Kim / Frees
  #  "The asymptotic variance is 1/n * F(...)"
  #   Hence OmegaHat = 1/n OmegaHat
  OmegaHat <- OmegaHat / num.groups

  colnames(OmegaHat) <- rownames(OmegaHat) <- names(coef.IV1)

  # Results ------------------------------------------------------------------------------------

  # Proposition 6:
  # The test statistic TS_robust = (b_2gmm-b_1gmm)' inv(OmegaHat) (b_2gmm-b_1gmm)
  stat.ht    <- as.numeric(Matrix::t(coef.IV2-coef.IV1) %*% corpcor::pseudoinverse(OmegaHat) %*% (coef.IV2-coef.IV1))
  names(stat.ht) <- "chisq"

  df      <- as.numeric(Matrix::rankMatrix(OmegaHat))
  p.val   <- stats::pchisq(q = stat.ht, df = df, lower.tail = FALSE)

  return(list(stat=stat.ht, df = df, p.val = p.val))
}
