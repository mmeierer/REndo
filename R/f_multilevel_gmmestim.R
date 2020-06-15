#' @importFrom Matrix tcrossprod t diag
#' @importFrom stats pnorm
#' @importFrom corpcor pseudoinverse
multilevel_gmmestim <- function(y, X, W, HIV, num.groups.highest.level){

  # GMM Estimator in Appendix A1, p529 ---------------------------------------------------------------------
  # b_GMM = GammaHat(H) (1/n) H' Wy
  #   where GammaHat(H) = inv(Ghat' inv(M_HH) GHat) Ghat' inv(M_HH)
  #           and Ghat = 1/n H' W X
  #           and M_HH = 1/n H' H
  #           and n is the number of highest level group (school)

  # p.512: "and s = 1,...,n schools"
  n <- num.groups.highest.level

  # Ghat = 1/n H' W X
  # M_HH = 1/n H' H
  # GammaHat(H) = inv(Ghat' inv(M_HH) GHat) Ghat' inv(M_HH)
  GHat    <- Matrix::t(HIV) %*% W %*% X / n
  MHH     <- Matrix::crossprod(HIV) / n
  ginvMHH <- corpcor::pseudoinverse(MHH)
  Gamma.H <- corpcor::pseudoinverse( Matrix::t(GHat) %*% ginvMHH %*% GHat) %*% Matrix::t(GHat) %*% ginvMHH

  # Actual parameter estimate
  # b_GMM = GammaHat(H) (1/n) H' Wy
  bIV     <- Gamma.H %*% Matrix::t(HIV) %*% W %*% y / n


  # Standard Error, pval, p.530 --------------------------------------------------------------------------------
  #   b_GMM has an asymptotic (n -> Inf) normal distribution with mean beta
  #     and asymptotic variance Gamma(H) Lambda Gamma(H)'

  Lambda  <- MHH

  gmm.vcov     <- as.matrix(Gamma.H %*% Matrix::tcrossprod(Lambda, Gamma.H) / n)
  Mstderr_bIV <- sqrt(Matrix::diag(gmm.vcov))

  # round values to "digits". Mostly done to make coefs for FE close to 0 actually 0
  bIV                <- round(as.vector(bIV), digits = getOption(x = "digits"))
  names(bIV)         <- colnames(X)
  Mstderr_bIV        <- as.vector(Mstderr_bIV)
  names(Mstderr_bIV) <- names(bIV)
  rownames(gmm.vcov) <- colnames(gmm.vcov) <- names(bIV)
  return(list(coef=bIV, SE = Mstderr_bIV, Gamma.H=Gamma.H, vcov = gmm.vcov))
}
