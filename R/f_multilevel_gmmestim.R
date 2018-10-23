#' @importFrom Matrix tcrossprod t diag
#' @importFrom stats pnorm
#' @importFrom corpcor pseudoinverse
multilevel_gmmestim <- function(y, X, W, HIV){

  # For both levels, num.indep is number elements (childs)
  num.indep <- nrow(W)

  # KM07 Appendix 1  -----------------------------------------------------------------------------------

  # From A.1 (21)
  # Original (from raluca):

  # Precalculations:
  yf      <- W %*% y
  Xf      <- W %*% X

  # Ghat = 1/nH'WX
  GHat    <- Matrix::t(HIV) %*% Xf/num.indep
  # M_HH = 1/nH'H
  MHH     <- Matrix::crossprod(HIV)/num.indep
  ginvMHH <- corpcor::pseudoinverse(MHH)
  # GammaHat = inv(Ghat'inv(M_HH)GHat)Ghat'inv(M_HH)
  GammaH  <- corpcor::pseudoinverse(Matrix::t(GHat) %*% ginvMHH %*% GHat)%*% Matrix::t(GHat) %*% ginvMHH

  # A.1 GMM Estimator
  bIV     <- GammaH %*% (Matrix::t(HIV) %*% yf) / num.indep

  # Model residuals - Gmm (Proposition 2)
  # residW  <- yf - Xf %*% bIV
  # *** tcrossprod(residW, W)?? where is this in the paper??
  # rwrw <- W %*% Matrix::crossprod(W, residW)

  # Standard Error, pval --------------------------------------------------------------------------------

  Lambda  <- MHH

  MVarbIV     <- GammaH %*% Matrix::tcrossprod(Lambda, GammaH)/num.indep
  Mstderr_bIV <- sqrt(Matrix::diag(MVarbIV))


  # *** Move to summary function
  # z         <- as.matrix(bIV/Mstderr_bIV) # make sparse to regular matrix for pval
  # p.val     <- 2*stats::pnorm(-abs(z))

  bIV <- as.vector(bIV)
  names(bIV) <- colnames(X)
  Mstderr_bIV <- as.vector(Mstderr_bIV)
  names(Mstderr_bIV) <- names(bIV)
  return(list(coef=bIV, SE = Mstderr_bIV, GammaH=GammaH))
}
