#' @importFrom Matrix bdiag Matrix Diagonal crossprod tcrossprod drop0
#' @importFrom data.table as.data.table setkeyv
#' @importFrom lme4 lFormula lmer VarCorr
#' @importFrom corpcor pseudoinverse
#' @importFrom stats setNames
multilevel_3levels <- function(cl, f.orig, dt.model.data, res.VC,
                               name.group.L2, name.group.L3,
                               name.y, names.X, names.X1, names.Z2, names.Z3,
                               verbose){

  # cran silence
  .SD <- NULL

  # Splitting names: What is used in DT "by" to form groups
  name.split.by.L3 <- name.group.L3
  name.split.by.L2 <- c(name.group.L3, name.group.L2)

  # Sort data by group to ensure that the groupwise readout (split) and the direct
  #   column readout has the same order
  data.table::setkeyv(dt.model.data, cols = name.split.by.L2)


  # Build y ------------------------------------------------------------------------------------
  # L3.y is only needed for calculating residuals in omitted var test
  y      <- multilevel_colstomatrix(dt = dt.model.data, name.cols = name.y)
  l.L3.y <- multilevel_splittomatrix(dt = dt.model.data, name.group = name.y, name.by = name.split.by.L3)


  # Build X, X1 --------------------------------------------------------------------------------
  # Do not use the X component present in l4.form as their ordering is somewhat obscure.
  # X1 is everything but endogenous

  l.L2.X  <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.X,  name.by = name.split.by.L2)
  l.L2.X1 <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.X1, name.by = name.split.by.L2)

  l.L3.X  <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.X,  name.by = name.split.by.L3)
  l.L3.X1 <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.X1, name.by = name.split.by.L3)

  X    <- multilevel_colstomatrix(dt = dt.model.data, name.cols = names.X)
  X1   <- multilevel_colstomatrix(dt = dt.model.data, name.cols = names.X1)


  if(verbose){
    message("Detected multilevel model with 3 levels.")
    message("For ", name.group.L2, " (Level 2), ", length(l.L2.X), " groups were found.")
    message("For ", name.group.L3, " (Level 3), ", length(l.L3.X), " groups were found.")
  }

  # Build Z2, Z3 --------------------------------------------------------------------------------
  # Only extract names from l4.form and build Z self because of unknown ordering

  l.L2.Z2  <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.Z2, name.by = name.split.by.L2)
  l.L3.Z3  <- multilevel_splittomatrix(dt = dt.model.data, name.group = names.Z3, name.by = name.split.by.L3)
  Z2       <- multilevel_colstomatrix(dt = dt.model.data, name.cols = names.Z2)
  Z3       <- multilevel_colstomatrix(dt = dt.model.data, name.cols = names.Z3)


  # Fit REML -----------------------------------------------------------------------------------
  # get D.2, D.3 and variance of residuals from VarCor
  #  sc is only the the residual standard deviation

  D.2      <- res.VC[[name.group.L2]]
  D.3      <- res.VC[[name.group.L3]]
  sigma.sq <- (attr(res.VC, "sc")^2)

  # ensure sorting of D cols is same as Z columns
  D.2 <- D.2[colnames(l.L2.Z2[[1]]), colnames(l.L2.Z2[[1]]), drop = FALSE]
  D.3 <- D.3[colnames(l.L3.Z3[[1]]), colnames(l.L3.Z3[[1]]), drop = FALSE]


  # Calc V -------------------------------------------------------------------------------------
  # Relevant Formula:
  #   p 515: V_s = R_s + Z_2sVar(err(2)_s)Z'_2s+Z_3sVar(err_s(3))Z'_3s
  #          -> For L2 case drop the Z_3 part
  # Structure:
  #   (18) V=blkdiag(V_s) and V = blkdiag(V1, . . . , Vn)
  #
  # To ensure the calculations are strictly per L2 and L3, they are done in
  #   separate blocks and then added "on top of each other"
  #   -> structure: L3 block with multiple L2 blocks in it

  l.L2.V.part <- lapply(l.L2.Z2, FUN = function(g.z2){
    g.z2 %*% D.2 %*% t(g.z2)
  })

  l.L3.V.part <- lapply(l.L3.Z3, FUN = function(g.z3){
    g.z3 %*% D.3 %*% t(g.z3)
  })

  # Needed for return object
  V <- Matrix::Diagonal(sigma.sq, n=nrow(X)) +
            Matrix::bdiag(l.L3.V.part) +
            Matrix::bdiag(l.L2.V.part)
  colnames(V) <- rownames(V) <- rownames(X)

  # Calc W -------------------------------------------------------------------------------------
  # Formula:
  #   p.510:
  #    W=blkdiag(W_1,..., W_n)
  #   "the weight can be the inverse of the square root of the variance–covariance matrix of the disturbance term V_{s}^(-1/2)"
  # Do eigen decomp on each block. Has to be L3 as L2 would omits non-zero vars

  l.L3.V   <- multilevel_splitmatrixtolist(m=V, dt.model.data=dt.model.data, name.split.by=name.split.by.L3)

  # Do eigen decomp on each block
  l.L3.W <- lapply(l.L3.V, function(g.v){
    ei                     <- eigen(g.v)
    ei$values[ei$values<0] <- 0
    sValS                  <- 1/sqrt(ei$values)
    sValS[ei$values==0]    <- 0
    ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))})


  W <- Matrix::bdiag(l.L3.W)
  colnames(W) <- rownames(W) <- rownames(X)


  # Calc Q -------------------------------------------------------------------------------------
  # Q has to be caluclated at a L3 and L2 level for building the instruments
  # Formula:
  #   L3: between(10-11):
  #         Q(2)_s=I_s-P(Z_3s), Z_3s=stacked Z_3sct
  #         where P(H) = H(H'H)^(-1)H' (p.510)
  #
  #
  # Structure:
  #   L3: p.512 (11)
  #       Q(2)=blkdiag(Q(2)_s)

  # . Q at L3 level ------------------------------------------------------------------------------------
  # Move the diagonal outside as the blocks are all square and therefore the diagonal is the same
  l.L3.Q <- mapply(l.L3.Z3, l.L3.W, FUN = function(g.z3, g.w3){
    g.w3 %*% g.z3 %*% corpcor::pseudoinverse(Matrix::crossprod(g.z3, g.w3) %*% g.w3 %*% g.z3) %*%
      Matrix::crossprod(g.z3, g.w3)
  })
  L3.Q <- Matrix::Diagonal(x=1, n=nrow(W)) - Matrix::bdiag(l.L3.Q)


  # . Q at L2 level ------------------------------------------------------------------------------------
  # Q at L2 only (no reference to L3 at all)

  # Split W into L2 groups
  l.L2.W   <- multilevel_splitmatrixtolist(m = W, dt.model.data=dt.model.data,
                                           name.split.by=name.split.by.L2)

  # Move the diagonal outside as the blocks are all square and therefore the diagnoal is the same
  l.L2.Q <- mapply(l.L2.Z2, l.L2.W, FUN = function(g.z2, g.w2){
    g.w2 %*% g.z2 %*%corpcor::pseudoinverse(t(g.z2)%*%(g.w2%*%g.w2)%*%g.z2) %*% Matrix::crossprod(g.z2, g.w2)
  })
  L2.Q <- Matrix::Diagonal(x=1, n=nrow(W)) - Matrix::bdiag(l.L2.Q)

  # Calc P -------------------------------------------------------------------------------------
  # Formula:
  #   L3:
  #   L2:
  # Structure:
  #   L3:
  #   L2:

  L2.P <- Matrix::Diagonal(x=1, n=nrow(L2.Q)) - L2.Q
  L3.P <- Matrix::Diagonal(x=1, n=nrow(L3.Q)) - L3.Q

  # Drop near zero values ----------------------------------------------------------------------
  # Very small values (ca 0) can cause troubles during inverse caluclation
  #   remove

  W  <- Matrix::drop0(W, tol=sqrt(.Machine$double.eps))
  X  <- Matrix::drop0(X, tol=sqrt(.Machine$double.eps))
  X1 <- Matrix::drop0(X1, tol=sqrt(.Machine$double.eps))
  L2.Q  <- Matrix::drop0(L2.Q, tol=sqrt(.Machine$double.eps))
  L3.Q  <- Matrix::drop0(L3.Q, tol=sqrt(.Machine$double.eps))
  L2.P  <- Matrix::drop0(L2.P, tol=sqrt(.Machine$double.eps))
  L3.P  <- Matrix::drop0(L3.P, tol=sqrt(.Machine$double.eps))


  # Build instruments --------------------------------------------------------------------------
  # Formula: independent of level
  #   GMM, p518: H_{1,GLS} = (Q(1)_gls V^(-0.5) X : P(1)_GLS V^(-0.5) X_1)

  HIV.FE_L2  <- L2.Q %*% (W %*% X)
  HIV.GMM_L2 <- cbind(L2.Q %*% (W %*% X), L2.P %*% (W %*% X1))

  HIV.FE_L3  <- L3.Q %*% (W %*% X)
  HIV.GMM_L3 <- cbind(L3.Q %*% (W %*% X), L3.P %*% (W %*% X1))

  HREE   <- W %*% X



  # Estimate GMM --------------------------------------------------------------------------------------
  n <- length(l.L3.X) # num schools
  res.gmm.HREE     <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HREE,       num.groups.highest.level = n)
  res.gmm.FE_L3    <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.FE_L3,  num.groups.highest.level = n)
  res.gmm.GMM_L3   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.GMM_L3, num.groups.highest.level = n)
  res.gmm.FE_L2    <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.FE_L2,  num.groups.highest.level = n)
  res.gmm.GMM_L2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.GMM_L2, num.groups.highest.level = n)

  # Omitted Variable ---------------------------------------------------------------------------------
  # To conduct the OVT correctly, the order of the IVs have to be,
  #   that IV1 is always the efficient estimator:
  #     IV1 is always FE when comparing it with REF and GMM;
  #     IV1 is FE_L2 when comparing it with FE_L3
  #     IV1 is GMM when comparing it with REF
  #     IV1 is GMM_L2 when comparing it with GMM_L3

  # HIVc1 vs HREE
  FE_L2_vs_REF <- multilevel_omittedvartest(IV1 = HIV.FE_L2, IV2 = HREE,
                                            res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.HREE,
                                            W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs1 vs HREE
  FE_L3_vs_REF <- multilevel_omittedvartest(IV1 = HIV.FE_L3, IV2 = HREE,
                                            res.gmm.IV1 = res.gmm.FE_L3, res.gmm.IV2 = res.gmm.HREE,
                                            W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HREE
  GMM_L2_vs_REF <- multilevel_omittedvartest(IV1 = HIV.GMM_L2, IV2 = HREE,
                                             res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.HREE,
                                             W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HREE
  GMM_L3_vs_REF <- multilevel_omittedvartest(IV1 = HIV.GMM_L3, IV2 = HREE,
                                              res.gmm.IV1 = res.gmm.GMM_L3, res.gmm.IV2 = res.gmm.HREE,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)

  # HIVc1 vs HIVs1
  FE_L2_vs_FE_L3 <- multilevel_omittedvartest(IV1 = HIV.FE_L2, IV2 = HIV.FE_L3,
                                              res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.FE_L3,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs2
  GMM_L2_vs_GMM_L3 <- multilevel_omittedvartest(IV1 = HIV.GMM_L2, IV2 = HIV.GMM_L3,
                                                res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.GMM_L3,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HIVc2
  FE_L2_vs_GMM_L2 <- multilevel_omittedvartest(IV1 = HIV.FE_L2, IV2 = HIV.GMM_L2,
                                                res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.GMM_L2,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HIVs1 - was missing in example code!
  FE_L3_vs_GMM_L3  <- multilevel_omittedvartest(IV1 = HIV.FE_L3, IV2 = HIV.GMM_L3,
                                                res.gmm.IV1 = res.gmm.FE_L3, res.gmm.IV2 = res.gmm.GMM_L3,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs1 - was missing in example code!
  FE_L3_vs_GMM_L2 <- multilevel_omittedvartest(IV1 = HIV.FE_L3, IV2 = HIV.GMM_L2,
                                               res.gmm.IV1 = res.gmm.FE_L3, res.gmm.IV2 = res.gmm.GMM_L2,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HIVs2
  FE_L2_vs_GMM_L3 <- multilevel_omittedvartest(IV1 = HIV.FE_L2, IV2 = HIV.GMM_L3,
                                               res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.GMM_L3,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)

  # Return --------------------------------------------------------------------------------------------

  return(new_rendo_multilevel(
            call = cl,
            formula = f.orig,
            num.levels = 3,
            l.group.size = list(L2 = setNames(length(l.L2.X), name.group.L2),
                                L3 = setNames(length(l.L3.X), name.group.L3)),
            dt.model.data = dt.model.data,
            V = V,
            W = W,
            # The list names determine the final naming of the coefs
            l.gmm = list(REF    = res.gmm.HREE,
                         FE_L2  = res.gmm.FE_L2,
                         FE_L3  = res.gmm.FE_L3,
                         GMM_L2 = res.gmm.GMM_L2,
                         GMM_L3 = res.gmm.GMM_L3),
            l.ovt = list(GMM_L2_vs_REF = GMM_L2_vs_REF,
                         GMM_L3_vs_REF = GMM_L3_vs_REF,
                         GMM_L2_vs_GMM_L3= GMM_L2_vs_GMM_L3,
                         FE_L2_vs_REF    = FE_L2_vs_REF,
                         FE_L3_vs_REF    = FE_L3_vs_REF,
                         FE_L2_vs_FE_L3  = FE_L2_vs_FE_L3,
                         FE_L2_vs_GMM_L2 = FE_L2_vs_GMM_L2,
                         FE_L2_vs_GMM_L3 = FE_L2_vs_GMM_L3,
                         FE_L3_vs_GMM_L3 = FE_L3_vs_GMM_L3,
                         FE_L3_vs_GMM_L2 = FE_L3_vs_GMM_L2),
            y = y,
            X = X))
}
