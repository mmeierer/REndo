#' @importFrom lme4 lFormula VarCorr lmer
#' @importFrom Matrix Diagonal crossprod bdiag
#' @importFrom corpcor pseudoinverse
#' @importFrom data.table as.data.table setkeyv
multilevel_2levels <- function(cl, f.orig, f.lmer.part, l4.form, data, name.endo){

  .SD <- .I <- NULL

  name.group.L2 <- names(l4.form$reTrms$flist)[[1]] # CID
  message("name.group.L2", name.group.L2)


  # Extract data ---------------------------------------------------------------

  # Make model variables to data.table to efficiently split into groups
  mm <- model.matrix(object = terms(l4.form$fr), data = l4.form$fr)
  dt.model.matrix <- data.table::as.data.table(mm, keep.rownames = TRUE)

  # to extract response (dv) which is not in model.matrix
  dt.model.frame  <- data.table::as.data.table(l4.form$fr, keep.rownames = TRUE)

  data.table::setkeyv(dt.model.matrix, cols = name.group.L2)
  data.table::setkeyv(dt.model.frame, cols = name.group.L2)

  name.y <- colnames(l4.form$fr)[[1L]] # always at first position, same as model.response reads out
  y   <- multilevel_colstomatrix(dt = dt.model.frame, name.cols = name.y)
  l.y <- multilevel_splittomatrix(dt = dt.model.frame, name.group = name.y,  name.by = name.group.L2)


  # Build X, X1 --------------------------------------------------------------------------------
  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, name.endo)

  l.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.group.L2)
  l.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.group.L2)
  X    <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X)
  X1   <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X1)


  # Build Z2 ------------------------------------------------------------------------------------

  names.Z2 <- l4.form$reTrms$cnms[[1]]
  l.Z2 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z2, name.by = name.group.L2)
  message("num.Z2 groups", length(l.Z2))

  # Fit REML -----------------------------------------------------------------------------------

  VC       <- lme4::VarCorr(lme4::lmer(formula=f.lmer.part, data=data))
  D.2      <- VC[[name.group.L2]]
  sigma.sq <- attr(VC, "sc")

  # ensure sorting of D cols is same as Z columns
  D.2 <- D.2[names.Z2, names.Z2]
  if(!all(colnames(D.2) == colnames(l.Z2[[1]])))
    stop("D.2 is wrongly sorted!")

  # Calc V -------------------------------------------------------------------------------------
  # V = blkdiag(V1, . . . , Vn)
  # Calculate per school level

  l.V <- lapply(l.Z2, FUN = function(g.z2){
    Matrix::Diagonal(sigma.sq, n=nrow(g.z2)) + g.z2 %*% D.2 %*% t(g.z2)
  })

  # Needed as vcov matrix
  V <- Matrix::bdiag(l.V)
  # **add col/rownames?

  # Calc W -------------------------------------------------------------------------------------
  # W = V^(-1/2)
  # Do eigen decomp on each block
  l.W <- lapply(l.V, function(g.v){
    ei                     <- eigen(g.v)
    ei$values[ei$values<0] <- 0
    sValS                  <- 1/sqrt(ei$values)
    sValS[ei$values==0]    <- 0
    ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))
  })

  W <- Matrix::bdiag(l.W)

  # Calc Q -------------------------------------------------------------------------------------
  l.Q <- mapply(l.Z2, l.W, FUN = function(g.z2, g.w){
    Matrix::Diagonal(x=1, n=nrow(g.z2)) - g.w %*% g.z2 %*%
      corpcor::pseudoinverse(Matrix::crossprod(g.z2, g.w) %*% g.w %*% g.z2) %*%
      Matrix::crossprod(g.z2, g.w)
  })

  Q <- Matrix::bdiag(l.Q)

  # Calc P -------------------------------------------------------------------------------------
  P <- Matrix::Diagonal(x=1, n=nrow(data)) - Q


  print(paste0("X done:", class(X)))
  print(paste0("X1 done:", class(X1)))
  print(paste0("Q done:", class(Q)))
  print(paste0("P done:", class(P)))
  print(paste0("W done:", class(W)))

  # Build instruments --------------------------------------------------------------------------

  HIV.s1 <- Q %*% (W%*%(X))

  # End page 517:
  # H_1sgls = (Q(1)_sglsV^-0.5X_s:P(1)_sglsV^-0.5X_1s)
  #   where Q(1)_sgls = I_sc-P(V^0.5_sZ_2sc)
  #   and   P(1)_sgls = I_sc-Q(1)_sgls
  #   ** what is P(1)_gls ??

  # 2=GMM
  HIV.s2 <- cbind(Q %*% W%*%X, P%*%W%*%X1)

  #
  HREE   <- W %*% X

  # ** drop0, tol = sqrt(machine)
  # HIV.s1 <- Matrix::drop0(x=)

  print(paste0("HIV.s1 done:", class(HIV.s1)))
  print(paste0("HIV.s2 done:", class(HIV.s2)))
  print(paste0("HREE done:", class(HREE)))


  # Estimate GMMs for IVs ------------------------------------------------------------------------------

  res.gmm.s1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s1)
  res.gmm.s2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s2)
  res.gmm.HREE <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HREE)

  # Omitted Var tests ----------------------------------------------------------------------------------

  FE_L2_vs_REF   <-  multilevel_ommitedvartest(IV1 = HREE, IV2=HIV.s1,
                                             res.gmm.IV1 = res.gmm.HREE,
                                             res.gmm.IV2 = res.gmm.s1,
                                             W=W, l.Lhighest.X = l.X, l.Lhighest.y = l.y)
  GMM_L2_vs_REF  <-  multilevel_ommitedvartest(IV1 = HIV.s2, IV2=HREE,
                                             res.gmm.IV1 = res.gmm.s2,
                                             res.gmm.IV2 = res.gmm.HREE,
                                             W=W, l.Lhighest.X = l.X, l.Lhighest.y = l.y)
  FE_L2_vs_GMM_L2  <-  multilevel_ommitedvartest(IV1 = HIV.s1, IV2 = HIV.s2,
                                             res.gmm.IV1 = res.gmm.s1,
                                             res.gmm.IV2 = res.gmm.s2,
                                             W=W, l.Lhighest.X = l.X, l.Lhighest.y = l.y)

  # Return --------------------------------------------------------------------------------

  return(new_rendo_multilevel(
              call = cl,
              formula = f.orig,
              num.levels = 2,
              dt.mf = dt.model.frame,
              dt.mm = dt.model.matrix,
              V = V,
              W = W,
              # The list names determine the final naming of the coefs
              l.gmm = list(FE_L2  = res.gmm.s1,
                           GMM_L2 = res.gmm.s2,
                           REF  = res.gmm.HREE),
              l.ovt = list(FE_L2_vs_REF  = FE_L2_vs_REF,
                           GMM_L2_vs_REF  = GMM_L2_vs_REF,
                           FE_L2_vs_GMM_L2 = FE_L2_vs_GMM_L2),
              y = y,
              X = X))
}


  # LEAVE FOLLOWING COMMENTS FOR UNDERSTANDING =============================================
  #
  # coefs
  # SE
  # vcovM
  # W
  # call
  # model
  # ommitedvartest
  #
  #
  #
  #
  #
  # # Build X, X1 ----------------------------------------------------------
  # # Do not use the X component present in l4.form as their ordering is
  # #   somewhat obscure.
  # # X1 is everything but endogenous
  #
  #
  #
  # # Build Z2 -------------------------------------------------------------
  # # Only extract names from l4.form and build Z self because of unknown ordering
  #
  # # Calc V ---------------------------------------------------------------
  # # V=blkdiag(V1,..,Vn)
  # # V=blkdiag(V_s) (18) Var ∂s=Vs
  #
  # # p 515: V_s = R_s + Z_2sVar(€(2)_s)Z'_2s+Z_3sVar(€_s(3))Z'_3s
  # # -> For L2 case drop the Z_3 part
  #
  #
  # # sigma.sq, D.2
  # #  get D.2 and random error from VarCor
  #
  # # **Wrong**
  # V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.data)) + Z2 %*% D.2 %*% t(Z2)
  #
  #
  # # Calc W ---------------------------------------------------------------
  # # W = V^(-1/2)
  # # W = the weight can be the inverse of the square root of the variance–covariance matrix of the disturbance term
  # # https://scicomp.stackexchange.com/questions/10375/efficient-computation-of-the-matrix-square-root-inverse
  # # "In many cases you can instead use a Cholesky factor of the inverse of the covariance matrix
  # #   (or practically the same, the Cholesky factor of the covariance matrix itself.)"
  # # Maybe also do block-wise??
  #
  #
  # # inv(sqrtm(BDIAG)) is same as applied on each block but faster and can create a sparse (dgCMatrix) matrix
  #
  # # Calc Q, P, -----------------------------------------------------------
  # # ** Formula for Q??
  # #
  # # Q: **Which Q should be built? Q(2) or Q(1) ??
  # # between(10-11):Q_s(2)=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  # # (11): Q(2)=blkdiag(Q(2)_s)
  # # Q(1)_1sQ(2)_1s = Q(1)_1s -> Projection matrices are nested
  # # Q(2) = blkdiag(Q(2)_s)
  # # -> Q(1) = blkdiag(Q(1)_sc) (p.512)
  # # -> Base on (8): Q(1)_sc=I_sc-P(Z_2sc)
  # #
  # # P:
  # # (8): Q(1)_sc=I_sc-P(Z_2sc) -?> P(Z_2sc) = Isc-Q_sc?
  # # End page 512: P(1)_sc=I_sc-Q(1)_sc
  # # (12):P(1)_sc=I_sc-Q(1)_sc
  # # Where P(1)=blkdiag(P(1)_sc)
  #
  # # ** Which level are we trying to figure out?
  # # (1) = level-1 model
  # #   (1)_sc = variability at child level
  # #
  # # (2) = level-2 model
  # #   Z(2)_sc, X(2)_sc=child or school and do not vary over time
  # #
  # # (3) = level-3 model = variability at school level
  # #   X(3)_s = may depend on school
  # #
  # # Define:
  # #   Z_2sct = Z(1)_sct
  # #   Z_3sct = Z(1)_sctZ(2)_sc
  # #   Z_3s = stacked Z_3sct
  # #
  # #
  # #   X_sct = (X(1)_sct:Z_2sct)
  # #
  # #   X(1)=stacked X(1)_sc
  # #
  # #
  #
  #
  # # Calc HIV1s1, HIVs2, HREE ---------------------------------------------
  # # original code for the L2 case only HIVs12, line 318ff
  # # "Consequently, the instruments H consist of QX and PX1" (p.516)
  # # H_2s=(Q(2)_sX_s : P(2)_sX2s)
  #
  # # "As noted above, bGMM equals bRE when X = X1, where all variables are assumed to be exogenous.
  # #   At the opposite end, where X1 is empty, we obtain bFE by using only the QX part of the instruments (see Section 3.1).
  # #   Therefore, X1 can be as small as empty and as large as X. In between, we obtain different bGMM’s for different sets of X1"
  #
  # # 1=FIXED EFFECT
  # # c1 but same formula for s1
  # # ** Where does the W come from then if "by using only the QX part of the instruments" ??
  # HIV.s1 <- Q %*% (W%*%(X))
  #
  # # End page 517:
  # # H_1sgls = (Q(1)_sglsV^-0.5X_s:P(1)_sglsV^-0.5X_1s)
  # #   where Q(1)_sgls = I_sc-P(V^0.5_sZ_2sc)
  # #   and   P(1)_sgls = I_sc-Q(1)_sgls
  # #   ** what is P(1)_gls ??
  #
  # # 2=GMM
  # HIV.s2 <- cbind(Q %*% W%*%X, P%*%W%*%X1)
  #
  #
  # # For L3 case:
  # # Next, we define X3,s to be those columns belonging to X that are exogenous to the transformed disturbance terms
  # # in the sense that E X′3,s Q(2) δ = 0. We use the set of instruments H3 = (Q(1)X : P(1)P(2)X3).
  #
  #
  # # ??? Comments say s1,s2 are for level 3 case??
  # # HIVc1 <- Interim$HIVc1    # fixed effects level 2
  # # HIVc2 <- Interim$HIVc2    # GMM level 2
  # #
  # # HIVs1 <- Interim$HIVs1    # if 3 levels model - fixed effects level 3/ if 2 levels model - fixed effect slevel 2
  # # HIVs2 <- Interim$HIVs2    # if 3 levels model - GMM level 3/ if 2 levels - GMM level 2
  #
  #
