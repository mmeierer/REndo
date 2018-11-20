#' @importFrom Matrix bdiag Matrix Diagonal crossprod tcrossprod drop0 nnzero
#' @importFrom data.table as.data.table setkeyv
#' @importFrom lme4 lFormula lmer VarCorr
#' @importFrom corpcor pseudoinverse
multilevel_3levels <- function(cl, f.orig, f.lmer.part, l4.form, data, name.endo, verbose){

  .SD <- .I <- NULL

  num.levels <- length(l4.form$reTrms$flist) + 1

  name.group.L2    <- names(l4.form$reTrms$flist)[[1]] # CID
  name.group.L3    <- names(l4.form$reTrms$flist)[[2]] # SID
  # Splitting names: What is used in DT "by" to form groups
  name.split.by.L3 <- name.group.L3
  name.split.by.L2 <- c(name.group.L3, name.group.L2)


  # Extract data ---------------------------------------------------------------

  # Make model variables to data.table to efficiently split into groups
  mm <- model.matrix(object = terms(l4.form$fr), data = l4.form$fr)
  dt.model.matrix <- data.table::as.data.table(mm, keep.rownames=TRUE)
  # to extract response which is not in model.matrix
  dt.model.frame  <- data.table::as.data.table(l4.form$fr, keep.rownames=TRUE)
  data.table::setkeyv(dt.model.matrix, cols = name.split.by.L2)
  data.table::setkeyv(dt.model.frame,  cols = name.split.by.L2)

  # Build y ------------------------------------------------------------------------------------
  # Only needed for calculating residuals in ommitted var test
  name.y <- colnames(l4.form$fr)[[1L]] # always at first position, same as model.response reads out
  y      <- multilevel_colstomatrix(dt = dt.model.frame, name.cols = name.y)
  l.L3.y <- multilevel_splittomatrix(dt = dt.model.frame, name.group = name.y, name.by = name.split.by.L3)


  # Build X, X1 --------------------------------------------------------------------------------
  # Do not use the X component present in l4.form as their ordering is somewhat obscure.
  # X1 is everything but endogenous

  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, name.endo)

  l.L2.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.split.by.L2)
  l.L2.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.split.by.L2)
  l.L3.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.split.by.L3)
  l.L3.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.split.by.L3)
  X    <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X)
  X1   <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X1)

  if(verbose){
    message("Detected multilevel model with 3 levels.")
    message("For ", name.group.L2, " (Level 2), ", length(l.L2.X), " groups were found")
    message("For ", name.group.L3, " (Level 3), ", length(l.L3.X), " groups were found")
  }

  # Build Z2, Z3 --------------------------------------------------------------------------------
  # Only extract names from l4.form and build Z self because of unknown ordering

  names.Z2 <- l4.form$reTrms$cnms[[1]]
  names.Z3 <- l4.form$reTrms$cnms[[2]]
  l.L2.Z2 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z2, name.by = name.split.by.L2)
  l.L3.Z3 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z3, name.by = name.split.by.L3)
  Z2   <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.Z2)
  Z3   <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.Z3)

  # Sorting verification ------------------------------------------------------------
  # Before doing any math, verify that each group contains the same observations in the same order

  fct.check.all.same.names <- function(...){
    l.args <- list(...)
    fct.check <- function(...){
      l.args <- list(...)
      l.args.rn <- lapply(l.args, rownames)
      return(all(sapply(l.args.rn[-1], FUN = identical, l.args.rn[[1]])))
    }
    stopifnot(all(do.call(what = mapply, c(list(FUN=fct.check), l.args))))
  }

  # Every L2 and L3 group has the same obs in exact same order
  fct.check.all.same.names(l.L2.Z2, l.L2.X, l.L2.X1)
  fct.check.all.same.names(l.L3.Z3, l.L3.X, l.L3.X1)

  # Fit REML -----------------------------------------------------------------------------------
  # get D.2, D.3 and random error from VarCor
  VC       <- lme4::VarCorr(lme4::lmer(formula=f.lmer.part, data=data, REML = TRUE))
  D.2      <- VC[[name.group.L2]]
  D.3      <- VC[[name.group.L3]]
  sigma.sq <- attr(VC, "sc")

  # ensure sorting of D cols is same as Z columns
  D.2 <- D.2[names.Z2, names.Z2]
  D.3 <- D.3[names.Z3, names.Z3]
  if(!all(colnames(D.2) == colnames(l.L2.Z2[[1]])) ||
     !all(rownames(D.2) == colnames(l.L2.Z2[[1]])))
    stop("D.2 is wrongly sorted!")
  if(!all(colnames(D.3) == colnames(l.L3.Z3[[1]])) ||
     !all(rownames(D.3) == colnames(l.L3.Z3[[1]])))
    stop("D.3 is wrongly sorted!")

  # Calc V -------------------------------------------------------------------------------------
  # TODO ** L2 or L3 ** add formula from paper
  # Relevant Formula:
  #   p 515: V_s = R_s + Z_2sVar(err(2)_s)Z'_2s+Z_3sVar(err_s(3))Z'_3s
  #          -> For L2 case drop the Z_3 part
  # Structure:
  # (18) V=blkdiag(V_s) and V = blkdiag(V1, . . . , Vn)

  l.L2.V.part <- lapply(l.L2.Z2, FUN = function(g.z2){
    g.z2 %*% D.2 %*% t(g.z2)
  })

  l.L3.V.part <- lapply(l.L3.Z3, FUN = function(g.z3){
    g.z3 %*% D.3 %*% t(g.z3)
  })

  # Needed as vcov matrix
  V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.model.frame)) +
            Matrix::bdiag(l.L3.V.part) +
            Matrix::bdiag(l.L2.V.part)
  rownames(V) <- colnames(V) <- dt.model.matrix$rn


  # Calc W -------------------------------------------------------------------------------------
  # Formula:
  #   p.510: "the weight can be the inverse of the square root of the variance–covariance matrix of the disturbance term"
  #   W = V_{s}^(-1/2)
  # **TODO: Is this actually meant to be always at a per school level and not whole V ?? **
  # Do eigen decomp on each block. Has to be L3 as L2 would omits non-zero vars

  g.L3.idx <- dt.model.matrix[, list(g.idx=list(.I)), by=name.split.by.L3]$g.idx
  l.L3.V   <- lapply(g.L3.idx, function(g.id) {V[g.id, g.id, drop=FALSE]})

  # Do eigen decomp on each block
  l.L3.W <- lapply(l.L3.V, function(g.v){
    ei                     <- eigen(g.v)
    ei$values[ei$values<0] <- 0
    sValS                  <- 1/sqrt(ei$values)
    sValS[ei$values==0]    <- 0
    ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))})


  W <- Matrix::bdiag(l.L3.W)
  rownames(W) <- colnames(W) <- dt.model.matrix$rn


  # Calc Q -------------------------------------------------------------------------------------
  # Q has to be caluclated at a L3 and L2 level for building the instruments
  # Formula:
  #   L3: between(10-11):
  #         Q(2)_s=I_s-P(Z_3s), Z_3s=stacked Z_3sct
  #         where P(H) = H(H'H)^(-1)H' (p.510)
  #
  #   L2: ?
  #
  # Structure:
  #   L3: p.512 (11)
  #       Q(2)=blkdiag(Q(2)_s)
  #   L2: ?


  # . Q at L3 level ------------------------------------------------------------------------------------

  # ** Exists already but rownames wrong. worth splitting again?
  l.L3.W   <- lapply(g.L3.idx, function(g.id) {W[g.id, g.id, drop=FALSE]})
  fct.check.all.same.names(l.L3.Z3, l.L3.W)
  fct.check.all.same.names(l.L3.V, l.L3.W)

  # Q at L3 only (according to raluca's code, no reference to L2 at all)
  # Ql3 <- diag(CT) - Wl3 %*% Zl3 %*% corpcor::pseudoinverse(crossprod(Zl3, Wl3) %*% Wl3 %*% Zl3) %*% crossprod(Zl3, Wl3)
  l.L3.Q <- mapply(l.L3.Z3, l.L3.W, FUN = function(g.z3, g.w3){
    Matrix::Diagonal(x=1, n=nrow(g.z3)) - g.w3 %*% g.z3 %*%
      corpcor::pseudoinverse(Matrix::crossprod(g.z3, g.w3) %*% g.w3 %*% g.z3) %*%
      Matrix::crossprod(g.z3, g.w3)
  })
  L3.Q <- Matrix::bdiag(l.L3.Q)

  L3.Q.simple <- Matrix::Diagonal(x=1, n=nrow(Z3)) - W %*% Matrix::bdiag(l.L3.Z3) %*%
    corpcor::pseudoinverse(Matrix::crossprod(Matrix::bdiag(l.L3.Z3), W) %*% W %*% Matrix::bdiag(l.L3.Z3)) %*%
    Matrix::crossprod(Matrix::bdiag(l.L3.Z3), W)
  # print(all.equal(L3.Q,L3.Q.simple)) # TRUE

  # NO!
  # Q.naive <- Matrix::Diagonal(x=1, n=nrow(Z3)) - W %*% Z3 %*%
  #   corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*%
  #   Matrix::crossprod(Z3, W)

  # . Q at L2 level ------------------------------------------------------------------------------------

  # Split into L2 groups
  g.L2.idx <- dt.model.matrix[, list(g.idx=list(.I)), by=name.split.by.L2]$g.idx
  l.L2.W <- lapply(g.L2.idx, function(g.id) {W[g.id, g.id, drop=FALSE]})

  # Q at L2 only (according to raluca's code, no reference to L3 at all)
  l.L2.Q <- mapply(l.L2.Z2, l.L2.W, FUN = function(g.z2, g.w2){
    Matrix::Diagonal(x=1, n=nrow(g.z2)) - g.w2 %*% g.z2 %*%
      corpcor::pseudoinverse(t(g.z2)%*%(g.w2%*%g.w2)%*%g.z2) %*% Matrix::crossprod(g.z2, g.w2)
  })
  L2.Q <- Matrix::bdiag(l.L2.Q)

  # Move the diagonal outside as the blocks are all square and therefore the diagnoal is the same
  l.L2.Q.out <- mapply(l.L2.Z2, l.L2.W, FUN = function(g.z2, g.w2){
    g.w2 %*% g.z2 %*%
      corpcor::pseudoinverse(t(g.z2)%*%(g.w2%*%g.w2)%*%g.z2) %*% Matrix::crossprod(g.z2, g.w2)
  })
  L2.Q.out <- Matrix::Diagonal(x=1, n=nrow(L2.Q)) - Matrix::bdiag(l.L2.Q.out)
  # print(all.equal(Matrix::drop0(L2.Q.out, tol=1e-15), Matrix::drop0(L2.Q, tol = 1e-15))) # TRUE

  # too slow...? and wrong size
  # L2.Q.simple <- Matrix::Diagonal(x=1, n=nrow(Z2)) - W %*% Matrix::bdiag(l.L2.Z2) %*%
  #     corpcor::pseudoinverse(t(Matrix::bdiag(l.L2.Z2))%*%(W%*%W)%*%
  #                              Matrix::bdiag(l.L2.Z2)) %*% Matrix::crossprod(Matrix::bdiag(l.L2.Z2), W)
  # print(all.equal(L2.Q, L2.Q.simple))

  # Also wrong...??
  # L2.Q.naive <- Matrix::Diagonal(x=1, n=nrow(Z2)) - W %*% Z2 %*%
  #     corpcor::pseudoinverse(t(Z2)%*%(W%*%W)%*%Z2) %*% Matrix::crossprod(Z2, W)
  # print(all.equal(L2.Q, L2.Q.naive))


  # Calc P -------------------------------------------------------------------------------------
  # Formula:
  #   L3:
  #   L2:
  # Structure:
  #   L3:
  #   L2:

  L2.P <- Matrix::Diagonal(x=1, n=nrow(data)) - L2.Q
  L3.P <- Matrix::Diagonal(x=1, n=nrow(data)) - L3.Q

  # Drop near zero values ----------------------------------------------------------------------
  # Very small values (ca 0) can cause troubles during inverse caluclation
  #   remove, tolerance same as zapsmall()

  W  <- Matrix::drop0(W, tol=sqrt(.Machine$double.eps))
  X  <- Matrix::drop0(X, tol=sqrt(.Machine$double.eps))
  X1 <- Matrix::drop0(X1, tol=sqrt(.Machine$double.eps))
  L2.Q  <- Matrix::drop0(L2.Q, tol=sqrt(.Machine$double.eps))
  L3.Q  <- Matrix::drop0(L3.Q, tol=sqrt(.Machine$double.eps))
  L2.P  <- Matrix::drop0(L2.P, tol=sqrt(.Machine$double.eps))
  L3.P  <- Matrix::drop0(L3.P, tol=sqrt(.Machine$double.eps))


  # Build instruments --------------------------------------------------------------------------
  # Formula: independent of level (** RALUCA: Double-check these)
  #   HREE,p?: ?
  #   FE,  p?: ?
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

  # Ommitted Variable ---------------------------------------------------------------------------------
  # HIVc1 vs HREE
  FE_L2_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.FE_L2, IV2 = HREE,
                                            res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.HREE,
                                            W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs1 vs HREE
  FE_L3_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.FE_L3, IV2 = HREE,
                                            res.gmm.IV1 = res.gmm.FE_L3, res.gmm.IV2 = res.gmm.HREE,
                                            W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HREE
  GMM_L2_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.GMM_L2, IV2 = HREE,
                                             res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.HREE,
                                             W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HREE
  GMM_L3_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.GMM_L3, IV2 = HREE,
                                              res.gmm.IV1 = res.gmm.GMM_L3, res.gmm.IV2 = res.gmm.HREE,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HREE, id=2 ** double but different id ??

  # HIVc1 vs HIVs1
  FE_L2_vs_FE_L3 <- multilevel_ommitedvartest(IV1 = HIV.FE_L2, IV2 = HIV.FE_L3,
                                              res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.FE_L3,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs2
  GMM_L2_vs_GMM_L3 <- multilevel_ommitedvartest(IV1 = HIV.GMM_L2, IV2 = HIV.GMM_L3,
                                                res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.GMM_L3,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVc1
  GMM_L2_vs_FE_L2 <- multilevel_ommitedvartest(IV1 = HIV.GMM_L2, IV2 = HIV.FE_L2,
                                                res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.FE_L2,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HIVs1 ** missing?!
  GMM_L3_vs_FE_L3  <- multilevel_ommitedvartest(IV1 = HIV.GMM_L3, IV2 = HIV.FE_L3,
                                                res.gmm.IV1 = res.gmm.GMM_L3, res.gmm.IV2 = res.gmm.FE_L3,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs1
  GMM_L2_vs_FE_L3 <- multilevel_ommitedvartest(IV1 = HIV.GMM_L2, IV2 = HIV.FE_L3,
                                               res.gmm.IV1 = res.gmm.GMM_L2, res.gmm.IV2 = res.gmm.FE_L3,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HIVs2
  FE_L2_vs_GMM_L3 <- multilevel_ommitedvartest(IV1 = HIV.FE_L2, IV2 = HIV.GMM_L3,
                                               res.gmm.IV1 = res.gmm.FE_L2, res.gmm.IV2 = res.gmm.GMM_L3,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # phtest in plmer
  return(new_rendo_multilevel(
            call = cl,
            formula = f.orig,
            num.levels = 3,
            dt.mf = dt.model.frame,
            dt.mm = dt.model.matrix,
            V = V,
            W = W,
            # The list names determine the final naming of the coefs
            l.gmm = list(REF = res.gmm.HREE,
                         FE_L2 = res.gmm.FE_L2,
                         FE_L3 = res.gmm.FE_L3,
                         GMM_L2 = res.gmm.GMM_L2,
                         GMM_L3 = res.gmm.GMM_L3),
            l.ovt = list(FE_L2_vs_REF   = FE_L2_vs_REF,
                         FE_L3_vs_REF   = FE_L3_vs_REF,
                         GMM_L2_vs_REF = GMM_L2_vs_REF,
                         GMM_L3_vs_REF = GMM_L3_vs_REF,
                         FE_L2_vs_FE_L3 = FE_L2_vs_FE_L3,
                         GMM_L2_vs_GMM_L3 = GMM_L2_vs_GMM_L3,
                         GMM_L2_vs_FE_L2   = GMM_L2_vs_FE_L2,
                         GMM_L3_vs_FE_L3   = GMM_L3_vs_FE_L3,
                         GMM_L2_vs_FE_L3  = GMM_L2_vs_FE_L3,
                         FE_L2_vs_GMM_L3 = FE_L2_vs_GMM_L3),
            y = y,
            X = X))
}
