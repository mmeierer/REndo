#' @importFrom Matrix bdiag Matrix Diagonal crossprod tcrossprod drop0 nnzero
#' @importFrom data.table as.data.table setkeyv
#' @importFrom lme4 lFormula lmer VarCorr
#' @importFrom corpcor pseudoinverse
multilevel_3levels <- function(cl, f.orig, f.lmer.part, l4.form, data, name.endo){

  .SD <- .I <- NULL

  num.levels <- length(l4.form$reTrms$flist) + 1

  name.group.L2    <- names(l4.form$reTrms$flist)[[1]] # CID
  name.group.L3    <- names(l4.form$reTrms$flist)[[2]] # SID
  name.split.by.L3 <- name.group.L3
  name.split.by.L2 <- c(name.group.L3, name.group.L2)
  message("name.split.by.L3", name.split.by.L3)
  message("name.split.by.L2", name.split.by.L2)


  # Extract data ---------------------------------------------------------------

  # Make model variables to data.table to efficiently split into groups
  mm <- model.matrix(object = terms(l4.form$fr), data = l4.form$fr)
  dt.model.matrix <- data.table::as.data.table(mm, keep.rownames=TRUE)
  # to extract response which is not in model.matrix
  dt.model.frame  <- data.table::as.data.table(l4.form$fr, keep.rownames=TRUE)
  data.table::setkeyv(dt.model.matrix, cols = name.split.by.L2)
  data.table::setkeyv(dt.model.frame,  cols = name.split.by.L2)

  name.y <- colnames(l4.form$fr)[[1L]] # always at first position, same as model.response reads out
  y      <- multilevel_colstomatrix(dt = dt.model.frame, name.cols = name.y)
  l.L3.y <- multilevel_splittomatrix(dt = dt.model.frame, name.group = name.y, name.by = name.split.by.L3)


  # Build X, X1 --------------------------------------------------------------------------------
  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, name.endo)

  l.L2.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.split.by.L2)
  l.L2.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.split.by.L2)
  l.L3.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.split.by.L3)
  l.L3.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.split.by.L3)
  X    <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X)
  X1   <- multilevel_colstomatrix(dt = dt.model.matrix, name.cols = names.X1)

  # Build Z2, Z3 --------------------------------------------------------------------------------

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
  VC       <- lme4::VarCorr(lme4::lmer(formula=f.lmer.part, data=data))
  D.2      <- VC[[name.group.L2]]
  D.3      <- VC[[name.group.L3]]
  sigma.sq <- attr(VC, "sc")

  # ensure sorting of D cols is same as Z columns
  D.2 <- D.2[names.Z2, names.Z2]
  D.3 <- D.2[names.Z3, names.Z3]
  if(!all(colnames(D.2) == colnames(l.L2.Z2[[1]])) ||
     !all(rownames(D.2) == colnames(l.L2.Z2[[1]])))
    stop("D.2 is wrongly sorted!")
  if(!all(colnames(D.3) == colnames(l.L3.Z3[[1]])) ||
     !all(rownames(D.3) == colnames(l.L3.Z3[[1]])))
    stop("D.3 is wrongly sorted!")

  # Calc V -------------------------------------------------------------------------------------
  # V = blkdiag(V1, . . . , Vn)
  # Calculate per school level
  # TODO ** L2 or L3

  l.L2.V.part <- lapply(l.L2.Z2, FUN = function(g.z2){
    g.z2 %*% D.2 %*% t(g.z2)
  })

  l.L3.V.part <- lapply(l.L3.Z3, FUN = function(g.z3){
    g.z3 %*% D.3 %*% t(g.z3)
  })

  # Needed as vcov matrix
  # **add col/rownames?
  V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.model.frame)) +
            Matrix::bdiag(l.L3.V.part) +
            Matrix::bdiag(l.L2.V.part)
  rownames(V) <- colnames(V) <- dt.model.matrix$rn

  # print(Matrix::image(Matrix::bdiag(l.L3.V.part)))
  # print(Matrix::image(Matrix::bdiag(l.L2.V.part)))
  # print(Matrix::image((V)))

  # Calc W -------------------------------------------------------------------------------------
  # W = V^(-1/2)
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
  # print(Matrix::image((W)))


  # Calc Q -------------------------------------------------------------------------------------
  # between(10-11):
  #   Q(2)_s=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  #   (11): Q(2)=blkdiag(Q(2)_s)

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
  print(all.equal(L3.Q,L3.Q.simple)) # TRUE

  # NO!
  # Q.naive <- Matrix::Diagonal(x=1, n=nrow(Z3)) - W %*% Z3 %*%
  #   corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*%
  #   Matrix::crossprod(Z3, W)

  # Split into L2 groups
  g.L2.idx <- dt.model.matrix[, list(g.idx=list(.I)), by=name.split.by.L2]$g.idx
  l.L2.W <- lapply(g.L2.idx, function(g.id) {W[g.id, g.id, drop=FALSE]})

  # *** Q formulas for L2 and L3 are different?? ***
  # Q at L2 only (according to raluca's code, no reference to L3 at all)
  # Ql2 <- diag(Tl2) - Wl2 %*% Zl2 %*% (case with > 1 obs)
  #   corpcor::pseudoinverse(t(Zl2) %*% (Wl2 %*% Wl2) %*% Zl2) %*% crossprod(Zl2,Wl2)
  l.L2.Q <- mapply(l.L2.Z2, l.L2.W, FUN = function(g.z2, g.w2){
    Matrix::Diagonal(x=1, n=nrow(g.z2)) - g.w2 %*% g.z2 %*%
      corpcor::pseudoinverse(t(g.z2)%*%(g.w2%*%g.w2)%*%g.z2) %*% Matrix::crossprod(g.z2, g.w2)
  })
  L2.Q <- Matrix::bdiag(l.L2.Q)

  l.L2.Q.out <- mapply(l.L2.Z2, l.L2.W, FUN = function(g.z2, g.w2){
    g.w2 %*% g.z2 %*%
      corpcor::pseudoinverse(t(g.z2)%*%(g.w2%*%g.w2)%*%g.z2) %*% Matrix::crossprod(g.z2, g.w2)
  })
  L2.Q.out <- Matrix::bdiag(l.L2.Q.out)
  L2.Q.out <- Matrix::Diagonal(x=1, n=nrow(L2.Q.out)) - L2.Q.out

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

  L2.P <- Matrix::Diagonal(x=1, n=nrow(data)) - L2.Q
  L3.P <- Matrix::Diagonal(x=1, n=nrow(data)) - L3.Q

  # Build instruments --------------------------------------------------------------------------
  # HIVs1[sloc,] <- Ql3 %*% (Wl3 %*% Xl3)
  # HIVs2[sloc,] <- cbind(Ql3 %*% (Wl3 %*% Xl3), Pl3 %*% (Wl3 %*% X1l3))

  # Wl2 is W split on L2 level
  # Pl2 <- diag(Tl2)- Ql2
  # HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)
  # HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))

  fct.drop.near.zeros <- function(M){
    nnz.before <- Matrix::nnzero(M)
    M <- Matrix::drop0(M, tol=1e-15)
    nnz.after <- Matrix::nnzero(M)
    print(paste0("perc zeros dropped:",(nnz.before-nnz.after)/nnz.before))
    return(M)
  }

  W <- fct.drop.near.zeros(W)
  X <- fct.drop.near.zeros(X)
  X1 <- fct.drop.near.zeros(X1)
  L2.Q <- fct.drop.near.zeros(L2.Q)
  L3.Q <- fct.drop.near.zeros(L3.Q)
  L2.P <- fct.drop.near.zeros(L2.P)
  L3.P <- fct.drop.near.zeros(L3.P)

  HIV.c1 <- L2.Q %*% (W %*% X)
  HIV.c2 <- cbind(L2.Q %*% (W %*% X), L2.P %*% (W %*% X1))

  HIV.s1 <- L3.Q %*% (W %*% X)
  HIV.s2 <- cbind(L3.Q %*% (W %*% X), L3.P %*% (W %*% X1))

  HREE   <- W %*% X

  # print(paste0("W: ", class(W)))
  # print(paste0("V: ", class(V)))
  # print(paste0("X: ", class(X)))
  # print(paste0("X1: ", class(X1)))
  # print(paste0("L2.Q: ", class(L2.Q)))
  # print(paste0("L3.Q: ", class(L3.Q)))
  # print(paste0("L2.P: ", class(L2.P)))
  # print(paste0("L3.P: ", class(L3.P)))
  # print(paste0("HIV.c1: ", class(HIV.c1)))
  # print(paste0("HIV.c2: ", class(HIV.c2)))
  # print(paste0("HIV.s1: ", class(HIV.s1)))
  # print(paste0("HIV.s2: ", class(HIV.s2)))
  # print(paste0("HREE: ", class(HREE)))

  # Estimate GMM --------------------------------------------------------------------------------------

  res.gmm.HREE <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HREE)
  res.gmm.s1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s1)
  res.gmm.s2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s2)
  res.gmm.c1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.c1)
  res.gmm.c2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.c2)

  # Ommitted Variable ---------------------------------------------------------------------------------
  # HIVc1 vs HREE, id=1
  FE_L2_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.c1, IV2 = HREE,
                                           res.gmm.IV1 = res.gmm.c1, res.gmm.IV2 = res.gmm.HREE,
                                           W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs1 vs HREE, id=2
  FE_L3_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.s1, IV2 = HREE,
                                            res.gmm.IV1 = res.gmm.s1, res.gmm.IV2 = res.gmm.HREE,
                                            W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HREE, id=1
  GMM_L2_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.c2, IV2 = HREE,
                                              res.gmm.IV1 = res.gmm.c2, res.gmm.IV2 = res.gmm.HREE,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HREE, id=2
  GMM_L3_vs_REF <- multilevel_ommitedvartest(IV1 = HIV.s2, IV2 = HREE,
                                              res.gmm.IV1 = res.gmm.s2, res.gmm.IV2 = res.gmm.HREE,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HREE, id=2 ** double but different id ??

  # HIVc1 vs HIVs1, id=1
  FE_L2_vs_FE_L3 <- multilevel_ommitedvartest(IV1 = HIV.c1, IV2 = HIV.s1,
                                              res.gmm.IV1 = res.gmm.c1, res.gmm.IV2 = res.gmm.s1,
                                              W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs2, id=1
  GMM_L2_vs_GMM_L3 <- multilevel_ommitedvartest(IV1 = HIV.c2, IV2 = HIV.s2,
                                                res.gmm.IV1 = res.gmm.c2, res.gmm.IV2 = res.gmm.s2,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVc1, id=1
  GMM_L2_vs_FE_L2 <- multilevel_ommitedvartest(IV1 = HIV.c2, IV2 = HIV.c1,
                                                res.gmm.IV1 = res.gmm.c2, res.gmm.IV2 = res.gmm.c1,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVs2 vs HIVs1 ** missing?!
  GMM_L3_vs_FE_L3  <- multilevel_ommitedvartest(IV1 = HIV.s2, IV2 = HIV.s1,
                                                res.gmm.IV1 = res.gmm.s2, res.gmm.IV2 = res.gmm.s1,
                                                W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc2 vs HIVs1, id=1
  GMM_L2_vs_FE_L3 <- multilevel_ommitedvartest(IV1 = HIV.c2, IV2 = HIV.s1,
                                               res.gmm.IV1 = res.gmm.c2, res.gmm.IV2 = res.gmm.s1,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)
  # HIVc1 vs HIVs2, id=1
  FE_L2_vs_GMM_L3 <- multilevel_ommitedvartest(IV1 = HIV.c1, IV2 = HIV.s2,
                                               res.gmm.IV1 = res.gmm.c1, res.gmm.IV2 = res.gmm.s2,
                                               W = W, l.Lhighest.X=l.L3.X, l.Lhighest.y=l.L3.y)

  return(new_rendo_multilevel(
            call = cl,
            formula = f.orig,
            num.levels = 2,
            dt.mf = dt.model.frame,
            dt.mm = dt.model.matrix,
            V = V,
            W = W,
            # The list names determine the final naming of the coefs
            l.gmm = list(REF = res.gmm.HREE,
                         FE_L2 = res.gmm.c1,
                         FE_L3 = res.gmm.s1,
                         GMM_L2 = res.gmm.c2,
                         GMM_L3 = res.gmm.s2),
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


  # TRY OUT / OLD CODE ===============================================================

  # name.endo     <- "X25"
  # name.response <- as.character((formula)[[2]])
  #
  # l4.form <- lme4::lFormula(formula = formula, data=dt.data)
  #
  # name.L2.group <- names(l4.form$reTrms$flist)[[1]] # CID
  # name.L3.group <- names(l4.form$reTrms$flist)[[2]] # SID
  # message("name.L2.group", name.L2.group)
  # message("name.L3.group", name.L3.group)
  #
  # names.by <- c(name.L3.group, name.L2.group)
  #
  # data.table::setorderv(x = dt.data, cols = names.by)
  #
  # mf <- model.frame(formula, dt.data)
  # mm <- model.matrix(object = formula, data = mf)
  #
  # y <- as.matrix(model.response(mf))
  #
  #
  # # Build X, X1 ----------------------------------------------------------
  # # X1 is everything but endogenous
  #
  # names.X  <- colnames(l4.form$X)
  # names.X1 <- setdiff(names.X, name.endo)
  #
  # # X  <- as.matrix(dt.data[, .SD, .SDcols = names.X][, "(Intercept)" := 1])
  # # X1 <- as.matrix(dt.data[, .SD, .SDcols = names.X1][, "(Intercept)" := 1])
  # X  <- mm[, names.X,  drop=F]
  # X1 <- mm[, names.X1, drop=F]
  #
  # # ** Are the groups for X L2 or L3 wise ??
  # dt.mm <- data.table(mm)
  # dt.mm[, (name.L2.group) := dt.data[, .SD,.SDcols=name.L2.group]]
  # dt.mm[, (name.L3.group) := dt.data[, .SD,.SDcols=name.L3.group]]
  #
  # # For ommitted var test only (Residuals):
  # # l.Xgroups <- data.table:::split.data.table(dt.mm, by=name.L2.group, keep.by = FALSE)
  # # l.Xgroups <- lapply(l.Xgroups, as.matrix)
  # #
  # # l.y.groups <- data.table:::split.data.table(dt.data[, .SD, .SDcols=c(name.L2.group, name.response)],
  # #                                             by=name.L2.group, keep.by = FALSE)
  # # l.y.groups <- lapply(l.y.groups, as.matrix)
  #
  # # Build Z2, Z3 ---------------------------------------------------------
  # # Only extract names from l4.form and build Z self because of unknown ordering
  # names.Z2 <- l4.form$reTrms$cnms[[1]]
  # names.Z3 <- l4.form$reTrms$cnms[[2]]
  #
  # message("names.Z2", names.Z2)
  # message("names.Z3", names.Z3)
  #
  # Z2 <- mm[, names.Z2, drop=FALSE]
  # Z3 <- mm[, names.Z3, drop=FALSE]
  #
  # # Split into school level groups
  # l.Z2.groups <- data.table:::split.data.table(dt.mm[, .SD, .SDcols = c(names.Z2, name.L3.group)],
  #                                              by=name.L3.group, keep.by = FALSE)
  # l.Z3.groups <- data.table:::split.data.table(dt.mm[, .SD, .SDcols = c(names.Z3, name.L3.group)],
  #                                              by=name.L3.group, keep.by = FALSE)
  # l.Z2.groups <- lapply(l.Z2.groups, as.matrix)
  # l.Z3.groups <- lapply(l.Z3.groups, as.matrix)
  #
  # # Calc V ---------------------------------------------------------------
  #
  # # sigma.sq, D.2, D.3
  # #  get D.2 and random error from VarCor
  # VC       <- lme4::VarCorr(lme4::lmer(formula=formula, data=dt.data))
  # D.2      <- VC[[name.L2.group]]
  # D.3      <- VC[[name.L3.group]]
  # sigma.sq <- attr(VC, "sc")
  #
  #
  # # V = blkdiag(V1, . . . , Vn)
  # # "and s=1,...,n schools"
  # # n = number of schools
  # # p 515: V_s = R_s + Z_2sVar(€(2)_s)Z'_2s+Z_3sVar(€_s(3))Z'_3s
  # # R_s is sigma.sq
  # # Var(€) is D.2 and D.3
  #
  # # Calculate per school level
  # l.groups.V <- mapply(l.Z2.groups, l.Z3.groups, FUN = function(g.z2, g.z3){
  #   Matrix::Diagonal(sigma.sq, n=length(g.z2)) + g.z2 %*% D.2 %*% t(g.z2) + g.z3 %*% D.3 %*% t(g.z3)
  # })
  #
  # V <- Matrix::bdiag(l.groups.V)
  #
  #
  # # V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.data)) + Z2 %*% D.2 %*% t(Z2) + Z3 %*% D.3 %*% t(Z3)
  #
  #
  # # Calc W ---------------------------------------------------------------
  # # W = V^(-1/2)
  #
  # # split V into its L3 blocks
  # g.idx <- dt.data[, list(g.idx=list(.I)), by=name.L3.group]$g.idx
  # l.V.groups <- lapply(g.idx, function(g.id) {V[g.id, g.id]})
  #
  # # Do eigen decomp on each block
  # l.W.groups <- lapply(l.V.groups, function(V.group){
  #   ei                     <- eigen(V.group)
  #   ei$values[ei$values<0] <- 0
  #   sValS                  <- 1/sqrt(ei$values)
  #   sValS[ei$values==0]    <- 0
  #   ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))})
  # W <- Matrix::bdiag(l.W.groups)
  # l.W.groups <- lapply(g.idx, function(g.id) {W[g.id, g.id, drop=FALSE]})
  # print(paste0("W done:", class(W)))
  #
  # # Calc Q, P, -----------------------------------------------------------
  # # between(10-11):
  # #   Q(2)_s=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  # #   (11): Q(2)=blkdiag(Q(2)_s)
  # # ** Where is this huge formula from ??
  #
  # l.groups.Q <- mapply(l.Z3.groups, l.W.groups, FUN = function(g.z3, g.w){
  #   Matrix::Diagonal(x=1, n=nrow(g.z3)) - g.w %*% g.z3 %*%
  #     corpcor::pseudoinverse(Matrix::crossprod(g.z3, g.w) %*% g.w %*% g.z3) %*%
  #     Matrix::crossprod(g.z3, g.w)
  # })
  #
  # Q.bd <- Matrix::bdiag(l.groups.Q)
  #
  # bd.Z3 <- Matrix::bdiag(l.Z3.groups)
  # Q.using.bd <-Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% bd.Z3 %*%
  #   corpcor::pseudoinverse(Matrix::crossprod(bd.Z3, W) %*% W %*% bd.Z3) %*%
  #   Matrix::crossprod(bd.Z3, W)
  #
  # Q <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% Z3 %*%
  #   corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*%
  #   Matrix::crossprod(Z3, W)
  #
  # print(all.equal(as.matrix(Q.bd), as.matrix(Q)), check.attributes=F) # FALSE
  # print(all.equal(as.matrix(Q.bd), as.matrix(Q.using.bd)), check.attributes=F) # TRUE
  # Q <- Q.bd
  #
  #
  # P <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - Q
  #
  # print(paste0("Q done:", class(Q)))
  # print(paste0("P done:", class(P)))
  #
  #
  # # Instruments ------------------------------------------------------------------------------------
  #
  # l.L2.matrices <- multilevel_buildmatrices()
  # l.L3.matrices <- multilevel_buildmatrices()
  #
  # # HIVs1[sloc,] <- Ql3 %*% (Wl3 %*% Xl3)
  # # HIVs2[sloc,] <- cbind(Ql3 %*% (Wl3 %*% Xl3), Pl3 %*% (Wl3 %*% X1l3))
  #
  # # HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)
  # # HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))
  #
  # # HIV.s1 <-
  # # HIV.s2 <-
  # # HIV.c1 <-
  # # HIV.c2 <-
  # # HREE <- W %*% as.matrix(X)

