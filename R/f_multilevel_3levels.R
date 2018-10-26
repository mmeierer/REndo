multilevel_3levels <- function(formula, data){


  l4.form <- lme4::lFormula(formula = formula, data=data)
  num.levels <- length(l4.form$reTrms$flist) + 1

  name.group.L2 <- names(l4.form$reTrms$flist)[[1]] # CID
  name.group.L3 <- names(l4.form$reTrms$flist)[[2]] # SID
  message("name.group.L3", name.group.L3)
  message("name.group.L2", name.group.L2)


  VC       <- lme4::VarCorr(lme4::lmer(formula=formula, data=data))
  D.2      <- VC[[name.group.L2]]
  D.3      <- VC[[name.group.L3]]
  sigma.sq <- attr(VC, "sc")


  l.m.L2 <- multilevel_buildmatrices(data = data, l4.form = l4.form, formula = formula,
                                     names.endo = "X15", num.levels = 2,
                                     name.group.by = name.group.L3,
                                     sigma.sq = sigma.sq, D.2 = D.2, D.3 = D.3)

  l.m.L3 <- multilevel_buildmatrices(data = data, l4.form = l4.form, formula = formula,
                                     names.endo = "X15", num.levels = 3,
                                     name.group.by = name.group.L3,
                                     sigma.sq = sigma.sq, D.2 = D.2, D.3 = D.3)


  # HIVs1[sloc,] <- Ql3 %*% (Wl3 %*% Xl3)
  # HIVs2[sloc,] <- cbind(Ql3 %*% (Wl3 %*% Xl3), Pl3 %*% (Wl3 %*% X1l3))

  # HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)
  # HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))

  HIV.s1 <- l.m.L3$Q %*% (l.m.L3$W %*% l.m.L3$X)
  HIV.s2 <- cbind(l.m.L3$Q %*% (l.m.L3$W %*% l.m.L3$X), l.m.L3$P %*% (l.m.L3$W %*% l.m.L3$X1))

  # ** is W from L3 or L2 ?? Raluca code is L3. But L2 Q and L2 P incorporate W at L2 as well...(???)
  HIV.c1 <- l.m.L2$Q %*% (l.m.L3$W %*% l.m.L2$X)
  HIV.c2 <- cbind(l.m.L2$Q %*% (l.m.L3$W %*% l.m.L2$X), l.m.L2$P %*% (l.m.L3$W %*% l.m.L2$X1))



  # res.gmm.s1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s1)
  # res.gmm.s2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s2)
  # res.gmm.c1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.c1)
  # res.gmm.c2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.c2)
  # res.gmm.HREE <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HREE)

  # ** AT WHICH LEVEL?
  # HREE <- W %*% as.matrix(X)

  return(list(HIV.s1=HIV.s1, HIV.s2=HIV.s2, HIV.c1=HIV.c1, HIV.c2=HIV.c2,
              L2.W=l.m.L2$W, L3.W=l.m.L3$W))

  # return(list(HIV.s1=HIV.s1, HIV.s2=HIV.s2, HIV.c1=HIV.c1, HIV.c2=HIV.c2,
  #             res.gmm.s1=res.gmm.s1, res.gmm.s2=res.gmm.s2,
  #             res.gmm.c1=res.gmm.c1, res.gmm.c2=res.gmm.c2))


  name.endo     <- "X25"
  name.response <- as.character((formula)[[2]])

  l4.form <- lme4::lFormula(formula = formula, data=dt.data)

  name.L2.group <- names(l4.form$reTrms$flist)[[1]] # CID
  name.L3.group <- names(l4.form$reTrms$flist)[[2]] # SID
  message("name.L2.group", name.L2.group)
  message("name.L3.group", name.L3.group)

  names.by <- c(name.L3.group, name.L2.group)

  data.table::setorderv(x = dt.data, cols = names.by)

  mf <- model.frame(formula, dt.data)
  mm <- model.matrix(object = formula, data = mf)

  y <- as.matrix(model.response(mf))


  # Build X, X1 ----------------------------------------------------------
  # X1 is everything but endogenous

  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, name.endo)

  # X  <- as.matrix(dt.data[, .SD, .SDcols = names.X][, "(Intercept)" := 1])
  # X1 <- as.matrix(dt.data[, .SD, .SDcols = names.X1][, "(Intercept)" := 1])
  X  <- mm[, names.X,  drop=F]
  X1 <- mm[, names.X1, drop=F]

  # ** Are the groups for X L2 or L3 wise ??
  dt.mm <- data.table(mm)
  dt.mm[, (name.L2.group) := dt.data[, .SD,.SDcols=name.L2.group]]
  dt.mm[, (name.L3.group) := dt.data[, .SD,.SDcols=name.L3.group]]

  # For ommitted var test only (Residuals):
  # l.Xgroups <- data.table:::split.data.table(dt.mm, by=name.L2.group, keep.by = FALSE)
  # l.Xgroups <- lapply(l.Xgroups, as.matrix)
  #
  # l.y.groups <- data.table:::split.data.table(dt.data[, .SD, .SDcols=c(name.L2.group, name.response)],
  #                                             by=name.L2.group, keep.by = FALSE)
  # l.y.groups <- lapply(l.y.groups, as.matrix)

  # Build Z2, Z3 ---------------------------------------------------------
  # Only extract names from l4.form and build Z self because of unknown ordering
  names.Z2 <- l4.form$reTrms$cnms[[1]]
  names.Z3 <- l4.form$reTrms$cnms[[2]]

  message("names.Z2", names.Z2)
  message("names.Z3", names.Z3)

  Z2 <- mm[, names.Z2, drop=FALSE]
  Z3 <- mm[, names.Z3, drop=FALSE]

  # Split into school level groups
  l.Z2.groups <- data.table:::split.data.table(dt.mm[, .SD, .SDcols = c(names.Z2, name.L3.group)],
                                               by=name.L3.group, keep.by = FALSE)
  l.Z3.groups <- data.table:::split.data.table(dt.mm[, .SD, .SDcols = c(names.Z3, name.L3.group)],
                                               by=name.L3.group, keep.by = FALSE)
  l.Z2.groups <- lapply(l.Z2.groups, as.matrix)
  l.Z3.groups <- lapply(l.Z3.groups, as.matrix)

  # Calc V ---------------------------------------------------------------

  # sigma.sq, D.2, D.3
  #  get D.2 and random error from VarCor
  VC       <- lme4::VarCorr(lme4::lmer(formula=formula, data=dt.data))
  D.2      <- VC[[name.L2.group]]
  D.3      <- VC[[name.L3.group]]
  sigma.sq <- attr(VC, "sc")


  # V = blkdiag(V1, . . . , Vn)
  # "and s=1,...,n schools"
  # n = number of schools
  # p 515: V_s = R_s + Z_2sVar(€(2)_s)Z'_2s+Z_3sVar(€_s(3))Z'_3s
  # R_s is sigma.sq
  # Var(€) is D.2 and D.3

  # Calculate per school level
  l.groups.V <- mapply(l.Z2.groups, l.Z3.groups, FUN = function(g.z2, g.z3){
    Matrix::Diagonal(sigma.sq, n=length(g.z2)) + g.z2 %*% D.2 %*% t(g.z2) + g.z3 %*% D.3 %*% t(g.z3)
  })

  V <- Matrix::bdiag(l.groups.V)


  # V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.data)) + Z2 %*% D.2 %*% t(Z2) + Z3 %*% D.3 %*% t(Z3)


  # Calc W ---------------------------------------------------------------
  # W = V^(-1/2)

  # split V into its L3 blocks
  g.idx <- dt.data[, .(g.idx=list(.I)), by=name.L3.group]$g.idx
  l.V.groups <- lapply(g.idx, function(g.id) {V[g.id, g.id]})

  # Do eigen decomp on each block
  l.W.groups <- lapply(l.V.groups, function(V.group){
    ei                     <- eigen(V.group)
    ei$values[ei$values<0] <- 0
    sValS                  <- 1/sqrt(ei$values)
    sValS[ei$values==0]    <- 0
    ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))})
  W <- Matrix::bdiag(l.W.groups)
  l.W.groups <- lapply(g.idx, function(g.id) {W[g.id, g.id]})
  print(paste0("W done:", class(W)))

  # Calc Q, P, -----------------------------------------------------------
  # between(10-11):
  #   Q(2)_s=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  #   (11): Q(2)=blkdiag(Q(2)_s)
  # ** Where is this huge formula from ??

  l.groups.Q <- mapply(l.Z3.groups, l.W.groups, FUN = function(g.z3, g.w){
    Matrix::Diagonal(x=1, n=nrow(g.z3)) - g.w %*% g.z3 %*%
      corpcor::pseudoinverse(Matrix::crossprod(g.z3, g.w) %*% g.w %*% g.z3) %*%
      Matrix::crossprod(g.z3, g.w)
  })

  Q.bd <- Matrix::bdiag(l.groups.Q)

  bd.Z3 <- Matrix::bdiag(l.Z3.groups)
  Q.using.bd <-Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% bd.Z3 %*%
    corpcor::pseudoinverse(Matrix::crossprod(bd.Z3, W) %*% W %*% bd.Z3) %*%
    Matrix::crossprod(bd.Z3, W)

  Q <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% Z3 %*%
    corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*%
    Matrix::crossprod(Z3, W)

  print(all.equal(as.matrix(Q.bd), as.matrix(Q)), check.attributes=F) # FALSE
  print(all.equal(as.matrix(Q.bd), as.matrix(Q.using.bd)), check.attributes=F) # TRUE
  Q <- Q.bd


  P <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - Q

  print(paste0("Q done:", class(Q)))
  print(paste0("P done:", class(P)))


  # Instruments ------------------------------------------------------------------------------------

  l.L2.matrices <- multilevel_buildmatrices()
  l.L3.matrices <- multilevel_buildmatrices()

  # HIVs1[sloc,] <- Ql3 %*% (Wl3 %*% Xl3)
  # HIVs2[sloc,] <- cbind(Ql3 %*% (Wl3 %*% Xl3), Pl3 %*% (Wl3 %*% X1l3))

  # HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)
  # HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))

  # HIV.s1 <-
  # HIV.s2 <-
  # HIV.c1 <-
  # HIV.c2 <-
  # HREE <- W %*% as.matrix(X)

  # Ommitted Variable ---------------------------------------------------------------------------------
  # HIVc1 vs HREE, id=1
  # HIVc2 vs HREE, id=1
  # HIVs2 vs HREE, id=2
  # HIVs1 vs HREE, id=2
  # HIVc1 vs HREE, id=2 ** double but different id ??

  # HIVc2 vs HIVs2, id=1
  # HIVc2 vs HIVs1, id=1
  # HIVc1 vs HIVs2, id=1
  # HIVc1 vs HIVs1, id=1
  # HIVc2 vs HIVc1, id=1
  # HIVs1 vs HIVs2 ** missing?!

  # ovt_GMML2_GMML3 <- OmittedVarTest(m1=bGMM_L2,m2=bGMM_L3,HIV1 = HIVc2, HIV2 = HIVs2,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  # ovt_GMML2_FEL3 <- OmittedVarTest(m1=bGMM_L2,m2=bFE_L3,HIV1 = HIVc2, HIV2 = HIVs1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  #
  # ovt_GMML3_FEL2 <- OmittedVarTest(m1=bFE_L2,m2=bGMM_L3,HIV1 = HIVc1, HIV2 = HIVs2,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  #
  # ovt_FEL2_FEL3 <- OmittedVarTest(m1=bFE_L2,m2=bFE_L3,HIV1 = HIVc1, HIV2 = HIVs1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  # ovt_FEL2_GMML2 <- OmittedVarTest(m1=bGMM_L2,m2=bFE_L2,HIV1 = HIVc2, HIV2 = HIVc1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)

  #
}
