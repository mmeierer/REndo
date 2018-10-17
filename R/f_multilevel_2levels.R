#' @importFrom lme4 lFormula VarCorr lmer
#' @importFrom Matrix Diagonal crossprod bdiag
#' @importFrom corpcor pseudoinverse
multilevel_2levels <- function(formula, dt.data){
  name.endo     <- "X25"
  name.response <- as.character((formula)[[2]])

  l4.form <- lme4::lFormula(formula = formula, data=dt.data)

  name.L2.group <- names(l4.form$reTrms$flist)[[1]] # SCHOOLID

  data.table::setorderv(x = dt.data, cols = name.L2.group)

  # by stacking y = (y′1,...,y′n)′, X = (X′1,...,X′n)′
  # y <- dt.data[, .SD, .SDcols = name.response]

  mf <- model.frame(formula, dt.data)
  mm <- model.matrix(object = formula, data = mf)

  y <- as.matrix(model.response(mf))


  # Build X, X1 ----------------------------------------------------------
  # Do not use the X component present in l4.form as their ordering is
  #   somewhat obscure.
  # X1 is everything but endogenous
  # **Does X/X1 include an intercept???
  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, name.endo)

  # X  <- as.matrix(dt.data[, .SD, .SDcols = names.X][, "(Intercept)" := 1])
  # X1 <- as.matrix(dt.data[, .SD, .SDcols = names.X1][, "(Intercept)" := 1])
  X  <- mm[, names.X,  drop=F]
  X1 <- mm[, names.X1, drop=F]

  dt.mm <- data.table(mm[, names.X, drop=FALSE])
  dt.mm[, (name.L2.group) := dt.data[, .SD,.SDcols=name.L2.group]]
  l.Xgroups <- data.table:::split.data.table(dt.mm, by=name.L2.group, keep.by = FALSE)
  l.Xgroups <- lapply(l.Xgroups, as.matrix)
  bd.X      <- Matrix::bdiag(l.Xgroups)


  l.y.groups <- data.table:::split.data.table(dt.data[, .SD, .SDcols=c(name.L2.group, name.response)],
                                              by=name.L2.group, keep.by = FALSE)
  l.y.groups <- lapply(l.y.groups, as.matrix)

  # Build Z2 -------------------------------------------------------------
  # Only extract names from l4.form and build Z self because of unknown ordering
  names.Z2 <- l4.form$reTrms$cnms[[1]]

  Z2 <- mm[, names.Z2, drop=FALSE]

  l.Z2.groups <- data.table:::split.data.table(dt.mm[, .SD, .SDcols=c(name.L2.group, names.Z2)],
                                              by=name.L2.group, keep.by = FALSE)
  l.Z2.groups <- lapply(l.Z2.groups, as.matrix)
  bd.Z2 <- Matrix::bdiag(l.Z2.groups)

  # Calc V ---------------------------------------------------------------
  # V=blkdiag(V1,..,Vn)
  # V=blkdiag(V_s) (18) Var ∂s=Vs


  # sigma.sq, D.2
  #  get D.2 and random error from VarCor
  VC       <- lme4::VarCorr(lme4::lmer(formula=formula, data=dt.data))
  D.2      <- VC[[name.L2.group]]
  sigma.sq <- attr(VC, "sc")


  # ** Where is the whole by-group thingee here ??
  # Create bdiag matrix with D2s *** Really? or simply stack them ??
  # num.groups.L2 <- nrow(dt.data[, .N, by=name.L2.group])
  bd.D.2 <- Matrix::bdiag(rep(list(D.2), times=length(l.Z2.groups))) # rep D.2 in as many times as there are groups
  V.bd <- Diagonal(sigma.sq, n=nrow(dt.data)) + bd.Z2 %*% bd.D.2 %*% t(bd.Z2)

  l.Vs <- lapply(l.Z2.groups, function(z2.s){
    diag(x = sigma.sq, nrow = nrow(z2.s), ncol = nrow(z2.s)) + z2.s %*% D.2 %*% t(z2.s) })
  V.bd.l <- Matrix::bdiag(l.Vs)

  # **Wrong**
  V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.data)) + Z2 %*% D.2 %*% t(Z2)

V<-V.bd.l

  # Calc W ---------------------------------------------------------------
  # W = V^(-1/2)
  # W = the weight can be the inverse of the square root of the variance–covariance matrix of the disturbance term
  # https://scicomp.stackexchange.com/questions/10375/efficient-computation-of-the-matrix-square-root-inverse
  # "In many cases you can instead use a Cholesky factor of the inverse of the covariance matrix
  #   (or practically the same, the Cholesky factor of the covariance matrix itself.)"
  # Maybe also do block-wise??

  # ei.V <- eigen(V)
  # ei.V$values[ei.V$values<0] <- 0
  # sValS                      <- 1/sqrt(ei.V$values)
  # sValS[ei.V$values==0]      <- 0
  # W <- ei.V$vectors %*% (Matrix::Diagonal(x=sValS,n=NROW(sValS)) %*% Matrix::t(ei.V$vectors))

  # inv(sqrtm(BDIAG)) is same as applied on each block but faster and can create a sparse (dgCMatrix) matrix

  # split V into its L2 blocks

  g.idx <- dt.data[, .(g.idx=list(.I)), by=name.L2.group]$g.idx
  l.V.groups <- lapply(g.idx, function(g.id) {V[g.id, g.id]})

  # Do eigen decomp on each block
  l.W.groups <- lapply(l.V.groups, function(V.group){
    ei                     <- eigen(V.group)
    ei$values[ei$values<0] <- 0
    sValS                  <- 1/sqrt(ei$values)
    sValS[ei$values==0]    <- 0
    ei$vectors %*% (diag(x=sValS,nrow=NROW(sValS)) %*% t(ei$vectors))})
  W <- bdiag(l.W.groups)

  print(paste0("W done:", class(W)))
# W.chol <- Matrix::chol(V)
# print(all.equal(as.matrix(W), as.matrix(W.chol), check.attributes=F))

  # Calc Q, P, -----------------------------------------------------------
  # ** Formula for Q??
  #
  # Q: **Which Q should be built? Q(2) or Q(1) ??
  # between(10-11):Q_s(2)=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  # (11): Q(2)=blkdiag(Q(2)_s)
  # Q(1)_1sQ(2)_1s = Q(1)_1s -> Projection matrices are nested
  # Q(2) = blkdiag(Q(2)_s)
  # -> Q(1) = blkdiag(Q(1)_sc) (p.512)
  # -> Base on (8): Q(1)_sc=I_sc-P(Z_2sc)
  #
  # P:
  # (8): Q(1)_sc=I_sc-P(Z_2sc) -?> P(Z_2sc) = Isc-Q_sc?
  # End page 512: P(1)_sc=I_sc-Q(1)_sc
  # (12):P(1)_sc=I_sc-Q(1)_sc


  # s = school
  # c = child within s
  # t = time

  # ** Which level are we trying to figure out?
  # (1) = level-1 model
  #   (1)_sc = variability at child level
  #
  # (2) = level-2 model
  #   Z(2)_sc, X(2)_sc=child or school and do not vary over time
  #
  # (3) = level-3 model = variability at school level
  #   X(3)_s = may depend on school
  #
  # Define:
  #   Z_2sct = Z(1)_sct
  #   Z_3sct = Z(1)_sctZ(2)_sc
  #   Z_3s = stacked Z_3sct
  #
  #
  #   X_sct = (X(1)_sct:Z_2sct)
  #
  #   X(1)=stacked X(1)_sc
  #
  #
  #

  Q <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% Z2 %*%
    corpcor::pseudoinverse(Matrix::crossprod(Z2, W) %*% W %*% Z2) %*%
    Matrix::crossprod(Z2, W)

  P <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - Q

  print(paste0("Q done:", class(Q)))
  print(paste0("P done:", class(P)))

  # Calc HIV1s1, HIVs2, HREE ---------------------------------------------
# original code for the L2 case only HIVs12, line 318ff
  # "Consequently, the instruments H consist of QX and PX1" (p.516)
  # H_2s=(Q(2)_sX_s : P(2)_sX2s)
  # ** Where is this one from in the paper??
  # ** How long/dimensions are the IVs expected to be??

  HIV.s1 <- Q %*% (W%*%(X))
  # End page 517:
  # H_1sgls = (Q(1)_sglsV^-0.5X_s:P(1)_sglsV^-0.5X_1s)
  #   where Q(1)_sgls = I_sc-P(V^0.5_sZ_2sc)
  #   and   P(1)_sgls = I_sc-Q(1)_sgls
  #   ** what is P(1)_gls ??
  HIV.s2 <- cbind(Q %*% W%*%X, P%*%W%*%X1)

  # ***Is it correct that X1 is only used once to construct HIV.s2??? Ie appears multiple times in the paper, say (15)

  HREE   <- W %*% X

  print(paste0("HIV.s1 done:", class(HIV.s1)))
  print(paste0("HIV.s2 done:", class(HIV.s2)))
  print(paste0("HREE done:", class(HREE)))

  # ??? Comments say s1,s2 are for level 3 case??
  # HIVc1 <- Interim$HIVc1    # fixed effects level 2
  # HIVc2 <- Interim$HIVc2    # GMM level 2
  #
  # HIVs1 <- Interim$HIVs1    # if 3 levels model - fixed effects level 3/ if 2 levels model - fixed effect slevel 2
  # HIVs2 <- Interim$HIVs2    # if 3 levels model - GMM level 3/ if 2 levels - GMM level 2

  # Estimate GMMs for IVs ------------------------------------------------

  res.gmm.s1   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s1)
  res.gmm.s2   <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HIV.s2)
  res.gmm.HREE <- multilevel_gmmestim(y=y, X=X, W=W, HIV=HREE)

  # return(list(HIV.s1=HIV.s1, HIV.s2=HIV.s2,bd.X=bd.X, y=y, HREE=HREE, W=W,V=V,
  #             l.Xgroups=l.Xgroups, l.y.groups=l.y.groups,
  #             resFEc=resFEc,resGMMc=resGMMc,resREE=resREE))

  # Omitted Var tests ----------------------------------------------------

  FEL_vs_REF   <-  multilevel_ommitedvartest(IV1 = HREE, IV2=HIV.s1,
                                             coef.IV1   = res.gmm.HREE$coef,   coef.IV2   = res.gmm.s1$coef,
                                             gammaH.IV1 = res.gmm.HREE$GammaH, gammaH.IV2 = res.gmm.s1$GammaH,
                                             l.L2.groups.X = l.Xgroups, l.L2.groups.y = l.y.groups,
                                             W=W)
  GMML_vs_REF  <-  multilevel_ommitedvartest(IV1 = HIV.s2, IV2=HREE,
                                              coef.IV1   = res.gmm.s2$coef,   coef.IV2   = res.gmm.HREE$coef,
                                              gammaH.IV1 = res.gmm.s2$GammaH, gammaH.IV2 = res.gmm.HREE$GammaH,
                                              l.L2.groups.X = l.Xgroups, l.L2.groups.y = l.y.groups,
                                              W=W)
  FEL_vs_GMML  <-  multilevel_ommitedvartest(IV1 = HIV.s1, IV2 = HIV.s2,
                                             coef.IV1   = res.gmm.s1$coef,   coef.IV2   = res.gmm.s2$coef,
                                             gammaH.IV1 = res.gmm.s1$GammaH, gammaH.IV2 = res.gmm.s2$GammaH,
                                             l.L2.groups.X = l.Xgroups, l.L2.groups.y = l.y.groups,
                                             W=W)

  # *** HOW should all of this be reported / returned?

  return(list(HIV.s1=HIV.s1, HIV.s2=HIV.s2, HREE=HREE,
              res.gmm.s1 = res.gmm.s1,res.gmm.s2=res.gmm.s2,res.gmm.HREE=res.gmm.HREE))

  # Results --------------------------------------------------------------
  coefs
  SE
  vcovM
  W
  call
  model
  ommitedvartest
  # OVT <- list(REF = cbind(GMML2_vs_REF,FEL2_vs_REF, FEL2_vs_GMML2), GMM_L2=cbind(GMML2_vs_REF,FEL2_vs_REF, FEL2_vs_GMML2),FE_L2=cbind(GMML2_vs_REF,FEL2_vs_REF,FEL2_vs_GMML2))

}
