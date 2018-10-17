multilevel_3levels <- function(formula, dt.data){
  name.endo     <- "X25"
  name.response <- as.character((formula)[[2]])

  l4.form <- lme4::lFormula(formula = formula, data=dt.data)

  name.L2.group <- names(l4.form$reTrms$flist)[[1]] # SCHOOLID
  name.L3.group <- names(l4.form$reTrms$flist)[[2]] # CLASS ID

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
  dt.mm <- data.table(mm[, names.X, drop=FALSE])
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

  Z2 <- mm[, names.Z2, drop=FALSE]
  Z3 <- mm[, names.Z3, drop=FALSE]


  # Calc V ---------------------------------------------------------------

  # sigma.sq, D.2, D.3
  #  get D.2 and random error from VarCor
  VC       <- lme4::VarCorr(lme4::lmer(formula=formula, data=dt.data))
  D.2      <- VC[[name.L2.group]]
  D.3      <- VC[[name.L3.group]]
  sigma.sq <- attr(VC, "sc")

  V <- Matrix::Diagonal(sigma.sq, n=nrow(dt.data)) + Z2 %*% D.2 %*% t(Z2) + Z3 %*% D.3 %*% t(Z3)


  # Calc W ---------------------------------------------------------------
  # W = V^(-1/2)

  # split V into its (L3) blocks (**arbitrary choosen??)
  g.idx <- dt.data[, .(g.idx=list(.I)), by=name.L3.group]$g.idx
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

  # Calc Q, P, -----------------------------------------------------------
  # between(10-11):Q_s(2)=I_s-P(Z_3s), Z_3s=stacked Z_3sct -> Q(2)=blkdiag(Q_s(2))
  # (11): Q(2)=blkdiag(Q(2)_s)

  # **Only change Z3 here?

  Q <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - W %*% Z3 %*%
    corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*%
    Matrix::crossprod(Z3, W)

  P <- Matrix::Diagonal(x=1, n=nrow(dt.data)) - Q

  print(paste0("Q done:", class(Q)))
  print(paste0("P done:", class(P)))



}
