multilevel_buildmatrices <- function(data, formula, l4.form, num.levels, names.endo, name.group.by,
                                     sigma.sq, D.2, D.3 = NULL){

  if(num.levels == 3 & is.null(D.3))
    stop("Wrong!")

  message("name.group.by", name.group.by)

  y  <- Matrix::Matrix(model.response(l4.form$fr), sparse = TRUE)
  mm <- model.matrix(object = terms(l4.form$fr), data = l4.form$fr)

  dt.model.matrix <- as.data.table(mm)
  data.table::setkeyv(dt.model.matrix, cols = name.group.by)

  # Build X, X1 --------------------------------------------------------------------------------
  names.X  <- colnames(l4.form$X)
  names.X1 <- setdiff(names.X, names.endo)

  l.X  <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X,  name.by = name.group.by)
  l.X1 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.X1, name.by = name.group.by)
  X    <- Matrix::Matrix(as.matrix(dt.model.matrix[, .SD, .SDcols = names.X]),  sparse = TRUE)
  X1   <- Matrix::Matrix(as.matrix(dt.model.matrix[, .SD, .SDcols = names.X1]), sparse = TRUE)


  # Build Z2, Z3 -------------------------------------------------------------------------------
  if(num.levels == 3){
    names.Z2 <- l4.form$reTrms$cnms[[1]]
    names.Z3 <- l4.form$reTrms$cnms[[2]]
    l.Z2 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z2, name.by = name.group.by)
    l.Z3 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z3, name.by = name.group.by)
  }else{
    names.Z2 <- l4.form$reTrms$cnms[[1]]
    l.Z2 <- multilevel_splittomatrix(dt = dt.model.matrix, name.group = names.Z2, name.by = name.group.by)
  }

  message("num.Z2 groups", length(l.Z2))

  # Fit REML -----------------------------------------------------------------------------------


  # ** check if sorting of D cols is same as Z columns!!

  # Calc V -------------------------------------------------------------------------------------
  # V = blkdiag(V1, . . . , Vn)
  # Calculate per school level
  # **could move diagonal outside to speed up
  if(num.levels == 3){
    l.V <- mapply(l.Z2, l.Z3, FUN = function(g.z2, g.z3){
      Matrix::Diagonal(sigma.sq, n=nrow(g.z2)) + g.z2 %*% D.2 %*% t(g.z2) + g.z3 %*% D.3 %*% t(g.z3)
    })
  }else{
    l.V <- lapply(l.Z2, FUN = function(g.z2){
      Matrix::Diagonal(sigma.sq, n=nrow(g.z2)) + g.z2 %*% D.2 %*% t(g.z2)
    })
  }

  # Never used...
  V <- Matrix::bdiag(l.V)

  # Raluca did: Z3 on school level and Z2 on child level, then add
  # Check if same

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
  #   (11): Q(2)=blkdiag(Q(2)_s)
  if(num.levels == 3){
    l.Q <- mapply(l.Z3, l.W, FUN = function(g.z3, g.w){
      Matrix::Diagonal(x=1, n=nrow(g.z3)) - g.w %*% g.z3 %*%
        corpcor::pseudoinverse(Matrix::crossprod(g.z3, g.w) %*% g.w %*% g.z3) %*%
        Matrix::crossprod(g.z3, g.w)
    })
  }else{
    l.Q <- mapply(l.Z2, l.W, FUN = function(g.z2, g.w){
      Matrix::Diagonal(x=1, n=nrow(g.z2)) - g.w %*% g.z2 %*%
        corpcor::pseudoinverse(Matrix::crossprod(g.z2, g.w) %*% g.w %*% g.z2) %*%
        Matrix::crossprod(g.z2, g.w)
    })
  }

  Q <- Matrix::bdiag(l.Q)

  # Calc P -------------------------------------------------------------------------------------
  P <- Matrix::Diagonal(x=1, n=nrow(data)) - Q

  # Return -------------------------------------------------------------------------------------
  return(list(y = y, X=X, X1=X1, W=W, Q=Q, P=P))

}

multilevel_splittomatrix <- function(dt, name.group, name.by){
  l.groups <- data.table:::split.data.table(dt[, .SD, .SDcols=c(name.group, name.by)],
                                            by = name.by, keep.by = FALSE)
  l.groups <- lapply(l.groups, as.matrix)
  return(l.groups)
}
