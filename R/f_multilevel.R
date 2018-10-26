#' @importFrom lme4 lmer VarCorr lFormula
multilevel <- function(formula, data, verbose){

# ** CHECK THAT EVERY CHILD ONLY APPEARS IN 1 SCHOOL
  # .N, by=SID, CID, then uniqueN(CID) == nrow() (ie CID appears only once for every SID )


  # Check input ----------------------------------------------------------

  # Extract data ---------------------------------------------------------
  dt.data <- data.table(data)

  # Let lme4 do the formula processing
  l4.form    <- lme4::lFormula(formula = formula, data=data)
  num.levels <- length(l4.form$reTrms$flist)+1
  stopifnot(num.levels %in% c(2,3))

  names.by.L2 <- c(name.L2.group, name.L3.group)
  name.by.L3  <- name.L3.group


  # Build X, X1 ----------------------------------------------------------
  # Stacked



  # Build Z2 (Z3) --------------------------------------------------------


  # Read brackets/random effects name, ie (1+log(CLASS_SI)|CID) + (1|SID)
  z2.names <- l4.form$reTrms$cnms[[1]]

  if(num.levels == 3){
    z3.names <- l4.form$reTrms$cnms[[2]]
  }





  # Calc V --------------------------------------------------------------



  # VarCor stuff - get Ds and random error
  # *** try-catch
  VC       <- VarCorr(lmer(formula=formula, data=data))


  if(num.levels == 3){
    # Create bdiag matrix with D3s
    num.groups.L3 <- nrow(dt.data[, .N, by=name.by.L3])
    bd.D.3 <- bdiag(rep(list(D.3), times=num.groups.L3))

    # Add to V
    V <- V + bd.Z3 %*% bd.D.3 %*% t(bd.Z3)
  }

  # Calc W --------------------------------------------------------------
  ei.V <- eigen(V)
  ei.V$values[ei.V$values<0] <- 0
  sValS                      <- 1/sqrt(ei.V$values)
  sValS[ei.V$values==0]      <- 0
  W <- ei.V$vectors %*% (Diagonal(x=sValS,n=NROW(sValS)) %*% t(ei.V$vectors))


  # Q, P, HIV1, HIV2 ----------------------------------------------------
  # Qsc = projection matrix => deviations from the time series averages
  # Q2_s = I_s − P(Z_3s), where Z is the stacked version of Z_3sct
  if(num.levels == 2)
    Q <- Diagonal(x=1, n=nrow(dt.data)) - W %*% Z2 %*%  corpcor::pseudoinverse(Matrix::crossprod(Z2, W) %*% W %*% Z2) %*% Matrix::crossprod(Z2, W)
  else
    Q <- Diagonal(x=1, n=nrow(dt.data)) - W %*% Z3 %*%  corpcor::pseudoinverse(Matrix::crossprod(Z3, W) %*% W %*% Z3) %*% Matrix::crossprod(Z3, W)

  P <- Diagonal(x=1, n=nrow(dt.data)) - Q




  #### different estimators are produced depending on the level - 3 or 2 levels #####
  if (Levels==3) {
    resFEc <- printGMM(HIVc1,id=1)
    resGMMc <- printGMM(HIVc2,id=1)
    resFEs <- printGMM(HIVs1,id=2)
    resGMMs <- printGMM(HIVs2,id=2)
    HREE <- W %*% as.matrix(X)
    resREE <- printGMM(y,X,HREE,id=0,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)

    # 10 OmittedVarTest
  }


}
