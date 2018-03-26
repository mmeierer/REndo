# Raluca Gui - 02.10.2017
# adapt from Kim and Frees 2007
# multilevelIV for 2 and 3 levels
#'@title Multilevel GMM Estimation
#'@aliases multilevelIV
# Description
#'@description  Estimates multilevel models (max. 3 levels) employing the GMM approach presented in Kim and Frees (2007). One of the important features is that, using the hierarchical 
#'structure of the data, no external instrumental variables are needed, unlike traditional instrumental variable techniques.
# Arguments
#'@param    formula  an object of type 'formula': a symbolic description of the model to be fitted. 
#'@param    endoVar  a matrix or data frame containing the variables assumed to be endogenous.  
#'@param    data  optional data frame or list containing the variables of the model.
#details
#'@details    When all model variables are assumed exogenous the GMM estimator is the usual GLS estimator. 
#'While the GLS model assumes all explanatory variables are uncorrelated with the random intercepts and slopes in the model,
#'fixed effects models allow for endogeneity of all effects but sweeps out the random components as well as the explanatory variables at the same levels. 
#'The more general estimator presented here allows for some of the explanatory variables to be endogenous and uses this information to build internal instrumental variables.
#'The multilevel GMM estimator uses both the between and within variations of the exogenous variables, but only the within variation of the variables assumed 
#' endogenous. The mixed GMM estimator equals the random effects estimator when all variables are assumed exogenous and is equal to the fixed effects estimator 
#' when all variables are assumed endogenous. In between different GMM estimators are obtained for different sets of endogenous/exogenous variables.  
#'@return    returns the estimated coefficients together with their standard errors and p-values. It also returns the variance -covariance matrix and the weigth matrix used in estimation.
#'\item{coefficients}{the estimated coefficients.}
#'\item{coefSdErr}{the standard errors of the estimated coefficients.}
#'\item{vcovMat}{the variance-covariance matrix.}
#'\item{weigthMat}{the weight matrix used in estimation.}
#'\item{mixedCall}{the call of the estimated model.}
#'@keywords  endogeneity
#'@keywords  GMM
#'@keywords  instrumental
#'@keywords  instruments
#'@author The implementation of the model formula by Raluca Gui based on the paper of Kim and Frees (2007).
#'@references   Kim, Jee-Seon and Frees, Edward W. (2007). 'Multilevel Modeling with Correlated Effects'. \emph{Psychometrika},72(4), 505-533.
#'@seealso \code{\link{internalIV}}, \code{\link[AER]{ivreg}}, \code{\link{latentIV}},\code{\link{copulaCorrection}}
#'@examples
#'\dontrun{
#'data(dataMultilevelIV)
#'endoVars <- matrix(dataMultilevelIV[,"X15"])
#'colnames(endoVars) <- c("X15")
#'formula1 <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + 
#'X31 + X32 + X33 + (1 + X11 | CID) + (1|SID)
#'model1 <- multilevelIV(formula=formula1, endoVar=endoVars, data=dataMultilevelIV)
#'summary(model1,"REF")
#'}
#'@export

multilevelIV <- function(formula, endoVar, data = NULL) {

print("Attention! The endogeneous regressor names in endoVar have to match the names as they appear in the data")
  
mcl1 <- match.call()  
#### call the main function multilevelGMM ####
Interim <- suppressWarnings(multilevelGMM(formula, endoVar = endoVar, data = data))
Levels <- Interim$Levels
W <- Interim$Wmat    # weight matrix
V <- Interim$Vmat    # varcovar matric
X <- Interim$X       # regressors
y <- Interim$y       # dep. variable
Z <- Interim$Z       # slope parameter
### HIV are the instruments used, at different levels - c is level 2 (child), s - level 3 (school) ####
HIVc1 <- Interim$HIVc1    # fixed effects level 2
HIVc2 <- Interim$HIVc2    # GMM level 2
HIVs1 <- Interim$HIVs1    # if 3 levels model - fixed effects level 3/ if 2 levels model - fixed effect slevel 2
HIVs2 <- Interim$HIVs2    # if 3 levels model - GMM level 3/ if 2 levels - GMM level 2
predictors <- Interim$pred
obsid <- Interim$obsid    # id of each observation
nTot <- Interim$nTot      # total number of observations
endo.env <- Interim$envir   # environment

###  GMM estimation  ###
printGMM <- function(y,X,HIV,id,Wmat, Vmat, nLevels, nTot, obsid, envir){
  
  estimGMM <- GmmEstim(y,X,HIV,id,Wmat, Vmat, nLevels, nTot, obsid, envir)
  b <- estimGMM$bIV
  se <- round(estimGMM$MSdError,3)
  z <- round(b/se,3)
  pval <- round(2*stats::pnorm(-abs(z)),3)
  printRes <- list(b=b,se=se, z=z, pval=pval, estim = estimGMM)
  return(printRes)
}

### Function to Print Results ####
print.res <- function(coeff, stderr,z,pval){
  z[!is.finite(z)] <- NA
  pval[!is.finite(pval)] <- NA
  b <- cbind(coeff,stderr,z,pval)
  colnames(b) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(b) <- predictors
  return(b)
}
#### different estimators are produced depending on the level - 3 or 2 levels #####
if (Levels==3) {

  resFEc <- printGMM(y,X,HIVc1,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFE_L2 <- resFEc$estim
  bFEc  <- resFEc$b
  sebFEc <- resFEc$se
  zFEc <- resFEc$z
  pvalFEc <- resFEc$pval
  
  resGMMc <- printGMM(y,X,HIVc2,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMM_L2 <- resGMMc$estim
  bGMMc <- resGMMc$b
  sebGMMc <- resGMMc$se
  zGMMc <- resGMMc$z
  pvalGMMc <- resGMMc$pval
  
  resFEs <- printGMM(y,X,HIVs1,id=2,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFE_L3 <- resFEs$estim
  bFEs <- resFEs$b
  sebFEs <- resFEs$se
  zFEs <- resFEs$z
  pvalFEs <- resFEs$pval

  resGMMs <- printGMM(y,X,HIVs2,id=2,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMM_L3 <- resGMMs$estim
  bGMMs <- resGMMs$b
  sebGMMs <- resGMMs$se
  zGMMs <- resGMMs$z
  pvalGMMs <- resGMMs$pval

  HREE <- W %*% as.matrix(X)
  resREE <- printGMM(y,X,HREE,id=0,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  b_REF <- resREE$estim
  bREE <- resREE$b
  sebREE <- resREE$se
  zREE <- resREE$z
  pvalREE <- resREE$pval
  

  
  ######################## Omitted Variable Test ####################

 # ovt_REF_FEL2   <- OmittedVarTest(m1=bFE_L2,m2=b_REF,HIV1 = HIVc1, HIV2=HREE,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  ovt_REF_GMML2  <-  OmittedVarTest(m1=bGMM_L2,m2=b_REF,HIV1=HIVc2, HIV2 = HREE, id=1, nLevels=Levels,nTot=nTot, obsid=obsid, envir=endo.env)
  ovt_REF_GMML3  <-  OmittedVarTest(m1=bGMM_L3,m2=b_REF,HIV1 = HIVs2, HIV2 = HREE,id=2,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  ovt_REF_FEL3   <- OmittedVarTest(m1=bFE_L3,m2=b_REF,HIV1 = HIVs1, HIV2 = HREE,id=2,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)      
  ovt_REF_FEL2   <- OmittedVarTest(m1=bFE_L2,m2=b_REF,HIV1 = HIVc1, HIV2 =HREE,id=2,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)      
  

  ovt_GMML2_GMML3 <- OmittedVarTest(m1=bGMM_L2,m2=bGMM_L3,HIV1 = HIVc2, HIV2 = HIVs2,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  ovt_GMML2_FEL3 <- OmittedVarTest(m1=bGMM_L2,m2=bFE_L3,HIV1 = HIVc2, HIV2 = HIVs1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)

  ovt_GMML3_FEL2 <- OmittedVarTest(m1=bFE_L2,m2=bGMM_L3,HIV1 = HIVc1, HIV2 = HIVs2,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)

  ovt_FEL2_FEL3 <- OmittedVarTest(m1=bFE_L2,m2=bFE_L3,HIV1 = HIVc1, HIV2 = HIVs1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  ovt_FEL2_GMML2 <- OmittedVarTest(m1=bGMM_L2,m2=bFE_L2,HIV1 = HIVc2, HIV2 = HIVc1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  
  ovt_REF   <- cbind(FEL2_vs_REF = ovt_REF_FEL2,GMML2_vs_REF = ovt_REF_GMML2,FEL3_vs_REF = ovt_REF_FEL3, GMML3_vs_REF = ovt_REF_GMML3)
  ovt_GMML3 <- cbind(FEL2_vs_GMML3 = ovt_GMML3_FEL2, GMML2_vs_GMML3 = ovt_GMML2_GMML3, GMML3_vs_REF = ovt_REF_GMML3)
  ovt_GMML2 <- cbind(FEL2_vs_GMML2 = ovt_FEL2_GMML2,GMML2_vs_FEL3 = ovt_GMML2_FEL3, GMML2_vs_GMML3 = ovt_GMML2_GMML3, GMML2_vs_REF = ovt_REF_GMML2)
  ovt_FEL3  <- cbind(FEL2_vs_FEL3 = ovt_FEL2_FEL3, GMML2_vs_FEL3 = ovt_GMML2_FEL3, FEL3_vs_REF = ovt_REF_FEL3)
  ovt_FEL2  <- cbind(FEL2_vs_GMML2 = ovt_FEL2_GMML2,FEL2_vs_FEL3 = ovt_FEL2_FEL3, FEL2_vs_GMML3 = ovt_GMML3_FEL2)
  
  OVT <- list(REF=ovt_REF,GMM_L3 = ovt_GMML3,GMM_L2 = ovt_GMML2,FE_L3 = ovt_FEL3, FE_L2 = ovt_FEL2)
  
  ### Results ###
  bFE_Lev2 <- print.res(bFEc,sebFEc, zFEc, pvalFEc)
  bFE_Lev3 <- print.res(bFEs,sebFEs, zFEs, pvalFEs)
  bGMM_Lev2 <- print.res(bGMMc,sebGMMc,zGMMc, pvalGMMc)
  bGMM_Lev3 <- print.res(bGMMs,sebGMMs,zGMMs, pvalGMMs)
  bRanEff <- print.res(bREE,sebREE,zREE, pvalREE)
  
  coef <- cbind(bFEc,bGMMc,bFEs,bGMMs, bREE)
  colnames(coef) <- c("FE_L2","GMM_L2","FE_L3","GMM_L3","RandomEffects")
  rownames(coef) <- predictors
  coefSdErr <- list(FE_L2 = bFE_Lev2, FE_L3 = bFE_Lev3, GMM_L2 = bGMM_Lev2, GMM_L3 = bGMM_Lev3, REF = bRanEff)
  
  results <- list(coefficients=coef, coefSdErr=coefSdErr,vcovMat = V, weightMat = W, formula = formula, model = data, ovt=OVT)
  
} else if (Levels==2){

  resFEc <- printGMM(y,X,HIVs1,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFE_L2 <- resFEc$estim
  bFEc <- resFEc$b
  sebFEc <- resFEc$se
  zFEc <- resFEc$z
  pvalFEc <- resFEc$pval
 
  resGMMc <- printGMM(y,X,HIVs2,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMM_L2 <- resGMMc$estim
  bGMMc <- resGMMc$b
  sebGMMc <- resGMMc$se
  zGMMc <- resGMMc$z
  pvalGMMc <- resGMMc$pval
  # 
  HREE <- W %*% as.matrix(X)
  
  resREE <- printGMM(y,X,HREE,id=0,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  b_REF <- resREE$estim
  bREE <- resREE$b
  sebREE <- resREE$se
  zREE <- resREE$z
  pvalREE <- resREE$pval
  
  ########################  Print Omitted Variable Test ####################
  FEL2_vs_REF   <- OmittedVarTest(m1=bFE_L2,m2=b_REF,HIV1 = HREE, HIV2=HIVs1,id=1,nLevels=Levels,nTot=nTot,obsid=obsid, envir=endo.env)
  GMML2_vs_REF  <-  OmittedVarTest(m1=bGMM_L2,m2=b_REF,HIV1=HIVs2, HIV2=HREE, id=1, nLevels=Levels,nTot=nTot, obsid=obsid, envir=endo.env)
  FEL2_vs_GMML2 <-  OmittedVarTest(m1=bFE_L2,m2=bGMM_L2,HIV1=HIVs1, HIV2=HIVs2, id=1, nLevels=Levels,nTot=nTot, obsid=obsid, envir=endo.env)
  
  OVT <- list(REF = cbind(GMML2_vs_REF,FEL2_vs_REF, FEL2_vs_GMML2), GMM_L2=cbind(GMML2_vs_REF,FEL2_vs_REF, FEL2_vs_GMML2),FE_L2=cbind(GMML2_vs_REF,FEL2_vs_REF,FEL2_vs_GMML2))
 
  ###  Results ###
  bFE_Lev2 <- print.res(bFEc,sebFEc, zFEc, pvalFEc)
  bGMM_Lev2 <- print.res(bGMMc,sebGMMc,zGMMc, pvalGMMc)
  bRanEff <- print.res(bREE,sebREE,zREE, pvalREE)
  
  coefSdErr <- list(FE_L2 = bFE_Lev2, GMM_L2 = bGMM_Lev2, REF = bRanEff)
  coef <- cbind(bFEc, bGMMc, bREE)
  colnames(coef) <- c("FixedEffects","GMM","RandomEffects")
  rownames(coef) <- predictors
  results <- list(coefficients=coef, coefSdErr = coefSdErr,vcovMat = V, weightMat = W, formula = formula, model = data, ovt = OVT)
  
}

object <- methods::new("mixedREndo")

object@coefficients <- results$coefficients    # only coefficients 
object@coefSdErr    <- results$coefSdErr    # coefficients with sdErrors and t-values
object@vcovMat      <- results$vcovMat
object@weightMat    <- results$weightMat
object@mixedCall    <- mcl1
object@omittedVarTest <- results$ovt
object
}

