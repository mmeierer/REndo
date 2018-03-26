# Copyright (c) 2016. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# Proj: REndo R package
# Desc: Kim and Frees 2007 replication paper
# Auth: Raluca Gui (RG) # Date: 2015/02/15


#'@title     Multilevel GMM estimation
#'@description    the backbone function for the multilevelIV function
#'@param    formula  an object of type 'formula': a symbolic description of the model to be fitted. 
#'@param    endoVar  a matrix or data frame containing the variables assumed to be endogenous.  
#'@param    data  optional data frame or list containing the variables of the model.
#'@return   returns the instrumental variables used in estimating the coefficients, variance-covariance matrix, the weight matrix used in estimating the model, etc.
#'@keywords internal
# endoVar should be a matrix with the endogenous variables
multilevelGMM <- function(formula, endoVar, data = NULL) {
rendo.env <- new.env()    # create a REndo pkg environment
mf <- model.frame(formula = formula, data = data)
y <- mf[,1]
nTot <- length(y)
regs <- unlist(strsplit(as.character(formula)[3], "\\(")) 
nLevels <- length(regs)    # the number of levels
predictors <- unlist(strsplit(regs[1], " [+] "))

# list of the random part of the formula
rTrms <- lme4::lFormula(formula = formula, data=data)$reTrms
# list of grouping factors used in the random-effects terms- e.g. id, school number, etc.
listIDs <- rTrms$flist
# max number of levels of the first grouping factor - level 2
assign("nLev2", max(as.numeric(as.character(listIDs[[1]]))), envir=rendo.env)

data <- as.data.frame(data)
lev2ID <- as.numeric(unlist(listIDs[[1]]))
assign("idL2", as.numeric(unlist(listIDs[1])), envir=rendo.env) # Level2
maxL2 <- max(rendo.env$idL2)    
minL2 <- min(rendo.env$idL2)  
if (nLevels==3){
assign("idL3", as.numeric(unlist(listIDs[2])), envir=rendo.env)
maxL3 <- max(rendo.env$idL3)    
minL3 <- min(rendo.env$idL3)    
assign("nLev3", max(as.numeric(unlist(listIDs[[2]]))), envir=rendo.env)

}
## Z contains slope variables - assumed to be exogenous
Z1 <- list()
zz <- matrix(c(1), nTot, 1)
# for 3 levels rTrms#cnms always has length 2
if (nLevels==2){
 if (length(rTrms$cnms[[1]])==1)
  {Z1[[1]] <- zz} else {
  for (i in 2:length(rTrms$cnms[[1]]))
    Z1[[1]] <- cbind(zz,data[,rTrms$cnms[[1]][[i]]])
}
} else if (nLevels==3){
  if ((length(rTrms$cnms[[2]])==1) & (length(rTrms$cnms[[1]])==1)) {
        Z1[[1]] <- zz
        Z1[[2]] <- zz
     } 
  if ((length(rTrms$cnms[[2]])==1) & (length(rTrms$cnms[[1]])==2)){
        Z1[[2]] <- zz
       for (i in 2:length(rTrms$cnms[[1]]))
         Z1[[1]] <- cbind(zz,data[,rTrms$cnms[[1]][[i]]]) 
     }
   if ((length(rTrms$cnms[[2]])==2) & (length(rTrms$cnms[[1]])==2)){
       for (i in 2:length(rTrms$cnms[[1]]))
            {Z1[[1]] <- cbind(zz,data[,rTrms$cnms[[1]][[i]]])}
        for (i in 2:length(rTrms$cnms[[2]]))
            {Z1[[2]] <- cbind(zz,data[,rTrms$cnms[[2]][[i]]])}
     }  
  if ((length(rTrms$cnms[[2]])==2) & (length(rTrms$cnms[[1]])==1)){
    Z1[[1]] <- zz
    for (i in 2:length(rTrms$cnms[[2]]))
      Z1[[2]] <- cbind(zz,data[,rTrms$cnms[[2]][[i]]]) 
  }
}
Z <- Z1
# get the regressors from formula 
mf1 <- mf[-1]    # first in mf is the response

regressors <- mf1[-((dim(mf1)[2] - nLevels+2):dim(mf1)[2])]
X <- cbind(zz,regressors)
predictors <- c("Intercept", predictors) 
colnames(X) <- predictors

K <- dim(X)[2];
q <- dim(Z[[1]])[2]; 

#  X1 contains the exogenous variables plus the intercept - X - endoVar
# ATTENTION!  X and endoVar should have the same colnames!!!!
X1 <- X[,-which(colnames(X) %in% colnames(endoVar))]
K1 <- dim(X1)[2]

obsid <- t(1:nTot)

Qs <- matrix(c(0),nTot,nTot)
#  the var-covar matrix
V <- diag(x=1,nTot,nTot)
#  the weight matrix 
W <- diag(x=1, nTot, nTot)
# --------------------------------------
# in order to get the variances of the random coefficients, I run random effects model using lmer() and I save the variances
# I use these estimators to create the covariance matrix that I need afterwards for the weight matrix
# --------------------------------------
#source("mixedGMM_VarCorr.R")
var.Cor <- VarCor(formula=formula, data=data,nLevels=nLevels, rTrms=rTrms)
D.2 <- var.Cor$D.2
D.3 <- var.Cor$D.3
sigma.sq <- var.Cor$sigma.sq

####################################################################################################
#----------------------------------------------------------------------------------------------------
# computes the overall covariance matrix and the weight matrix (W) = inverse of the covariance matrix
#----------------------------------------------------------------------------------------------------
####################################################################################################
# Computes Var-Covar matrix at level 2 
if (nLevels==3) {
for (scount in minL3:maxL3 ){
  
  #counts the number of obs in Level3
  numL3 <- table(rendo.env$idL3)[[scount]]
  
  if (numL3==0)  {cat("End")}
  else {
    
    # locate the position of school "scount" (1,...,60)
    iloc <- which(rendo.env$idL3==scount)
    # get the L2 id for the given level3 id
    L2IDs <- as.numeric(as.character(listIDs[[1]]))[iloc]
    
    # I put in Zs only the obs. for the given L3 level
    Zl33 <- as.matrix(Z[[2]][iloc,])
    
    Zl32 <- as.matrix(Z[[1]][iloc,])
    
     Vi.c <- Zl33 %*% (D.3 %*% t(Zl33)) + Matrix::bdiag(sigma.sq *  diag(1,numL3,numL3))
    
    
    idnum <- L2IDs
    s <- unique(idnum)
    
    for(sc in 1:length(s))
    {
      # count the number of times the same child appears in ChildIDs
      Tsc <- table(idnum)[[sc]]
      
      if (Tsc == 0) {print("End")
      } else{
        # locate the position where the child with id  is located in idnumS = arary of children ids
        scloc <- which(idnum %in% s[sc])
        
        if (Tsc==1){
          if (length(rTrms$cnms[[1]])==1){
            Vi.c[scloc,scloc] <- Vi.c[scloc,scloc]+ t(as.matrix(Zl32[scloc])) %*% (D.2 %*% as.matrix(Zl32[scloc]))
          } else {
          Vi.c[scloc,scloc] <- Vi.c[scloc,scloc]+ t(as.matrix(Zl32[scloc,])) %*% (D.2  %*% as.matrix(Zl32[scloc,]))
          }
        }else 
        {
          if (length(rTrms$cnms[[1]])==1){
            Vi.c[scloc,scloc] <- Vi.c[scloc,scloc]+ as.matrix(Zl32[scloc]) %*% (D.2 %*% t(Zl32[scloc])) 
          } else{
          Vi.c[scloc,scloc] <- Vi.c[scloc,scloc]+ as.matrix(Zl32[scloc,]) %*% (D.2 %*% t(Zl32[scloc,])) 
          }
        }
      }
    }
  }
    V[iloc,iloc] <- as.matrix(Vi.c)
    # computing W = sqrt(inverse(V))
    ei <- eigen(as.matrix(Vi.c))
    valS <- ei$values
    valS[valS < 0] <- 0 
    sValS <- 1/sqrt(valS) 
    sValS[ valS==0] <- 0 
    evecS <- ei$vectors
    
    W[iloc,iloc] <- evecS %*% diag(sValS) %*% t(evecS)
  }
} else if(nLevels==2) {
    for (scount in minL2:maxL2 ){

      #counts the number of obs in Level3
      numL2 <- table(rendo.env$idL2)[[scount]]

      if (numL2==0)  {cat("End")
      } else {

        # locate the position of school "scount" (1,...,60)
        iloc <- which(rendo.env$idL2==scount)
        # get the L2 id for the given level3 id
      #  L2IDs<- as.numeric(as.character(listIDs[[1]]))[iloc]
        Zl32 <- as.matrix(Z[[1]][iloc,])
        Vi.c <- Matrix::bdiag(sigma.sq *  diag(1,numL2,numL2))
        if (length(iloc)==1)   {
            Vi.c <- Vi.c+ t(as.matrix(Zl32)) %*% (D.2 %*% as.matrix(Zl32))
          }else
            {
              Vi.c <- Vi.c+ as.matrix(Zl32) %*% (D.2 %*% t(Zl32))
            }

        }
        V[iloc,iloc] <- as.matrix(Vi.c)
        # computing W = sqrt(inverse(V))
        ei <- eigen(as.matrix(Vi.c))
        valS <- ei$values
        valS[valS < 0] <- 0
        sValS <- 1/sqrt(valS)
        sValS[ valS==0] <- 0
        evecS <- ei$vectors
        if (length(iloc)==1) {
          W[iloc,iloc] <- evecS %*% sValS %*% t(evecS)
        } else {
        W[iloc,iloc] <- evecS %*% diag(sValS) %*% t(evecS)
        }
  }
    }
  
#------------------------------------------------------------------------------------------------------------------
#  compute the IVs to be used in the GMM estimation, at the 2nd level, HIVc1 and HIVc2, and 3rd level HIVs1 and HIVs2
# -----------------------------------------------------------------------------------------------------------------
# 
qHIVc1 <- K
HIVc1 <- matrix(c(0),nTot, qHIVc1)
qHIVc2 <- K + dim(X1)[2]
HIVc2 <-  matrix(c(0),nTot, qHIVc2)
qHIVs1 <- K
HIVs1 <- matrix(c(0),nTot,qHIVs1)
qHIVs2 <- K + dim(X1)[2]
HIVs2 <- matrix(c(0),nTot,qHIVs2)


if (nLevels==3) {   # if there are 3 levels
  
  s1 <- unique(rendo.env$idL2)
  
  for (sc in 1:length(s1)){
    Tl2 <- table(rendo.env$idL2)[[sc]]  # count level2 id's
    if (Tl2!=0) {
      
      scloc <- which(rendo.env$idL2 %in% sc)
      Xl2 <- data.matrix(X[scloc,])
      X1l2 <- data.matrix(X1[scloc,])
      Zl2 <- Z[[1]][scloc,]
      Wl2 <- as.matrix(W[scloc,scloc])
      
      # if only 1 obs per child - the matrix computation has to be treated different 
      if (Tl2==1){  
        
        # Qsc = projection matrix => deviations from the time series averages
        Ql2 <- diag(Tl2) -  Wl2 %*% t(Zl2)  %*% corpcor::pseudoinverse(Zl2 %*%  (Wl2 %*% Wl2) %*% t(Zl2) ) %*% Zl2 %*%  Wl2 
        # Psc transforms vectors in time-series averages
        Pl2 <- diag(Tl2)- Ql2
        
        HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)  # all endogeneous - fixed effect
        HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))
      } else
      {
        Ql2 <- diag(Tl2) -  Wl2 %*% Zl2 %*% corpcor::pseudoinverse(t(Zl2) %*% (Wl2 %*% Wl2) %*% Zl2) %*% crossprod(Zl2,Wl2)
        Pl2 <- diag(Tl2)- Ql2  
        HIVc1[scloc,] <- Ql2 %*% (Wl2 %*% Xl2)
        HIVc2[scloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))
        
      }  
    } else {
      cat("END")}
  }
  #---------------------------------------------------------------------------------------
  #  comute the IVs to be used in the GMM estimation, at the 3nd level, HIVs1 and HIVs2 
  # --------------------------------------------------------------------------------------
  for (scount in minL3:maxL3) {
    
    #CT = how many times does school scount appear
    CT <- table(rendo.env$idL3)[[scount]]
    if (CT==0) {print("END") 
    } else {
      # sloc = the position of school scount
      sloc <- which(rendo.env$idL3==scount)
      Xl3 <- data.matrix(X[sloc,])
      X1l3 <- data.matrix(X1[sloc, ])
      Zl3 <- Z[[2]][sloc,]
      Wl3 <- W[sloc,sloc]
      
      Ql3 <- diag(CT) - Wl3 %*% Zl3 %*% corpcor::pseudoinverse(crossprod(Zl3, Wl3) %*% Wl3 %*% Zl3) %*% crossprod(Zl3, Wl3)
      Pl3 <- diag(CT) - Ql3
      HIVs1[sloc,] <- Ql3 %*% (Wl3 %*% Xl3)
      HIVs2[sloc,] <- cbind(Ql3 %*% (Wl3 %*% Xl3), Pl3 %*% (Wl3 %*% X1l3))
    } 
  } 
  } else if (nLevels==2){
   
  for (scount in minL2:maxL2) {    # if there are 2 levels
  
    CT <- table(rendo.env$idL2)[[scount]]
    if (CT==0) {print("END") 
    } else {
      # sloc = the position of school scount
      sloc <- which(rendo.env$idL2==scount)
      Xl2 <- data.matrix(X[sloc,])
      X1l2 <- data.matrix(X1[sloc, ])
      Zl2 <- as.matrix(Z[[1]][sloc,])
      Wl2 <- W[sloc,sloc]
 
      if (CT==1) {
        Ql2 <- diag(CT) - tcrossprod(Wl2,Zl2) %*% corpcor::pseudoinverse(Zl2 %*% Wl2 %*% tcrossprod(Wl2,Zl2)) %*% Zl2 %*% Wl2 
        Pl2 <- diag(CT) - Ql2
        HIVs1[sloc,] <- Ql2 %*% Wl2 %*% Xl2
        HIVs2[sloc,] <- cbind(Ql2 %*% Wl2 %*% Xl2, Pl2 %*% Wl2 %*% X1l2)
      } else {
      Ql2 <- diag(CT) - Wl2 %*% Zl2 %*% corpcor::pseudoinverse( crossprod(Zl2,Wl2)  %*% Wl2 %*% Zl2) %*% crossprod(Zl2, Wl2) 
      Pl2 <- diag(CT) - Ql2
      HIVs1[sloc,] <- Ql2 %*% (Wl2 %*% Xl2)
      HIVs2[sloc,] <- cbind(Ql2 %*% (Wl2 %*% Xl2), Pl2 %*% (Wl2 %*% X1l2))
      }
    } 
  }
}
InterimRes <- list(Levels= nLevels, Wmat = W, Vmat=V, HIVc1=HIVc1, HIVc2=HIVc2, HIVs1=HIVs1, HIVs2=HIVs2, pred=predictors, X=X, y=y, Z=Z, nTot=nTot, obsid=obsid, envir=rendo.env)
InterimRes
}
##############################################################################
####################### VARCOVAR MATRIX ######################################
##############################################################################
VarCor <- function(formula, data, nLevels,rTrms){
  
  modelREE <- lme4::lmer(formula=formula, data=data)
  VC <- as.data.frame(lme4::VarCorr(modelREE))[,4] # get D.2, D.3 and random error
  VC <- as.matrix(VC)
  
  if (nLevels == 3) { # 3 LEVELS
    
    # random slope at level 2
    if ((length(rTrms$cnms[[1]])==2) & (length(rTrms$cnms[[2]])==1)){
      D.2 <- matrix(c(VC[1],VC[3],VC[3],VC[2]),2,2)
      D.3 <- VC[4]
      sigma.sq <- VC[5]
    }
    # random slope at level 3
    if ((length(rTrms$cnms[[1]])==1) & (length(rTrms$cnms[[2]])==2)) {
      D.2 <- VC[1]
      D.3 <- matrix(c(VC[2],VC[4],VC[4],VC[3]),2,2)
      sigma.sq <- VC[5]
    }
    # 2 random slopes at level 3
    if ((length(rTrms$cnms[[1]])==1) & (length(rTrms$cnms[[2]])==3)) {
      # if random slope at level 3
      D.2 <- VC[1]
      D.3 <- matrix(c(VC[2],VC[5],VC[6],VC[5], VC[3], VC[7], VC[6], VC[7],VC[4]),3,3)
      sigma.sq <- VC[8]
    }
    # random slopes at level 2 and 3
    if ((length(rTrms$cnms[[1]])==2) & (length(rTrms$cnms[[2]])==2)) {
      # if random slope at level 3
      D.2 <- matrix(c(VC[1],VC[3],VC[3],VC[2]),2,2)
      D.3 <- matrix(c(VC[4],VC[6],VC[6],VC[5]),2,2)
      sigma.sq <- VC[7]
    }
    
    # only random intercepts at both levels
    if ((length(rTrms$cnms[[1]])==1) & (length(rTrms$cnms[[2]])==1)) {
      # if random slope at level 3
      D.2 <- VC[1]
      D.3 <- VC[2]
      sigma.sq <- VC[3]
    }
    VarCorRes <- list(D.2 = D.2, D.3=D.3, sigma.sq=sigma.sq) 
    
  } else {  # 2 LEVELS
    
    # get D.2 and random error. D.2 is the covariance matrix at level 2 
    #  only intercept vs. intercept and slope
    if (dim(VC)[1]==2) {
      D.2 <- VC[1]
      sigma.sq <- VC[2]
    } else
    {
      D.2 <- matrix(c(VC[1],VC[3],VC[3],VC[2]),2,2)
      sigma.sq <- VC[4]
    }
    VarCorRes <- list(D.2 = D.2, sigma.sq=sigma.sq)  
  }
 
  VarCorRes  
}
##############################################################################
###################### GMM Estimation ########################################
##############################################################################
GmmEstim <- function(y,X,HIV,id, Wmat, Vmat, nLevels, nTot, obsid, envir){

  Wf <- data.matrix(Wmat)
  Vf <- data.matrix(Vmat)
 
  if (nLevels==3) {    
    numindep <- (id==0)*nTot + (id==1) * envir$nLev2 + (id==2)*envir$nLev3
  } else {
    numindep <- (id==0)*nTot + (id==1) * envir$nLev2 
  }
  yf <- Wf %*% y
  Xf <- Wf %*% data.matrix(X)
  
  # see KM07 Appendix 1
  GHat <- t(HIV) %*% Xf/numindep
  MHH <- t(HIV) %*% HIV/numindep
  ginvMHH <- corpcor::pseudoinverse(MHH)
  GammaH <- corpcor::pseudoinverse(t(GHat) %*% ginvMHH %*% GHat)%*% t(GHat) %*% ginvMHH
  bIV <- GammaH %*% (t(HIV) %*% yf) / numindep
  
  residW <- yf - Xf %*% bIV
  
  # ------------------------------------------
  # Model std. error calculation
  # ------------------------------------------
  
  if (nLevels==3) {
    
  idnumN <- (id==0)*obsid +(id==1)*envir$idL2 + (id==2)*envir$idL3}
  else { idnumN <- (id==0)*obsid +(id==1)*envir$idL2}
  numinstruments <- ncol(HIV)
  
  Lambda <- matrix(0,numinstruments, numinstruments)
  LambdaHat <- matrix(0,numinstruments, numinstruments)
  
  for (ccount in 1:numindep) {
    
    # location of ccount in idnum
    iloc <- which(idnumN==ccount)
    residWi <- residW[iloc,]
    
    # there is a pb if a childID appears just once- rowSums gives error-it needs residWi to have 2 dimensions - so we create special case when residWi is a number
    if (length(iloc) !=1){
      rwrw <- tcrossprod(residWi)
      rwrw <- Wf[iloc,iloc] %*% (as.matrix(rwrw) %*% Wf[iloc,iloc])
      Vf[iloc,iloc] <- Wf[iloc,iloc] %*% Vf[iloc,iloc] %*% Wf[iloc,iloc]
      
      
      HIV[iloc,] <- data.matrix(HIV[iloc,])
      LambdaHat <- LambdaHat + crossprod(HIV[iloc,], as.matrix(rwrw)) %*%  HIV[iloc,]
      Lambda <- Lambda + t(HIV[iloc,]) %*% Vf[iloc,iloc] %*% HIV[iloc,]
    } else {
      rwrw <- Wf[iloc,iloc] * residWi * Wf[iloc,iloc]
      rwrw <- tcrossprod(rwrw)
      Vf[iloc,iloc] <- Wf[iloc,iloc] %*% Vf[iloc,iloc] %*% Wf[iloc,iloc]
      LambdaHat <- LambdaHat +  HIV[iloc,] * (as.vector(rwrw) * HIV[iloc,])
      Lambda <- Lambda + HIV[iloc,] * Vf[iloc,iloc] * HIV[iloc,]
    } 
  }
  
  LambdaHat <- LambdaHat/numindep
  Lambda <- MHH
  MVarbIV <- GammaH %*% tcrossprod(Lambda, GammaH)/numindep
  dd <- diag(MVarbIV)
  Mstderr_bIV <-sqrt(matrix(dd,length(dd),1))
  Mstderr_bIV <-round(Mstderr_bIV,4)
  bbIV <-round(bIV,3)
  dd <- list(bIV=bbIV, MSdError=Mstderr_bIV, resid = residW, Gamma = GammaH)
  dd
}

##############################################################################
#######################  Omitted Variable Test ##############################
##############################################################################
OmittedVarTest <- function(HIV1,HIV2,m1,m2,id,nLevels, nTot, obsid, envir){
  
  if (nLevels==3){
  idnum <- (id==0)*obsid + (id==1) *envir$idL2  + (id==2)*envir$idL3} else{
    
    idnum <- (id==0)*obsid + (id==1) *envir$idL2 }
   
  r1 <- ncol(HIV1);  r2 <- ncol(HIV2);
  
  LambdaHat11 <- matrix(c(0),r1,r1)
  LambdaHat22 <-  matrix(c(0),r2,r2)
  LambdaHat12 <-  matrix(c(0),r1,r2)
  numindep <- nTot
  
  GammaH1 <- m1$Gamma
  GammaH2 <- m2$Gamma
  for (ccount in 1: numindep){
    
    iloc <- which(idnum == ccount);
    
    residWi <- as.matrix(m2$resid[iloc,])
    
    if (length(iloc)!=1) {
      
      rwrw <- residWi %*% t(residWi)
      
      
      LambdaHat11 <- LambdaHat11 + t(HIV1[iloc,]) %*% rwrw %*% HIV1[iloc,]
      LambdaHat22 <- LambdaHat22 + t(HIV2[iloc,]) %*% rwrw %*% HIV2[iloc,]
      LambdaHat12 <- LambdaHat12 + t(HIV1[iloc,]) %*% rwrw %*% HIV2[iloc,]
    } else 
    {    rwrw <- t(residWi) %*% residWi
    
    LambdaHat11 <- LambdaHat11 + HIV1[iloc,] %*% as.matrix(rwrw) %*% t(HIV1[iloc,])
    LambdaHat22 <- LambdaHat22 + HIV2[iloc,] %*% as.matrix(rwrw) %*% t(HIV2[iloc,])
    LambdaHat12 <- LambdaHat12 + HIV1[iloc,] %*% as.matrix(rwrw) %*% t(HIV2[iloc,])
    }
  }
  LambdaHat11 <- LambdaHat11/numindep
  LambdaHat22 <- LambdaHat22/numindep
  LambdaHat12 <- LambdaHat12/numindep;
  
  
  OmegaHat <- GammaH2 %*% LambdaHat22 %*% t(GammaH2) + GammaH1 %*% LambdaHat11 %*% t(GammaH1) - GammaH1 %*% LambdaHat12 %*% t(GammaH2) -  t(GammaH1 %*% LambdaHat12 %*% t(GammaH2))
  
  commonrows <- which(m1$bIV!=0) 
  dbeta <- m1$bIV[commonrows,] - m2$bIV[commonrows,]
  diffbeta <- m1$bIV[commonrows,] - m2$bIV[commonrows,]
  diff_VarCov <- corpcor::make.positive.definite(OmegaHat[commonrows, commonrows]/numindep)
  stat_ht <- as.numeric(t(diffbeta) %*% solve(diff_VarCov,diffbeta))
  names(stat_ht) <- "chisq"
  df <- Matrix::rankMatrix(OmegaHat[commonrows, commonrows])
  pval_ht <- pchisq(stat_ht,df=df,lower.tail = FALSE)
  
  res <- list(stat=stat_ht, df = df, pval = pval_ht)
  
}

##############################################################################
####################### MISC Functions########################################
##############################################################################
oneof <- function(x){
  x <- names(x)
  last <- x[length(x)]
  x <- x[-length(x)]
  x <- paste(x,collapse=", ")
  x <- paste(x,last,sep=" and ")
  x
}

model.list.3levels <- c(FE_L2 = "FE_L2",
                        FE_L3  = "FE_L3",
                        GMM_L2 = "GMM_L2",
                        GMM_L3 = "GMM_L3",
                        REF    = "REF"
)
model.list.2levels <- c(FE_L2 = "FE_L2",
                        GMM_L2 = "GMM_L2",
                        REF     = "REF"
)