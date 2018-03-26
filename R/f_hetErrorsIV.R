
gmmcl <- function(formula1, formula2, data, cluster){
 
  # create data.frame
  data$id1 <- 1:dim(data)[1]
  formula3 <- paste(as.character(formula1)[3],"id1", sep=" + ")
  formula4 <- paste(as.character(formula1)[2], formula3, sep=" ~ ")
  formula4 <- as.formula(formula4)
  formula5 <- paste(as.character(formula2)[2],"id1", sep=" + ")
  formula6 <- paste(" ~ ", formula5, sep=" ")
  formula6 <- as.formula(formula6)
  frame1 <- model.frame(formula4, data)
  frame2 <- model.frame(formula6, data)
  dat1 <- plyr::join(data, frame1, type="inner", match="first")
  dat2 <- plyr::join(dat1, frame2, type="inner", match="first")
  
  # matrix of instruments
  Z1 <- model.matrix(formula2, dat2)
  
  # step 1
  gmm1 <- gmm:: gmm(formula1, formula2, data = dat2, vcov="iid")
  
  # clustering weight matrix
  cluster <- factor(dat2[,cluster])
  u <- residuals(gmm1)
  estfun <- sweep(Z1, MARGIN=1, u,'*')
  u <- apply(estfun, 2, function(x) tapply(x, cluster, sum))  
  S <- 1/(length(residuals(gmm1)))*crossprod(u)
  
  # step 2
  gmm2 <- gmm::gmm(formula1, formula2, data=dat2, 
             vcov="TrueFixed", weightsMatrix = chol2inv(chol(S)))
  return(gmm2)
}

ftest <- function(data.new, ne, nx, nz, nh, robust, clustervar){
  # get endogenous variable projections 
  cols.1 <- (1:ncol(data.new))[colnames(data.new)!=clustervar]
  if(is.null(clustervar)){
    data.temp <- data.new
  } else {
    data.temp <- data.new[,(1:ncol(data.new))[colnames(data.new)!=clustervar]]
  }
  
  # get projections
  for(i in 1:ne){
    d.temp <- as.matrix(cbind(data.temp[,(i+1)],data.temp[,(2+ne):ncol(data.temp)]))
    r.temp <- stats::lm(d.temp[,1] ~ -1 + d.temp[,-1])
    data.temp[[paste("py2",i,sep=".")]] <- fitted.values(r.temp)
  }
  
  f.test.stats <- rep(NA, ne)
  
  # get residuals and run ftests
  for(i in 1:ne){
    # make formula
    lhs.1 <- paste("y2",i,sep=".")
    rhs.1 <- ifelse(ne>1, paste("py2", (1:ne)[!(1:ne %in% i)], sep=".", collapse=" + "), "")
    rhs.2 <- paste("x1", 1:nx, sep=".", collapse=" + ")
    rhs.3 <- ifelse(ne>1, paste(rhs.1, rhs.2, sep = " + "), rhs.2)
    form.1 <- as.formula(paste(lhs.1, rhs.3, sep=" ~ -1 + "))
    # reg endog regressor on exog
    data.temp[["residuals"]] <- residuals(stats::lm(form.1, data.temp))
    # take residuals and run on IV
    rhs.4 <- ifelse(nz>0, paste("z1", 1:nz, sep = ".", collapse=" + "), "")
    rhs.5 <- paste("het.z", 1:nh, sep = "", collapse=" + ")
    rhs.6 <- ifelse(nz>0, paste(rhs.4, rhs.5, sep = " + "), rhs.5)
    form.2 <- as.formula(paste("residuals", rhs.6, sep=" ~ -1 + "))
    
    m1 <- stats::lm(form.2, data.temp)
    m2 <- stats:: lm(residuals ~ -1 , data.temp)
    
    if(is.null(clustervar) & robust == FALSE){f1 <- lmtest::waldtest(m1, m2)}
    if(is.null(clustervar) & robust == TRUE){f1 <- lmtest::waldtest(m1, m2, vcov = sandwich::vcovHC(m1, type = "HC0"))}
    if(!is.null(clustervar)){
      data.temp[[clustervar]] <- data.new[[clustervar]]
      f1 <- lmtest::waldtest(m1, m2, vcov = clusterVCV(data.temp, m1, cluster1=clustervar))
    }    
    f.test.stats[i] <- f1$F[2]
  }
  
  return(f.test.stats)
}

# Calculate the sandwich covariance estimate
covc <- function(cluster, estfun, N1, K1, NROW1, fm1) {
  cluster <- factor(cluster)
  # Calculate the "meat" of the sandwich estimators
  # call this middle to avoid partial matching
  u <- apply(estfun, 2, function(x) tapply(x, cluster, sum))
  middle <- crossprod(u)/N1
  
  # Calculations for degrees-of-freedom corrections, followed 
  # by calculation of the variance-covariance estimate.
  # NOTE: NROW/N is a kluge to address the fact that sandwich uses the
  # wrong number of rows (includes rows omitted from the regression).
  M <- length(levels(cluster))
  dfc <- (M/(M-1)) * ((N1-1)/(N1-K1))
  return(dfc * NROW1/N1 * sandwich::sandwich(fm1, meat.=middle))
}

clusterVCV <- function(data, fm, cluster1, cluster2=NULL) {
  
  # Calculation shared by covariance estimates
  est.fun <- sandwich:: estfun(fm)
  inc.obs <- complete.cases(data[,all.vars(formula(fm))])
  
  # Shared data for degrees-of-freedom corrections
  N  <- dim(fm$model)[1]
  NROW <- NROW(est.fun)
  K  <- fm$rank
  
  # Calculate the covariance matrix estimate for the first cluster.
  cluster1 <- data[inc.obs,cluster1]
  cov1 <- covc(cluster1, estfun=est.fun, N1=N, K1=K, NROW1=NROW, fm1=fm)
  
  if(is.null(cluster2)) {
    # If only one cluster supplied, return single cluster
    # results
    return(cov1)
  } else {
    # Otherwise do the calculations for the second cluster
    # and the "intersection" cluster.
    cluster2 <- data[inc.obs,cluster2]
    cluster12 <- paste(cluster1,cluster2, sep="")
    
    # Calculate the covariance matrices for cluster2, the "intersection"
    # cluster, then then put all the pieces together.
    cov2   <- covc(cluster2, estfun=est.fun, N1=N, K1=K, NROW1=NROW, fm1=fm)
    cov12  <- covc(cluster12, estfun=est.fun, N1=N, K1=K, NROW1=NROW, fm1=fm)
    covMCL <- (cov1 + cov2 - cov12)
    
    # Return the output of coeftest using two-way cluster-robust
    # standard errors.
    return(covMCL)
  }
}


lewbel.est <-
  function(formula, data, clustervar = NULL, robust = TRUE){
    
    # remove missing data
  
    mns <- unlist(strsplit(as.character(formula)[c(2,3)],"\\|"))  
    if(length(mns)<4){
      stop("Error in formula")
    }
    mns <- gsub("factor\\(", "", mns)
    mns <- gsub("\\)", "", mns)
    mns <- gsub(" ", "", mns)
    mns <- unlist(strsplit(mns, "\\+"))
    mns <- unique(unlist(strsplit(mns,":")))
    mns <- unique(unlist(strsplit(mns,"\\*")))
    mns <- gsub("\\)", "", mns)
    mns <- gsub("\\n", "", mns)
    mns <- gsub(" ", "", mns)
    mns <- unlist(strsplit(mns, "\\+"))
    
    mns <- unique(mns[mns!="NULL"])
    if(!is.null(clustervar)){mns = c(mns, clustervar)}
    
    inc.obs <- complete.cases(data[,mns])
    data <- data[inc.obs, ]
    
    # what variables 
    outc <- as.character(formula)[2]  
    regs <- unlist(strsplit(as.character(formula)[3],"\\|"))  
    endg <- regs[1]
    endg <- gsub("\\n", "", endg)  
    exog <- regs[2]
    exog <- gsub("\\n", "", exog)
    hetv <- regs[3]
    hetv <- gsub("\\n", "", hetv)
    
    if(length(grep("NULL", as.character(regs[4])))!=0){
      regs <- regs[-4]
    } 
    if(!is.na(regs[4])){
      inst <- regs[4]
      inst <- gsub("\\n", "", inst)
    } else {
      inst <- NULL
    }
    
    # should we exclude any for singularities?
    ex.form1 <- as.formula(paste(outc, paste(endg, exog, sep=" + "), sep=" ~ "))
    ex.var <- NULL
    ex.var <- coef(stats::lm(ex.form1, data))
    ex.var <- names(ex.var[is.na(ex.var)])
    
    # outcome
    Y2 <- model.matrix(as.formula(paste("~ -1", outc, sep=" + ")), data = data)
    colnames(Y2) <- "y1"
    # endog regressors
    Y1 <- model.matrix(as.formula(paste("~ -1", endg, sep=" + ")), data = data)
    colnames(Y1) <- paste("y2", 1:ncol(Y1), sep=".")
    # exog regressors
    X1 <- model.matrix(as.formula(paste("~ ", exog, sep=" + ")), data = data)
    X1 <- X1[,!colnames(X1) %in% ex.var]
    colnames(X1) <- paste("x1", 1:ncol(X1), sep=".")
    # instruments (if necessary)
    if(!is.na(regs[4])){
      Z1 <- model.matrix(as.formula(paste("~", inst, sep=" + ")), data = data)
      Z1 <- matrix(Z1[,-which(colnames(Z1) %in% "(Intercept)")], nrow = nrow(Y2))
      colnames(Z1) <- paste("z1", 1:ncol(Z1), sep=".")
    }  
    # heteroskedastic instruments  
    Z2 <- model.matrix(as.formula(paste("~", hetv, sep=" + ")), data = data)
    Z2 <- Z2[,!colnames(Z2) %in% ex.var]
    Z2 <- matrix(Z2[,-which(colnames(Z2) %in% "(Intercept)")], nrow = nrow(data))
    Z2 <- apply(Z2, 2, function(x){x-mean(x)} )
    colnames(Z2) <- paste("z2", 1:ncol(Z2), sep=".")
    
    # get residual based IVs
    XF <- X1
    if(!is.na(regs[4]) | length(grep("NULL",regs[4]))!=0){XF = cbind(X1, Z1)}
    E1 <- matrix(residuals(lm(Y1 ~ XF - 1)), nrow = dim(data)[1])
    Z3 <- matrix(apply(E1, 2, function(x){x*Z2}), nrow = dim(data)[1])
    colnames(Z3) <- paste("het.z", 1:ncol(Z3), sep="")
    data.new <- data.frame(Y2, Y1, XF, Z3)
    if(!is.null(clustervar)){data.new[[clustervar]] = data[,clustervar]}
    
    frm1 <- paste(colnames(Y2), paste(colnames(Y1), collapse=" + "), sep=" ~ ")
    frm1 <- paste(frm1, paste(colnames(X1), collapse=" + "), sep=" + ")
    frm2 <- paste("", paste(colnames(X1), collapse=" + "), sep=" ~ ")
    if(!is.na(regs[4])){
      frm2 <- paste(frm2, paste(colnames(Z1), collapse=" + "), sep=" + ")
    }
    frm2  <- paste(frm2, paste(colnames(Z3), collapse=" + "), sep=" + ") 
    
    frm1 <- paste(frm1, " -1" , sep="")
    frm2 <- paste(frm2, " -1" , sep="")
    frm1 <- as.formula(frm1)  
    frm2 <- as.formula(frm2)  
    
    # efficient gmm coefficient estimates
    if(is.null(clustervar) & robust == FALSE){
      gmm1 <- gmm::gmm(frm1, frm2, data = data.new, vcov="iid")
    }
    if(is.null(clustervar) & robust == TRUE){
      data.new$fakeid <- as.character(1:nrow(data.new))
      gmm1 <- suppressMessages(gmmcl(frm1, frm2, data = data.new, cluster = "fakeid"))
    }
    if(!is.null(clustervar)){
      gmm1 <- suppressMessages(gmmcl(frm1, frm2, data = data.new, cluster = clustervar))
    }
    
    coef.names <- c(colnames(model.matrix(as.formula(paste("~ -1", endg, sep=" + ")), data = data)),
                   colnames(model.matrix(as.formula(paste("~ ", exog, sep=" + ")), data = data)))
    # excluded
    coef.names <- coef.names[!coef.names %in% ex.var]
    results <- summary(gmm1)$coefficients[1:length(coef.names),]
    rownames(results) <- coef.names
    
    # add excluded
    if(length(ex.var)>0){
      excl.results <- t(as.matrix(results[1:length(ex.var),]))
      excl.results[,] <- NA
      row.names(excl.results) <- ex.var
      results <- rbind(results, excl.results)
    }
    
    # now get f-tests
    ne <- ncol(Y1)
    nx <- ncol(X1)
    if(!exists("Z1")){
      Z1 <- NULL
    }
    nz <- ifelse(is.null(ncol(Z1)), 0, ncol(Z1))
    nh <- ncol(Z2)
    
    data.new$fakeid <- NULL
    f.test.stats <- ftest(data.new, ne=ne, nx=nx, nz=nz, nh=nh, clustervar=clustervar, robust=robust)
    names(f.test.stats) <- colnames(model.matrix(as.formula(paste("~ -1", endg, sep=" + ")), data = data)) 
    
    results <- list(coefs = results, 
                    formula = formula,
                   j.test = as.list(summary(gmm1)$stest), 
                   num.obs = nrow(data.new),
                   f.test.stats = f.test.stats)
    return(results)
  }


