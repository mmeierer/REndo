# To reuse the function in the discrete only confint simulation, also a model.frame object can be passed as
# the data parameter instead of a regular data.frame. Reason is that the fitted lm contains only the
# model.frame but not the original data. The MF can be re-used by using the lm.fit function directly.
# Cannot use model.frame as data parameter because in the model frame the names may be changed due to transformations
# in the formula and therefore different to the names in the specials
copulaCorrection_linearmodel <- function(F.formula, data, data.is.model.frame = FALSE,
                                         names.vars.continuous, names.vars.discrete){

  # Generate PStar data ---------------------------------------------------------------------------------------
  l.pstar.data.continuous <- NULL #init so that they exists for cbind
  l.pstar.data.discrete   <- NULL

  # if(!data.is.model.frame){
  #   # transform data required for pstar as specified by the formula
  #   mf.data <- model.frame(F.formula, rhs=1, lhs=0, data=data)
  #   # Rename for access by name (transformations of endo columns rename them)
  #   colnames(mf.data) <- colnames(data[])
  # }else{
  #   mf.data <- data
  # }
  mf.data <- model.frame(F.formula, rhs=1, lhs=0, data=data)

  if(length(names.vars.continuous) > 0){
    l.pstar.data.continuous <- lapply(names.vars.continuous, function(n){
      copulaCorrectionContinuous_pstar(vec.data.endo=mf.data[[n]]) })
    names(l.pstar.data.continuous) <- paste0("PStar.", names.vars.continuous, sep="")
  }

  if(length(names.vars.discrete) > 0){
    l.pstar.data.discrete   <- lapply(names.vars.discrete, function(n){
      copulaCorrectionDiscrete_pstar(vec.data.endo=mf.data[[n]])})
    names(l.pstar.data.discrete) <- paste0("PStar.", names.vars.discrete, sep="")
  }
  df.data.pstar <- do.call(cbind, c(l.pstar.data.continuous, l.pstar.data.discrete))


  if(!data.is.model.frame){
    # Update formula to include PStar data ----------------------------------------------------------------------
    # use the formula RHS1 and also include P.star to fit lm
    F.lm  <- formula_build_mainmodel_data(F.formula = F.formula, data=df.data.pstar)

    # Run linear model ------------------------------------------------------------------------------------------
    df.data.copula <- cbind(data, df.data.pstar)
    lm.res <- lm(formula = F.lm, data = df.data.copula)
  }else{
    print(head(df.data.pstar))
    print(head(data))
    # Replace the already existing PStar data in the model.frame
    mf.data[, colnames(df.data.pstar)] <- df.data.pstar
    print(head(data))
    lm.res <- lm.fit(x = model.matrix(terms(mf.data), data=data), y=model.response(mf.data))
    # Create proper lm result of this
    class(lm.res) <-"lm"
    lm.res$na.action <- attr(mf, "na.action")
    lm.res$xlevels <- .getXlevels(mt, mf)
    lm.res$call <- cl
    lm.res$terms <- mt
    # z$model <- mf
    # z$x <- x
    # z$y <- y
    # z$qr <- NULL
  }


  # Return object --------------------------------------------------------------------------------------
  return(lm.res)
}
