#' @importFrom stats model.response model.frame
#' @importFrom Formula model.part
higherMomentsIV_IIV <- function(F.formula, data, g=NULL, iiv,  ...){

  # Catch ellispsis
  l.iivregressors <- list(...)

  # Check inputs ---------------------------------------------------------------------------------------------
  check_err_msg(checkinput_highermomentsiv_g(g=g))
  check_err_msg(checkinput_highermomentsiv_iiv(iiv=iiv))
  check_err_msg(checkinput_highermomentsiv_iivVSg(g=g, iiv=iiv))
  check_err_msg(checkinput_highermomentsiv_iivregressors(l.iivregressors=l.iivregressors,
                                                         F.formula=F.formula, iiv=iiv))

  # discard given exogenous regressors and g if not needed because otherwise description is wrong
  if(iiv %in% c("y2", "p2", "yp")){
    l.iivregressors <- list()
    g <- NULL
  }

  # Read out needed data -------------------------------------------------------------------------------------
  # To multiply: Read out as matrices as data.frames allow no element-wise mutliplication if dimensions do
  #               not match. For iiv = gp, gy this may be the case when col(g)>1
  names.exo.regs    <- unique(unlist(l.iivregressors))

  mf                <- model.frame(F.formula, rhs=1, lhs=1, data=data)
  vec.data.endo     <- model.part(F.formula, data=mf, rhs=2, lhs = 0, drop=TRUE)  # endo data from rhs 2 AS VECTOR
  # The exogenous regressors are only the ones specified in the IIV() part, not all exogenous ones
  #   Therefore read out the data by names. If none (NULL) results in zero-row data.frame
  df.data.exo.iiv   <- mf[, names.exo.regs, drop=FALSE]
  vec.data.y        <- model.response(mf)

  # Calculate internal IVs -----------------------------------------------------------------------------------

  # determine g function, if needed
  if(!is.null(g))
    fct.g <- switch(g,
                    "x2"  = function(x){x^2},
                    "x3"  = function(x){x^3},
                    "lnx" = function(x){log(x)},
                    "1/x" = function(x){1/x})

  # Col-wise de-mean helper function
  de.mean <- function(x){
    if(length(dim(x)) > 1){
      # >1 col (data.frame,...).
      # Use sweep contrary to apply because it again returns data.frame
      # return(apply(x, MARGIN = 2, FUN = function(x){x-mean(x)}))
      return(sweep(x = x, MARGIN = 2, STATS = colMeans(x=x, na.rm = TRUE), FUN = "-"))
    }else{
      # vector
      return(x-mean(x))
    }
  }

  # IIV calculations
  df.IIV <- data.frame(res.iiv =
    switch(EXPR = iiv,
           # to allow element-wise multiplication for data.frames.
           #  The vector data HAS to be the first input
           "g"  = de.mean(fct.g(df.data.exo.iiv)),                           # IIV1
           "gp" = de.mean(fct.g(df.data.exo.iiv)) * de.mean(vec.data.endo),  # IIV2
           "gy" = de.mean(fct.g(df.data.exo.iiv)) * de.mean(vec.data.y),     # IIV3
           "yp" = de.mean(vec.data.y)             * de.mean(vec.data.endo),  # IIV4
           "p2" = de.mean(vec.data.endo)^2,                                  # IIV5
           "y2" = de.mean(vec.data.y)^2))                                    # IIV6

  # Naming ------------------------------------------------------------------------------------------------
  # Rename after IIV()
  #   Cannot make in single paste() cmd as double . are introduced for emtpy chars
  colnames.iiv <- paste0("IIV.is",iiv)
  colnames.iiv <- if(is.null(g))              colnames.iiv else paste0(colnames.iiv,".gis",g)
  colnames.iiv <- if(!length(names.exo.regs)) colnames.iiv else paste0(colnames.iiv,".regis",names.exo.regs)

  # Keep make.names for the case g=1/x and too be sure its always correct
  colnames(df.IIV) <- make.names(colnames.iiv)

  # Readable description
  desc.iiv <- paste0("IIV(iiv=",iiv)
  desc.iiv <- if(is.null(g))              desc.iiv else paste0(desc.iiv,",g=",g)
  desc.iiv <- if(!length(names.exo.regs)) desc.iiv else paste0(desc.iiv,",",paste(names.exo.regs,collapse = ","))
  desc.iiv <- paste0(desc.iiv,")")


  # Return ------------------------------------------------------------------------------------------------
  # Return list with readable description and IIV as data.frame

  return(list(desc.IIV = desc.iiv, df.IIV = df.IIV))
}
