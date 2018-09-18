#' @importFrom Formula as.Formula
#' @importFrom stats update.formula reformulate model.response
#' @importFrom AER ivreg
#' @export
higherMomentsIV <- function(formula, data,
                            IIV=c(),  # c("g","gp","gy","yp","p2","y2"))
                            g=c()){     # c("x2", "x3", "lnx", "1/x")

# *** fail raluca check: underlying assumptions not satisfied - stop()
  cl <- match.call()

  # Input checks -------------------------------------------------------------------------------------
  check_err_msg(checkinput_highermomentsiv_g(g=g))
  check_err_msg(checkinput_highermomentsiv_iiv(IIV=IIV))
  # **** data.exo and data.endo (gp) have matching dimensions

  # Read out data ------------------------------------------------------------------------------------
  # In old:     response ~ endogenous | all exogenous | which to build internal IV | EIV
  F.formula    <- as.Formula(formula)
  mf           <- model.frame(formula = F.formula, data = data)
  vec.data.y   <- model.response(mf)
  df.data.endo <- mf[, all.vars(formula(F.formula, lhs=0, rhs=2))]
  df.data.exo  <- mf[, all.vars(formula(F.formula, lhs=0, rhs=3))]


  # Calculate internal IVs ---------------------------------------------------------------------------
  # proposed by Lewbel 97 - g, gp, gy, yp, y2, p2

  # ** Is this col-wise or mean(whole matrix)? Doing col-wise
  de.mean <- function(x){ if(NCOL(x) > 1)
                            return(apply(x, MARGIN = 2, FUN = function(x){x-mean(x)})) # matrix/data.frame/array
                            # sweep(x = x, MARGIN = 2, STATS = colMeans(x=df, na.rm = T), FUN = "-")
                          else
                            return(x-mean(x))} # vector

  # Define actual functions for the given g
  fct.g <- switch(g,
                  "x2"  = function(x){x^2},
                  "x3"  = function(x){x^3},
                  "lnx" = function(x){log(x)},
                  "1/x" = function(x){1/x})

  # IIV calculations
  #   Do for every type of given IIV
  names(IIV) <- IIV # Set names so that lapply colnames will be set and can use iiv in formula
  df.data.IIV <- as.data.frame(lapply(IIV, function(iiv){
                        sw.res <- switch(EXPR = iiv,
                               "g"  = de.mean(fct.g(df.data.exo)),                         # IIV1
                               "gp" = de.mean(fct.g(df.data.exo)) * de.mean(df.data.endo), # IIV2
                               "gy" = de.mean(fct.g(df.data.exo)) * de.mean(vec.data.y),   # IIV3
                               "yp" = de.mean(vec.data.y)         * de.mean(df.data.endo), # IIV4
                               "p2" = de.mean(df.data.endo)^2,                             # IIV5
                               "y2" = de.mean(vec.data.y)^2)                               # IIV6
                        return(sw.res)}))


  # Build formula for IVreg --------------------------------------------------------------------------
  # In:         response ~ endogenous | all exogenous | which to build internal IV | EIV
  # Needed:     response ~ endogenous + exogenous | exogenous + interal IVs + external IVs

  # resp ~ endogenous + exogenous
  # F.ivreg <- formula(F.formula, lhs = 1, rhs=c(1,2), collapse = T, update = T)
  F.ivreg <- formula(F.formula, lhs = 1, rhs = 1, collapse = T, update = T)

  # 2nd Part: ~ exogenous + interal IVs + external IVs
  # exogenous and external IVs (if present)
  rhs.2nd.part     <- if(length(F.formula)[[2]] == 4) c(2,4) else c(2)  # include external IVs only if present
  F.ivreg.2nd.part <- formula(F.formula, lhs = 0, rhs=rhs.2nd.part, collapse = T, update = T)
  # add internal IVs. Add c(".") to include all existing parts
  # intercept=T to not remove if there is one. Will not add one if there is none.
  F.ivreg.2nd.part <- update.formula(F.ivreg.2nd.part,
                                     reformulate(termlabels = c(".", colnames(df.data.IIV)),
                                                 response = NULL,
                                                 intercept = T))

  # Add 2nd part to formula
  F.ivreg <- as.Formula(F.ivreg, F.ivreg.2nd.part)

  # Call ivreg ---------------------------------------------------------------------------------------
  # Put data together: user data + internal instruments
  df.data.ivreg <- cbind(data, df.data.IIV)

  # cat("Formula used for ivreg: ")
  # print(F.ivreg)
  # print(head(df.data.ivreg))

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  res <- structure(class="rendo.highermomentsiv",
                   list(call          = cl,
                        formula       = formula,
                        coefficients  = coef(res.ivreg),
                        res.ivreg     = res.ivreg))

  return(res)
}
