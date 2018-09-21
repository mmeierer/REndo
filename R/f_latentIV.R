#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame model.matrix sd update
#' @importFrom optimx optimx
#' @export
latentIV <- function(formula, start.params=c(), data){
  cl <- match.call()

  # Input checks ------------------------------------------------------------
  check_err_msg(checkinput_latentIV_data(data=data))
  check_err_msg(checkinput_latentIV_formula(formula=formula))
  check_err_msg(checkinput_latentIV_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_latentIV_startparams(start.params=start.params, formula=formula))

  # Extract data ------------------------------------------------------------
  F.formula          <- as.Formula(formula)
  mf.data.y          <- model.frame(formula = F.formula,data = data, lhs=1, rhs=0) # separately needed for names of fitted values
  vec.data.y         <- model.response(mf.data.y)
  vec.data.endo      <- model.matrix(object = update(F.formula, .~.-1), data = data, lhs=0, rhs=1) # model.frame(F.formula, data = data, lhs=0, rhs=1)[, 1]
  m.data.mvnorm      <- cbind(vec.data.y, vec.data.endo) # response (y) + data of model (excl intercept)

  # Cannot use the names of coef(lm) as optimx will alter it and cannot use the original names to read out anymore.
  # Therefore use generic b00 and a1.
  # Only iff the given model includes an intercept, one more paramters is needed (b00)
  use.intercept             <- as.logical(attr(terms(F.formula), "intercept"))
  names.optimx.main.coefs   <- if(use.intercept) c("b00", "a1") else "a1" # Save in separate variable as needed later on

  # Initial start.params if not provided ------------------------------------
  if(is.null(start.params)){
    res.lm.start.param <- lm(F.formula, data)
    # ***stop if lm fails / coef contains any NAs
    names.original.main.coefs <- names(coef(res.lm.start.param))
    main.coefs                <- coef(res.lm.start.param)
    names(main.coefs)         <- names.optimx.main.coefs
    start.params <- c(main.coefs,
                      pi1 = mean(vec.data.endo),
                      pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                      theta5 = 1, theta6 = 1, theta7 = 1,
                      theta8 = 0.5)

    str.brakets <- paste0("(", paste(names(start.params), "=", round(start.params,3), collapse = ", ", sep=""), ")")
    warning("No start parameters were given. The linear model ",deparse(formula(F.formula, lhs=1, rhs=1)),
            " was fitted and start parameters c",str.brakets," are used.", call. = FALSE, immediate. = TRUE)

  }else{
    # start.params provided which only contain params for dependent and independent vars
    # Re-name for optimx but first save original names for renaming later on
    names.original.main.coefs   <- names(start.params)
    names(start.params)         <- names.optimx.main.coefs
    # Add other params
    start.params <- c(start.params,
                      pi1 = mean(vec.data.endo),
                      pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                      theta5 = 1, theta6 = 1, theta7 = 1,
                      theta8 = 0.5)
  }



  # Optimize LL ------------------------------------------------------------------------
  # Bounds are defined in LL by returning Inf for theta8 outside [0,1]
  # **try catch
  res.optimx <- optimx(par = start.params, fn=latentIV_LL, m.data.mvnorm = m.data.mvnorm,
                       use.intercept = use.intercept,
                       method = "L-BFGS-B",
                       lower = c(rep(-Inf,length(start.params)-1), 0),
                       upper = c(rep( Inf,length(start.params)-1), 1),
                       hessian = TRUE, control = list(trace=0))


  # Calculate Returns ------------------------------------------------------------------
  # Params and SE
  # Read out params and rename main coefficients to original parameter names
  estimated.params        <- coef(res.optimx)[1,]
  names(estimated.params) <- c(names.original.main.coefs, "pi1",  "pi2","theta5","theta6","theta7","theta8")

  # Hessian
  hessian  <- attr(res.optimx, "details")[,"nhatend"][[1]]
  rownames(hessian) <- colnames(hessian) <- names(estimated.params)
  fct.se.warn.error <- function(e){
                              warning("Hessian cannot be solved for the standard errors. All SEs set to NA.", call. = F)
                              return(rep(NA_real_,length(estimated.params)))}

  param.se <- tryCatch(expr = sqrt(diag(solve(hessian))),
                       # Return same NAs vector if failed to solve so can still read-out results
                       warning = fct.se.warn.error,
                       error   = fct.se.warn.error)
  names(param.se)         <- names(estimated.params)

  # Fitted and residuals
  if(use.intercept)
    fitted        <- estimated.params[[names.original.main.coefs[1]]]*1 +
                        estimated.params[[names.original.main.coefs[2]]]*vec.data.endo
  else
    fitted        <- estimated.params[[names.original.main.coefs[1]]]*vec.data.endo
  fitted          <- as.vector(fitted)
  names(fitted)   <- names(vec.data.y)

  residuals        <- as.vector(vec.data.y - fitted)
  names(residuals) <- names(vec.data.y)

  vcov.error      <- matrix(c(estimated.params["theta5"]^2,
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta6"]^2+estimated.params["theta7"]^2),
                           nrow=2, ncol=2)


  # Put together returns ------------------------------------------------------------------
  res <- new_rendo_optim_LL(call=cl, F.formula=F.formula, start.params=start.params,
                            estim.params=estimated.params, estim.params.se = param.se,
                            names.main.coefs=names.original.main.coefs,
                            res.optimx=res.optimx, log.likelihood=res.optimx$value,
                            hessian=hessian, fitted.values=fitted,
                            residuals=residuals, vcov.error=vcov.error)

                   # list(coefficients   = estimated.params[names.original.main.coefs],
                   #      group.means    = estimated.params[c("pi1", "pi2")],
                   #      prob.group1    = estimated.params[["theta8"]],
                   #      coefficients.se= param.se[names.original.main.coefs],
                   #      group.means.se = param.se[c("pi1", "pi2")],
                   #      prob.group1.se = param.se[["theta8"]]))
  return(res)
}
