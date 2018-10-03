#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame model.matrix sd update setNames
#' @importFrom optimx optimx
#' @importFrom methods is
#' @export
latentIV <- function(formula, start.params=c(), data, verbose=TRUE){
  cl <- match.call()

  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_latentIV_data(data=data))
  check_err_msg(checkinput_latentIV_formula(formula=formula))
  check_err_msg(checkinput_latentIV_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_latentIV_startparams(start.params=start.params, formula=formula))
  check_err_msg(checkinput_latentIV_verbose(verbose=verbose))

  # Extract data ------------------------------------------------------------------------------
  F.formula          <- as.Formula(formula)
  mf                 <- model.frame(formula = F.formula, data = data)
  vec.data.y         <- model.response(mf)
  vec.data.endo      <- model.part(object=F.formula, data = mf, lhs=0, rhs=1, drop = TRUE)
  # response (y) + data of model (excl intercept)
  m.data.mvnorm      <- cbind(vec.data.y, vec.data.endo)

  # Start parameter and names ------------------------------------------------------------------
  # Iff the given model includes an intercept, one more paramters is needed
  use.intercept      <- as.logical(attr(terms(F.formula), "intercept"))
  name.intercept     <- "(Intercept)" # convenience, always set

  # Readout / generate start values for the main model params
  if(is.null(start.params)){
    # generate start vals from lm
    if(verbose)
      message("No start parameters were given. The linear model ",deparse(formula(F.formula, lhs=1, rhs=1)),
              " is fitted to derive them.")
    res.lm.start.param    <- lm(F.formula, data)
    if(anyNA(coef(res.lm.start.param)))
      stop("The start parameters could not be derived by fitting a linear model.", call. = FALSE)
    start.vals.main.model <-  coef(res.lm.start.param)
  }else{
    # valid start.params given by user
    start.vals.main.model <- start.params
  }

  # read out names
  name.endo.param  <- setdiff(names(start.vals.main.model), name.intercept)
  names.main.model <- if(use.intercept) c(name.intercept, name.endo.param) else name.endo.param
  # ensure sorting is (Intercept), ENDO
  start.vals.main.model <- setNames(start.vals.main.model[names.main.model], names.main.model)

  start.vals.support.params <- c(pi1 = mean(vec.data.endo),
                                 pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                                 theta5 = 1, theta6 = 1, theta7 = 1,
                                 theta8 = 0.5)
  names.support.params      <- names(start.vals.support.params)

  # Append support params to start params
  optimx.start.params  <- c(start.vals.main.model, start.vals.support.params)




  # Optimize LL --------------------------------------------------------------------------------
  # Print start params
  if(verbose){
    str.brakets <- paste0("(", paste(names(optimx.start.params), "=", round(optimx.start.params,3),
                                     collapse = ", ", sep=""), ")")
    message("The start parameters c",str.brakets," are used for optimization.")
  }

  optimx.name.endo.param <- make.names(name.endo.param)
  optimx.name.intercept  <- make.names(name.intercept)

  # Constrain theta8 to [0,1]
  lower.bounds <- setNames(rep(-Inf, length(optimx.start.params)), names(optimx.start.params))
  upper.bounds <- setNames(rep( Inf, length(optimx.start.params)), names(optimx.start.params))
  lower.bounds[names(lower.bounds) == "theta8"] <- 0
  upper.bounds[names(upper.bounds) == "theta8"] <- 1

  # Fit LL with optimx
  res.optimx <- tryCatch(expr =
                           optimx(par = optimx.start.params,
                                  fn=latentIV_LL,
                                  m.data.mvnorm = m.data.mvnorm,
                                  use.intercept = use.intercept,
                                  name.intercept  = name.intercept,
                                  name.endo.param = name.endo.param,
                                  method = "L-BFGS-B",
                                  # method = "Nelder-Mead",
                                  lower = lower.bounds,
                                  upper = upper.bounds,
                                  hessian = TRUE,
                                  control = list(trace = 0,
                                                 maxit = 5000)),
                         error   = function(e){ return(e)})

  if(is(res.optimx, "error"))
    stop("Failed to optimize the log-likelihood function with error \'", res.optimx$message,
         "\'. Please revise your start parameter and data.", call. = FALSE)


  # Read out params ----------------------------------------------------------------------------
  # Ensure naming and ordering

  # Read out params
  optimx.estimated.params  <- coef(res.optimx)[1,]

  # rename main coefficients in correct order to original parameter names
  all.estimated.params   <- setNames(optimx.estimated.params[c(make.names(names.main.model),names.support.params)],
                                     c(names.main.model, names.support.params))
  estim.param.main.model <- all.estimated.params[names.main.model]
  estim.param.support    <- all.estimated.params[names.support.params]

  # Hessian and SE ------------------------------------------------------------------------------
  # Read out hessian.
  # If optimx failed, single NA is returned as the hessian. Replace it with correctly-sized
  #   matrix of NAs
  hessian  <- attr(res.optimx, "details")[,"nhatend"][[1]]
  if(length(hessian)==1 & all(is.na(hessian)))
    hessian <- matrix(data = NA_real_, nrow = length(all.estimated.params), ncol = length(all.estimated.params))

  rownames(hessian) <- colnames(hessian) <- names(all.estimated.params)
  fct.se.warn.error <- function(ew){
                              warning("Hessian cannot be solved for the standard errors. All SEs set to NA.",
                                      call. = FALSE, immediate. = TRUE)
                              return(rep(NA_real_,length(all.estimated.params)))}

  all.param.se <- tryCatch(expr = sqrt(diag(solve(hessian))),
                       # Return same NAs vector if failed to solve so can still read-out results
                       warning = fct.se.warn.error,
                       error   = fct.se.warn.error)
  names(all.param.se)         <- names(all.estimated.params)

  # Fitted and residuals
  if(use.intercept)
    fitted        <- all.estimated.params[[name.intercept]]*1 +
                        all.estimated.params[[name.endo.param]]*vec.data.endo
  else
    fitted        <- all.estimated.params[[name.endo.param]]*vec.data.endo
  fitted          <- as.vector(fitted)
  names(fitted)   <- names(vec.data.y)

  residuals        <- as.vector(vec.data.y - fitted)
  names(residuals) <- names(vec.data.y)

  vcov.error      <- matrix(c(all.estimated.params["theta5"]^2,
                              all.estimated.params["theta5"]   * all.estimated.params["theta6"],
                              all.estimated.params["theta5"]   * all.estimated.params["theta6"],
                              all.estimated.params["theta6"]^2 + all.estimated.params["theta7"]^2),
                           nrow=2, ncol=2)


  # Put together returns ------------------------------------------------------------------
  res <- new_rendo_optim_LL(call=cl, F.formula=F.formula, mf  = mf,
                            start.params=optimx.start.params,
                            estim.params     = all.estimated.params,
                            estim.params.se  = all.param.se,
                            names.main.coefs = names.main.model,
                            res.optimx = res.optimx, log.likelihood=res.optimx$value,
                            hessian = hessian, fitted.values=fitted,
                            residuals=residuals, vcov.error=vcov.error)

                   # list(coefficients   = estimated.params[names.original.main.coefs],
                   #      group.means    = estimated.params[c("pi1", "pi2")],
                   #      prob.group1    = estimated.params[["theta8"]],
                   #      coefficients.se= param.se[names.original.main.coefs],
                   #      group.means.se = param.se[c("pi1", "pi2")],
                   #      prob.group1.se = param.se[["theta8"]]))
  return(res)
}
