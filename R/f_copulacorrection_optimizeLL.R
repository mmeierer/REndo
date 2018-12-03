#' @importFrom optimx optimx
#' @importFrom stats lm coef sd model.matrix model.frame
#' @importFrom Formula as.Formula model.part
#' @importFrom utils txtProgressBar setTxtProgressBar
copulaCorrection_optimizeLL <- function(F.formula, data, name.var.continuous, verbose,
                                        start.params=NULL, num.boots=1000, optimx.args = list(),
                                        cl, ...){
  # Catch
  l.ellipsis <- list(...)

  # Further checks required for the parameters in copualCorrection's ... ------------------------------
  check_err_msg(checkinput_copulacorrection_numboots(num.boots=num.boots))
  check_err_msg(checkinput_copulacorrection_optimxargs(optimx.args = optimx.args))
  # start.params are only checked when the model.matrix has been fitted


  # Tell what is done ---------------------------------------------------------------------------------
  if(verbose){
    # Tell what is done
    message("Optimizing LL for the single continuous endogenous regressor ",name.var.continuous,".")
  }

  # Warn because user does not understand what he is doing
  if(length(l.ellipsis)>0)
    warning("Additional parameters besides \'start.params\' and \'num.boots\' given in the ... argument are ignored.",
            call. = FALSE, immediate. = TRUE)


  # Read out needed data ------------------------------------------------------------------------------
  # Using model.part() directly does not work if there are transformations in the DV
  mf                      <- model.frame(formula = F.formula, data = data, rhs=1, lhs=1)
  vec.data.y              <- as.vector(model.part(object = F.formula, data = mf, lhs=1, rhs = 0, drop=TRUE))
  # rhs1 contains exo and endo, as per input-checks
  m.model.data.exo.endo   <- model.matrix(object = F.formula, data = mf, lhs=0, rhs = 1)
  vec.data.endo           <- m.model.data.exo.endo[, name.var.continuous, drop=TRUE]

  # Warn if the data is binomial=only has to values=dummy
  #   Cannot really check this before data is transformed
  checkinput_copulacorrection_warnbinomialendodata(data = mf,
                                                   names.vars.continuous = name.var.continuous,
                                                   names.vars.discrete   = character(0L))


  # Create start parameters for optimx ----------------------------------------------------------------
  if(is.null(start.params)){
    # Generate with lm if they are missing
    res.lm.start.param <- lm(formula = formula(F.formula, lhs=1, rhs=1), data = data)
    if(anyNA(coef(res.lm.start.param)))
      stop("The start parameters could not be derived by fitting a linear model.", call. = FALSE)
    start.params <- coef(res.lm.start.param)

    str.brakets  <- paste0("(", paste(names(start.params), "=", round(start.params,3), collapse = ", ", sep=""), ")")
    if(verbose)
      message("No start parameters were given. The linear model ", deparse(formula(F.formula, lhs=1, rhs=1)),
              " was fitted and start parameters c",str.brakets," are used.")
  }else{
    # Check if start.params is named correctly. Can only be done here after model.matrix is known
    check_err_msg(checkinput_copulacorrection_startparams(start.params=start.params, F.formula=F.formula,
                                                          m.endo.exo = m.model.data.exo.endo))
  }

  # Double check if start params and model matrix have the same names
  names.model.mat    <- colnames(m.model.data.exo.endo)
  names.start.params <- names(start.params)
  stopifnot( setequal(names.start.params, names.model.mat) == TRUE,
             length(unique(names.start.params)) == length(unique(names.model.mat)))


  # Add rho and sigma with defaults
  #   Same order as model.matrix/formula. This is done in LL again,
  #   but do here to have consistent output (inputorder to optimx counts for this)
  start.params <- c(start.params, rho=0, sigma=log(exp(1))) # rho=0 -> rho=0.5 in LL
  start.params <- start.params[c(names.model.mat, "rho", "sigma")]


  # Definition: Optimization function -----------------------------------------------------------------
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, vec.data.endo, hessian=FALSE){

    # Default arguments for optimx
    # Bounds for rho (0,1): LL returns Inf if outside as NelderMead cannot deal with bounds
    optimx.default.args <- list(par     = optimx.start.params,
                                fn      = copulaCorrection_LL,
                                method  = "Nelder-Mead",
                                itnmax  = 100000,
                                hessian = hessian,
                                control = list(trace=0,
                                               dowarn = FALSE),
                                vec.y   = vec.data.y,
                                m.data.exo.endo = m.model.data.exo.endo,
                                vec.data.endo   = vec.data.endo)

    # Update default args with user given args for optimx
    optimx.call.args <- modifyList(optimx.default.args, val = optimx.args, keep.null = FALSE)

    # Call optimx with assembled args
    res.optimx <- tryCatch(expr = do.call(what = optimx, args = optimx.call.args),
                           error   = function(e){ return(e)})

    if(is(res.optimx, "error"))
      stop("Failed to optimize the log-likelihood function with error \'", res.optimx$message,
           "\'. Please revise your start parameter and data.", call. = FALSE)

    return(res.optimx)
  }


  # Run once for coef estimates with real data---------------------------------------------------------
  res.real.data.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y,
                                          m.model.data.exo.endo = m.model.data.exo.endo,
                                          vec.data.endo = vec.data.endo, hessian = TRUE)

  # bootstrap num.boots times
  # Bootstrapping for SE ------------------------------------------------------------------------------
  if(verbose){
    message("Running ",num.boots," bootstraps to derive standard errors.")
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  }

  res.boots <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      if(verbose)
        setTxtProgressBar(pb, i)

      indices                 <- sample(x = length(vec.data.y), size=length(vec.data.y)*0.8, replace = TRUE)
      i.y                     <- vec.data.y[indices]
      i.m.model.data.exo.endo <- m.model.data.exo.endo[indices, ,drop=FALSE]
      i.vec.data.endo         <- vec.data.endo[indices]
      # return as vector / first row (only 1 method used). As matrix it cannot be used again as input to optimx
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y,
                                  m.model.data.exo.endo = i.m.model.data.exo.endo,
                                  vec.data.endo = i.vec.data.endo))[1,])
    })

  if(verbose)
    close(pb)

  # Calculate data --------------------------------------------------------------------------------------------
  # Prepare data to create a rendo LL optim object
  # Ordering of coefs is same as input to optimx

  # Parameter and se
  # Boots results: Rows = per parameter,  Columns = for each boots run
  coefficients          <- coef(res.real.data.optimx)[1,]     # extract parameters from single fit
  parameter.se          <- apply(res.boots, 1, stats::sd, na.rm=TRUE)   # SD of bootstrapped parameters
  names(coefficients)   <- names(parameter.se) <- names(start.params)

  names.params.exo.endo <- setdiff(names(coefficients), c("rho", "sigma"))

  # Read out hessian.
  hessian <- extract.hessian(res.optimx = res.real.data.optimx, names.hessian = names(start.params))

  fitted.values         <- as.vector(coefficients[names.params.exo.endo] %*% t(m.model.data.exo.endo))
  names(fitted.values)  <- rownames(m.model.data.exo.endo)
  residuals             <- vec.data.y - fitted.values
  names(residuals)      <- rownames(m.model.data.exo.endo)

  # Return data as object -------------------------------------------------------------------------------------
  return(new_rendo_optim_LL(call = cl, F.formula = F.formula, mf = mf,
                            start.params = start.params,
                            estim.params = coefficients, estim.params.se = parameter.se,
                            names.main.coefs = names.params.exo.endo, hessian = hessian,
                            res.optimx = res.real.data.optimx,
                            fitted.values = fitted.values, residuals = residuals,
                            log.likelihood = res.real.data.optimx$value))
}
