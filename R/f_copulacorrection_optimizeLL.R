#' @importFrom optimx optimx
#' @importFrom stats lm coef sd model.matrix model.frame
#' @importFrom Formula as.Formula model.part
#' @importFrom utils txtProgressBar setTxtProgressBar
copulaCorrection_optimizeLL <- function(F.formula, data, name.var.continuous, verbose,
                                        start.params=NULL, num.boots, optimx.args = list(),
                                        cl, ...){
  # Catch
  l.ellipsis <- list(...)

  # Further checks required for the parameters in copualCorrection's ... ------------------------------
  check_err_msg(checkinput_copulacorrection_optimxargs(optimx.args = optimx.args))
  # start.params are only checked after the model.matrix has been created


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

  # Warn if the data is binomial=only has two values=dummy
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
  #     rho=0 -> rho=0.5 in LL
  #     sigma=0 -> sigma=1 in LL
  start.params <- c(start.params, rho=0, sigma=0)
  start.params <- start.params[c(names.model.mat, "rho", "sigma")]


  # Definition: Optimization function -----------------------------------------------------------------
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, vec.data.endo,
                              do.kkt){

    # P.star -----------------------------------------------------------------------------------
    # Calculate p.star that is part of the LL already here because it is constant
    #   across LL calls (per data set) and takes a considerable time
    vec.data.endo.pstar <- copulaCorrectionContinuous_pstar(vec.data.endo = vec.data.endo)

    # Default arguments for optimx
    # Bounds for rho [0,1] and sigma (0, Inf):
    #   LL transforms rho/(1+rho) and exp(sigma) because NelderMead does not accept bounds
    #   and returning Inf/NA breaks L-BFGS-B. Hence these transformations.
    #   This implies that the same transformations need to be applied to the found solution
    #     to report the values that are really used in the LL

    param.pos.rho   <- which(names(optimx.start.params) == "rho")
    param.pos.sigma <- which(names(optimx.start.params) == "sigma")
    param.pos.data  <- which(!(names(optimx.start.params) %in% c("rho", "sigma")))

    optimx.default.args <- list(par     = optimx.start.params,
                                fn      = copulaCorrection_LL_rcpp,
                                method  = "Nelder-Mead",
                                itnmax  = 100000,
                                hessian = FALSE,
                                control = list(trace  = 0,
                                               kkt = do.kkt,
                                               dowarn = FALSE),
                                vec_y = vec.data.y,
                                m_data_exo_endo = m.model.data.exo.endo,
                                vec_data_endo_pstar = vec.data.endo.pstar,
                                param_pos_data = param.pos.data,
                                param_pos_sigma = param.pos.sigma,
                                param_pos_rho = param.pos.rho)

    # Update default args with user given args for optimx
    optimx.call.args <- modifyList(optimx.default.args, val = optimx.args, keep.null = FALSE)

    # Call optimx with assembled args
    res.optimx <- tryCatch(expr  = do.call(what = optimx, args = optimx.call.args),
                           error = function(e){ return(e)})

    if(is(res.optimx, "error"))
      stop("Failed to optimize the log-likelihood function with error \'", res.optimx$message,
           "\'. Please revise your start parameter and data.", call. = FALSE)

    # Apply the boundary transformation for rho/sigma
    #   to report results as the coefs were really used in the LL
    #   apply here because the same function is used for bootstrapping
    res.optimx$sigma <- exp(res.optimx$sigma)
    res.optimx$rho   <- exp(res.optimx$rho)/(1+exp(res.optimx$rho))

    return(res.optimx)
  }


  # Run once for coef estimates with real data---------------------------------------------------------
  res.real.data.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y,
                                          m.model.data.exo.endo = m.model.data.exo.endo,
                                          vec.data.endo = vec.data.endo,
                                          do.kkt = TRUE)

  # Bootstrap num.boots times -------------------------------------------------------------------------
  if(verbose){
    message("Running ",num.boots," bootstraps.")
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  }

  boots.params <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      if(verbose)
        setTxtProgressBar(pb, i)

      boot.indices            <- sample.int(n = length(vec.data.y), replace = TRUE)
      i.y                     <- vec.data.y[boot.indices]
      i.m.model.data.exo.endo <- m.model.data.exo.endo[boot.indices, ,drop=FALSE]
      i.vec.data.endo         <- vec.data.endo[boot.indices]
      # return coefs as vector / first row (only 1 method used)
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y,
                                  m.model.data.exo.endo = i.m.model.data.exo.endo,
                                  vec.data.endo = i.vec.data.endo,
                                  do.kkt = FALSE))[1,])
    })

  if(verbose)
    close(pb)

  # Prepare data to return object ------------------------------------------------------------------

  coefficients          <- coef(res.real.data.optimx)[1,]
  # Ordering of coefs is same as input to optimx
  names(coefficients)   <- names(start.params)

  # Also rename the bootstrapped params coef names to original names as they are likely
  #   changed by optimx
  rownames(boots.params) <- names(coefficients)


  names.params.exo.endo <- setdiff(names(coefficients), c("rho", "sigma"))

  fitted.values         <- as.vector(coefficients[names.params.exo.endo] %*% t(m.model.data.exo.endo))
  names(fitted.values)  <- rownames(m.model.data.exo.endo)
  residuals             <- vec.data.y - fitted.values
  names(residuals)      <- rownames(m.model.data.exo.endo)

  # Return data as object --------------------------------------------------------------------------
  return(new_rendo_copula_correction(call         = cl,
                                     F.formula    = F.formula,
                                     mf           = mf,
                                     coefficients = coefficients,
                                     names.main.coefs = names.params.exo.endo,
                                     fitted.values = fitted.values,
                                     residuals = residuals,

                                     boots.params = boots.params,
                                     copula.case  = 1,
                                     names.vars.continuous = name.var.continuous,
                                     names.vars.discrete   = character(),

                                     res.optimx   = res.real.data.optimx,
                                     start.params = start.params))
}
