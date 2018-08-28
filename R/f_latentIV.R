#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame model.matrix sd update
#' @importFrom optimx optimx
#' @export
latentIV <- function(formula, start.params=c(), data){
  cl <- match.call()

  # Input checks ------------------------------------------------------------
  # stopifnot(length(all.vars(formula))==2)

  # Extract data ------------------------------------------------------------
  F.formula  <- as.Formula(formula)
  df.data.y          <- model.frame(formula = F.formula,data = data, lhs=1, rhs=0) # separately needed for names of fitted values
  vec.data.y         <- df.data.y[,1] # ** change to model.response()
  vec.data.endo      <- model.frame(F.formula, data = data, lhs=0, rhs=1)[, 1]
  m.model.all        <- cbind( # response (y) + model of data (incl intercept)
                          vec.data.y,
                          model.matrix(object = update(F.formula, .~.-1), data = data,lhs=0, rhs=1))


  # if the given model includes an intercept, one more paramters is needed additionally (b00)
  use.intercept <- as.logical(attr(terms(F.formula), "intercept"))

  # Initial start.params if not provided ------------------------------------
  if(is.null(start.params)){
    res.lm.start.param <- lm(F.formula, data)

    # Cannot use the names of coef(lm) as optimx will alter it and cannot use the original names to read out anymore.
    # Therefore use generic b00 and a1
    main.coefs                <- coef(res.lm.start.param)
    names.original.main.coefs <- names(coef(res.lm.start.param))
    names.optimx.main.coefs   <- if(use.intercept) c("b00", "a1") else "a1" #Save in separate variable as needed later on
    names(main.coefs)         <- names.optimx.main.coefs

    start.params <- c(main.coefs,
                      pi1 = mean(vec.data.endo),
                      pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                      theta5 = 1, theta6 = 1, theta7 = 1,
                      theta8 = 0.5)
  }

  # print(start.params)


  # Optimize LL -------------------------------------------------------------
  # Bounds are defined in LL by returning Inf for theta8 outside [0,1]
  res.optimx <- optimx(par = start.params, fn=latentIV_LL, m.data.mvnorm = m.model.all,
                       use.intercept = use.intercept,
                       method = "Nelder-Mead", hessian = T, control = list(trace=6))

# print(res.optimx)

  # Return ------------------------------------------------------------------
  hessian  <- attr(res.optimx, "details")[,"nhatend"][[1]]
  param.se <- tryCatch(expr = sqrt(diag(solve(hessian))),
                       # Return same sized NAs matrix if failed to solve so can still read-out results
                       error=function(e){
                         warning("Hessian cannot be solved for the Standard Errors. All SEs set to NA.", call. = F)
                         return(matrix(NA,ncol=ncol(hessian), nrow=nrow(hessian)))})

  # Read out params and rename main coefficients to original parameter names
  estimated.params        <- coef(res.optimx)[1,]
  names(estimated.params) <- c(names.original.main.coefs, "pi1",  "pi2","theta5","theta6","theta7","theta8")
  names(param.se)         <- names(estimated.params)

  if(use.intercept)
    fitted        <- estimated.params[[names.original.main.coefs[1]]]*1 +
                        estimated.params[[names.original.main.coefs[2]]]*vec.data.endo
  else
    fitted        <-                        estimated.params[[names.original.main.coefs[1]]]*vec.data.endo
  names(fitted) <- rownames(df.data.y)

  res <- structure(class = "rendo.latentiv",
                   list(call           = cl,
                        formula        = formula,
                        initial.values = start.params,
                        coefficients   = estimated.params[names.original.main.coefs],
                        group.means    = estimated.params[c("pi1", "pi2")],
                        prob.group1    = estimated.params[["theta8"]],
                        coefficients.se= param.se[names.original.main.coefs],
                        group.means.se = param.se[c("pi1", "pi2")],
                        prob.group1.se = param.se[["theta8"]],
                        log.likelihood = res.optimx$value,
                        conv.code      = res.optimx$convcode,
                        res.optimx     = res.optimx,
                        hessian        = hessian,
                        fitted         = fitted,
                        residuals      = vec.data.y - fitted))

  res$vcov.error <- matrix(c(estimated.params["theta5"]^2,
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta6"]^2+estimated.params["theta7"]^2),
                           nrow=2, ncol=2)
  return(res)
}
