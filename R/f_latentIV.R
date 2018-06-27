#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame sd
#' @importFrom optimx optimx
#' @export
latentIV <- function(formula, start.params=c(), data){
  cl <- match.call()

  # Input checks ------------------------------------------------------------
  # stopifnot(length(all.vars(formula))==2)

  # Extract data ------------------------------------------------------------
  F.formula  <- as.Formula(formula)
  vec.data.y         <- model.frame(formula = F.formula,data = data, lhs=1, rhs=0)[,1]
  vec.data.endo      <- model.frame(F.formula, data = data, lhs=0, rhs=1)[, 1]
  m.model.all        <-cbind( # response (y) + model of data (incl intercept)
                          vec.data.y,
                          model.matrix(object = update(F.formula, .~.-1), data = data,lhs=0, rhs=1))


  print(head(m.model.all))
  print(head(vec.data.endo))

  # Initial start.params if not provided ------------------------------------
  if(is.null(start.params)){
    # Ensure there is an intercept
    F.lm.formula <- update(F.formula, .~.+1)
    res.lm.start.param <- lm(F.lm.formula, data)
    print(coef(res.lm.start.param))
    start.params <- c(b00 = coef(res.lm.start.param)[[1]], # [[]] to unname
                      a1  = coef(res.lm.start.param)[[2]],
                      pi1 = mean(vec.data.endo),
                      pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                      theta5 = 1, theta6 = 1, theta7 = 1,
                      theta8 = 0.5)
  }

  print(start.params)

  # ** optimx bounds
  # ** print only estimated parameters from formula (and change names)
  # pi1 and pi2 € (0,1)


  # Optimize LL -------------------------------------------------------------
  res.optimx <- optimx(par = start.params, fn=latentIV_LL, m.data.mvnorm = m.model.all,
                       method = "BFGS", hessian = T, control = list(trace=0))

  # Warnings *** do
  # *** Print summary: incl group means... regular show: only parameters
  # if (obj@seMeans[1] =="NaN") warning("Group means' standard errors unable to be computed. Check initial parameter values")
  # if (obj@seCoefficients[1] =="NaN") warning("Coefficient's standard errors unable to be computed. Check initial parameter values")

  # Return ------------------------------------------------------------------
  hessian <- attr(res.optimx, "details")[,"nhatend"][[1]]
  param.se <- sqrt(diag(solve(hessian)))

  names(param.se)  <- colnames(coef(res.optimx))
  estimated.params <- coef(res.optimx)[1,]

  fitted.values <- estimated.params[["b00"]]*1 + estimated.params[["a1"]]*vec.data.endo

  res <- structure(class = "rendo.ivlatent",
                   list(call = cl, formula = formula,
                        initial.values = start.params,
                        coefficients   = estimated.params[c("b00", "a1")],
                        group.means    = estimated.params[c("pi1", "pi2")],
                        prob.group1    = estimated.params[["theta8"]],
                        coefficients.se= param.se[c("b00", "a1")],
                        group.means.se = param.se[c("pi1", "pi2")],
                        prob.group1.se = param.se[["theta8"]],
                        log.likelihood = res.optimx$value,
                        conv.code      = res.optimx$convcode,
                        hessian        = hessian,
                        fitted.values  = fitted.values,
                        residuals      = vec.data.y - fitted.values))

  res$AIC <- (-2)*(-res$log.likelihood) + 2*length(start.params)
  res$BIC <- (-2)*(-res$log.likelihood) + nrow(data)*length(start.params)
  res$vcov.error <- matrix(c(estimated.params["theta5"]^2,
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta5"]*estimated.params["theta6"],
                             estimated.params["theta6"]^2+estimated.params["theta7"]^2),
                           nrow=2, ncol=2)
  return(res)
}
