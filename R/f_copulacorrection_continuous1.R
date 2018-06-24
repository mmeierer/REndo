#' @importFrom optimx optimx
#' @importFrom stats lm coef confint sd model.matrix model.frame
#' @importFrom Formula as.Formula
#' @export
copulaCorrectionContinuous1  <- function(formula, start.params = c(), num.boots=10, data){

  cl <- match.call()

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula               <- as.Formula(formula)
  vec.data.y              <- as.vector(model.part(object = F.formula, data = data, lhs=1, rhs = 0)[,1])

  # *** Do they have intercepts or not?! (model.frame = w/o I/dummies, model.matrix =w/ I. Both do transformations)
  m.model.data.exo.endo   <- model.matrix(object = F.formula, data = data, lhs=0, rhs = c(1,2))
  m.data.endo             <- as.matrix(model.frame(formula = F.formula, data = data, lhs=0, rhs = 2))
  # df.data.exo.endo         <-model.frame(formula = F.formula, data = data, lhs=0, rhs = c(1,2))


  # Create start parameters for optimx ----------------------------------------------------------------

  # Generate with lm if they are missing
  if(is.null(start.params))
    start.params <- coef(lm(formula = formula(F.formula, lhs=1, rhs=1), data = data))
    # f.start.params <- if(use.intercept){y~.}else{y~.+0}
    # start.params <- coef(lm(formula = f.start.params, data = data.frame(y=vec.data.y, df.data.exo.endo)))

  # Add rho and sigma with 0.001 and 0.9998 as defaults
  start.params <- c(start.params, rho=0.001, sigma=0.9998)


  # Bootstrap for SD of coefs--------------------------------------------------------------------------

  # Definition: Helper function to call LL optimization for convenience
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, m.data.endo){
    return(optimx(par = optimx.start.params, fn = copulaCorrection_LL,
                       vec.y=vec.data.y, m.data.exo.endo=m.model.data.exo.endo, m.data.endo=m.data.endo,
                       method="BFGS", control=list(trace=0)))
  }

  # Run once for coef estimates and start parameters for bootstrapping
  res.once.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y, m.model.data.exo.endo = m.model.data.exo.endo, m.data.endo = m.data.endo)
  res.start.params <- coef(res.once.optimx)[1,] #extract parameters

  # Could pre-define all bootstrap indices for minimal speed improvement
  # bootstrapping.indices <- matrix(sample(x=length(vec.data.y)))
  # pb <- txtProgressBar(initial = 1, max = num.boots, style = 3)
  res.boots <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      # setTxtProgressBar(pb, i-1)
      indices                 <- sample(x = length(vec.data.y), size=length(vec.data.y)*0.8, replace = TRUE)
      i.y                     <- vec.data.y[indices]
      i.m.model.data.exo.endo <- m.model.data.exo.endo[indices, ,drop=FALSE]
      i.m.data.endo           <- m.data.endo[indices, ,drop=FALSE]
      # return as vector / first row (only 1 method used). As matrix it cannot be used again as input to optimx
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y, m.model.data.exo.endo = i.m.model.data.exo.endo, m.data.endo = i.m.data.endo))[1,])
    })

  # print(res.boots)

  # Calculate SD and mean of bootstrapped parameters
  # Rows = per parameter,  Columns = for each boots run
  parameter.sd   <- apply(res.boots, 1, sd)
  parameter.mean <- apply(res.boots, 1, mean)
  names(parameter.mean) <- names(parameter.sd) <- names(start.params)

  # Return --------------------------------------------------------------------------------------------
  names.params.exo.endo <- setdiff(names(parameter.mean), c("rho", "sigma"))
  # Basic content
  ans <- structure(list(call = cl,
                        parameter.mean = parameter.mean, parameter.sd=parameter.sd,
                        log.likelihood = res.once.optimx[,"value"], conv.code = res.once.optimx[,"convcode"],
                        regressors = m.model.data.exo.endo), # **model or data??
                   class="rendo.copulacorrection.continuous1")
  # Add more
  ans$AIC <- (-2)*(ans$log.likelihood) + 2                 * length(parameter.mean)
  ans$BIC <- (-2)*(ans$log.likelihood) + NROW(vec.data.y)  * length(parameter.mean)
  ans$fitted.values <- as.numeric(parameter.mean[names.params.exo.endo]) * ans$regressors
  ans$residuals     <- as.matrix(vec.data.y-ans$fitted.values)

  return(ans)
}
