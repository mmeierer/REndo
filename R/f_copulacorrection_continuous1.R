#' @importFrom optimx optimx
#' @importFrom stats lm coef confint sd model.matrix model.frame
#' @importFrom Formula as.Formula model.part
#' @importFrom utils txtProgressBar setTxtProgressBar
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
  # *** Give warning if used
  if(is.null(start.params))
    start.params <- coef(lm(formula = formula(F.formula, lhs=1, rhs=1), data = data))

  # Add rho and sigma with 0.001 and 0.9998 as defaults
  start.params <- c(start.params, rho=0.001, sigma=0.9998)

  # Bootstrap for SD of coefs--------------------------------------------------------------------------

  # Definition: Helper function to call LL optimization for convenience
  fct.optimize.LL <- function(optimx.start.params, vec.data.y, m.model.data.exo.endo, m.data.endo){
    # Bounds for rho (0,1): LL returns Inf if outside as NelderMead cannot deal with bounds
    return(optimx(par = optimx.start.params, fn = copulaCorrection_LL,
                    method="Nelder-Mead", control=list(trace=0),
                    vec.y=vec.data.y, m.data.exo.endo=m.model.data.exo.endo, m.data.endo=m.data.endo))
  }

  # Run once for coef estimates -----------------------------------------------------------------------
  res.once.optimx  <- fct.optimize.LL(optimx.start.params = start.params, vec.data.y = vec.data.y, m.model.data.exo.endo = m.model.data.exo.endo, m.data.endo = m.data.endo)

  # Bootstrapping -------------------------------------------------------------------------------------
  # Could pre-define all bootstrap indices for minimal speed improvement
  # bootstrapping.indices <- matrix(sample(x=length(vec.data.y)))
  pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  res.boots <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      setTxtProgressBar(pb, i)
      indices                 <- sample(x = length(vec.data.y), size=length(vec.data.y)*0.8, replace = TRUE)
      i.y                     <- vec.data.y[indices]
      i.m.model.data.exo.endo <- m.model.data.exo.endo[indices, ,drop=FALSE]
      i.m.data.endo           <- m.data.endo[indices, ,drop=FALSE]
      # return as vector / first row (only 1 method used). As matrix it cannot be used again as input to optimx
      return(coef(fct.optimize.LL(optimx.start.params = start.params, vec.data.y = i.y, m.model.data.exo.endo = i.m.model.data.exo.endo, m.data.endo = i.m.data.endo))[1,])
    })


  # Return --------------------------------------------------------------------------------------------
  # Parameter and sd
  # Boots results: Rows = per parameter,  Columns = for each boots run
  parameter.sd          <- apply(res.boots, 1, sd)   # SD of bootstrapped parameters
  coefficients          <- coef(res.once.optimx)[1,] # extract parameters from single fit
  names(coefficients)   <- names(parameter.sd) <- names(start.params)

  names.params.exo.endo <- setdiff(names(coefficients), c("rho", "sigma"))
  # Basic content
  ans <- structure(list(call           = cl,
                        formula        = formula,
                        initial.values = start.params,
                        coefficients   = coefficients,
                        parameter.sd   = parameter.sd,
                        res.optimx     = res.once.optimx,
                        log.likelihood = res.once.optimx[,"value"],
                        conv.code      = res.once.optimx[,"convcode"],
                        regressors     = m.model.data.exo.endo), # ****model.matrix or data??
                   class="rendo.copulacorrection.continuous1")
  # Add more
  ans$fitted        <- as.vector(coefficients[names.params.exo.endo] %*% t(ans$regressors))
  names(ans$fitted) <- rownames(m.model.data.exo.endo)
  ans$residuals     <- vec.data.y - ans$fitted

  return(ans)
}
