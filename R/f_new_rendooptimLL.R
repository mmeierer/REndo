#' @importFrom Formula as.Formula
new_rendo_optim_LL <- function(call, F.formula, mf, start.params, estim.params, estim.params.se,
                               names.main.coefs,
                               res.optimx, log.likelihood, hessian,
                               fitted.values, residuals, vcov.error) {

  return(structure(
            class = "rendo.optim.LL",
            list(call = call,
                 formula = as.Formula(F.formula), #to be sure its a Formula
                 model   = mf,
                 terms   = terms(mf),
                 start.params = start.params,
                 estim.params = estim.params,
                 estim.params.se = estim.params.se,
                 names.main.coefs = names.main.coefs,
                 res.optimx = res.optimx,
                 log.likelihood = log.likelihood,
                 hessian = hessian,
                 fitted.values = fitted.values,
                 residuals = residuals,
                 vcov.error = vcov.error)))

  # copula
  # # Basic content
  # ans <- structure(list(call           = cl,
  #                       formula        = formula,
  #                       initial.values = start.params,
  #                       coefficients   = coefficients,
  #                       parameter.sd   = parameter.sd,
  #                       res.optimx     = res.once.optimx,
  #                       hessian        = hessian,
  #                       log.likelihood = res.once.optimx[,"value"],
  #                       conv.code      = res.once.optimx[,"convcode"],
  #                       regressors     = m.model.data.exo.endo), # ****model.matrix or data??
  #                  class="rendo.copulacorrection.continuous1")

  # res <- structure(class = "rendo.latentiv",
  #                  list(call           = cl,
  #                       formula        = formula,
  #                       initial.values = start.params,
  #                       coefficients   = estimated.params[names.original.main.coefs],
  #                       group.means    = estimated.params[c("pi1", "pi2")],
  #                       prob.group1    = estimated.params[["theta8"]],
  #                       coefficients.se= param.se[names.original.main.coefs],
  #                       group.means.se = param.se[c("pi1", "pi2")],
  #                       prob.group1.se = param.se[["theta8"]],
  #                       log.likelihood = res.optimx$value,
  #                       conv.code      = res.optimx$convcode,
  #                       res.optimx     = res.optimx,
  #                       hessian        = hessian,
  #                       fitted         = fitted,
  #                       residuals      = residuals))
  #
  # res$vcov.error <- matrix(c(estimated.params["theta5"]^2,
  #                            estimated.params["theta5"]*estimated.params["theta6"],
  #                            estimated.params["theta5"]*estimated.params["theta6"],
  #                            estimated.params["theta6"]^2+estimated.params["theta7"]^2),
  #                          nrow=2, ncol=2)
}
