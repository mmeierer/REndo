#' @export
#'
copulaIMA <- function(formula, data, cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),num.boots = 199,verbose = TRUE)
 {

  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copulaIMA_formula(formula))
  check_err_msg(checkinput_copulaIMA_data(data))
  check_err_msg(checkinput_copulaIMA_dataVSformula(data, formula))
  check_err_msg(checkinput_copulaIMA_numboots(num.boots))
  check_err_msg(checkinput_copulaIMA_verbose(verbose))
  check_err_msg(checkinput_copulaIMA_cdf(cdf))

  cdf <- match.arg(cdf)

  F.formula <- Formula::as.Formula(formula)
  mf <- model.frame(F.formula, data = data, na.action = na.omit)

  # Fitting with copula IMA model
  if (verbose) message("Fitting Haschka's copula-based IMA model")

  estimated <- CopulaIMA_fit(F.formula, mf, cdf)

  if (num.boots <= 0) {
    return(build_rendo_base_ima(cl, F.formula, mf, estimated))
  }

  # Bootstrapping
  if (verbose) message("Running ", num.boots, " bootstraps")

  boots <- copulaIMA_bootstrap(F.formula, mf, cdf, num.boots)

  build_rendo_boots_ima(cl, F.formula, mf, estimated, boots)
}

