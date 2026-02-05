#' @export
#'
copulaIMA <- function(formula, data, cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),num.boots = 1000,verbose = TRUE)
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
  fit <- CopulaIMA_fit(F.formula, mf, cdf)

  if (num.boots <= 0) {
    return(build_rendo_base_ima(cl, F.formula, mf, fit))
  }

  # Bootstrapping
  if (verbose) message("Running ", num.boots, " bootstraps")

  n <- nrow(mf)
  coef.names <-names(coef(fit))
  #creating a matrix to add bootstrap in each column
  boots <- matrix(NA, nrow = length(coef.names), ncol = num.boots, dimnames = list(coef.names, NULL))

  #looping over the bootstrap replicates
  for(b in seq_len(num.boots)){
    index <- sample.int(n, replace = TRUE) #nonpara bootstrap. n with replacement
    mf.b <- mf[index, ,drop = FALSE]

    fit.b <- try(CopulaIMA_fit(F.formula, mf.b,cdf), silent = TRUE) # fitting model on the bootstrap sample again

    #storing only successful fits and not NAs
    if (!inherits(fit.b, "try-error")){
      boots[, b] <- coef(fit.b)
    }
  }

  build_rendo_boots_ima(cl, F.formula, mf, fit, boots)
}

