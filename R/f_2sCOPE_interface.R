#' @export
#'
copula2sCOPE <- function(
  formula,
  data,
  cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),
  num.boots = 1000,
  verbose = TRUE
) {
  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copula2scope_formula(formula))
  check_err_msg(checkinput_copula2scope_data(data))
  check_err_msg(checkinput_copula2scope_dataVSformula(data, formula))
  check_err_msg(checkinput_copula2scope_numboots(num.boots))
  check_err_msg(checkinput_copula2scope_verbose(verbose))
  check_err_msg(checkinput_copula2scope_cdf(cdf))

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)

  # Fitting with 2sCOPE
  if (verbose) {
    message("Fitting 2sCOPE model")
  }
  fit <- copula2scope_fit(F.formula, data, cdf)

  # Bootstrapping
  if (verbose) {
    message("Running ", num.boots, " bootstraps")
  }

  n <- nrow(data)
  coef.names <- names(coef(fit))

  #creating a matrix to add bootstrap in each column
  boots <- matrix(
    NA,
    nrow = length(coef.names),
    ncol = num.boots,
    dimnames = list(coef.names, NULL)
  )

  b <- 1 #tracking the number of successful bootstrap replications
  failed <- 0 # tracking the number of failed attempts
  attempt <- 0 #total attempts

  repeat {
    if (b > num.boots) {
      break
    } # stop repeating if successful B bootstrap is drawn

    attempt <- attempt + 1

    index <- sample.int(n, replace = TRUE) #resampling
    data.b <- data[index, , drop = FALSE]

    #estimating
    fit.b <- try(copula2scope_fit(F.formula, data.b, cdf), silent = TRUE)

    #taking into account only adequate draws
    if (!inherits(fit.b, "try-error")) {
      boots[, b] <- coef(fit.b)
      b <- b + 1
    } else {
      failed <- failed + 1
    }
  }

  if (failed > 0) {
    fail.rate <- failed / attempt

    warning(
      round(100 * fail.rate, 2),
      "% of bootstrap samples were degenerate and discarded",
      call. = FALSE
    )
  }

  return(new_rendo_copula2scope(
    call = cl,
    F.formula = F.formula,
    res.lm = fit,
    boots.params = boots,
    n.boots.attempted = attempt,
    n.boots.failed = failed,
    cdf = cdf
  ))
}
