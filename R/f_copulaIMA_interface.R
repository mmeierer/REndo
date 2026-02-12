#' #' Copula-based Instrumental Variable Model (IMA)
#'
#' Fitting Haschka's copula-based Instrumental Variable Model (IMA) using
#' Gaussian copulas to address endogeneity without external instruments.
#'
#' @param formula Description of the model to be fitted.
#'   The formula follows a two-part notation separated by a vertical bar
#'   (\code{|}), where the first part specifies the structural equation and
#'   the second part specifies the endogenous regressors.
#' @param data A data.frame containing all variables used in the model.
#' @param cdf Character string specifying the method used to estimate the
#'   marginal distribution functions of the endogenous regressors.
#'   One of \code{"adj.ecdf"}, \code{"resc.ecdf"}, \code{"ecdf"}, or \code{"kde"}.
#' @param num.boots Integer giving the number of bootstrap replications
#'   used for inference.
#' @param verbose if \code{TRUE}, progress messages are printed.
#'
#' @return An object of classes \code{rendo.copula.ima}, \code{rendo.boots},
#'   and \code{rendo.base}, which contains the fitted model, coefficient
#'   estimates, and bootstrap results.
#'
#' @description
#' The copula-based IMA approach corrects endogeneity by constructing
#' control functions from the marginal distributions of the endogenous
#' regressors and incorporating them into the estimation equation.
#' Inference is based on bootstrapping.
#'
#' @details
#' The method relies on estimating the marginal distribution functions of
#' the endogenous regressors using either empirical distribution functions
#' or kernel density estimation. These are transformed via the inverse
#' normal distribution and used to construct copula-based control terms.
#'
#' @examples
#' \dontrun{
#' data(dataCopIMABiExo)
#' res <- copulaIMA(
#'   y ~ x1 + x2 | p,
#'   data = dataCopIMABiExo,
#'   cdf = "adj.ecdf",
#'   num.boots = 100
#' )
#' summary(res)
#' }
#'
#' @export
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

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)
  mf <- model.frame(formula(F.formula, rhs = 1, lhs = 1), data = data)

  # Fitting with copula IMA model
  if (verbose) message("Fitting Haschka's copula-based IMA model")
  fit <- CopulaIMA_fit(F.formula, mf, cdf)

  # Bootstrapping
  if (verbose) message("Running ", num.boots, " bootstraps")

  n <- nrow(mf)
  coef.names <-names(coef(fit))

  #creating a matrix to add bootstrap in each column
  boots <- matrix(NA, nrow = length(coef.names), ncol = num.boots, dimnames = list(coef.names, NULL))

  b <- 1 #tracking the number of successful bootstrap replications

  repeat{

    if (b > num.boots) break # stop repeating if successful bootstrap is drawn

    index <- sample.int(n, replace = TRUE) #resampling
    mf.b <- mf[index, , drop = FALSE]

    #estimating
    fit.b <- try(CopulaIMA_fit(F.formula, mf.b, cdf), silent = TRUE)

    #taking into account only adequate draws
    if(!inherits(fit.b, "try-error")){
      boots[, b] <- coef(fit.b)
      b <- b + 1

    }
  }

  return(build_rendo_boots_ima(cl, F.formula, mf, fit, boots))
}

