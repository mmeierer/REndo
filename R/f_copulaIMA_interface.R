#' #' Copula-based Instrumental Variable Model (IMA)
#'
#' Fitting Haschka's copula-based Instrumental Variable Model (IMA) using
#' Gaussian copulas to address endogeneity without external instruments.
#'
#' @template template_param_formuladataverbose
#'
#' @param cdf Character string specifying the method used to estimate the
#'   marginal distribution functions of the endogenous regressors.
#'   One of \code{"adj.ecdf"}, \code{"resc.ecdf"}, \code{"ecdf"}, or \code{"kde"}.
#' @param num.boots Integer giving the number of bootstrap replications
#'   used for inference.
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
#' Bootstrap inference is performed by resampling the data with replacement.
#' Degenerate bootstrap samples (e.g. singular design matrices or failed
#' model estimation) are discarded and resampled until the requested number
#' of valid bootstrap replications is obtained. The percentage of discarded
#' samples is reported as a warning. Confidence intervals are computed
#' using only successful bootstrap replications.
#'
#' Haschka (2025) reported that simulation results showed that copula-based IMA
#' estimator may exhibit a slightly larger bias than alternative two-stage
#' approaches when the sample size is very small (e.g., n = 100).
#' However, the bias decreases rapidly as sample size increases and become
#' negligible for moderate sample sizes (around n >= 600). The estimator appears
#' asymptotically unbiased.
#'
#' @references
#' Haschka, R. E. (2025). Robustness of copula-correction models in causal
#' analysis: Exploiting between-regressor correlation.IMA Journal of Management
#' Mathematics, 36, 161–180
#'
#' @examples
#' data(dataCopIMABiExo)
#' res <- copulaIMA(
#'   y ~ X + P | continuous(P),
#'   data = dataCopIMABiExo,
#'   cdf = "adj.ecdf",
#'   num.boots = 1000
#' )
#' summary(res)
#'
#'
#' @export
#' @importFrom stats coef
#' @importFrom utils txtProgressBar setTxtProgressBar close
copulaIMA <- function(formula, data, cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"),num.boots = 1000,verbose = TRUE)
 {

  cl <- match.call()

  # input checks
  check_err_msg(checkinput_copulaIMA_formula(formula))
  check_err_msg(checkinput_copulaIMA_data(data))
  check_err_msg(checkinput_copulaIMA_dataVSformula(data=data, formula=formula))
  check_err_msg(checkinput_copulaIMA_numboots(num.boots))
  check_err_msg(checkinput_copulaIMA_verbose(verbose))
  check_err_msg(checkinput_copulaIMA_cdf(cdf))

  cdf <- match.arg(cdf, choices = c("adj.ecdf", "resc.ecdf", "ecdf", "kde"))

  F.formula <- Formula::as.Formula(formula)

  # Fitting with copula IMA model
  if (verbose) message("Fitting Haschka's copula-based IMA model")
  fit <- CopulaIMA_fit(F.formula = F.formula, data = data, cdf = cdf)

  # Bootstrapping
  if (verbose){
    message("Running ", num.boots, " bootstraps.")
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  }

  n <- nrow(data)
  coef.names <-names(coef(fit))

  #creating a matrix to add bootstrap in each column
  boots <- matrix(NA, nrow = length(coef.names), ncol = num.boots, dimnames = list(coef.names, NULL))

  b <- 1 #tracking the number of successful bootstrap replications
  failed <- 0 # tracking the number of failed attempts
  attempt <- 0 #total attempts

  repeat{

    if (b > num.boots) break # stop repeating if successful B bootstrap is drawn

    attempt <- attempt + 1

    index <- sample.int(n, replace = TRUE) #resampling
    data.b <- data[index, , drop = FALSE]

    #estimating
    fit.b <- try(CopulaIMA_fit(F.formula = F.formula, data = data.b, cdf = cdf), silent = TRUE)

    #taking into account only adequate draws
    if(!inherits(fit.b, "try-error")){
      boots[, b] <- coef(fit.b)
      b <- b + 1

      if(verbose){
        setTxtProgressBar(pb, b)
      }
    } else{

     failed <- failed + 1
    }
  }

  if(verbose){
    close(pb)
  }

    if (failed >0){

    fail.rate <- failed / attempt

    warning(round(100 * fail.rate, 2), "% of bootstrap samples were degenerate and discarded", call. = FALSE)
  }

  return(build_rendo_boots_ima(call = cl, F.formula = F.formula, res.lm=fit, boots=boots))
}

