#' @title Calculate Variance-Covariance Matrix for Models Fitted with Bootstrapped Parameters
#' @param object a fitted model object with bootstrapped parameters. Typically from \code{copulaCorrection}
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#' The variance-covariance matrix is derived from the bootstrapped parameter estimates stored in the object.
#' It is based on Efron (1979) and calculates the result as follows:
#'
#' \ifelse{html}{\out{<center> 1/(B-1) * &Sigma;(&theta;<sub>b</sub>-&theta;&#773)(&theta;<sub>b</sub>-&theta;&#773)' </center>}}{\deqn{\frac{1}{B-1}\sum_{b=1}^{B}{(\theta_b-\bar\theta)(\theta_b-\bar\theta)}}}
#'
#' where B is the number of bootstraps and \ifelse{html}{\out{&theta;&#773}}{\eqn{\bar\theta}} is the mean of the bootstrapped coefficients.
#'
#'
#' @return
#' A matrix of the estimated covariances between the parameter estimates of the model.
#' The row and column names correspond to the parameter names given by the \code{coef} method.
#'
#' @references
#' Effron, B.(1979). "Bootstrap Methods: Another Look at the Jackknife", The Annals of Statistics, 7(1), 1-26.
#'
#' @export
#' @importFrom stats vcov
vcov.rendo.boots <- function(object, ...){

  #   Vcov is created from the bootstrapped params
  #   Vcov: Vcov= 1/(B-1) * Sum( (theta_b-theta_hat)(theta_b-theta_hat)' )
  #           where theta_hat = 1/B * Sum(theta), ie mean per coef
  mean.coef    <- rowMeans(object$boots.params)

  # Vcov
  #   For every bootstrapped run, do: (theta_b-theta_hat)(theta_b-theta_hat)
  l.diffs <- lapply(seq(ncol(object$boots.params)), function(i){
    params <- object$boots.params[,i,drop=FALSE]
    return((params - mean.coef) %*% t(params - mean.coef))
  })
  m.vcov <-  Reduce(f = "+", x = l.diffs) / (length(l.diffs) - 1)

  rownames(m.vcov) <- colnames(m.vcov) <- names(coef(object))
  return(m.vcov)
}


#' @title Confidence Intervals for Bootstrapped Model Parameters
#' @inheritParams stats::confint.default
#' @param object a fitted model object with bootstrapped parameters. Typically from \code{copulaCorrection}
#' @param ... ignored, for consistency with the generic function.
#'
#' @details
#'
#' Computes the two-sided percentile confidence intervals from the bootstrapped parameter estimates. The intervals are
#' obtained by selecting the quantile of the bootstrapped parameter estimates corresponding to the given alpha level.
#'
#' A minimum of \ifelse{html}{\out{1/min(level, 1-level)}}{\eqn{\frac{1}{min(level, 1-level)}}}
#' parameters estimates are needed to derive the confidence interval. The reason for this is that there is
#' otherwise no natural way to derive the percentiles (ie one cannot reasonably estimate the 95\% quantile of only 7 values).
#'
#' @importFrom stats quantile
#' @export
confint.rendo.boots <- function(object, parm, level=0.95, ...){

  # From package bootBCa:
  # "Like the ordinary percentile interval, the BCa interval becomes inaccurate when the number of
  # bootstrap replications is not much larger than 1/min(alpha,1-alpha) because there are not enough
  # theta* values from which to extract the desired quantile
  # (e.g., one cannot plausibly estimate a 99 % quantile given fewer than 100 values)."

  if(any(rowSums(!is.na(object$boots.params)) < 1/(1-level)))
    stop("Not enough bootstraps (<1/(1-level)) were performed to derive the confidence interval or too many estimates were NA!", call. = FALSE)

  # warn if any bootstrap estimates are NA
  if(anyNA(object$boots.params))
    warning("Confidence intervals might be unreliable because there are NAs in the bootstrapped parameter estimates.", call. = FALSE)

  # This largely follows stats:::confint.lm to exhibit the exact same behavior

  # Only to get names
  estim.coefs <- coef(object, complete = TRUE)

  # Param selection --------------------------------------------------------------------------------
  if(missing(parm))
    # Use all by default
    parm <- names(estim.coefs)
  else
    if(is.numeric(parm))# Make numbers to respective names
      parm <- names(estim.coefs)[parm]


  # Select CI ---------------------------------------------------------------------------------------
  # Percentile confidence intervals from bootstrapped parameter estimates
  #   For the given level, search the corresponding quantiles per parameter (=row-wise)

  req.a <- (1-level) / 2
  req.a <- c(req.a, 1 - req.a)

  ci <- t(apply(object$boots.params, MARGIN = 1, FUN = quantile, probs=req.a, na.rm=TRUE))

  # Return ----------------------------------------------------------------------------------------

  # from stats:::format.perc - cannot call with ::: as gives CRAN note
  names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
  res[] <- ci[parm, ]
  return(res)
}



# called from summary methods of child classes to derive bootstrapping part of summary
#' @export
#' @importFrom stats coef confint vcov
#' @importFrom methods is
summary.rendo.boots <- function(object, ...){

  # Copy from input
  res <- object[c("call", "names.main.coefs")]


  # Coefficient table --------------------------------------------------------------------
  # Return the full coefficient table. The subset is to relevant rows is done in the
  #   printing

  all.coefs <- coef(object, complete = TRUE)

  res$vcov <- vcov(object)
  se       <- sqrt(diag(res$vcov))

  # If confint is not available because not enough bootstraps, show NA
  ci <- tryCatch(confint(object=object, level = 0.95), error=function(e)return(e))
  if(is(ci, "error"))
    ci <- array(data = NA, dim = c(length(all.coefs), 2L))

  res$coefficients <- cbind(all.coefs,
                            se,
                            ci)

  rownames(res$coefficients) <- names(all.coefs)
  # alternative nameing: "Boots CI 97.5%"
  colnames(res$coefficients) <- c("Point Estimate", "Boots SE",
                                  "Lower Boots CI (95%)", "Upper Boots CI (95%)")

  res$num.boots <- ncol(object$boots.params)

  # Return object ------------------------------------------------------------------------
  class(res)    <- "summary.rendo.boots"

  return(res)
}

# Not really needed because coef.default already works but for clarity
#' @export
coef.summary.rendo.boots <- function(object, ...){
  return(object$coefficients)
}

#' @export
vcov.summary.rendo.boots <- function(object, ...){
  return(object$vcov)
}


#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.boots <- function(x, digits=max(3L, getOption("digits")-3L),
                                         signif.stars = getOption("show.signif.stars"), ...){

  # Max width to not exceed the fixed width of some prints (printCoef/call)
  # max.width <- min(max(nchar(deparse(x$call))), 0.9 * getOption("width"))
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # Main model coefficients of original data point estimate
  cat("Coefficients:\n")
  # Treat all params as coef/se type for printing
  printCoefmat(x$coefficients[x$names.main.coefs, ,drop=FALSE],
               na.print = "-", digits = digits, cs.ind = 1:4, tst.ind = numeric(),...)
  cat(paste0("Number of bootstraps: ", x$num.boots))

  invisible(x)
}


