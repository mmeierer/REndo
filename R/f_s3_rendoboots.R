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


#' @details
#' From package bootBCa:
#' "Like the ordinary percentile interval, the BCa interval becomes inaccurate when the number of
#' bootstrap replications is not much larger than 1/min(alpha,1-alpha) because there are not enough
#' theta* values from which to extract the desired quantile
#' (e.g., one cannot plausibly estimate a 99 % quantile given fewer than 100 values)."
#'
#' corresponding percentile using quantile() of standard type
#'
#' @importFrom stats quantile
#' @export
confint.rendo.boots <- function(object, parm, level=0.95, ...){

  if(ncol(object$boots.params) < 1/(1-level))
    stop("Not enough bootstraps (<1/(1-level)) were performed to derive the confidence interval!", call. = FALSE)

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

  # If confint is not available because not enough bootstraps, show NA
  ci <- tryCatch(confint(object=object, level = 0.95), error=function(e)return(e))
  if(is(ci, "error"))
    ci <- array(data = NA, dim = c(length(all.coefs), 2L))

  se <-  tryCatch(sqrt(diag(vcov(object))), error=function(e)return(e))
  if(is(se, "error"))
    se <- rep(NA_real_, length(all.coefs))

  res$coefficients <- cbind(all.coefs,
                            se,
                            ci)

  rownames(res$coefficients) <- names(all.coefs)
  # alternative nameing: "Boots CI 97.5%"
  colnames(res$coefficients) <- c("Point Estimate", "Boots SE",
                                  "Lower Boots CI (95%)", "Upper Boots CI (95%)")

  res$num.boots <- ncol(object$boots.params)

  # Return object ------------------------------------------------------------------------
  # return NA_ placeholder if cannot calculate vcov
  res$vcov <- vcov(object)

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
  cat("\n\n")

  # Non main model coefs - only show the values, not the statistcs
  cat("Further parameters estimated during model fitting:\n")

  names.non.main.coefs <- setdiff(rownames(x$coefficients), x$names.main.coefs)
  # For single non main coef the vec will lose its name, regardless of drop=FALSE. Therefore always add names
  coefs.non.main <- x$coefficients[names.non.main.coefs, "Point Estimate", drop = TRUE]
  names(coefs.non.main) <- names.non.main.coefs
  print.default(format(coefs.non.main, digits = digits), print.gap = 1L, quote = FALSE)
  cat("(see help file for details)\n")

  invisible(x)
}


