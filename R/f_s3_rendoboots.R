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

#' @export
confint.rendo.boots <- function(object, parm, level=0.95, ...){

  if(ncol(object$boots.params) < 100)
    stop("Cannot calculate confidence intervals with less than 100 bootstrapped estimates!", call. = FALSE)

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
  #   For the given level, search the corresponding coefficients in
  #     the sorted bootstrapped params

  # Sort bootstrapped params by size
  #   Apply returns with params colwise!
  sorted.boots.params <- apply(object$boots.params, 1, sort)

  # sorted low values on top -> lower = first values (=1-level)
  lower.cut <- nrow(sorted.boots.params) * (1-level)
  upper.cut <- nrow(sorted.boots.params) * level

  # t() because apply transposed it first
  #   apply t() first to access inexistent parms graciously and return NA (does not work if in columns)
  ci <- t(sorted.boots.params)[parm, c(lower.cut, upper.cut), drop=FALSE]

  # Return ----------------------------------------------------------------------------------------
  req.a <- (1-level) / 2
  req.a <- c(req.a, 1 - req.a)
  # from stats:::format.perc - cannot call with ::: as gives CRAN note
  names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
  res[] <- ci
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

  all.coefs <- coef(object,complete = TRUE)

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


