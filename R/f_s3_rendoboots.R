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


#' @importFrom stats pt AIC BIC fitted
#' @export
summary.rendo.boots <- function(object, ...){

  # Copy from input
  res <- object[c("call", "names.main.coefs")]

  # Coefficient table --------------------------------------------------------------------
  # Return the full coefficient table. The subset is to relevant rows is done in the
  #   printing

  all.est.params <- coef(object, complete = TRUE) # get main coefs


  # ** BOOTSTRAP p-value **
  se <- sqrt(diag(vcov(object = object)))

  # z-score
  z.val <- all.est.params/se
  # p-values
  p.val <- 2*pt(q=(-abs(z.val)), df=NROW(fitted(object))-1)

  res$coefficients <- cbind(all.est.params,
                            se,
                            z.val,
                            p.val)

  rownames(res$coefficients) <- names(all.est.params)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

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

  # Main model coefficients
  cat("Coefficients:\n")
  printCoefmat(x$coefficients[x$names.main.coefs, ,drop=FALSE], digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)

  if(anyNA(x$coefficients[x$names.main.coefs,2]))
    cat("\n",paste0(strwrap("For some parameters the statistics could not be calculated because the Std. Errors are unavailable.",
                            width = max.width),
                    collapse = "\n"), "\n",sep = "")
  cat("\n")

  # Non main model coefs - only show the values, not the statistcs
  cat("Further parameters estimated during model fitting:\n")

  names.non.main.coefs <- setdiff(rownames(x$coefficients), x$names.main.coefs)
  # For single non main coef the vec will lose its name, regardless of drop=FALSE. Therefore always add names
  coefs.non.main <- x$coefficients[names.non.main.coefs, "Estimate", drop = TRUE]
  names(coefs.non.main) <- names.non.main.coefs
  print.default(format(coefs.non.main, digits = digits), print.gap = 1L, quote = FALSE)
  cat("(see help file for details)\n")

  invisible(x)
}





