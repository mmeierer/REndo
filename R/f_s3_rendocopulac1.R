#' @export
#' @importFrom stats nobs
logLik.rendo.copula.c1 <- function(object, ...){
  return(structure(nall  = nobs(object),
                   object$res.optimx$value,
                   nobs  = nobs(object),
                   df    = ncol(coef(object$res.optimx)),
                   class ="logLik"))
}

# Essentially the exact same summary function as the parent class
#   but also add GOF measures and convergence stuff
#' @export
summary.rendo.copula.c1 <- function(object, ...){
  # Get the normal summary from the rendo.boots parent class
  res <- NextMethod()

  # Also add the optimx stuff
  res$start.params   <- object$start.params
  res$log.likelihood <- as.numeric(logLik(object))
  res$AIC            <- AIC(object)
  res$BIC            <- BIC(object)
  res$conv.code      <- object$res.optimx$convcode
  res$KKT1           <- object$res.optimx[1, "kkt1"]
  res$KKT2           <- object$res.optimx[1, "kkt2"]
  # Keep the summary class from rendo.boots to use its print function
  class(res)    <- c("summary.rendo.copula.c1", class(res))
  return(res)
}


#' @export
print.summary.rendo.copula.c1 <- function(x, digits=max(3L, getOption("digits")-3L),
                                          signif.stars = getOption("show.signif.stars"), ...){
  # Print rendo.boots summary first part
  NextMethod()


  # Print copula C1 specific part -------------------------------------------------------------------------
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\n")

  cat("Initial parameter values:\n")
  cat( paste0(strwrap( paste0(paste(names(x$start.params), sep = "=",round(x$start.params,digits = digits)),
                              collapse=" "),
                       width = max.width),
              collapse = "\n"))

  # Also print the LL infos
  cat("\n\n")
  cat("The value of the log-likelihood function:", x$log.likelihood,"\n")
  cat("AIC:",  x$AIC,", BIC:",x$BIC, "\n")
  cat("KKT1:", x$KKT1, " KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")

  invisible(x)
}
