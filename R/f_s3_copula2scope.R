#' @export
summary.rendo.copula.2sCOPE <- function(object, ...) {
  # The default rendo.boots.degenerates.removed summary but add some copula2sCOPE things

  # Get the summary from the `rendo.boots.degenerates.removed` parent class ------------
  res <- NextMethod()

  # Add the copula2sCOPE specific parts ------------------------------------------------
  res$names.endo.regs <- object$names.endo.regs
  res$cdf <- object$cdf

  # Keep all the inherited summary classes from to use their print functions
  class(res) <- c("summary.rendo.copula.2sCOPE", class(res))

  return(res)
}

#' @export
print.summary.rendo.copula.2sCOPE <- function(
    x,
    digits = max(3L, getOption("digits") - 3L),
    signif.stars = getOption("show.signif.stars"),
    ...
) {
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  # Print summary of inherited classes (`summary.rendo.boots.degenerates.removed`) -----
  NextMethod()

  # Print copula2sCOPE specific parts --------------------------------------------------
  cat("\n")
  endo.vars <- paste0(
    strwrap(paste0(x$names.endo.regs, collapse = ", "), width = max.width),
    collapse = "\n"
  )
  cat("Continuous endogenous variables: ", endo.vars, "\n", sep = "")
  cat("Used cdf: ", x$cdf, "\n", sep = "")

  return(invisible(x))
}
