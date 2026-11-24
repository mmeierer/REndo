#' @export
summary.rendo.copula.2stage <- function(object, ...) {
  # The default rendo.boots.degenerates.removed summary but add some copula things

  # Get the summary from the `rendo.boots.degenerates.removed` parent class ------------
  res <- NextMethod()

  # Add the rendo.copula.2stage specific parts -----------------------------------------
  res$labels.endo <- object$labels.endo
  res$cdf <- object$cdf

  # Keep all the inherited summary classes to use their print functions
  class(res) <- c("summary.rendo.copula.2stage", class(res))

  return(res)
}

#' @export
print.summary.rendo.copula.2stage <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  signif.stars = getOption("show.signif.stars"),
  ...
) {
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  # Print summary of inherited classes (`summary.rendo.boots.degenerates.removed`) -----
  NextMethod()

  # Print rendo.copula.2stage specific parts -------------------------------------------
  cat("\n")
  endo.vars <- paste0(
    strwrap(paste0(x$labels.endo, collapse = ", "), width = max.width),
    collapse = "\n"
  )
  cat("Endogenous regressors: ", endo.vars, "\n", sep = "")

  # Not all methods have `cdf`
  if (!is.null(x$cdf)) {
    cat("Used cdf: ", x$cdf, "\n", sep = "")
  }

  return(invisible(x))
}
