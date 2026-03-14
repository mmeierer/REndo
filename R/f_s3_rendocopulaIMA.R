#' @export
summary.rendo.copula.ima <- function(object, ...) {
  # The default rendo.base summary but add some copulaIMA things

  # Get the summary from the `rendo.boots` parent class --------------------------------
  res <- NextMethod()

  # Add the copulaIMA specific parts ---------------------------------------------------
  res$names.endo.regs <- c(character(0), object$names.endo.regs)
  res$cdf <- object$cdf
  res$n.boots.attempted <- object$n.boots.attempted
  res$n.boots.failed <- object$n.boots.failed

  # Keep all the inherited summary classes from `rendo.boots` to use its print function
  class(res) <- c("summary.rendo.copula.ima", class(res))

  return(res)
}

#' @export
print.summary.rendo.copula.ima <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  signif.stars = getOption("show.signif.stars"),
  ...
) {
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  # Print summary of inherited classes (`summary.rendo.boots`) ------------------------
  NextMethod()

  # Print copulaIMA specific parts ----------------------------------------------------
  cat("\n")

  # Some more bootstrapping info before break
  fail.rate <- round(100 * x$n.boots.failed / x$n.boots.attempted, 2)
  cat("Num bootstraps attempted: ", x$n.boots.attempted, "\n", sep = "")
  cat(
    "Num bootstraps failed and discarded: ",
    x$n.boots.failed,
    " (",
    fail.rate,
    "%)\n",
    sep = ""
  )
  cat("\n")
  endo.vars <- paste0(
    strwrap(paste0(x$names.endo.regs, collapse = ", "), width = max.width),
    collapse = "\n"
  )
  cat("Continuous endogenous variables: ", endo.vars, "\n", sep = "")
  cat("Used cdf: ", x$cdf, "\n", sep = "")

  return(invisible(x))
}
