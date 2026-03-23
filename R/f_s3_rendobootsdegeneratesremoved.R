#' @export
summary.rendo.boots.degenerates.removed <- function(object, ...) {
  # The default rendo.boots summary but add some

  # Get the summary from the `rendo.boots` parent class --------------------------------
  res <- NextMethod()

  # Add the .degenerates.removed specific parts ----------------------------------------
  res$n.boots.attempted <- object$n.boots.attempted
  res$n.boots.failed <- object$n.boots.failed

  # Keep all the inherited summary classes from `rendo.boots` to use its print function
  class(res) <- c("summary.rendo.boots.degenerates.removed", class(res))

  return(res)
}

#' @export
print.summary.rendo.boots.degenerates.removed <- function(
    x,
    digits = max(3L, getOption("digits") - 3L),
    signif.stars = getOption("show.signif.stars"),
    ...
) {
  x
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  # Print summary of inherited classes (`summary.rendo.boots.degenerates.removed`) -----
  NextMethod()

  # Print .degenerates.removed specific parts ------------------------------------------
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

  return(invisible(x))
}
