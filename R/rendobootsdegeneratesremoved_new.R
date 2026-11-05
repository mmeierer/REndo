doc_rendobootsdegeneratesremoved_return_list <- function() {
  doc_boots <- doc_rendoboots_return_list()
  doc_boots_removed <- c(
    n.boots.attempted = "\\item{\\code{n.boots.attempted}}{The number of bootstrap iterations that were attempted.}",
    n.boots.failed = "\\item{\\code{n.boots.failed}}{The number of bootstrap samples discarded as degenerate (failed fit, non-finite coefs, or coefs of different length or names than original).}"
  )

  return(c(doc_boots, doc_boots_removed))
}


.new_rendo_boots_degenerates_removed <- function(
  call,
  F.formula,
  mf,
  coefficients,
  names.main.coefs,
  fitted.values,
  residuals,
  # rendo.boot specific
  boots.params,
  # rendo.boots.degenerates.removed specific
  n.boots.attempted,
  n.boots.failed,

  # Can be further extended
  subclass = character(),
  ...
) {
  return(.new_rendo_boots(
    # Stuff for rendo.boots
    call = call,
    F.formula = F.formula,
    mf = mf,
    coefficients = coefficients,
    names.main.coefs = names.main.coefs,
    fitted.values = fitted.values,
    residuals = residuals,
    boots.params = boots.params,

    # Stuff specific to degenerates.removed
    n.boots.attempted = n.boots.attempted,
    n.boots.failed = n.boots.failed,
    subclass = c(subclass, "rendo.boots.degenerates.removed"),

    # Anything else to add to the object
    ...
  ))
}


#' @importFrom utils txtProgressBar setTxtProgressBar
bootstrap_skip_degenerates <- function(fn.fit, data, num.boots, coef.names, verbose) {
  if (verbose) {
    message("Running ", num.boots, " bootstraps.")
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
    # always close: regular exist & cancel (ctrl+c)
    on.exit(close(pb), add = TRUE)
  }

  n <- nrow(data)

  # creating a matrix to add bootstrap in each column
  boots <- matrix(
    NA,
    nrow = length(coef.names),
    ncol = num.boots,
    dimnames = list(coef.names, NULL)
  )

  b <- 1 # the number of successful bootstrap replications
  failed <- 0 # the number of failed attempts
  attempt <- 0 # the number of total attempts
  # prevent infinite sampling: at most 2x num.boots attempts (50% degenerates)
  max.attempts <- 2 * num.boots
  last.reason <- NULL # why the last sample failed

  repeat {
    # stop repeating if successful B bootstrap have been drawn
    if (b > num.boots) {
      break
    }

    if (attempt >= max.attempts) {
      # fmt: skip
      stop(paste0(
        "Bootstrapping stopped after reaching the maximum of ", max.attempts, " attempts (2 x num.boots): ",
        failed, " samples were degenerate. Error in the last degenerate sample: ", last.reason),
        call. = FALSE)
    }

    attempt <- attempt + 1

    # resampling
    index <- sample.int(n, replace = TRUE)
    data.b <- data[index, , drop = FALSE]

    # estimating
    fit.b <- try(fn.fit(data.b = data.b), silent = TRUE)

    # taking into account only adequate draws
    # - did not throw any error
    # - coefs: equal names (incl order) and length
    # - coefs: no non-finite (NA/NaN/+-Inf etc from linear combinations)
    is.adequate <- !inherits(fit.b, "try-error") &&
      # identical checks both: length & order
      identical(names(coef(fit.b)), coef.names) &&
      all(is.finite(coef(fit.b)))

    if (is.adequate) {
      boots[, b] <- coef(fit.b)
      b <- b + 1

      if (verbose) {
        # b-1: minus 1 because b has already been increased (b+1)
        setTxtProgressBar(pb = pb, value = b - 1)
      }
    } else {
      failed <- failed + 1
      # store failure reason here to use in max.attempts check
      if (inherits(fit.b, "try-error")) {
        last.reason <- conditionMessage(attr(fit.b, "condition"))
      } else {
        last.reason <- "Coefficients differ from the original fit or are not finite."
      }
    }
  }

  if (failed > 0) {
    fail.rate <- failed / attempt

    warning(
      round(100 * fail.rate, 2),
      "% of bootstrap samples were degenerate and discarded. ",
      "Error in the last degenerate sample: ",
      last.reason,
      call. = FALSE
    )
  }

  return(list(
    boots.params = boots,
    n.failed = failed,
    n.attempted = attempt
  ))
}
