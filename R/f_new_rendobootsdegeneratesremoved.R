doc_rendobootsdegeneratesremoved_return_list <- function() {
  doc_boots <- doc_rendoboots_return_list()
  doc_boots_removed <- c(
    n.boots.attempted = "\\item{\\code{n.boots.attempted}}{The number of bootstrap iterations that were attempted.}",
    n.boots.failed = "\\item{\\code{n.boots.failed}}{The number of bootstrap iterations resulting in a failed fit.}"
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

  repeat {
    # stop repeating if successful B bootstrap have been drawn
    if (b > num.boots) {
      break
    }

    attempt <- attempt + 1

    # resampling
    index <- sample.int(n, replace = TRUE)
    data.b <- data[index, , drop = FALSE]

    # estimating
    fit.b <- try(fn.fit(data.b = data.b), silent = TRUE)

    # taking into account only adequate draws
    if (!inherits(fit.b, "try-error")) {
      boots[, b] <- coef(fit.b)
      b <- b + 1

      if (verbose) {
        setTxtProgressBar(pb = pb, value = b)
      }
    } else {
      failed <- failed + 1
    }
  }

  if (verbose) {
    close(pb)
  }

  if (failed > 0) {
    fail.rate <- failed / attempt

    warning(
      round(100 * fail.rate, 2),
      "% of bootstrap samples were degenerate and discarded",
      call. = FALSE
    )
  }

  return(list(
    boots.params = boots,
    n.failed = failed,
    n.attempted = attempt
  ))
}
