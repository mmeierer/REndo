#' @export
summary.rendo.copula.ima <- function(object, ...) {
  coefs <- object$coefficients
  coef.names <- names(coefs)

  # Case where there is no bootstrap. Shows estimates only
  if (is.null(object$boots.params)) {
    table <- cbind(Estimate = coefs) #building a coeff table to show only estimates and no SE, Zstat or pvalue
    res <- list(call = object$call,
      coefficients = table,
      has.boots = FALSE
    )
    class(res) <- "summary.rendo.copula.ima"
    return(res)
  }

  # Case where there is bootstrap
  boots <- object$boots.params #the rows are the coeffs and the cols are the bootstrap replicates

  # Aligning the rows
  boots <- boots[coef.names, , drop = FALSE] #bootstrap estimates to be the same order as the coefficients. To make sure SE is good.

  #calculating standard error, Zstat and the pvalue
  se <- apply(boots, 1, sd, na.rm = TRUE)
  z  <- coefs / se
  p  <- 2 * (1 - pnorm(abs(z)))

  #building the table for the coeffs
  table <- cbind(
    Estimate = coefs,
    `Std. Error` = se,
    `z value` = z,
    `Pr(>|z|)` = p
  )

  res <- list(call = object$call,
    coefficients = table,
    has.boots = TRUE,
    num.boots = ncol(boots)
  )

  class(res) <- "summary.rendo.copula.ima"
  res
}

#' @export
print.summary.rendo.copula.ima <- function(x, ...) {

  #print call
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, P.values = "Pr(>|z|)" %in% colnames(x$coefficients), has.Pvalue = "Pr(>|z|)" %in% colnames(x$coefficients))

  #adding a bootstrap footnote. Standard Errors were calculated from the bootstrap
  if (isTRUE(x$has.boots)) {cat("\n(Standard errors from bootstrap)\n")}

  invisible(x)
}

#' @export
print.rendo.copula.ima <- function(x, ...){
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(coef(x))

  if (!is.null(x$boots.params)){
    cat("\nPlease use summary() for bootstrap standard errors.\n")
  }

  invisible(x)
}

