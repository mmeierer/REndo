#' @export
fitted.rendo.base <- function(object, ...){
  return(object$fitted.values)
}

#' @export
residuals.rendo.base <- function(object, ...){
  return(object$residuals)
}

#' @export
nobs.rendo.base <- function(object, ...){
  return(NROW(object$residuals))
}

#' @export
#' @importFrom stats case.names
case.names.rendo.base <- function(object, ...){
  return(names(residuals(object)))
}


#' @export
coef.rendo.base <- function(object, complete = TRUE, ...){
  if(complete)
    return(object$coefficients)
  else
    return(object$coefficients[object$names.main.coefs])
}

#' @export
#' @importFrom stats qnorm
confint.rendo.base <- function(object, parm, level = 0.95, ...){
  # This largely follows stats:::confint.lm to exhibit the exact same behavior

  estim.coefs <- coef(object)

  # Param selection --------------------------------------------------------------------------------
  if(missing(parm))
    # Use all by default
    parm <- names(estim.coefs)
  else
    if(is.numeric(parm))
      # Make numbers to respective names
      parm <- names(estim.coefs)[parm]

    # CI calc ----------------------------------------------------------------------------------------
    req.a <- (1-level) / 2
    req.a <- c(req.a, 1 - req.a)

    zs <- qnorm(p = req.a, mean = 0, sd = 1)
    ci <- estim.coefs[parm] + sqrt(diag(vcov(object)))[parm] %o% zs

    # Return ----------------------------------------------------------------------------------------
    # from stats:::format.perc - cannot call with ::: as gives CRAN note
    names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
    res[] <- ci
    return(res)
}



#' @export
print.rendo.base <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # Short print similar to lm
  # Only print the main model coefs

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  print.default(format(coef(x)[x$names.main.coefs], digits = digits), print.gap = 2L,
                quote = FALSE)

  invisible(x)
}


