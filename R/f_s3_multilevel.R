#' @export
coef.rendo.multilevel <- function(object, ...){
  return(object$coefficients)
}

#' @export
fitted.rendo.multilevel <- function(object, model="REF", ...){
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  return(object$l.fitted[[model]])
}

#' @export
residuals.rendo.multilevel <- function(object, model="REF", ...){
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  return(object$l.residuals[[model]])
}

#' @export
nobs.rendo.multilevel <- function(object, ...){
  return(NROW(residuals(object=object, model="REF")))
}


#' @export
#' @importFrom stats vcov
vcov.rendo.multilevel <- function(object, ...){
  return(list(V=object$V, W=object$W))
}


#' @export
print.rendo.multilevel <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # Short print similar to lm
  # Only print the main model coefs

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Number of levels: ", x$num.levels,"\n\n", sep = "")

  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)

  invisible(x)
}


#' @importFrom stats pt fitted
#' @export
summary.rendo.multilevel <- function(object, model=c("REF"), ...){

  # check model input
  check_err_msg(checkinput_multilevel_model(object=object,model=model))

  # Copy from input
  res <- object[c("call","coefficients", "V", "W")]
  res$summary.model <- model

  # Coefficient table --------------------------------------------------------------------
  # Calculate statistics only for selected model

  # Coefficients are stored with names MODEL_vs_MODEL
  #   Select all coefs that belong to the given model
  n.coefs   <- grep(pattern = model, x = colnames(object$coefficients), value = TRUE)
  m.coefs   <- object$coefficients[,    n.coefs, drop=TRUE]
  m.coef.se <- object$coefficients.se[, n.coefs, drop=TRUE]

  # z-score
  z.vals     <- m.coefs/m.coef.se
  # p-values
  p.vals     <- 2*stats::pnorm(-abs(z.vals))

  res$coefficients <- cbind(m.coefs,
                            m.coef.se,
                            z.vals,
                            p.vals)
  rownames(res$coefficients) <- rownames(object$coefficients)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")


  # Ommited Variable tests ---------------------------------------------------------------

  # Make table with row for each test, and statistics
  #   Only select the ones needed
  n.ovt <- grep(pattern = model, x = names(object$l.ovt), value = TRUE)
  l.ovt <- object$l.ovt[n.ovt]
  res$OVT.table <- t(sapply(l.ovt, function(ovt){c(df         = ovt$df,
                                                   chisq.stat = ovt$stat,
                                                   p.val      = ovt$p.val)}))

  # Naming follows (roughly) summary.ivreg
  colnames(res$OVT.table) <- c("df", "Chisq", "p-value")

  # Return object ----------------------------------------------------------------------------
  class(res)    <- "summary.rendo.multilevel"
  return(res)
}


# Print structure:
#     Call
#     Main model coefficient (incl statistics)
#     Ommited variable tests
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.multilevel <- function(x, digits = max(3L, getOption("digits")-3L),
                                         signif.stars = getOption("show.signif.stars"), ...){

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # Main model coefficients ----------------------------------------------------------------------
  cat("Coefficients for model ", x$summary.model, ":\n", sep = "")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)


  # Omitted variable test ------------------------------------------------------------------------
  cat("\nOmitted variable tests for model ",x$summary.model,":\n", sep = "")
  printCoefmat(x$OVT.table, digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)

  return(invisible(x))
}

#' @export
coef.summary.rendo.multilevel <- function(object, ...){
  return(object$coefficients)
}

#' @export
vcov.summary.rendo.multilevel <- function(object, ...){
  return(list(V=object$V, W=object$W))
}
