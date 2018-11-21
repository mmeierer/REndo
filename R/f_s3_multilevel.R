#' @export
coef.rendo.multilevel <- function(object, ...){
  return(object$coefficients)
}

#' @export
fitted.rendo.multilevel <- function(object, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)
  return(object$l.fitted[[model]])
}

#' @export
residuals.rendo.multilevel <- function(object, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)
  return(object$l.residuals[[model]])
}

#' @export
nobs.rendo.multilevel <- function(object, ...){
  return(NROW(residuals(object=object, model="REF")))
}


#' @export
#' @importFrom stats vcov
vcov.rendo.multilevel <- function(object, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){
  check_err_msg(checkinput_multilevel_model(object=object, model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)

  # return vcov of respective gmm estimation
  return(object$l.vcov[[model]])
}

#' @export
#' @importFrom stats qnorm confint
confint.rendo.multilevel <- function(object, parm, level = 0.95,  model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){
  # check model input
  check_err_msg(checkinput_multilevel_model(object=object, model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)

  # This largely follows stats:::confint.lm to exhibit the exact same behavior

  estim.coefs <- coef(object)[, model, drop = TRUE]

  # KimFrees: p.530:
  # "b_GMM has an asymptotic (n -> Inf) normal distribution with mean beta
  #     and asymptotic variance Gamma(H) Lambda Gamma(H)'" (=vcov)

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
# ** RALUCA: SD = 1 correct ???  ***
    zs <- stats::qnorm(p = req.a, mean = 0, sd = 1)
    ci <- estim.coefs[parm] + sqrt(diag(vcov(object, model = model)))[parm] %o% zs

    # Return ----------------------------------------------------------------------------------------
    # from stats:::format.perc - cannot call with ::: as gives CRAN note
    names.perc <- paste(format(100 * req.a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    res <- array(data = NA, dim = c(length(parm), 2L), dimnames = list(parm, names.perc))
    res[] <- ci
    return(res)
}


#' @export
print.rendo.multilevel <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # Short print similar to lm

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Number of levels: ", x$num.levels,"\n\n", sep = "")

  # Use zapsmall for printing as some coefs are nearly 0
  cat("Coefficients:\n")
  print.default(format(zapsmall(coef(x)), digits = digits), print.gap = 2L, quote = FALSE)

  invisible(x)
}


#' @importFrom stats pt fitted
#' @export
summary.rendo.multilevel <- function(object, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){

  # check model input
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)

  # Copy from input
  res <- object[c("call","coefficients")]
  res$vcov <- vcov(object = object, model = model)
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
  # Use zapsmall for printing as some coefs are nearly 0
  coefs <- x$coefficients
  coefs[,"Estimate"] <- zapsmall(coefs[,"Estimate"])
  printCoefmat(coefs, digits = digits, na.print = "NA",
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
  # Model is determined in summary already
  return(object$vcov)
}
