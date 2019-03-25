#' @export
#' @importFrom stats nobs
logLik.rendo.latent.IV <- function(object, ...){
  return(structure(nall  = nobs(object),
                   object$res.optimx$value,
                   nobs  = nobs(object),
                   df    = ncol(coef(object$res.optimx)),
                   class ="logLik"))
}


#' @export
#' @importFrom stats vcov
#' @importFrom corpcor pseudoinverse
vcov.rendo.latent.IV <- function(object, ...){

  # check hessian
  if(any(!is.finite(object$hessian)))
    stop("The vcov matrix cannot be calulated because the hessian contains non-finite values!", call. = FALSE)

  # invert hessian
  hessian.inv <- corpcor::pseudoinverse(object$hessian)

  # ensure the ordering is correct
  m.delta.diag <- object$m.delta.diag
  m.delta.diag <- m.delta.diag[rownames(object$hessian), colnames(object$hessian)]

  # to get vcov, do deltamethod with diag matrix saved when fitting the model
  m.vcov <- m.delta.diag %*% hessian.inv %*% m.delta.diag
  rownames(m.vcov) <- colnames(m.vcov) <- colnames(object$hessian)

  return(m.vcov)
}



#' @importFrom stats pt AIC BIC logLik fitted
#' @export
summary.rendo.latent.IV <- function(object, ...){

  # Copy from input
  res <- object[c("call","start.params", "names.main.coefs")]

  # Coefficient table --------------------------------------------------------------------
  # Return the full coefficient table. The subset is to relevant rows is done in the
  #   printing

  all.est.params <- coef(object) # get all coefs


  # If the SEs cannot be calculated because
  #   the vcov contains non-numerics: warn + put NAs
  #   the diag(vcov) is negative: warn + use SEs anyway (they contain NAs then)
  #   the warnings are printed here already and not in the printing because result could
  #     be used directly as object
  se <- tryCatch(suppressWarnings(sqrt(diag(vcov(object)))),
                 error = function(e)return(e))


  if(is(object = se, class2 = "simpleError")){
    warning("The standard errors could not be calculated because the vcov contains non-numeric elements.", call. = FALSE)
    se <- rep(NA, times=length(all.est.params))
  }else{
    # only check if not error
    if(anyNA(se))
      warning("For some parameters the standard error could not be calculated.", call. = FALSE)
  }

  # z-score
  z.val <- all.est.params/se
  # p-values
  p.val <- 2*pt(q=(-abs(z.val)), df=NROW(fitted(object))-1)

  res$coefficients <- cbind(all.est.params,
                            se,
                            z.val,
                            p.val)
  rownames(res$coefficients) <- names(all.est.params)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  # Return object ------------------------------------------------------------------------
  # return NA_ placeholder if cannot calculate vcov
  res$vcov <- tryCatch(vcov(object), error = function(e){
    h <- object$hessian
    h[,] <- NA_real_
    return(h)})

  # optimx conf stuff
  res$log.likelihood <- as.numeric(logLik(object))
  res$AIC            <- AIC(object)
  res$BIC            <- BIC(object)
  res$conv.code      <- object$res.optimx$convcode
  res$KKT1           <- object$res.optimx[1, "kkt1"]
  res$KKT2           <- object$res.optimx[1, "kkt2"]
  class(res)         <- "summary.rendo.latent.IV"

  return(res)
}



# Read out full coefficients table from summary, including the non-main model params
#' @export
coef.summary.rendo.latent.IV <- function(object, ...){
  return(object$coefficients)
}

# Read out full vcov matrix
#' @export
vcov.summary.rendo.latent.IV <- function(object, ...){
  return(object$vcov)
}


# Print structure:
#     Call
#     Main model coefficient (incl statistics)
#     Further params used during model fitting
#     Optimality stuff
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.latent.IV <- function(x, digits=max(3L, getOption("digits")-3L),
                                         signif.stars = getOption("show.signif.stars"), ...){

  # Max width to not exceed the fixed width of some prints (printCoef/call)
  # max.width <- min(max(nchar(deparse(x$call))), 0.9 * getOption("width"))
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # Main model coefficients
  cat("Coefficients:\n")
  printCoefmat(x$coefficients[x$names.main.coefs, ,drop=FALSE], digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars, ...)

  if(anyNA(x$coefficients[x$names.main.coefs,2]))
    cat("\n",paste0(strwrap("For some parameters the statistics could not be calculated because the Std. Errors are unavailable.",
                            width = max.width),
                    collapse = "\n"), "\n",sep = "")
  cat("\n")

  # Non main model coefs - only show the values, not the statistcs
  cat("Further parameters estimated during model fitting:\n")

  names.non.main.coefs <- setdiff(rownames(x$coefficients), x$names.main.coefs)
  print.default(format(x$coefficients[names.non.main.coefs, "Estimate"], digits = digits), print.gap = 1L,
                quote = FALSE)
  cat("(see help file for details)\n")

  cat("\n")

  cat("Initial parameter values:\n")
  cat( paste0(strwrap( paste0(paste(names(x$start.params), sep = "=",round(x$start.params,digits = digits)),
                              collapse=" "),
                       width = max.width),
              collapse = "\n"))
  cat("\n\n")

  cat("The value of the log-likelihood function:", x$log.likelihood,"\n")
  cat("AIC:",  x$AIC,", BIC:",x$BIC, "\n")
  cat("KKT1:", x$KKT1, " KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")
  invisible(x)
}




