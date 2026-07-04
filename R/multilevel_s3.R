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
  # Short print similar to lmer

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Number of levels: ", x$num.levels,"\n", sep = "")
  cat("Number of observations: ", nobs(x),"\n", sep = "")
  if(x$num.levels == 2)
    cat("Number of groups: L2(",names(x$l.group.size[["L2"]]),"): ",x$l.group.size[["L2"]],"\n\n",sep="")
  else
    cat("Number of groups: L2(",names(x$l.group.size[["L2"]]),"): ",x$l.group.size[["L2"]],
        "  L3(",names(x$l.group.size[["L3"]]),"): ",x$l.group.size[["L3"]],
        "\n\n",sep="")

  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)

  invisible(x)
}


#' @title Summarizing Multilevel GMM Estimation with Endogenous Regressors Model Fits
#'
#' @param object an object of class "rendo.multilevel", usually, a result of a call to \code{multilevelIV}.
#' @param model character string to indicate which fitted model should be summarized.
#' Possible values are: \code{"REF", "FE_L2", "FE_L3", "GMM_L2"}, or \code{"GMM_L3"}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#'
#' \code{summary} method for class "\code{rendo.multilevel}".
#'
#' @details
#'
#' The multilevelIV() function estimates three models, namely: the usual random
#' effects model (REF), the fixed effects model (FE) and the hierarchical GMM model (GMM) proposed by Kim and Frees (2007).
#' The fixed effects and the GMM estimators are calculated at each level - so in the case of a three-level model, the function estimates,
#' besides the random effects, fixed effects models at level two (FE_L2) and at level three (FE_L3).
#' The same is true for the GMM estimators, the multilevelIV() function will return a GMM estimator
#' at level-three (GMM_L3) and a GMM estimator at level two (GMM_L2).
#'
#' In order to facilitate the choice of estimator to be used, the \code{summary()} function also returns an omitted variable test (OVT).
#' This test is based on the Hausman test for panel data. The OVT allows the comparison of a robust eastimator and an estimator which is efficient
#' under the null hypothesis of no omitted variables. Moreover, it allows the comparison of two robust
#' estimators at different levels.
#'
#' For the model specified in  argument \code{model}, the \code{summary()} function returns the
#' summary statistics of the estimated coefficients, together with the results of the omitted variable test
#' between the specified model and each other model.
#'
#' @return
#' For the model specified in argument \code{model}, the function \code{summary.rendo.multilevel} computes and returns
#' a list of summary statistics and the results of the omitted variable tests for the fitted multilevel object given in \code{object}.
#'
#' An object of class \code{summary.rendo.multilevel} is returned that is a list using the component \code{call} of argument \code{object}, plus,
#'
#' \item{summary.model}{the model parameter with which the summary function was called.}
#' \item{coefficients}{a \code{px4} matrix with columns for the estimated coefficients, its standard error,
#' the t-statistic and corresponding (two-sided) p-value.}
#' \item{OVT.table}{results of the Hausman omitted variable test for the specified model compared to all other models.}
#' \item{vcov}{variance covariance matrix derived from the GMM fit of this model.}
#'
#' @seealso The model fitting function \code{\link[REndo:multilevelIV]{multilevelIV}}
#' @seealso Function \code{coef} will extract the \code{coefficients} matrix and
#' function \code{vcov} will extract the component \code{vcov}.
#'
#' @examples
#'
#' data("dataMultilevelIV")
#' # Fit two levels model
#' res.ml.L2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                               X32 + X33 + (1|SID) | endo(X15),
#'                           data = dataMultilevelIV, verbose = FALSE)
#'
#' # Get summary for FE_L2 (does not print)
#' res.sum <- summary(res.ml.L2, model = "FE_L2")
#' # extract table with coefficients summary statistics
#' sum.stat.FE_L2 <- coef(res.sum)
#' # extract vcov of model FE_L2
#' FE_L2.vcov <- vcov(res.sum)
#' # same as above
#' FE_L2.vcov <- vcov(res.ml.L2, model = "FE_L2")
#'
#' @importFrom stats pt fitted pnorm
#' @export
summary.rendo.multilevel <- function(object, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){

  # check model input
  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)

  # Copy from input
  res <- object[c("call", "l.group.size", "num.levels")]
  res$nobs <- nobs(object)
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


  # Omitted Variable tests ---------------------------------------------------------------

  # Make table with row for each test, and statistics
  #   OVT are stored with names MODEL_vs_MODEL
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
#     Omitted variable tests
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.multilevel <- function(x, digits = max(3L, getOption("digits")-3L),
                                         signif.stars = getOption("show.signif.stars"), ...){

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Number of levels: ", x$num.levels,"\n", sep = "")
  cat("Number of observations: ", x$nobs,"\n", sep = "")
  if(x$num.levels == 2)
    cat("Number of groups: L2(",names(x$l.group.size[["L2"]]),"): ",x$l.group.size[["L2"]],"\n\n",sep="")
  else
    cat("Number of groups: L2(",names(x$l.group.size[["L2"]]),"): ",x$l.group.size[["L2"]],
                        "  L3(",names(x$l.group.size[["L3"]]),"): ",x$l.group.size[["L3"]],
        "\n\n",sep="")


  # Main model coefficients ----------------------------------------------------------------------
  cat("Coefficients for model ", x$summary.model, ":\n", sep = "")

  coefs <- x$coefficients
  printCoefmat(coefs, digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)


  # Omitted variable test ------------------------------------------------------------------------
  cat("\nOmitted variable tests for model ",x$summary.model,":\n", sep = "")
  printCoefmat(x$OVT.table, digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)

  return(invisible(x))
}

# Not really needed because coef.default already works but for clarity
#' @export
coef.summary.rendo.multilevel <- function(object, ...){
  return(object$coefficients)
}

#' @export
vcov.summary.rendo.multilevel <- function(object, ...){
  # Model is determined in summary already
  return(object$vcov)
}

#' @title Predict method for Multilevel GMM Estimations
#'
#' @description
#' Predicted values based on multilevel models employing the GMM approach for hierarchical data with
#' endogenous regressors.
#'
#' @param object Object of class inheriting from "rendo.multilevel"
#' @param newdata An optional data frame in which to look for variables with which to predict.
#' If omitted, the fitted values for the specified model are returned.
#' @param model character string to indicate for which fitted model predictions are made.
#' Possible values are: \code{"REF", "FE_L2", "FE_L3", "GMM_L2"}, or \code{"GMM_L3"}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @return
#' \code{predict.rendo.multilevel} produces a vector of predictions
#'
#' @seealso The model fitting function \code{\link[REndo:multilevelIV]{multilevelIV}}
#'
#' @examples
#' data("dataMultilevelIV")
#'
#' # Two levels
#' res.ml.L2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                           X32 + X33 + (1|SID) | endo(X15),
#'                           data = dataMultilevelIV, verbose = FALSE)
#' predict(res.ml.L2, model = "FE_L2")
#'
#' # using the data used for fitting also for predicting,
#' #    correctly results in fitted values
#' all.equal(predict(res.ml.L2, dataMultilevelIV, model = "GMM_L2"),
#'           fitted(res.ml.L2, model = "GMM_L2")) # TRUE
#'
#' @importFrom Formula as.Formula
#' @importFrom lme4 lFormula
#' @export
predict.rendo.multilevel <- function(object, newdata, model=c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), ...){

  check_err_msg(checkinput_multilevel_model(object=object,model=model))
  model <- match.arg(arg = model, choices = c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"), several.ok = FALSE)

  if(length(list(...)))
    warning("The arguments in ... are ignored.", call. = FALSE, immediate. = TRUE)

  if(missing(newdata)){
    return(fitted(object, model=model))
  }else{

    model.coef <- coef(object)[, model, drop=TRUE]

    # Get X by processing newdata by lme4 (same steps as when fitting the model)
    f.lmer  <- formula(Formula::as.Formula(object$formula), lhs = 1, rhs = 1)
    l4.form <- lme4::lFormula(formula = f.lmer, data=newdata)
    X       <- l4.form$X

    return(drop(X %*% model.coef))
  }
}
