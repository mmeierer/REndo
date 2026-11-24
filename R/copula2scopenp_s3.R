#' Summarizing Nonparametric 2sCOPE Model Fits
#'
#' @param object an object of class `rendo.copula.2scope.np`, a result of a call to
#'   [copula2sCOPEnp()].
#' @param ... ignored
#'
#' @description
#'
#' `summary` method for a model of class `rendo.copula.2scope.np` resulting
#' from fitting [copula2sCOPEnp()].
#'
#' @details
#'
#' In the first stage, [np::npcdistbw()] computes bandwidths for each endogenous
#' regressor (which later feed the conditional CDF estimation). The summary reports one
#' block per endo regressor, each a table of the selected bandwidths with the following
#' columns:
#'
#' * **Role**: Whether the variable is the dependent variable in the conditional CDF
#'   (endogenous) or an explanatory variable (exogenous).
#' * **Type**: Type detected by [np::npcdistbw()]: One of `continuous`, `ordered`, or
#'   `unordered`.
#' * **Scale factor**: Continuous variables only: The bandwidth scale factor.
#' * **Bandwidth**: Continuous variables only: The bandwidth in original units.
#' * **Lambda**: Discrete variables only (`ordered`/`unordered`): The estimated bandwidth.
#' * **Lambda Max**: Discrete variables only: The maximum possible value of `Lambda`.
#'
#' @seealso [np::npcdistbw()], [copula2sCOPEnp()]
#'
#' @md
#' @export
summary.rendo.copula.2scope.np <- function(object, ...) {
  # Get the summary from the parent class ------------------------------------
  res <- NextMethod()

  # bandwidth summary data
  res$bws.summaries <- lapply(
    X = object$labels.endo,
    FUN = function(endo.i) {
      return(copula2scopenp_summary_condbw(object = object, endo.label = endo.i))
    }
  )

  # Keep all the inherited summary classes from to use their print functions
  class(res) <- c("summary.rendo.copula.2scope.np", class(res))
  return(res)
}


#' @export
print.summary.rendo.copula.2scope.np <- function(
  x,
  digits = max(3L, getOption("digits") - 3L),
  signif.stars = getOption("show.signif.stars"),
  ...
) {
  # parent sections first
  NextMethod()

  blocks <- x$bws.summaries
  max.width <- min(65, 0.9 * getOption("width"))
  ruler <- strrep("-", max.width)

  # is the same in all endo blocks?
  metadata_all_same <- function(block) {
    return(paste(
      block$pmethod,
      block$ptype,
      # block$pcxkertype,
      sep = " "
    ))
  }
  all.same <- length(unique(vapply(blocks, metadata_all_same, character(1)))) == 1

  # shared section header (only when metadata is unifor
  if (all.same) {
    first <- blocks[[1]]
    cat("\n")
    cat("First-stage bandwidth selection\n")
    cat("  Method: ", first$pmethod, "\n", sep = "")
    cat("  Bandwidth type: ", first$ptype, "\n", sep = "")
    # Skip kernel type because other wise also have to print for ordered & unordered
    # cat("  Continuous Kernel type: ", first$pcxkertype, "\n", sep = "")
  }

  # per-endo blocks
  for (b in blocks) {
    cat(ruler, "\n", sep = "")
    copula2scopenp_print_condbw(block = b, print.meta = !all.same, digits = digits, ...)
    cat("\n")
  }

  return(invisible(x))
}


# Collect bw data for summary print (for single endo)
#' @importFrom stats formula
copula2scopenp_summary_condbw <- function(object, endo.label) {
  bw <- object$bws[[endo.label]]
  mf <- object$first.stage.frames[[endo.label]]

  # get string labels from boolean masks
  type_label <- function(is.continuous, is.unordered) {
    return(ifelse(
      is.continuous,
      "continuous",
      ifelse(is.unordered, "unordered", "ordered")
    ))
  }

  # Content in np::condistbw
  # - x$bandwidth: bandwidth (continuous) + lambda (discrete)
  # - x$sumNum: scale factor (continuous) + lambda max (discrete)
  #
  # Mapping: Source by type
  # Continuous variables
  #   - raw bandwidth: x$bandwidth
  #   - scale factor: x$sumNum
  #   - lambda: <NA>
  #   - lambda max: <NA>
  #
  # Discrete variables:
  #   - raw bandwidth: <NA>
  #   - scale factor: <NA>
  #   - lambda: x$bandwidth
  #   - lambda max: x$sumNum
  #

  # Dep var: Endo (first row)
  dv <- data.frame(
    name = bw$ynames,
    role = "endo (dep var)",
    type = type_label(is.continuous = bw$iycon, is.unordered = bw$iyuno),
    # continuous
    scale = ifelse(bw$iycon, bw$sumNum$y, NA_real_),
    raw = ifelse(bw$iycon, bw$bandwidth$y, NA_real_),
    # discrete
    lambda = ifelse(bw$iycon, NA_real_, bw$bandwidth$y),
    lambda.max = ifelse(bw$iycon, NA_real_, bw$sumNum$y),
    stringsAsFactors = FALSE
  )

  # explanatory variables (one row each)
  exo <- data.frame(
    name = bw$xnames,
    role = "exo (exp var)",
    type = type_label(is.continuous = bw$ixcon, is.unordered = bw$ixuno),
    # continuous
    scale = ifelse(bw$ixcon, bw$sumNum$x, NA_real_),
    raw = ifelse(bw$ixcon, bw$bandwidth$x, NA_real_),
    # discrete
    lambda = ifelse(bw$ixcon, NA_real_, bw$bandwidth$x),
    lambda.max = ifelse(bw$ixcon, NA_real_, bw$sumNum$x),
    stringsAsFactors = FALSE
  )

  bandwidths <- rbind(dv, exo, stringsAsFactors = FALSE)
  row.names(bandwidths) <- NULL

  res <- list(
    endo = endo.label,
    formula = formula(mf),
    labels.exo = object$labels.exo,
    n.exo = length(object$labels.exo),
    method = bw$method,
    pmethod = bw$pmethod,
    ptype = bw$ptype,
    # pcxkertype = bw$pcxkertype,
    bandwidths = bandwidths
  )

  # optimization only happened with cv.ls, not rule of thumb
  if (identical(bw$method, "cv.ls")) {
    res$fval <- list(
      best = bw$fval,
      range = range(bw$fval.history),
      nmulti = length(bw$fval.history)
    )
  }

  return(res)
}


copula2scopenp_print_condbw <- function(
  block,
  print.meta = TRUE,
  digits = max(3L, getOption("digits") - 3L),
  ...
) {
  # intro ---------------------------------------------------------------
  cat(
    "Bandwidths for estimating F(",
    block$endo,
    " | <all exo vars>)\n",
    sep = ""
  )

  if (print.meta) {
    cat("Method: ", block$pmethod, "\n", sep = "")
    cat("Bandwidth type: ", block$ptype, "\n", sep = "")
    # cat("Continuous Kernel type: ", block$pcxkertype, "\n", sep = "")
  }

  # fval.history (if done cv) --------------------------------------------
  if (!is.null(block$fval)) {
    cat("\n")
    cat(
      "Objective function at minimum (fval) across ",
      block$fval$nmulti,
      " restarts:\n",
      sep = ""
    )
    cat(
      "  range = [",
      format(x = block$fval$range[1], digits = digits),
      ", ",
      format(x = block$fval$range[2], digits = digits),
      "]\n",
      sep = ""
    )
    cat("  best  = ", format(x = block$fval$best, digits = digits), "\n", sep = "")
  }

  # bandwidth table -----------------------------------------------------
  cat("\n")
  bw <- block$bandwidths

  fmt <- function(z) {
    return(ifelse(is.na(z), "-", format(x = z, digits = digits)))
  }

  col.names <- c("Role", "Type", "Scale factor", "Bandwidth", "Lambda", "Lambda Max")
  tab <- matrix(
    data = "",
    nrow = nrow(bw),
    ncol = length(col.names),
    dimnames = list(bw$name, col.names)
  )

  tab[, "Role"] <- bw$role
  tab[, "Type"] <- bw$type
  tab[, "Scale factor"] <- fmt(bw$scale)
  tab[, "Bandwidth"] <- fmt(bw$raw)
  tab[, "Lambda"] <- fmt(bw$lambda)
  tab[, "Lambda Max"] <- fmt(bw$lambda.max)

  print(tab, quote = FALSE, right = FALSE, print.gap = 2)

  return(invisible(block))
}
