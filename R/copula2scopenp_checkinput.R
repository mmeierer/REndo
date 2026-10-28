#' @importFrom utils getS3method
checkinput_copula2scopenp_npcdistbwargs <- function(npcdistbw.args) {
  # No checks if parameter default
  if (identical(npcdistbw.args, list())) {
    return(c())
  }

  err.msg <- c()

  # Plain list
  if (!identical(class(npcdistbw.args), "list")) {
    return("Parameter 'npcdistbw.args' must be a plain list.")
  }

  # Must be named
  nms <- names(npcdistbw.args)
  if (is.null(nms) || any(nms == "")) {
    return("All elements of 'npcdistbw.args' must be named.")
  }

  # Reserved: Used to pass data into np:npcdistbw
  reserved.nms <- intersect(nms, c("xdat", "ydat"))
  if (length(reserved.nms) > 0) {
    err.msg <- c(
      err.msg,
      paste0(
        "'npcdistbw.args' may not contain reserved argument(s): ",
        toString(reserved.nms)
      )
    )
  }

  # Check names vs arguments of np:npcdistbw()
  valid.nms <- names(formals(getS3method(f = "npcdistbw", class = "default")))
  unknown.nms <- setdiff(nms, valid.nms)
  if (length(unknown.nms) > 0) {
    # only WARN because there is the `...` parameter in np::npcdistbw which may be used!
    warning(
      "Unknown argument(s) in 'npcdistbw.args': ",
      toString(unknown.nms),
      ". Check the documentation '?np::npcdistbw' for accepted parameters.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  return(err.msg)
}

checkinput_copula2scopenp_bws <- function(bws, labels.endo, labels.exo) {
  # NULL is default value
  if (is.null(bws)) {
    return(c())
  }

  err.msg <- c()

  if (!is(bws, "list")) {
    return("Parameter 'bws' must be a list.")
  }

  if (is.null(names(bws)) || any(names(bws) == "")) {
    return("Parameter 'bws' must be a named list.")
  }

  if (anyDuplicated(names(bws))) {
    return("Parameter 'bws' may not contain duplicate names.")
  }

  if (!setequal(names(bws), labels.endo)) {
    return(paste0(
      "Parameter 'bws' must be named after the endogenous variables: ",
      toString(labels.endo),
      "."
    ))
  }

  for (name in names(bws)) {
    if (!is(bws[[name]], "condbandwidth")) {
      err.msg <- c(
        err.msg,
        paste0(
          "Element '",
          name,
          "' of parameter 'bws' must be of class 'condbandwidth', usually the output of 'np::npcdistbw(xdat=<all exo>, ydat=<single endo>)'."
        )
      )
    }
  }

  # TODO: check that fit with xday/ydat and not formula interface?

  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # np applies the bandwidths by position - and not by name. Therefore, the
  # variables must be exactly the same, incl ordering as the cols of the
  # model frame (endo) ~ (all exo)
  colnames.exo <- vapply(
    labels.exo,
    FUN = copula2stage_label_to_colname,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  for (name in names(bws)) {
    colname.endo <- copula2stage_label_to_colname(name)
    # single response
    if (!identical(bws[[name]]$ynames, colname.endo)) {
      # fmt: skip
      err.msg <- c(err.msg, paste0(
        "Element '", name, "' of 'bws' was estimated for endogenous regressor '",
        toString(bws[[name]]$ynames), "' but must be for '", colname.endo, "'."))
    }
    # many dep vars
    if (!identical(bws[[name]]$xnames, colnames.exo)) {
      # fmt: skip
      err.msg <- c(err.msg, paste0(
        "Element '", name, "' of 'bws' was estimated for exogenous regressors '",
        toString(bws[[name]]$xnames), "' but must be for '", toString(colnames.exo), "' (same order required)."))
    }
  }

  return(err.msg)
}

# When bws are given, np::npcdistbw() takes the method from given `bws[[p]]$method`
# and silently ignores `npcdistbw.args=list(bwmethod=...)`.
# Therefore stop if they disagree.
# Assumes bws and npcdistbw.args were checked before
checkinput_copula2scopenp_bws_npcdistbwargs <- function(bws, npcdistbw.args) {
  bwmethod <- npcdistbw.args$bwmethod

  # No `bws`, no `npcdistbw.args$bwmethod`, no compute = no problem
  if (is.null(bws) || is.null(bwmethod) || isFALSE(npcdistbw.args$bandwidth.compute)) {
    return(c())
  }

  err.msg <- c()
  for (name in names(bws)) {
    bws.method <- bws[[name]]$method
    if (!identical(bws.method, bwmethod)) {
      # fmt: skip
      err.msg <- c(err.msg, paste0(
        "Element '", name, "' of parameter 'bws' was estimated with bwmethod '", bws.method, "' but 'npcdistbw.args' requires bwmethod '", bwmethod, "'. ",
        "When 'bws' is given, np::npcdistbw() (silently) uses the method in 'bws' and ignores the specified 'bwmethod'. Pass npcdistbw.args = list(bwmethod = '", bws.method, "') or omit 'bws'."))
    }
  }

  return(err.msg)
}
