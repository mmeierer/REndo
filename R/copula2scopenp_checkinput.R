# - >= 1 endogenous regressor
# - >= 1 exogenous regressor
# - exogenous: numeric, factor, ordered
# - endogenous: numeric, ordered
# - Block interaction terms: Are only resolved in model.matrix() but only use model.frame()
#    in this method. Specifying them would silently ignore them and NOT forward
#    them into np conditional CDF
#
checkinput_copula2scopenp_formula_data <- function(formula, data) {
  return(checkinput_copulashared_formula_data(
    formula = formula,
    data = data,
    min.num.exo = 1,
    msg.min.num.exo = "copula2sCOPEnp requires at least one exogenous regressor for the nonparametric conditional CDF estimation.",
    allow.interactions = FALSE,
    allow.constant.regressors = FALSE,
    allowed.classes.endo = c("numeric", "ordered"),
    allowed.classes.exo = c("numeric", "factor", "ordered")
  ))
}


#' @importFrom utils getS3method
checkinput_copula2scopenp_npcdistbwargs <- function(npcdistbw.args) {
  # No checks if parameter default
  if (identical(npcdistbw.args, list())) {
    return(c())
  }

  err.msg <- c()

  # Plain list
  if (!identical(class(npcdistbw.args), "list")) {
    return("Parameter `npcdistbw.args` must be a plain list.")
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
        "`npcdistbw.args` may not contain reserved argument(s): ",
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
      "Unknown argument(s) in `npcdistbw.args`: ",
      toString(unknown.nms),
      ". Check docu `?np::npcdistbw` for accepted parameters.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  return(err.msg)
}

checkinput_copula2scopenp_bws <- function(bws, labels.endo) {
  # NULL is default value
  if(is.null(bws)){
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
    return(paste0("Parameter 'bws' must be named after the endogenous variables: ",
                  toString(labels.endo),"."))
  }

  for (name in names(bws)) {
    if (!is(bws[[name]], "condbandwidth")) {
      err.msg <- c(err.msg,
                   paste0("Element '", name, "' of parameter 'bws' must be of class 'condbandwidth', usually the output of 'np::npcdistbw(xdat=<all exo>, ydat=<single endo>)'."))
    }
  }

  # check that fit with xday/ydat and not formula interface?

  return(err.msg)
}
