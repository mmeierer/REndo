# Check formula (which needs data) and data
#
# Bare terms in RHS2: y ~ X + P | P
#
# - >= 1 endogenous regressor
# - >= 1 exogenous regressor
# - exogenous: numeric, factor, ordered
# - endogenous: numeric, ordered
# - wrongly specified continuous()/discrete() specials
# - Block interaction terms: Are only resolved in model.matrix() but only use model.frame()
#    in this method. Specifying them would silently ignore them and NOT forward
#    them into np conditional CDF
#
#' @importFrom Formula as.Formula
#' @importFrom stats terms setNames
checkinput_copula2scopenp_formula_data <- function(formula, data) {
  err.msg <- checkinput_copulashared_formula_basics(formula = formula)
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  F.formula <- as.Formula(formula)

  err.msg <- checkinput_copulashared_vars_in_data(F.formula = F.formula, data = data)
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Expand dot against data
  # specials: check if user wrongly specified them
  specials <- c("discrete", "continuous")
  rhs1.terms <- terms(F.formula, lhs = 0, rhs = 1, data = data, specials = specials)
  rhs2.terms <- terms(F.formula, lhs = 0, rhs = 2, data = data, specials = specials)

  rhs1.labels <- labels(rhs1.terms)
  endo.labels <- labels(rhs2.terms)
  exo.labels <- rhs1.labels[!rhs1.labels %in% endo.labels]

  # Check after expanding dot
  err.msg <- checkinput_copulashared_response_not_in_rhs(
    F.formula = F.formula,
    rhs1.terms = rhs1.terms
  )
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Catch if wrongly specified specials
  if (length(unlist(attr(rhs2.terms, "specials"))) > 0) {
    err.msg <- c(err.msg, "discrete()/continuous() are not supported for this method.")
  }

  # No interactions allowed! Only model.matrix would do this but we pass model.frame()
  # output into np:npcdistbw() already
  if (any(attr(rhs1.terms, "order") > 1) || any(attr(rhs2.terms, "order") > 1)) {
    err.msg <- c(err.msg, "Interaction terms are not supported for this method.")
  }

  # Need at least one of each: endogenous to model, exogenous for identification.
  if (length(endo.labels) == 0) {
    err.msg <- c(err.msg, "At least one endogenous regressor (rhs2) is required.")
  }
  if (length(exo.labels) == 0) {
    err.msg <- c(
      err.msg,
      "copula2sCOPEnp requires at least one exogenous regressor for the nonparametric conditional CDF estimation."
    )
  }

  # Endo terms (RHS2) must also be in structural model (RHS1)
  not.in.rhs1 <- endo.labels[!endo.labels %in% rhs1.labels]
  if (length(not.in.rhs1) > 0) {
    # fmt: skip
    err.msg <- c(err.msg, paste0(
      "Endogenous regressor(s) not in structural model (rhs1): ",
      toString(not.in.rhs1), "."))
  }

  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # endo: continuous or ordered factors
  # exo: continuous or any factor
  allowed.classes <- c(
    setNames(rep(list(c("numeric", "ordered")), length(endo.labels)), endo.labels),
    setNames(
      rep(list(c("numeric", "factor", "ordered")), length(exo.labels)),
      exo.labels
    )
  )

  return(checkinput_copulashared_modelframe(
    F.formula = F.formula,
    data = data,
    allowed.classes = allowed.classes,
    labels.warn.low.card = NULL,
    forbid.constant = FALSE
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
