# Whether a given RHS of the formula is a single `0`
# - like y ~ W + P | P | 0
# - TRUE only if the part is literally 0
# - FALSE if there is any other term (W + 0) or any non-part term (1, -1)
# Cannot use the intercept to check because it is the same for 0 and -1
copulajams_is_rhs_part_lone_zero <- function(F.formula, rhs.part) {
  stopifnot(length(rhs.part) == 1)
  # The part has to exist
  stopifnot(rhs.part <= length(F.formula)[2])

  return(identical(attr(F.formula, "rhs")[[rhs.part]], 0))
}

# The *variables* (data cols) that each single term uses
# - example: `log(P1) + P2` gives `list("log(P1)" = "P1", P2 = "P2")`
# - named list with 1 element per term: Name is the term, content are its variables
# - an interaction is not an own element, only the terms it consists of
# - empty list if the part has no terms (0)
# There may be no dot because cannot expand without data
#
#' @importFrom stats terms
copulajams_get_vars_per_term <- function(F.formula, rhs.part) {
  stopifnot(length(rhs.part) == 1)
  # The part has to exist
  stopifnot(rhs.part <= length(F.formula)[2])

  terms.part <- terms(F.formula, lhs = 0, rhs = rhs.part)
  # Drop 1st element: The call to list()
  term.exprs <- as.list(attr(terms.part, "variables"))[-1]

  l.vars <- lapply(term.exprs, all.vars)
  names(l.vars) <- vapply(term.exprs, deparse1, character(1))
  return(l.vars)
}

# Checks a single RHS part (2, 3, or 4) with what only needs the formula and no data
# - allow.zero: Whether the part may be a single `0` (no terms)
# - if it is a single `0`: Nothing else to check
# - no dot (only allowed in part 1)
# - the terms can be read
# - has terms
# - no interactions (only allowed in part 1)
# - every term uses exactly 1 variable, every variable is in only 1 term
#
#' @importFrom stats formula terms
checkinput_copulajams_formula_single_part <- function(
  F.formula,
  rhs.part,
  allow.zero
) {
  # start of every message
  msg.part <- paste0("Right-hand part ", rhs.part, " of parameter 'formula' ")

  # Single 0 --------------------------------------------------------------------------
  is.lone.zero <- copulajams_is_rhs_part_lone_zero(
    F.formula = F.formula,
    rhs.part = rhs.part
  )
  if (is.lone.zero) {
    if (allow.zero) {
      # No terms: Nothing else to check
      return(c())
    }
    return(paste0(msg.part, "must have terms and may not be `0`."))
  }

  # No dot ----------------------------------------------------------------------------
  # all.vars(): Requires the formula to be unexpanded
  has.dot <- "." %in% all.vars(formula(F.formula, lhs = 0, rhs = rhs.part))
  if (has.dot) {
    # Later checks fail if there is a dot (cannot expand without data)
    # fmt: skip
    return(paste0(
      msg.part, "may not have a dot (`.`): It is only allowed in the 1st part."))
  }

  # Terms object ----------------------------------------------------------------------
  # No data required: There is no dot to expand
  # Report a part that cannot be read instead of failing hard
  terms.part <- tryCatch(
    terms(F.formula, lhs = 0, rhs = rhs.part),
    error = function(e) {
      return(conditionMessage(e))
    }
  )
  if (is.character(terms.part)) {
    return(paste0(msg.part, "could not be interpreted: ", terms.part))
  }

  err.msg <- c()

  # Has terms -------------------------------------------------------------------------
  # No terms but also not a single `0`, eg `1`
  if (length(labels(terms.part)) == 0) {
    if (allow.zero) {
      err.msg <- c(err.msg, paste0(msg.part, "must either have terms or be `0`."))
    } else {
      err.msg <- c(err.msg, paste0(msg.part, "must have terms."))
    }
  }

  # No interactions -------------------------------------------------------------------
  # order: Number of variables a term combines (1 = no interaction)
  if (any(attr(terms.part, "order") > 1)) {
    # fmt: skip
    err.msg <- c(err.msg, paste0(
      msg.part, "may not have interaction terms: They are only allowed in the 1st part."))
  }

  # 1 variable per term, every variable in only 1 term --------------------------------
  # Uses the variables of the terms and not their labels: An interaction (reported
  # above) is not an additional entry
  l.vars <- copulajams_get_vars_per_term(F.formula = F.formula, rhs.part = rhs.part)

  is.not.single.var <- lengths(l.vars) != 1
  if (any(is.not.single.var)) {
    # fmt: skip
    err.msg <- c(err.msg, paste0(
      msg.part, "must use exactly 1 variable in every term but these do not: ",
      toString(names(l.vars)[is.not.single.var]), "."))
  }

  vars <- unlist(l.vars)
  vars.repeated <- unique(vars[duplicated(vars)])
  if (length(vars.repeated) > 0) {
    # fmt: skip
    err.msg <- c(err.msg, paste0(
      msg.part, "may use every variable in only 1 term but these are used in more: ",
      toString(vars.repeated), "."))
  }

  return(err.msg)
}

# Checks RHS parts 2-4 that only need the formula and no data
# - each part on its own. Part 2 needs terms, parts 3 and 4 may be a single `0`
# - between the parts: Every variable is in only 1 part
#
checkinput_copulajams_formula_parts <- function(F.formula) {
  num.rhs.parts <- length(F.formula)[2]
  # 3 or 4 parts: Verified before by the basic formula check
  stopifnot(num.rhs.parts %in% c(3, 4))
  rhs.parts <- seq(from = 2, to = num.rhs.parts)

  # Each part on its own --------------------------------------------------------------
  err.msg <- c()
  for (rhs.part.i in rhs.parts) {
    # Only parts 3 and 4 may be a single `0`
    allow.zero.i <- rhs.part.i %in% c(3, 4)

    err.msg.i <- checkinput_copulajams_formula_single_part(
      F.formula = F.formula,
      rhs.part = rhs.part.i,
      allow.zero = allow.zero.i
    )
    err.msg <- c(err.msg, err.msg.i)
  }
  # Comparing the parts needs every part to be fine
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Between the parts: Every variable in only 1 part ----------------------------------
  l.part.vars <- lapply(rhs.parts, function(rhs.part.i) {
    l.vars.i <- copulajams_get_vars_per_term(
      F.formula = F.formula,
      rhs.part = rhs.part.i
    )
    return(unique(unlist(l.vars.i)))
  })
  vars.parts <- unlist(l.part.vars)
  vars.several.parts <- unique(vars.parts[duplicated(vars.parts)])
  if (length(vars.several.parts) > 0) {
    # fmt: skip
    return(paste0(
      "Every variable may only be used in 1 of the right-hand parts 2-4 of parameter ",
      "'formula' but these are used in several: ", toString(vars.several.parts), "."))
  }

  return(c())
}

# All variables of RHS parts 2-4 are also in part 1
# - compares variables (data cols) and not labels: `log(P)` in part 1 and `P` in
#   part 2 is fine
# - also reports the response in parts 2-4 because it is never in part 1
# Needs data to expand a dot in part 1
# All variables have to exist in data and there may be no dot in parts 2-4
#
#' @importFrom stats terms
checkinput_copulajams_all_vars_in_rhs1 <- function(F.formula, data) {
  num.rhs.parts <- length(F.formula)[2]
  # 3 or 4 parts: Verified before by the basic formula check
  stopifnot(num.rhs.parts %in% c(3, 4))
  rhs.parts <- seq(from = 2, to = num.rhs.parts)

  vars.rhs1 <- all.vars(terms(F.formula, lhs = 0, rhs = 1, data = data))

  err.msg <- c()
  for (rhs.part.i in rhs.parts) {
    l.vars.i <- copulajams_get_vars_per_term(
      F.formula = F.formula,
      rhs.part = rhs.part.i
    )
    vars.missing.i <- setdiff(unlist(l.vars.i), vars.rhs1)

    if (length(vars.missing.i) > 0) {
      # fmt: skip
      err.msg <- c(err.msg, paste0(
        "Right-hand part ", rhs.part.i, " of parameter 'formula' may only use ",
        "variables that are also in the 1st part but these are not: ",
        toString(vars.missing.i), "."))
    }
  }

  return(err.msg)
}

# Checks the data of a single RHS part (2, 3, or 4)
# - allowed.classes: Classes that every term of the part may have
# - warn.low.card: Whether to warn about low cardinality numeric terms
# - the model.frame of the part can be built (without response)
# - its columns: allowed classes, only finite values, no unused factor levels, not
#   constant
# There may be no dot in the part
#
#' @importFrom stats setNames terms
checkinput_copulajams_modelframe_single_part <- function(
  F.formula,
  data,
  rhs.part,
  allowed.classes,
  warn.low.card
) {
  # The part has to exist
  stopifnot(rhs.part <= length(F.formula)[2])

  labels.part <- labels(terms(F.formula, lhs = 0, rhs = rhs.part))

  # Expose build failures to user (bad transformations, generated NAs,...)
  mf <- tryCatch(
    copula2stage_build_mf_part(
      F.formula = F.formula,
      data = data,
      rhs.part = rhs.part,
      include.response = FALSE
    ),
    error = function(e) {
      return(conditionMessage(e))
    }
  )
  if (!is.data.frame(mf)) {
    # fmt: skip
    return(paste0(
      "The model frame of right-hand part ", rhs.part,
      " of parameter 'formula' could not be built: ", mf))
  }

  # Every term of the part may have the same classes
  l.allowed.classes <- setNames(
    rep(list(allowed.classes), length(labels.part)),
    labels.part
  )

  if (warn.low.card) {
    labels.warn.low.card <- labels.part
  } else {
    labels.warn.low.card <- NULL
  }

  return(checkinput_copula2stage_modelframe_columns(
    mf = mf,
    allowed.classes = l.allowed.classes,
    labels.warn.low.card = labels.warn.low.card,
    forbid.constant = TRUE
  ))
}

# Every stratum needs to have more obs than P (part 2) and W (part 3) have cols
# - this is the same check that also is done in the math
# - required for cov matrix inversion
# - better to fail here already with more understandable message, but should still be
#   also checked in the code because bootstrap samples do not pass the input checks
#
# Nothing to check if there are no strata
# Parts 2-4 have to be valid, check before
#
#' @importFrom stats terms
checkinput_copulajams_min_obs_per_stratum <- function(F.formula, data) {
  z <- copulajams_build_strata(F.formula = F.formula, data = data)
  if (is.null(z)) {
    return(c())
  }

  # P and W have 1 col per term (because 1 var per term), none if the part is `0`
  num.terms.P <- length(labels(terms(F.formula, lhs = 0, rhs = 2)))
  num.terms.W <- length(labels(terms(F.formula, lhs = 0, rhs = 3)))
  num.terms.PW <- num.terms.P + num.terms.W

  num.obs.per.stratum <- table(z)
  strata.too.small <- names(num.obs.per.stratum)[num.obs.per.stratum <= num.terms.PW]

  if (length(strata.too.small) > 0) {
    # fmt: skip
    return(paste0(
      "Every stratum built by part 4 in 'formula' needs to have more than ",
      num.terms.PW, " observations (the number of terms in parts 2 and 3) but these ",
      "do not: ", toString(paste0("'", strata.too.small, "'")), "."))
  }

  return(c())
}

# 3 or 4 part Formula of structure:
#   (response) ~ (structural model) | (P: endogenous) | (W: continuous exogenous) | (Z: strata)
#   eg y ~ W + Z + P | P | W | Z
#
# Returns after each stage that has errors because the next ones rely on it
#
# - formula basics (formula, 1 lhs, 3 or 4 rhs parts, single response)
# - parts 2-4 are valid, all variables are in data
# - response not in part 1, all variables of parts 2-4 are also in part 1
# - mf of every part: can be built, column classes, only finite values,
#   no unused factor levels, not constant
# - every stratum has enough observations
#
#' @importFrom Formula as.Formula
#' @importFrom stats terms
checkinput_copulajams_formula_data <- function(formula, data) {
  # Formula basics --------------------------------------------------------------------
  # 3 or 4 rhs parts: The 4th part (strata) is optional
  # The 3rd part (W) is mandatory (or `0` if there is no W): A 2-part formula as in
  # the other copula methods would silently not use X for the correction, misleading
  # many users
  err.msg <- checkinput_copula2stage_formula_basics(
    formula = formula,
    allowed.num.rhs.parts = c(3, 4)
  )
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  F.formula <- as.Formula(formula)

  # Formula parts and variables in data -----------------------------------------------
  # Independent of each other: Report together
  err.msg <- c(
    checkinput_copulajams_formula_parts(F.formula = F.formula),
    checkinput_copula2stage_vars_in_data(F.formula = F.formula, data = data)
  )
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Variables of the parts ------------------------------------------------------------
  # Needs the variables to be in data: A dot in part 1 is expanded
  rhs1.terms <- terms(F.formula, lhs = 0, rhs = 1, data = data)
  err.msg <- c(
    checkinput_copula2stage_response_not_in_rhs(
      F.formula = F.formula,
      rhs1.terms = rhs1.terms
    ),
    checkinput_copulajams_all_vars_in_rhs1(F.formula = F.formula, data = data)
  )
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # model.frame of every part ---------------------------------------------------------
  # Part 1 (with response): Any class
  err.msg <- checkinput_copula2stage_modelframe(
    F.formula = F.formula,
    data = data,
    allowed.classes = list(),
    labels.warn.low.card = NULL,
    forbid.constant = TRUE
  )

  # Parts 2 (P) and 3 (W): Continuous
  for (rhs.part.i in c(2, 3)) {
    err.msg.i <- checkinput_copulajams_modelframe_single_part(
      F.formula = F.formula,
      data = data,
      rhs.part = rhs.part.i,
      allowed.classes = "numeric",
      warn.low.card = TRUE
    )
    err.msg <- c(err.msg, err.msg.i)
  }

  # Part 4 (Z), if given : Discrete
  if (length(F.formula)[2] == 4) {
    err.msg.4 <- checkinput_copulajams_modelframe_single_part(
      F.formula = F.formula,
      data = data,
      rhs.part = 4,
      allowed.classes = c("factor", "ordered"),
      warn.low.card = FALSE
    )
    err.msg <- c(err.msg, err.msg.4)
  }

  # Multiple parts can generate the same error message
  err.msg <- unique(err.msg)
  if (length(err.msg) > 0) {
    return(err.msg)
  }

  # Observations per stratum ----------------------------------------------------------
  # Needs the model.frames of parts 2-4 to be valid
  return(checkinput_copulajams_min_obs_per_stratum(F.formula = F.formula, data = data))
}
