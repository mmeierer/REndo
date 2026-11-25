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
