# The model.frame of a single RHS part as matrix: P (part 2) or W (part 3)
# Rows and cols are as in the model.frame of the part, nothing is changed here
# The part may only have numeric terms (verified by the input checks)
#
#' @importFrom stats terms
copulajams_build_matrix_from_part <- function(F.formula, data, rhs.part) {
  # The part has to exist
  stopifnot(rhs.part <= length(F.formula)[2])

  # get model.frame of this part
  mf <- copula2stage_build_mf_part(
    F.formula = F.formula,
    data = data,
    rhs.part = rhs.part,
    include.response = FALSE
  )
  stopifnot(all(vapply(mf, is.numeric, logical(1))))

  M <- as.matrix(mf)

  # Make the contract visible
  # - no rows dropped
  # - 1 col per term, none if the part is a single `0`
  stopifnot(nrow(M) == nrow(data))
  labels.part <- labels(terms(F.formula, lhs = 0, rhs = rhs.part))
  stopifnot(ncol(M) == length(labels.part))

  return(M)
}

# Combines the factors in the cols of df to a single factor
# with 1 level per combination that occurs in the rows
#
# Example (used throughout this method)
#   Z  Z2       returned factor
#   1  a    ->  "Z=1, Z2=a"
#   0  b    ->  "Z=0, Z2=b"
#   0  a    ->  "Z=0, Z2=a"
#   0  a    ->  "Z=0, Z2=a"
#
# - Levels are `<colname>=<level>` per col, joined by ", "
# - No level are created for combinations that dont occur (here: Z=1 with Z2=b)
# - Levels are ordered by the first col, then by the next cols
#   The order needs to remain consistent, independent of the row order: The levels
#   determine the position of the correction terms coefs and the bootstrap requires
#   every sample to return coefs in the same order and otherwise discards them
# - Stops if different combinations get the same level
#   (possible if level names contain "=" and ", ")
# - Rows are grouped by a temporary internal key formed from their initial level codes,
#   not by level names which can collide across factors:
#   "a_b" + "c" and "a" + "b_c" are both "a_b_c" when pasted, but their codes differ.
#   Example: Z=1 (2nd level of Z) with Z2=a (1st level of Z2) has the key "2.1"
copulajams_combine_factors_to_strata <- function(df) {
  stopifnot(ncol(df) >= 1)
  stopifnot(all(vapply(df, is.factor, logical(1))))

  # For each factor: Internal codes of the levels
  # Example: list(c(2, 1, 1, 1), c(1, 2, 1, 1))
  l.codes <- unname(lapply(df, as.integer))

  # Temporary string key per row: Get unique id for this combination of factor by
  # combining the factors' internal codes
  # Example: "2.1", "1.2", "1.1", "1.1"
  key <- do.call(paste, c(l.codes, sep = "."))

  # First row of every combination
  # Example: 1, 2, 3
  first.rows.observed <- which(!duplicated(key))

  # Ordered by the first col, then by the next cols
  # Example: 3, 2, 1
  l.codes.observed <- lapply(l.codes, function(codes.i) {
    return(codes.i[first.rows.observed])
  })
  first.rows.observed <- first.rows.observed[do.call(order, l.codes.observed)]

  # Get new level (names) for each combination (key):
  # in each factor, get the labels of the first occurrence of unique key
  # Example: "Z=0, Z2=a", "Z=0, Z2=b", "Z=1, Z2=a"
  l.labels.per.col <- lapply(seq_len(ncol(df)), function(col.i) {
    levels.observed.i <- as.character(df[[col.i]][first.rows.observed])
    return(paste0(colnames(df)[col.i], "=", levels.observed.i))
  })
  # combine names across factors (only exactly 1 per key)
  labels.strata <- do.call(paste, c(l.labels.per.col, sep = ", "))

  # Different combinations with the same level (would also crash coef names later)
  # Example: Z "0, Z2=a" with Z2 "b" and Z "0" with Z2 "a, Z2=b"
  #          are both "Z=0, Z2=a, Z2=b"
  is.duplicated <- duplicated(labels.strata)
  if (any(is.duplicated)) {
    stop(
      "Different combinations of factor levels get the same stratum level ('name'): ",
      toString(paste0("'", unique(labels.strata[is.duplicated]), "'")),
      ". Please rename the factor levels.",
      call. = FALSE
    )
  }

  # Every row gets the level of its combination
  z <- factor(x = key, levels = key[first.rows.observed], labels = labels.strata)
  return(z)
}
