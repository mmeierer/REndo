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
