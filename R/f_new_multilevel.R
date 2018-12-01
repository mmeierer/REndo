# Constructor for rendo.multilevel objects
#   Does the steps shared between L2 and L3
#' @importFrom stats setNames
new_rendo_multilevel <- function(call, formula, num.levels, l.group.size, dt.model.data,
                                 V, W, l.gmm, l.ovt, y, X){


  # The list names in l.gmm determine the final naming of the coefs
  # Make coefficient(.se) table from estimated GMMs
  coefficients    <- do.call(cbind, lapply(l.gmm, "[[", "coef"))
  coefficients.se <- do.call(cbind, lapply(l.gmm, "[[", "SE"))

  # Calculate fitted and residuals for each estimated model
  #   Rownames stem from full-length matrices X and y
  l.fitted    <- lapply(l.gmm, function(gmm){ (X %*% gmm$coef)[,1,drop=TRUE]})
  l.residuals <- lapply(l.fitted, function(fitted){ (y - fitted)[,1,drop=TRUE]})
  # The fitted and residuals at this point are sorted group-wise to match the groups in V and W.
  #   Restoring the sorting of the original input data is done in the interface
  #   function because the sorting (rownames) of the original data is not available here

  # Read out vcov of each model
  l.vcov <- lapply(l.gmm, "[[", "vcov")

  return(structure(
            class = "rendo.multilevel",
            list(call = call,
                 formula = formula,
                 num.levels = num.levels,
                 l.group.size = l.group.size,
                 dt.model.data = dt.model.data,
                 coefficients = coefficients,
                 coefficients.se = coefficients.se,
                 l.fitted = l.fitted,
                 l.residuals = l.residuals,
                 l.vcov = l.vcov,
                 V = V,
                 W = W,
                 l.ovt = l.ovt)))
}
