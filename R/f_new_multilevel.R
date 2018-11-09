# Constructor for rendo.multilevel objects
#   Does the steps shared between L2 and L3
#' @importFrom stats setNames
new_rendo_multilevel <- function(call, formula, dt.mf, dt.mm, num.levels,
                                 V, W, l.gmm, l.ovt, y, X){

  # The list names in l.gmm determine the final naming of the coefs
  # Make coefficient(.se) table from estimated GMMs
  coefficients    <- do.call(cbind, lapply(l.gmm, "[[", "coef"))
  coefficients.se <- do.call(cbind, lapply(l.gmm, "[[", "SE"))

  # Calculate fitted and residuals for each estiamted model
  l.fitted    <- lapply(l.gmm, function(gmm){ (X %*% gmm$coef)[,1,drop=TRUE]}) #setNames(as.vector(X %*% gmm$coef), rownames(X))})
  l.residuals <- lapply(l.fitted, function(fitted){ (y - fitted)[,1,drop=TRUE]}) #setNames(as.vector(y - fitted), rownames(y))})

  return(structure(
            class = "rendo.multilevel",
            list(call = call,
                 formula = formula,
                 num.levels = num.levels,
                 dt.mf = dt.mf,
                 dt.mm = dt.mm,
                 coefficients = coefficients,
                 coefficients.se = coefficients.se,
                 l.fitted = l.fitted,
                 l.residuals = l.residuals,
                 V = V,
                 W = W,
                 l.ovt = l.ovt)))
}
