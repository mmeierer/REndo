new_rendo_multilevel <- function(call, formula, dt.mf, dt.mm, num.levels,
                                 V, W, l.gmm, l.ovt){

  # The list names in l.gmm determine the final naming of the coefs
  # Make coefficient(.se) table from estimated GMMs
  coefficients    <- do.call(cbind, lapply(l.gmm, "[[", "coef"))
  coefficients.se <- do.call(cbind, lapply(l.gmm, "[[", "SE"))
  return(structure(
            class = "rendo.multilevel",
            list(call = call,
                 formula = formula,
                 num.levels = num.levels,
                 dt.mf = dt.mf,
                 dt.mm = dt.mm,
                 coefficients = coefficients,
                 coefficients.se = coefficients.se,
                 V = V,
                 W = W,
                 l.ovt = l.ovt)))
}
