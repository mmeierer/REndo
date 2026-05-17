#' @importFrom stats qnorm lm residuals
copulaIMA_residuals <- function(P.star, endo.cols) {
  Z <- qnorm(P.star)

  if (!is.matrix(Z)) {
    Z <- as.matrix(Z)
  }

  P.names <- colnames(Z)
  if (is.null(P.names)) {
    stop("P.star must have column names")
  }

  # Exogenous normal scores: all the columns that are not endo
  # chapter 3.2 step 2: regressing P*l on X1*, ..., XK* (exogenous only)
  exo.cols <- colnames(Z)[!colnames(Z) %in% endo.cols]

  res <- matrix(NA, nrow = nrow(Z), ncol = length(endo.cols))
  colnames(res) <- paste0(endo.cols, "_cop")

  for (j in seq_along(endo.cols)) {
    Z.j <- Z[, endo.cols[j], drop = TRUE]

    if (length(exo.cols) ==0){
      res[, j] <- Z.j # no exo regressors
    } else{
      Z.exo <- Z[, exo.cols, drop = FALSE]
      lm.j <- lm(Z.j ~ Z.exo - 1)
      res[, j] <- residuals(lm.j)
    }
  }
  return(res)
}
