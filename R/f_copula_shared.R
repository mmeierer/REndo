copula_adj_ecdf <- function(x) {
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  n <- nrow(x)

  #Adjusted empirical CDF
  #U_i = rank_i/n * (n-1/n) + (1/2n) and simplifies to U_i = (rank_i-1/2)/ n.
  #This is a midrank adjustment used in nonparametric statistics to handle tied data points/identical values
  #when calculating the empirical CDF or assigning ranks.
  #This keeps the observations strictly inside (0,1) and avoids infinities when
  #applying qnorm().
  U <- apply(x, 2, rank, ties.method = "average") * ((n - 1) / n^2) + 1 / (2 * n)

  return(U)
}


#' @importFrom copula pobs
#' @importFrom ks kcde
#' @importFrom stats ecdf predict
copula_pstar <- function(P, cdf) {
  if (cdf == "kde") {
    P.star <- apply(P, 2, function(x) {
      Fhat <- ks::kcde(x)
      predict(Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf") {
    P.star <- apply(P, 2, copula::pobs)
  } else if (cdf == "adj.ecdf") {
    P.star <- apply(P, 2, copula_adj_ecdf)
  } else {
    ecdf0 <- apply(P, 2, ecdf)
    P.star <- sapply(seq_along(ecdf0), function(i) {
      u <- ecdf0[[i]](P[, i])
      u[u == min(u)] <- 10e-7
      u[u == max(u)] <- 1 - 10e-7
      u
    })
    P.star <- as.matrix(P.star)
  }

  colnames(P.star) <- colnames(P)
  return(P.star)
}


copula_compute_structural_fitted_residuals <- function(
  res.lm.aug,
  names.aux.regs
) {
  if (
    is.null(names.aux.regs) ||
      length(names.aux.regs) == 0 ||
      anyNA(names.aux.regs) ||
      any(nchar(names.aux.regs) == 0)
  ) {
    stop(
      "Internal error: 'names.aux.regs' is empty. This should not happen - please report as a bug!",
      call. = TRUE
    )
  }

  names.coefs.all <- names(coef(res.lm.aug))
  names.structural <- names.coefs.all[!names.coefs.all %in% names.aux.regs]
  coefs.structural <- coef(res.lm.aug)[names.structural]
  mm.structural <- model.matrix(res.lm.aug)[, names.structural, drop = FALSE]

  fitted.values <- drop(mm.structural %*% coefs.structural)
  names(fitted.values) <- row.names(mm.structural)

  residuals <- drop(model.response(model.frame(res.lm.aug)) - fitted.values)
  names(residuals) <- names(fitted.values)

  return(list(fitted.values = fitted.values, residuals = residuals))
}
