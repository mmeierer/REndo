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


#' @importFrom ks kcde
#' @importFrom stats ecdf predict
copula_pstar <- function(P, cdf) {
  stopifnot(is.matrix(P))
  if (cdf == "kde") {
    P.star <- apply(P, 2, function(x) {
      Fhat <- ks::kcde(x)
      predict(Fhat, x = x)
    })
  } else if (cdf == "resc.ecdf") {
    # same as copula::pobs()
    P.star <- apply(P, 2, rank, ties.method = "average") / (nrow(P) + 1)
  } else if (cdf == "adj.ecdf") {
    P.star <- apply(P, 2, copula_adj_ecdf)
  } else if (cdf == "ecdf") {
    ecdf0 <- apply(P, 2, ecdf)
    P.star <- sapply(seq_along(ecdf0), function(i) {
      u <- ecdf0[[i]](P[, i])
      u[u == min(u)] <- 10e-7
      u[u == max(u)] <- 1 - 10e-7
      return(u)
    })
    P.star <- as.matrix(P.star)
  } else {
    stop("Invalid `cdf` to copula_pstar!")
  }

  colnames(P.star) <- colnames(P)
  return(P.star)
}

# Stops if any column of is constant
copula_stop_if_constant <- function(X) {
  stopifnot(is.matrix(X) || is.data.frame(X))
  is.const <- vapply(
    seq_len(ncol(X)),
    function(j) {
      return(length(unique(X[, j])) == 1)
    },
    FUN.VALUE = logical(1)
  )
  if (any(is.const)) {
    stop("Constant regressor(s): ", toString(colnames(X)[is.const]), call. = FALSE)
  }
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

  # Check if all aux regs are here
  missing.aux <- setdiff(names.aux.regs, names.coefs.all)
  if (length(missing.aux)) {
    stop(
      "Aux regressor(s) not found in coefficients: ",
      toString(missing.aux),
      " This should not happen - please report as a bug!"
    )
  }

  names.structural <- names.coefs.all[!names.coefs.all %in% names.aux.regs]
  coefs.structural <- coef(res.lm.aug)[names.structural]
  mm.structural <- model.matrix(res.lm.aug)[, names.structural, drop = FALSE]

  fitted.values <- drop(mm.structural %*% coefs.structural)
  names(fitted.values) <- row.names(mm.structural)

  residuals <- drop(model.response(model.frame(res.lm.aug)) - fitted.values)
  names(residuals) <- names(fitted.values)

  return(list(fitted.values = fitted.values, residuals = residuals))
}

copula_create_1ststage_copdata_matrix <- function(nrow, labels.endo) {
  m <- matrix(NA_real_, nrow = nrow, ncol = length(labels.endo))
  # Make labels plain strings before using as colnames (ie strips backticks) because
  # they need to feed into reformulate() later
  raw.labels <- vapply(
    labels.endo,
    FUN = function(x) {
      return(deparse1(str2lang(x), backtick = FALSE))
    },
    FUN.VALUE = character(1)
  )
  colnames(m) <- paste0(raw.labels, "_cop")
  return(m)
}

copula_fit_2ndstage <- function(F.formula, data, cop.terms) {
  stopifnot(!is.null(colnames(cop.terms)))

  # Second stage: augmented OLS ----------------------------------------------------
  # Adding the correction term to the structural model and estimate by OLS

  # Get labels separately because needed to read-out coefs(lm)
  labels.pcop <- vapply(
    colnames(cop.terms),
    FUN = function(x) {
      deparse1(as.name(x), backtick = TRUE)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  f.pcop <- reformulate(
    termlabels = c(".", labels.pcop),
    response = NULL,
    intercept = TRUE
  )

  # update requires dot-expanded formula (may not contain a dot `.` in `old`)
  f.main <- terms(F.formula, data = data, lhs = 1, rhs = 1)
  f.final <- update(old = f.main, new = f.pcop)

  # TODO: Does cbind() work if non-continuous variables?
  # - Yes because will always dispatch to cbind.data.frame() if it contains any data.frame
  res.augmented <- lm(formula = f.final, data = cbind(data, cop.terms))

  return(list(
    res.augmented = res.augmented,
    # because cop.term is only numeric, coef() (actually model.matrix() used in lm())
    # preserves the terms as they are in the formula. For f.pcop these may be backticked
    # or not, depending if necessary.
    labels.pcop = labels.pcop
  ))
}
