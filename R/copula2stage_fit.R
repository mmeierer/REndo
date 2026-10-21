copula2stage_adj_ecdf <- function(x) {
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
copula2stage_pstar <- function(P, cdf) {
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
    P.star <- apply(P, 2, copula2stage_adj_ecdf)
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
    stop(
      "Internal error: Invalid 'cdf' passed to copula2stage_pstar(). This should not happen - please report as a bug!",
      call. = FALSE
    )
  }

  colnames(P.star) <- colnames(P)
  return(P.star)
}

# Stops if any column of is constant
copula2stage_stop_if_constant <- function(X) {
  stopifnot(is.matrix(X) || is.data.frame(X))
  is.const <- vapply(
    seq_len(ncol(X)),
    function(j) {
      return(length(unique(X[, j])) == 1)
    },
    FUN.VALUE = logical(1)
  )
  if (any(is.const)) {
    stop(
      "Regressor(s) must not be constant: ",
      toString(colnames(X)[is.const]),
      ".",
      call. = FALSE
    )
  }
}

copula2stage_get_1ststage_copterms <- function(nrow, labels.endo) {
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

copula2stage_fit_2ndstage <- function(F.formula, data, cop.terms) {
  stopifnot(!is.null(colnames(cop.terms)))

  # 2nd stage ----------------------------------------------------------------------
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

  # cbind() also works with non-continuous variables because it dispatches to
  # cbind.data.frame() if any argument is a data.frame
  res.lm.augmented <- lm(formula = f.final, data = cbind(data, cop.terms))

  return(list(
    res.lm.augmented = res.lm.augmented,
    # because cop.term is only numeric, coef() (actually model.matrix() used in lm())
    # preserves the terms as they are in the formula. For f.pcop these may be backticked
    # or not, depending if necessary.
    labels.pcop = labels.pcop
  ))
}
