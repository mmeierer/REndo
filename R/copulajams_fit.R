#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix terms formula update reformulate
copulajams_fit <- function(F.formula, data, cdf, verbose) {
  l.parts <- copulajams_build_model_parts(F.formula = F.formula, data = data)

  P <- l.parts$X.endo
  W <- l.parts$X.exo.cont
  df.Z <- l.parts$df.Z

  # Check for constant regressors as may happen in bootstrap sample.
  # Fail with a clear reason instead of a singular cov matrix in solve() later
  copula2stage_stop_if_constant(P)
  copula2stage_stop_if_constant(W)

  # Checking if there is any factor variables among exogenous regressors
  # if the factor Z is present, then it requires stratified correction per level (see equation 20 and 21)
  # if there is no factor Z, variance-covariance correction (eq. 17 to 19)
  if (is.null(df.Z)) {
    # case no Z
    if (verbose) {
      message(
        "No discrete (`factor`) exogenous variable(s) present: Estimating on full data."
      )
    }
    cop.terms <- copulajams_correction_continuous(P = P, W = W, cdf = cdf)
  } else {
    # case where Z is present

    if (verbose) {
      message(
        "Discrete (`factor`) exogenous variable(s) present: ",
        toString(l.parts$labels.exo.factor),
        ". Estimating separately within each subset."
      )
    }

    cop.terms <- copulajams_correction_discrete(P = P, W = W, df.Z = df.Z, cdf = cdf)
  }

  # 2nd stage: OLS ---------------------------------------------------------------------

  res.2nd.stage <- copula2stage_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.lm.augmented = res.2nd.stage$res.lm.augmented,
    labels.pcop = res.2nd.stage$labels.pcop,
    labels.exo = l.parts$labels.exo,
    labels.endo = l.parts$labels.endo,
    P = P,
    W = W,
    df.Z = df.Z
  ))
}

# Dont inline in fit but separate method for better testability
copulajams_build_model_parts <- function(F.formula, data) {
  labels.main <- labels(terms(F.formula, data = data, rhs = 1))
  labels.endo <- labels(terms(F.formula, data = data, rhs = 2))
  labels.exo <- labels.main[!(labels.main %in% labels.endo)]

  # TODO: Blanket exclude all interactions?? What about interactions in endo or purely exos like W1:W2?
  labels.exo <- labels.exo[!grepl(":", labels.exo, fixed = TRUE)]

  # Build a single model.frame/matrix from which all parts (W,P,Z) are read from to
  # guarantee they share row and col order
  mf.main <- model.frame(F.formula, lhs = 1, rhs = 1, data = data, na.action = na.fail)
  # get terms from mf to also contain dataClasses
  tr.main <- terms(mf.main)
  X.main <- model.matrix(tr.main, data = mf.main)

  # get term labels in correct order for the "assign" mapping of the model.matrix
  # (cannot use labels.main or union(labels.endo, labels.exo))
  term.labels <- labels(tr.main)
  # for each column in model matrix: Index of the term.label that generated it
  mm.assign <- attr(X.main, "assign")

  # find exo labels which are factors or not
  is.exo.factor <- vapply(
    labels.exo,
    FUN = function(lbl.exo) {
      dc <- attr(tr.main, "dataClasses")
      return(dc[copula2stage_label_to_colname(lbl.exo)] %in% c("factor", "ordered"))
    },
    FUN.VALUE = logical(1)
  )

  labels.exo.factor <- labels.exo[is.exo.factor]
  labels.exo.cont <- labels.exo[!is.exo.factor]

  X.endo <- X.main[,
    mm.assign %in% match(labels.endo, table = term.labels),
    drop = FALSE
  ]
  X.exo.cont <- X.main[,
    mm.assign %in% match(labels.exo.cont, table = term.labels),
    drop = FALSE
  ]

  # return Z already for testability
  # Z is only used to apply-by-factor: Has to remain factors and not made to dummies
  if (length(labels.exo.factor) == 0) {
    df.Z <- NULL
  } else {
    colnames.exo.factors <- vapply(
      labels.exo.factor,
      FUN = copula2stage_label_to_colname,
      FUN.VALUE = character(1)
    )
    df.Z <- mf.main[, colnames.exo.factors, drop = FALSE]
    colnames(df.Z) <- labels.exo.factor
  }

  return(list(
    mf.main = mf.main,
    X.main = X.main,
    X.endo = X.endo,
    X.exo.cont = X.exo.cont,
    df.Z = df.Z,
    labels.endo = labels.endo,
    labels.exo = labels.exo,
    labels.exo.cont = labels.exo.cont,
    labels.exo.factor = labels.exo.factor
  ))
}

#' @importFrom stats qnorm cov
copulajams_correction_continuous <- function(P, W, cdf) {
  # cols must be named and may not contain intercept
  stopifnot(!is.null(colnames(P)) & !("" %in% colnames(P)))
  # No names to check if there are no exo regressors (no W, no cols)
  if (ncol(W) > 0) {
    stopifnot(!is.null(colnames(W)) & !("" %in% colnames(W)))
  }
  stopifnot(!"(Intercept)" %in% colnames(P))
  stopifnot(!"(Intercept)" %in% colnames(W))

  # solve() fails as singular if matrix has fewer obs than cols (ncol(P)+ncol(W)).
  # Original data is verified in input checks. Here is only for bootstrapped samples.
  # Bootstrap sample in the continuous case should always have as many obs as the
  # original data. Also for the stratified case, it should be checked before reaching here.
  # Hence this should never fire.
  stopifnot(nrow(P) > ncol(P) + ncol(W))

  #computing the copula correction terms - continuous exogenous case
  # equation 17 : C(P_i, W_i), sigma_(C(P), C(W)} is the variance-covariance matrix
  #of (C(P), W(P)) (not the correlation matrix)
  # the final (I_{dp}, 0)' projection keeps only the d_p columns which correspond to the endo
  #regressors and giving one copula term per endo regressors

  PW <- cbind(P, W)

  #step 1: applying CDF to all regressors (P&W)
  P.star <- copula2stage_pstar(P = PW, cdf = cdf)

  #step 2: applying qnorm to obtain normal scores C(P) and C(W)
  C.PW <- apply(P.star, 2, qnorm) #using equation C(P_i) = Phi^{-1}(F_hat(P_i)) for each regressor

  #step 3: Estimate variance-covariance matrix \hat{sigma}_{C(P), C(W)}
  Sigma.hat <- cov(C.PW) #equation 21: mentioned that it is the variance-covariance so cov should be used
  #instead of cor(). i think ?

  #step 4: inverse of the variance-covariance matrix:
  Sigma.inverse <- solve(Sigma.hat)

  #step 4:finding the d_P copula term to project onto endogenous regressor columns
  # (I_{dp}, 0_{dw x dp})' selects the d_P endo columns of Sigma^{-1}
  # giving the d_P copula terms (one per endo reg)
  endo.index <- seq_len(ncol(P)) # P is in the first ncol() columns

  #C(P_i, W_i) = (C(P_i)', C(W_i)') \hat(Sigma^{-1}_{C(P), C(W)}) (I_{dp}, 0_{dw x dp})'
  P.cop <- C.PW %*% Sigma.inverse[, endo.index, drop = FALSE]
  P.cop <- as.matrix(P.cop)
  colnames(P.cop) <- paste0(colnames(P), "_cop")

  return(P.cop)
}

# Names of the correction terms of a single stratum: '<copterm name>[<stratum level>]'
# Example: "P1_cop" in the stratum "Z=0, Z2=a" is "P1_cop[Z=0, Z2=a]"
copulajams_get_stratum_copterm_names <- function(copterm.names, stratum.level) {
  stopifnot(length(copterm.names) > 0)
  stopifnot(length(stratum.level) == 1)

  return(paste0(copterm.names, "[", stratum.level, "]"))
}


#' @importFrom stats cov qnorm
copulajams_correction_discrete <- function(
  P,
  W,
  df.Z, # has to be factors, not dummies
  cdf
) {
  # str(df.Z)
  # str(W)
  # str(P)
  stopifnot(!is.null(colnames(df.Z)))
  l.results <- list()

  # For each factor separately: For each level: Process data
  for (label.Z.i in colnames(df.Z)) {
    # Data of single factor (drop: need vector not df to read with levels())
    Z.i <- df.Z[, label.Z.i, drop = TRUE]

    # levels() also applies for ordered factors
    for (lvl.i in levels(Z.i)) {
      # find where data with this level is
      idx.rows <- Z.i == lvl.i

      # skip if too few observations or no variation in endogenous regressors
      # More than p observations are strictly required to estimate a non-singular
      # p x p sample covariance matrix, especially where p = ncol(PW) = endogenous regressors +
      # continuous exogenous regressors.

      # Sample covariance matrix (1/(n-1))* X'X has rank min (n-1, p).
      #if n <= p, it is rank deficient.

      fn.warn.skip <- function(not.enough) {
        warning(
          "Skipping: The factor level `",
          lvl.i,
          "` of variable `",
          label.Z.i,
          "` does not have enough ",
          not.enough,
          " for a reliable covariance estimation.",
          call. = FALSE
        )
      }

      # check num obs before doing expensive data subset
      p.cov <- ncol(P) + ncol(W)
      if (sum(idx.rows) <= p.cov) {
        #p.cov = K + L which is the dim of the covariance matrix.
        fn.warn.skip(paste0("observations (need >", p.cov, ")")) #needs to be strictly more than p.cov obs. for non-singular
        next
      }

      P.sub <- P[idx.rows, , drop = FALSE]
      W.sub <- W[idx.rows, , drop = FALSE]

      # message("P")
      # str(P)
      # message("W")
      # str(W)
      # message("P.sub")
      # str(P.sub)
      # message("W.sub")
      # str(W.sub)
      # message("has variation")
      # str(apply(P.sub, 2, function(col) length(unique(col)) > 1))

      #Checking for variation in endogenous regressors per group
      # if endo regressors are constant in a Z group, CDF have same
      #probabiity for all obs. Cov matrix may have a zero row or column. solve()
      #will fail

      has.variation <- any(apply(P.sub, 2, function(col) length(unique(col)) > 1))
      if (!has.variation) {
        fn.warn.skip("variation")
        next
      }

      # now use non-factor columns only for the CDF transformation.
      # factor variables cannot enter the CDF transformation as they are discrete with
      # no meaningful continuous CDF

      # Other than for the continuous-only case, we do not want an error (like
      # non-invertible matrix) in a single strata to derail the whole pcop generation
      # process.
      #
      # So far, continue with all 0 only for errors related to matrix inversion.
      # Let all other errors & warnings go through
      # TODO: maybe wrap solve() itself + throw custom error to identify more robustly
      cop.terms.sub <- tryCatch(
        copulajams_correction_continuous(P = P.sub, W = W.sub, cdf = cdf),
        error = function(e) {
          # non-invertible signals an error containing "singular"
          if (grepl("singular", conditionMessage(e), ignore.case = TRUE)) {
            # print(e)
            return(NULL)
          }
          # fail / "re-throw" any other error
          stop(e)
        }
      )

      # there was an issue with inverting the matrix
      if (is.null(cop.terms.sub)) {
        warning(
          "Skipping: Matrix inversion failed for the factor level `",
          lvl.i,
          "` of variable `",
          label.Z.i,
          "`",
          call. = FALSE
        )
        next
      }

      # Expanding back to a full dataset
      # I(Z_i = z) from eq. 20. zero attributed for observations not in this level
      # As many rows as full data (P) but only as many cols as correction applied
      cop.terms.full <- matrix(0, nrow = nrow(P), ncol = ncol(cop.terms.sub))
      cop.terms.full[idx.rows, ] <- cop.terms.sub

      # name columns according to strata (name) and factor level info
      name.strata <- paste(label.Z.i, lvl.i, sep = "_")
      colnames(cop.terms.full) <- make.names(paste0(
        name.strata,
        "_",
        colnames(cop.terms.sub)
      ))
      l.results[[name.strata]] <- cop.terms.full
    }
  }

  # str(l.results)

  if (length(l.results) == 0) {
    stop(
      "No valid factor-level subsets found for correction term estimation. ",
      "Check that factor exogenous regressors have sufficient observations ",
      "per level (>",
      p.cov,
      ") and variation in the endogenous regressors.",
      call. = FALSE
    )
  }

  cop.terms.results <- do.call(cbind, l.results)
  # message("cop.terms.results")
  # str(cop.terms.results)
  return(cop.terms.results)
}
