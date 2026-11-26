#' @importFrom Formula as.Formula
#' @importFrom stats terms formula
copulajams_fit <- function(F.formula, data, cdf, verbose) {
  # Model parts ------------------------------------------------------------------------
  # P & W: 1 col per term. If part 3 is `0`, W has no cols (but all rows)
  P <- copulajams_build_matrix_from_part(
    F.formula = F.formula,
    data = data,
    rhs.part = 2
  )
  W <- copulajams_build_matrix_from_part(
    F.formula = F.formula,
    data = data,
    rhs.part = 3
  )
  # Strata (part 4): Single factor, NULL if none (part 4 is `0`)
  z <- copulajams_build_strata(F.formula = F.formula, data = data)

  # Single message about everything ----------------------------------------------------
  # Print what was identified for every role
  # - read from the data that is used
  # - before the calculations run
  if (verbose) {
    fn.list.or.none <- function(x) if (length(x) == 0) "none" else toString(x)

    # fmt: skip
    if (is.null(z)) {
        msg.strata <- paste0("none, the correction is estimated on all ", nrow(P), " observations together")
      } else {
        num.obs <- table(z)
        msg.strata <- paste0(nlevels(z), ", smallest with ", min(num.obs), " and largest with ",max(num.obs), " observations")
      }

    # Not in P, W, z: Vars of the structural model (part 1) in no other part
    vars.rhs1 <- all.vars(terms(F.formula, lhs = 0, rhs = 1, data = data))
    rhs.parts.other <- seq(from = 2, to = length(F.formula)[2])
    vars.other.parts <- all.vars(formula(F.formula, lhs = 0, rhs = rhs.parts.other))
    vars.no.other.part <- setdiff(vars.rhs1, vars.other.parts)

    # fmt: skip
    message(
          "Fitting JAMS copula model\n",
          "  Endogenous regressors: ", fn.list.or.none(colnames(P)), "\n",
          "  Exogenous regressors used for the correction: ", fn.list.or.none(colnames(W)), "\n",
          "  Strata: ", msg.strata, "\n",
          "  Other variables, not used for the correction: ", fn.list.or.none(vars.no.other.part))
  }

  # Correction terms -------------------------------------------------------------------
  # Check for constant regressors as may happen in bootstrap sample.
  # Fail with a clear reason instead of a singular cov matrix in solve() later
  copula2stage_stop_if_constant(P)
  copula2stage_stop_if_constant(W)

  # Calculate: Depends whether strata present
  # w/ strata: then it requires stratified correction per stratum (see equation 20 and 21)
  # w/o strata: variance-covariance correction (eq. 17 to 19)
  if (is.null(z)) {
    # case no strata
    cop.terms <- copulajams_correction_continuous(P = P, W = W, cdf = cdf)
  } else {
    # case where strata are present
    cop.terms <- copulajams_correction_discrete(P = P, W = W, z = z, cdf = cdf)
  }

  # 2nd stage: OLS ---------------------------------------------------------------------

  res.2nd.stage <- copula2stage_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  labels.endo <- labels(terms(F.formula, lhs = 0, rhs = 2))

  return(list(
    res.lm.augmented = res.2nd.stage$res.lm.augmented,
    labels.pcop = res.2nd.stage$labels.pcop,
    labels.exo = NULL,
    labels.endo = labels.endo,
    P = P,
    W = W,
    z = z
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
  z, # single factor: the stratum of every obs
  cdf
) {
  # str(df.Z)
  # str(W)
  # str(P)
  stopifnot(is.factor(z))
  stopifnot(length(z) == nrow(P))
  # At least 1 stratum. Should always be the case: Input checks verify + same number
  # in bootstrap samples
  stopifnot(nlevels(z) > 0)
  l.results <- list()

  # Foreach stratum (level of z):
  for (lvl.i in levels(z)) {
    # find where data with this level is
    idx.rows <- z == lvl.i

    # stop if too few observations or no variation in the regressors
    # More than p observations are strictly required to estimate a non-singular
    # p x p sample covariance matrix, especially where p = ncol(PW) = endogenous regressors +
    # continuous exogenous regressors.

    # Sample covariance matrix (1/(n-1))* X'X has rank min (n-1, p).
    #if n <= p, it is rank deficient.

    # The reason of every error in this stratum starts with the stratum
    msg.stratum <- paste0("Stratum '", lvl.i, "': ")

    # check num obs before doing expensive data subset
    p.cov <- ncol(P) + ncol(W)
    if (sum(idx.rows) <= p.cov) {
      #p.cov = K + L which is the dim of the covariance matrix.
      #needs to be strictly more than p.cov obs. for non-singular
      # fmt: skip
      stop(msg.stratum, "has ", sum(idx.rows), " observations but needs more than ",
           p.cov, " (the number of columns of P and W).", call. = FALSE)
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

    #

    # now use non-factor columns only for the CDF transformation.
    # factor variables cannot enter the CDF transformation as they are discrete with
    # no meaningful continuous CDF

    # Any error in this stratum stop()s with a message identifying the stratum.
    # - constants
    # - anything else in the math (copulajams_correction_continuous)
    cop.terms.sub <- tryCatch(
      {
        #Checking for variation in endogenous regressors per group
        # if endo regressors are constant in a Z group, CDF have same
        #probabiity for all obs. Cov matrix may have a zero row or column. solve()
        #will fail
        copula2stage_stop_if_constant(P.sub)
        copula2stage_stop_if_constant(W.sub)

        # Run the math per stratum
        copulajams_correction_continuous(P = P.sub, W = W.sub, cdf = cdf)
      },
      error = function(e) {
        stop(msg.stratum, conditionMessage(e), call. = FALSE)
      }
    )

    # Expanding back to a full dataset
    # I(Z_i = z) from eq. 20. zero attributed for observations not in this level
    # As many rows as full data (P) but only as many cols as correction applied
    cop.terms.full <- matrix(0, nrow = nrow(P), ncol = ncol(cop.terms.sub))
    cop.terms.full[idx.rows, ] <- cop.terms.sub

    # Names for per-stratum correction terms like "P1_cop[Z=0]"
    colnames(cop.terms.full) <- copulajams_get_stratum_copterm_names(
      copterm.names = colnames(cop.terms.sub),
      stratum.level = lvl.i
    )
    l.results[[lvl.i]] <- cop.terms.full
  }

  # str(l.results)

  # Check whether there are any results at all no longer required. Fails as soon as
  # any stratum has no result
  # if (length(l.results) == 0) {}

  cop.terms.results <- do.call(cbind, l.results)
  # message("cop.terms.results")
  # str(cop.terms.results)
  return(cop.terms.results)
}
