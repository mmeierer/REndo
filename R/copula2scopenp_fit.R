#' @importFrom stats qnorm
#' @importFrom np npcdist
copula2scopenp_fit <- function(F.formula, data, verbose, labels.endo, labels.exo, bws) {
  #conditional CDF smooths the discrete distribution through X

  # 1st stage: conditional CDF ------------------------------------------------------
  # Computing conditional CDF correction term nonparametrically (via helper) C_p
  # from equation 21 of Hu et al. 2025

  cop.terms <- copula2stage_get_1ststage_copterms(
    nrow = nrow(data),
    labels.endo = labels.endo
  )

  condists <- list()
  first.stage.frames <- list()

  for (k in seq_along(labels.endo)) {
    p.label <- labels.endo[k]

    if (verbose) {
      msg.k <- paste0(p.label, "' (", k, " of ", length(labels.endo), ") ...")
      message("Computing conditional CDF for endogenous regressor '", msg.k)
    }

    # specify endo as DV to know where to read from
    mf.p <- copula2scopenp_mf_endo_given_exo(
      F.formula = F.formula,
      data = data,
      p.label = p.label,
      labels.exo = labels.exo
    )
    copula2stage_stop_if_constant(mf.p)
    first.stage.frames[[p.label]] <- mf.p

    # TODO: Check if bw were fit with formula or 2 data inputs as user might specify differently?

    # nonpara conditional CDF estimate
    # calculating F hat_ (P_{i,k} | X_i) for each observation
    # cdf.fit <- np::npcdist(bws = h, newdata = data.frame(y.data, x.data))
    # When passing bw: REQUIRED to also pass txdat/tydat as otherwise (silently)
    # the data on which bws was fit is used!
    bw.p <- bws[[p.label]]
    txdat <- mf.p[, -1, drop = FALSE]
    tydat <- mf.p[, 1, drop = FALSE]
    # np applies the bandwidths by position: Name & order must match.
    # Checked in input checks but keep assertion for docu and safety
    stopifnot(
      identical(colnames(txdat), bw.p$xnames),
      identical(colnames(tydat), bw.p$ynames)
    )

    cdf.fit <- np::npcdist(
      bws = bw.p,
      txdat = txdat,
      tydat = tydat
    )

    # store for return
    condists[[p.label]] <- cdf.fit

    # applying normal quantile transformation:  C_{i,pk} = phi^{-1} (F hat_ (P_k | X))
    # table 3 stage 1 and equation 21 from Hu et al. 2025
    conditional.cdf <- cdf.fit$condist
    # write to results using index and not p.label (label) as colnames have been stripped
    # from backticks
    cop.terms[, k] <- qnorm(conditional.cdf)
  }

  # 2nd stage ----------------------------------------------------------------------
  # using equation 20: Y = mu + sum_{k=1} ^ {K} ( P_{i,k} * alpha_k + beta' X_i + sum_{k=1}^{K} C_{i,pk} * gamma_k + epsilon_i

  l.2nd.stage <- copula2stage_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.lm.augmented = l.2nd.stage$res.lm.augmented,
    labels.pcop = l.2nd.stage$labels.pcop,
    condists = condists,
    first.stage.frames = first.stage.frames
  ))
}


#computing the nonparametric conditional CDF correction terms

# Select bandwidth (h) for conditional CDF F hat_(P_k |X) through cross validation
#For each endogenous regressor P_k, we have to estimate the conditional CDF
#F hat_(P_k |X) nonparametrically using Kernel methods (Hu et al 2025, mentions mostly Nadaraya-Watson (NW) kernel
#before eq. 10) Stage 1 from table 3
#Then the normal quantile transformation is applied to get the copula correction term
#' @importFrom utils modifyList
#' @importFrom np npcdistbw npcdist
copula2scopenp_bandwidth <- function(
  F.formula,
  data,
  verbose,
  labels.endo,
  labels.exo,
  bws,
  npcdistbw.args
) {
  k <- 1

  l.bws <- lapply(labels.endo, function(p.label) {
    if (verbose) {
      message(
        "Computing bandwidth for endogenous regressor '",
        p.label,
        "' (",
        k,
        " of ",
        length(labels.endo),
        ") ..."
      )
    }
    # double arrow because manipulating `k` higher up in the call stack
    k <<- k + 1

    # Use npcistw: supposed to handle both cts and discrete (or combination of both) X
    # automatically through a generalised product kernel (Li and Racine 2008)

    # Using npcdistbw:
    # Needs to be able to recognize discrete data as such
    # -> not pass encoded dummies but keep as factors!
    #
    # Option formula interface `npcdistbw(formula, data)`: Would be nice, but their
    #   formula interface is broken. It does not seem to use the given `data` parameter
    #   (rather looks in the environment of formula which is also broken)
    #
    # Instead use xdat/ydat which requires to pass in model.frame() columns:
    #   - not model.matrix because need to preserve factors
    #   - not columns of `data` because transformations need to be applied

    mf.p <- copula2scopenp_mf_endo_given_exo(
      F.formula = F.formula,
      data = data,
      p.label = p.label,
      labels.exo = labels.exo
    )

    bw.call.args <- list(
      ydat = mf.p[, 1, drop = FALSE], # endo: response (first col)
      xdat = mf.p[, -1, drop = FALSE] # exo: all except response
    )

    # Add existing bandwidth object to call args, if user passed `bws`.
    # Dont set to NULL because docu doesnt explicitly specify this as not-specified.
    # Rather leave entirely unset.
    if (!is.null(bws)) {
      bw.call.args[["bws"]] <- bws[[p.label]]
    }
    bw.call.args <- modifyList(bw.call.args, npcdistbw.args)

    return(do.call(what = np::npcdistbw, args = bw.call.args))
  })

  names(l.bws) <- labels.endo
  return(l.bws)
}


# Model frame of (single endo) ~ (all exo), for bandwidth selection and fit()
#' @importFrom stats model.frame na.fail reformulate
copula2scopenp_mf_endo_given_exo <- function(F.formula, data, p.label, labels.exo) {
  # there is no intercept produced by model.frame() but be explicit here that the
  # exogenous data passed here is without intercept
  f.endo.exo <- reformulate(
    termlabels = labels.exo,
    response = p.label,
    intercept = FALSE
  )

  # evaluate in the user's formula env and not the env here.
  # Set with env(f) because reformulate(env=) requires R >= 3.6
  environment(f.endo.exo) <- environment(F.formula)

  mf <- model.frame(formula = f.endo.exo, data = data, na.action = na.fail)
  return(copula2scopenp_mf_drop_asis(mf))
}


# np::npcdistbw() has a but where it fails for cols with class "AsIs" (ie I()
# in formula). Therefore drop all such classes from columns that have it
copula2scopenp_mf_drop_asis <- function(mf) {
  is.asis <- vapply(mf, inherits, what = "AsIs", FUN.VALUE = logical(1))
  for (j in which(is.asis)) {
    cls <- class(mf[[j]])
    class(mf[[j]]) <- cls[cls != "AsIs"]
  }
  return(mf)
}
