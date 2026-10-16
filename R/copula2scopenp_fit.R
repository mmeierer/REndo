#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame reformulate update qnorm
#' @importFrom np npcdist
copula2scopenp_fit <- function(F.formula, data, labels.endo, labels.exo, bws, verbose) {
  #conditional CDF smooths the discrete distribution through X

  # First stage: conditional CDF ----------------------------------------------------
  # Computing conditional CDF correction term nonparametrically (via helper) C_p
  # from equation 21 of Hu et al. 2025

  cop.term <- copula_create_1ststage_copdata_matrix(
    n = nrow(data),
    labels.endo = labels.endo
  )

  condists <- list()
  mfs <- list()

  for (k in seq_along(labels.endo)) {
    p.label <- labels.endo[k]

    if (verbose) {
      msg.k <- paste0(p.label, "' (",k," of ",length(labels.endo),") ...")
      message("Computing conditional CDF for endogenous regressor '",msg.k)
    }

    # exo column is everything that is non-intercept and non-endo col
    f.endo.exo <- reformulate(
      response = p.label,
      termlabels = labels.exo
    )

    # specify endo as DV to know where to read from
    mf.p <- model.frame(formula = f.endo.exo, data = data, na.action = na.fail)
    copula_stop_if_constant(mf.p)
    mfs[[p.label]] <- mf.p

    # TODO: Check if bw were fit with formula or 2 data inputs as user might specify differently?

    # nonpara conditional CDF estimate
    # calculating F hat_ (P_{i,k} | X_i) for each observation
    # cdf.fit <- np::npcdist(bws = h, newdata = data.frame(y.data, x.data))
    # When passing bw: REQUIRED to also pass txdat/tydat as otherwise (silently)
    # the data on which bws was fit is used!
    bw.p <- bws[[p.label]]
    txdat <- mf.p[, -1, drop = FALSE]
    tydat <- mf.p[, 1, drop = FALSE]
    if (!identical(sort(colnames(txdat)), sort(bw.p$xnames))) {
      stop(
        "Names in fitted bandwith and names of generated variables mismatch. This is likely because the bootstrap sample is degenerate."
      )
    }

    # if (length(endo.cols) == 0) {
    #   stop("No endogenous regressors found in the design matrix.", call. = FALSE)
    # }
    #
    # if (length(endo.cols) < length(names.endo.regs)) {
    #   stop(
    #     paste0(
    #       "Bootstrap sample dropped at least one endogenous regressor. ",
    #       "This happened when a regressor becomes constant in the resampling."
    #     ),
    #     call. = FALSE
    #   )
    # }

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
    # write to results using index and not p.var (label) as colnames have been stripped
    # from backticks
    cop.term[, k] <- qnorm(conditional.cdf)
  }

  # Second stage: augmented OLS ----------------------------------------------------
  # using equation 20: Y = mu + sum_{k=1} ^ {K} ( P_{i,k} * alpha_k + beta' X_i + sum_{k=1}^{K} C_{i,pk} * gamma_k + epsilon_i

  res.2nd.stage <- copula_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.term
  )

  return(list(
    res.augmented = res.2nd.stage$res.augmented,
    labels.pcop = res.2nd.stage$labels.pcop,
    condists = condists,
    mfs = mfs
  ))
}


#computing the nonparametric conditional CDF correction terms

# Select bandwidth (h) for conditional CDF F hat_(P_k |X) through cross validation
#For each endogenous regressor P_k, we have to estimate the conditional CDF
#F hat_(P_k |X) nonparametrically using Kernel methods (Hu et al 2025, mentions mostly Nadaraya-Watson (NW) kernel
#before eq. 10) Stage 1 from table 3
#Then the normal quantile transformation is applied to get the copula correction term
#' @importFrom stats model.frame model.matrix
#' @importFrom np npcdistbw npcdist
copula2scopenp_bandwidth <- function(
  data,
  bws,
  npcdistbw.args,
  labels.exo,
  labels.endo,
  verbose
) {
  k <- 1

  l.bws <- lapply(labels.endo, function(p.var) {
    if (verbose) {
      message(
        "Computing bandwidth for endogenous regressor '",
        p.var,
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

    # formula (single endo) ~ (all exo)
    # there is no intercept produced by model.frame() but be explicit here that the
    # exogenous data passed here is without intercept
    f.endo.exo <- reformulate(
      termlabels = labels.exo,
      response = p.var,
      intercept = FALSE
    )

    mf.p <- model.frame(f.endo.exo, data = data, na.action = na.fail)

    bw.call.args <- list(
      ydat = mf.p[, 1, drop = FALSE], # endo: response (first col)
      xdat = mf.p[, -1, drop = FALSE] # exo: all except response
    )

    # Add existing bandwidth object to call args, if user passed `bws`.
    # Dont set to NULL because docu doesnt explicitly specify this as not-specified.
    # Rather leave entirely unset.
    if (!is.null(bws)) {
      bw.call.args[["bws"]] <- bws[[p.var]]
    }
    bw.call.args <- modifyList(bw.call.args, npcdistbw.args)

    return(do.call(what = np::npcdistbw, args = bw.call.args))
  })

  names(l.bws) <- labels.endo
  return(l.bws)
}
