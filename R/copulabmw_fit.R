#' @importFrom stats lm reformulate
copulabmw_fit <- function(F.formula, data, cdf, labels.endo, labels.exo) {
  # Stage 1 --------------------------------------------------------------------
  # BMW correction
  # step 1: first-stage in the original space
  # step 2: CDF on residuals
  # step 3: apply qnorm

  cop.terms <- copula_create_1ststage_copdata_matrix(
    nrow = nrow(data),
    labels.endo = labels.endo
  )

  for (k in seq_along(labels.endo)) {
    p.label <- labels.endo[k]

    # Step 1: Original space --------------------------------------------------------
    #first-stage OLS of Z on X in original space
    #BMW (2024) eq. 2.2, Z = delta'x + e

    # Regress endo ~ (all exo), intercept should be included because of assumption A4 from Breitrung et al.
    f.endo.k.on.all.exo <- reformulate(
      #page 367
      response = p.label,
      termlabels = labels.exo,
      intercept = TRUE
    )

    res.lm.first <- lm(formula = f.endo.k.on.all.exo, data = data)
    e.hat <- residuals(res.lm.first)

    # Step 2: CDF on residuals ------------------------------------------------------
    # Apply CDF to the first-stage residuals (e hat)
    # Does not apply on the original regressors

    m.ehat <- matrix(e.hat, ncol = 1)

    # usual cdfs
    # According to BMW(2024), they adopt a 'common practice' and rescale by n + 1
    # Recommendation from eq. 2.3. F hat_{e hat} (e hat_i) = rank (e hat_i)/(n+1)
    # so that no arbitary boundary constant is needed.
    # will still keep all the values strictly in (0,1)
    # This is implemented in cdf=`resc.ecdf`=rank(x)/(n+1)
    P.star <- copula_pstar(P = m.ehat, cdf = cdf)

    # Step 3: Apply qnorm ----------------------------------------------------------
    #Apply qnorm from eq. 2.3, eta hat =  phi^{-1} (F hat_{e hat} (e hat))

    P.cop <- apply(P.star, 2, qnorm) # eta hat is P_cop

    cop.terms[, k] <- as.vector(P.cop)
  }

  # Stage 2 --------------------------------------------------------------------
  res.2nd.stage <- copula_fit_2ndstage(
    F.formula = F.formula,
    data = data,
    cop.terms = cop.terms
  )

  return(list(
    res.augmented = res.2nd.stage$res.augmented,
    labels.pcop = res.2nd.stage$labels.pcop
  ))
}
