# For each method:
#   fit_<method>_fast
#   fit_<method>_defaults

# Suppress warnings ----------------------------------------------------------------
# Suppress warnings matching `regexp` but lets all other warnings propagate
suppress_warning_matching <- function(expr, regexp) {
  return(withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(pattern = regexp, x = conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  ))
}

# Suppress 1000 num.boots warning
suppress_lowboots_warning <- function(expr) {
  return(suppress_warning_matching(expr, regexp = "recommended to run 1000"))
}

# copulaIMA ----------------------------------------------------------------------------
fit_ima_fast <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 2,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaIMA(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}

fit_ima_defaults <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 1000,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaIMA(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}

# copula2sCOPE -------------------------------------------------------------------------

suppress_2scope_noexo_warning <- function(expr) {
  return(suppress_warning_matching(expr, regexp = "No exogenous regressors found"))
}

fit_2scope_fast <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 2,
  verbose = FALSE
) {
  return(suppress_2scope_noexo_warning(suppress_lowboots_warning(
    copula2sCOPE(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  )))
}

fit_2scope_defaults <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 1000,
  verbose = FALSE
) {
  return(suppress_2scope_noexo_warning(suppress_lowboots_warning(
    copula2sCOPE(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  )))
}

# copula2sCOPEnp -----------------------------------------------------------------------
fit_2scopenp_fast <- function(
  formula,
  data,
  num.boots = 2,
  verbose = FALSE,
  npcdistbw.args = list(bwmethod = "normal-reference"),
  bws = NULL
) {
  return(suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = formula,
      data = data,
      num.boots = num.boots,
      verbose = verbose,
      npcdistbw.args = npcdistbw.args,
      bws = bws
    )
  ))
}

fit_2scopenp_defaults <- function(
  formula,
  data,
  num.boots = 1000,
  verbose = FALSE,
  npcdistbw.args = list(bwmethod = "cv.ls"),
  bws = NULL
) {
  return(suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = formula,
      data = data,
      num.boots = num.boots,
      verbose = verbose,
      npcdistbw.args = npcdistbw.args,
      bws = bws
    )
  ))
}


# copulaBMW ----------------------------------------------------------------------------
fit_bmw_fast <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 2,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaBMW(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}

fit_bmw_defaults <- function(
  formula,
  data,
  cdf = "resc.ecdf",
  num.boots = 1000,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaBMW(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}

# copulaCorrection --------------------------------------------------------------------
fit_copulacorrection_fast <- function(
  formula,
  data,
  num.boots = 2,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaCorrection(
      formula = formula,
      data = data,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}
