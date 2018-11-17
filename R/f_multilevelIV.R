#'
#' @title Multilevel GMM Estimation
#'
#' @template template_param_formuladataverbose
#'
#' @description
#' ** THIS METHOD IS NOT AVAILABLE, YET!
#'
#' Estimates multilevel models (max. 3 levels) employing the GMM approach presented in Kim and Frees (2007).
#' One of the important features is that, using the hierarchical structure of the data, no external instrumental
#' variables are needed, unlike traditional instrumental variable techniques
#' @details
#' Method will be added to the package in the next major release.
#' @references   Kim, Jee-Seon and Frees, Edward W. (2007). "Multilevel Modeling with Correlated Effects". Psychometrika, 72(4), 505-533.
#' @importFrom lme4 lmer VarCorr lFormula
#' @export
multilevelIV <- function(formula, data, verbose=TRUE){

  cl <- match.call()

  # Check input -----------------------------------------------------------------
  check_err_msg(checkinput_multilevel_formula(formula=formula))
  check_err_msg(checkinput_multilevel_data(data=data))
  check_err_msg(checkinput_multilevel_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_multilevel_verbose(verbose = verbose))

  # May endo be in (ENDO | CID) ?


  # Extract information ---------------------------------------------------------
  F.formula <- as.Formula(formula)
  f.lmer    <- formula(F.formula, lhs = 1, rhs = 1)
  name.endo <- formula_readout_special(F.formula = F.formula, name.special = "endo",
                                       from.rhs = 2, params.as.chars.only = TRUE)

  # Let lme4 do the formula processing
  l4.form    <- lme4::lFormula(formula = f.lmer, data=data)
  num.levels <- lme4formula_get_numberoflevels(l4.form)

  message("Num levels detected: ", num.levels)

  if(num.levels == 2)
    res <- multilevel_2levels(cl = cl, f.orig=formula, f.lmer.part=f.lmer, l4.form=l4.form,
                              data=data, name.endo=name.endo)
  else
    if(num.levels == 3)
      res <- multilevel_3levels(cl = cl, f.orig=formula, f.lmer.part=f.lmer, l4.form=l4.form,
                                data=data, name.endo=name.endo)

  return(res)
}
