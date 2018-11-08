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
multilevelIV <- function(formula, name.endo, data, verbose=TRUE){

  cl <- match.call()

  # ** CHECK THAT EVERY CHILD ONLY APPEARS IN 1 SCHOOL
  # .N, by=SID, CID, then uniqueN(CID) == nrow() (ie CID appears only once for every SID )


  # Check input -----------------------------------------------------------------
  check_err_msg(checkinput_multilevel_formula(formula=formula))
  check_err_msg(checkinput_multilevel_data(data=data))
  check_err_msg(checkinput_multilevel_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_multilevel_verbose(verbose = verbose))

  # Extract information ---------------------------------------------------------

  # Let lme4 do the formula processing
  l4.form    <- lme4::lFormula(formula = formula, data=data)
  num.levels <- length(l4.form$reTrms$flist)+1
  stopifnot(num.levels %in% c(2,3))

  if(num.levels == 2)
    res <- multilevel_2levels(cl = cl, formula = formula, data=data, name.endo=name.endo)
  else
    if(num.levels == 3)
      res <- multilevel_3levels(cl = cl, formula = formula, data=data, name.endo=name.endo)
    else
      stop("wrong number of levels!")


  return(res)
}
