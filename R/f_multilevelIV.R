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

  # stop("The multilevelIV function is currently under active development and therefore unfortunately not available.", call. = FALSE)


  # ** CHECK THAT EVERY CHILD ONLY APPEARS IN 1 SCHOOL
  # .N, by=SID, CID, then uniqueN(CID) == nrow() (ie CID appears only once for every SID )


  # Check input -----------------------------------------------------------------

  # Extract information ---------------------------------------------------------
  dt.data <- data.table(data)

  # Let lme4 do the formula processing
  l4.form    <- lme4::lFormula(formula = formula, data=data)
  num.levels <- length(l4.form$reTrms$flist)+1
  stopifnot(num.levels %in% c(2,3))

  #
  if(num.levels == 2)
    res <- multilevel_2levels(formula = formula, data=data)
  else
    stop("Not yet supported")

}
