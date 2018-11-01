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
#' @export
multilevelIV <- function(formula, data, verbose=TRUE){

  stop("The multilevelIV function is currently under active development and therefore unfortunately not available.", call. = FALSE)

}
