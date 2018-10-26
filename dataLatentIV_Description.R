#' @title Simulated Dataset with One Endogeous Continuous Regressor
#' @description A dataset with one endogenous regressor \code{P}, an instrument \code{Z} 
#'  used to build \code{P}, an intercept and a dependent variable, \code{y}. 
#'  The true parameter values for the coefficients are: \code{b0 = 3} for the intercept
#'  and \code{a1 = -1} for \code{P}.
#' @name dataLatentIV
#' @usage dataLatentIV
#' @format A data frame with 2500 observations on 4 variables:
#' \itemize{
#' \item \code{y} - a numeric vector representing the dependent variable.
#' \item \code{I} - a numeric vector representing the intercept.
#' \item \code{P} - a numeric vector representing the endogenous variable.
#' \item \code{Z} - a numeric vector used in the construction of the endogenous variable, P.
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
#' @keywords dataLatentIV, latentIV, REndo, endogeneity, latent
NULL