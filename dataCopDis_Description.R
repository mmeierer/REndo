#' @title Simulated Dataset with One Endogeous Discrete Regressor
#' @description A dataset with two exogenous regressors,
#'  \code{X1},\code{X2}, and one endogenous, discrete (Poisson distributed) regressor,
#'  \code{P}.
#'  An intercept and a dependent variable, \code{y}, are also included. 
#'  The true parameter values for the coefficients are: \code{b0 = 2}, \code{b1 = 1.5}, 
#'  \code{b2 = -3} and the coefficient of the endogenous regressor, P, is equal to \code{a1 = -1}.
#' @name dataCopDis
#' @usage dataCopDis
#' @format A data frame with 2500 observations on 5 variables:
#' \itemize{
#' \item \code{y} - a numeric vector representing the dependent variable.
#' \item \code{I} - a numeric vector representing the intercept.
#' \item \code{X1} - a numeric vector, normally distributed and exogenous.
#' \item \code{X2} - a numeric vector, normally distributed and exogenous.
#' \item \code{P} - a numeric vector, continuous and endogenous having T-distribution with 3 degrees of freedom.
#' }
#' @docType data
#' @author Raluca Gui \email{raluca.gui@@business.uzh.ch}
#' @keywords dataCopDis, copulaCorrection, REndo, endogeneity
NULL