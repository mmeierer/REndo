#' @title Fitting Linear Models with Endogenous Regressors using Latent Instrumental Variables
#' @description
#' Fits linear models with endogenous regressor using latent instrumental variable approaches.
#'
#' The methods included in the package are Lewbel's (1997) <doi:10.2307/2171884> higher moments approach as well as
#' Lewbel's (2012) <doi:10.1080/07350015.2012.643126> heteroskedasticity approach, Park and Gupta's (2012) <doi:10.1287/mksc.1120.0718> joint estimation method
#' that uses Gaussian copula and Kim and Frees's (2007) <doi:10.1007/s11336-007-9008-1> multilevel generalized
#' method of moment approach that deals with endogeneity in a multilevel setting.
#' These are statistical techniques to address the endogeneity problem where no external instrumental variables are needed.
#'
#' Note that with version 2.0.0 sweeping changes were which greatly improve functionality but break backwards compatibility.
#' Various bugs were fixed, performance improved, handling of S3 objects and methods across the package was harmonized,
#' and a set of argument checks has been added. Starting with REndo 2.0, all functions support the use
#' of transformations such as I(x^2) or log(x) in the formulas.
#'
#' Check the NEWS file or our \href{https://github.com/mmeierer/REndo}{github page} for the latest updates and for reporting issues.
#'
#' @docType package
#' @name REndo
#'
NULL
