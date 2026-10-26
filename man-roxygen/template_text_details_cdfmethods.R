#' @details
#' \subsection{CDF Methods}{
#' \describe{
#' \item{cdf="adj.ecdf"}{Adjusted empirical CDF with midrank correction (Liengaard et al., 2024).
#'    \deqn{\hat{F}(x_i) = \frac{1}{2n} + \frac{n-1}{n^2} \sum_{i=1}^{n} \mathbf{1}(P_i \leq x)}
#'    where n is the sample size, \eqn{P_i} is the endogenous regressor for
#'    observation i = 1,..,n and \eqn{\mathbf{1} (.)} is the indicator function.
#'    This is a sample size-dependent adjustment of the ECDF. Correction term is acquired by minimising
#'    the true CDF's MSE, using the standard ECDF as a predictor in a linear regression.
#'    Keeps values strictly inside \eqn{(0,1)} but retains the ECDF's desirable large sample properties.
#'    }
#'
#' \item{cdf="resc.ecdf"}{Rescaled empirical cdf \deqn{\hat{F}(x_i) = \frac{\text{rank}(x_i)}{n + 1}}.
#'    Keeps values strictly inside \eqn{(0,1)} without an arbitrary boundary constant.
#'    Compared to "ecdf", the maximum value is  \eqn{\Phi^{-1}(n/(n+1))},
#'    which is considerably smaller in small samples (e.g., \eqn{\pm 2.8} for \eqn{n=400}).
#'    <%= if (exists("resc.ecdf.details.addition")) resc.ecdf.details.addition else "" %>
#'    }
#'
#' \item{cdf="ecdf"}{Standard empirical CDF with \eqn{10^{-7}} boundary replacement,
#'    giving \eqn{\Phi^{-1}(10^{-7}) = \pm 5.2} for the boundary observations.
#'    }
#'
#' \item{cdf="kde"}{Integral of a density estimator via \code{ks::kcde} used in (Park and Gupta, 2012).}
#'
#' }
#'
#' It is recommended to use \code{cdf = "adj.ecdf"} for the best finite-sample
#' performance (Liengaard et al. 2025).
#' }
