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
#' \item{cdf="ecdf"}{Standard empirical CDF with an endpoint adjustment. The minimum
#'      value is replaced by \eqn{10^{-7}} boundary and the maximum by \eqn{1 - 10^{-7}}
#'      in order to avoid to avoid \eqn{\Phi^{-1}(0) = -\infty} and \eqn{\Phi^{-1}(1) = +\infty}.
#'      The boundary replacement produces \eqn{\Phi^{-1}(10^{-7}) = \pm 5.2}. Note:
#'      (Becker et al. 2022) use this adjustment to show that Gaussian copula's performance worsen
#'      when intercept are included.
#'    }
#'
#' \item{cdf="kde"}{Integral of a density estimator via \code{ks::kcde} used in (Park and Gupta, 2012).
#'       \deqn{ \hat{h}(p) = \frac{1}{T \cdot b} \sum_{t=1}^{T} K\!\left(\frac{p - P_t}{b}\right)}
#'       The CDF is obtained by integrating a Kernel density estimator with and Epanechnikov kernel
#'       \eqn{K(x) = 0.75 \cdot (1 - x^2) \cdot \mathbb{1} (|x| \leq 1)} and a Silverman (1986) bandwidth
#'       \eqn{b = 0.9 \cdot T ^{-1/5} \cdot \min(s, \text{IQR}/1.34)} where IQR is the interquantile range,
#'       \eqn{s} is the data sample standard deviation and \eqn{T} is the number of tme periods observed in the data.}
#'
#' }
#'
#' It is recommended to use \code{cdf = "adj.ecdf"} for the best finite-sample
#' performance (Liengaard et al. 2025).
#' }
