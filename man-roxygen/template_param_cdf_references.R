#' @param cdf Character string specifying the method used to estimate the
#'   marginal distribution functions of the endogenous regressors.
#'   One of \code{"adj.ecdf"}, \code{"resc.ecdf"}, \code{"ecdf"}, or \code{"kde"}.
#'  \describe{
#'     \item{\code{"adj.ecdf"}}{Adjusted empirical CDF with midrank correction
#'       (Liengaard et al., 2024). Keeps values strictly inside (0,1).}
#'     \item{\code{"resc.ecdf"}}{Rescaled empirical CDF via
#'       \code{copula::pobs} (Qian et al., 2024).}
#'     \item{\code{"ecdf"}}{Empirical CDF with boundary replacement
#'       (Becker et al., 2022).}
#'     \item{\code{"kde"}}{Integral of a density estimator via \code{ks::kcde}
#'       used in (Park and Gupta, 2012).}
#'   }
#'
#' @references
#' Becker, JM., Proksch, D., & Ringle, C.M. Revisiting Gaussian
#' copulas to handle endogenous regressors. \emph{Journal of the Academy of
#' Marketing Science}, 50, 46--66 (2022).
#' \doi{10.1007/s11747-021-00805-y}
#'
#' Liengaard, B. D., Becker, J.-M., Bennedsen, M., Heiler, P., Taylor, L. N.,
#' & Ringle, C. M. (2025). Dealing with regression models' endogeneity by
#' means of an adjusted estimator for the Gaussian copula approach.
#' \emph{Journal of the Academy of Marketing Science}, 53(1), 279-299
#' \doi{10.1007/s11747-024-01055-4}
#'
#' Park, S. and Gupta, S. (2012). Handling endogenous regressors by joint
#' estimation using copulas. \emph{Marketing Science}, 31(4), 567--586.
#' \doi{10.1287/mksc.1120.0718}
#'
#' Qian, Y., Koschmann, A., and Xie, H. (2024). A practical guide to
#' endogeneity correction using copulas. NBER Working Paper No. w32231.
#' \doi{10.2139/ssrn.4754776}
