#' @details
#' \subsection{Bootstrap inference}{
#' Bootstrap inference is performed by resampling the data with replacement.
#' Degenerate bootstrap samples (e.g. singular design matrices or failed
#' model estimation) are discarded and resampled until the requested number
#' of valid bootstrap replications is obtained.
#' If the requested number cannot
#' be obtained within 2 x \code{num.boots} attempts, bootstrapping stops with an
#' error. The percentage of discarded
#' samples is reported as a warning. Standard errors and confidence intervals
#' are computed using only successful bootstrap replications.
#' }
