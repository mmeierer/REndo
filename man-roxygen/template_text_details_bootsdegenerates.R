#' @details
#' Bootstrap inference is performed by resampling the data with replacement.
#' Degenerate bootstrap samples (e.g. singular design matrices or failed
#' model estimation) are discarded and resampled until the requested number
#' of valid bootstrap replications is obtained. The percentage of discarded
#' samples is reported as a warning. Confidence intervals are computed
#' using only successful bootstrap replications.
