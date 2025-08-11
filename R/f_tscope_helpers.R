#' @title Internal: Helpers for 2sCOPE transformations
#' @description Internal utilities for tscope transformations.
#' @keywords internal
#' @importFrom stats ecdf qnorm
ecdf_transform_regressors <- function(regressor_matrix) {
  if (is.null(dim(regressor_matrix))) {
    regressor_matrix <- matrix(regressor_matrix, ncol = 1)
  }
  transformed <- apply(regressor_matrix, 2, function(single_column) {
    u <- stats::ecdf(single_column)(single_column)
    n <- length(single_column)
    u[u == 0] <- 1 - n / (n + 1)
    u[u == 1] <- n / (n + 1)
    stats::qnorm(u)
  })
  if (is.null(dim(transformed))) {
    transformed <- matrix(transformed, ncol = 1)
  }
  return(transformed)
}


