multilevel_splittomatrix <- function(dt, name.group, name.by){
  .SD <- NULL
  l.groups <- split(dt[, .SD, .SDcols=c("rn",name.group, name.by)],
                    by = name.by, keep.by = FALSE,
                    sorted = FALSE) # keep original ordering
  # take rownames from column rn which also removes this column from the resulting matrix
  l.groups <- lapply(l.groups, as.matrix, rownames =  "rn")
  return(l.groups)
}

#' @importFrom Matrix Matrix
multilevel_colstomatrix <- function(dt, name.cols){
  .SD <- NULL
  return(Matrix::Matrix(as.matrix(dt[, .SD, .SDcols = c(name.cols, "rn")],
                                  rownames = "rn"),  sparse = TRUE))
}



#' #' Formula:
#' #'   p510: P(H) = H(H'H)^(-1)H'
#' #' @importFrom corpcor pseudoinverse
#' multilevel_projectionP <- function(H){
#'   # crossprod:  t(x) %*% y
#'   # tcrossprod: x %*% t(y)
#'   # Matrix::crossprod returns a dsCMatrix (ie knows its symmetric)
#'   # return(H %*% Matrix::tcrossprod( Matrix::solve(Matrix::crossprod(H), sparse=TRUE), H))
#'   return(H %*% Matrix::tcrossprod( corpcor::pseudoinverse(Matrix::crossprod(H), sparse=TRUE), H))
#' }
