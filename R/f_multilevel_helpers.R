multilevel_splittomatrix <- function(dt, name.group, name.by){
  .SD <- NULL # cran silence
  if("rownames" %in% colnames(dt)){
    l.groups <- split(dt[, .SD, .SDcols=c("rownames",name.group, name.by)],
                      by = name.by,
                      keep.by = FALSE,
                      drop = TRUE, # drop unnused factors in group Ids (in case factors or chars given)
                      sorted = TRUE) # keep original ordering

    # take rownames from column rownames which also removes this column from the resulting matrix
    l.groups <- lapply(l.groups, as.matrix, rownames =  "rownames")

  }else{
    l.groups <- split(dt[, .SD, .SDcols=c(name.group, name.by)],
                      by = name.by,
                      keep.by = FALSE,
                      drop = TRUE, # drop unnused factors in group Ids (in case factors or chars given)
                      sorted = TRUE) # keep original ordering
    l.groups <- lapply(l.groups, as.matrix, rownames = FALSE)
  }

  return(l.groups)
}

#' @importFrom Matrix Matrix
multilevel_colstomatrix <- function(dt, name.cols){
  .SD <- NULL # cran silence
  if("rownames" %in% colnames(dt))
    return(Matrix::Matrix(as.matrix(dt[, .SD, .SDcols = c(name.cols, "rownames")],
                                    rownames = "rownames"),
                          sparse = TRUE))
  else
    return(Matrix::Matrix(as.matrix(dt[, .SD, .SDcols = name.cols],
                                    rownames = FALSE),
                          sparse = TRUE))
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
