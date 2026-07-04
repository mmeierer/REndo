
# Splits the given datatable by name.by and returns the content of name.group as list of matrices
#   Rownames are defaults discarded as it is slow and not needed.
#   Can be kept for verification purposes on intermediate steps
multilevel_splittomatrix <- function(dt, name.group, name.by, keep.rownames=FALSE){
  .SD <- NULL # cran silence
  if(keep.rownames){
    l.groups <- split(dt[, .SD, .SDcols=c("rownames",name.group, name.by)],
                      by = name.by,
                      keep.by = FALSE,
                      drop = TRUE, # drop unnused factors in group Ids (in case factors or chars given)
                      sorted = TRUE) # keep original ordering

    # take rownames from column rownames which also removes this column from the resulting matrix
    l.groups <- lapply(l.groups, as.matrix, rownames =  FALSE)
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

# Returns the content of name.cols in data.table dt as single sparse matrix.
#   Always returns named because no large overhead and because these full-length matrices are used
#   to calculate fitted&residuals which should be named.
#' @importFrom Matrix Matrix
multilevel_colstomatrix <- function(dt, name.cols){
  .SD <- NULL # cran silence
  return(Matrix::Matrix(as.matrix(dt[, .SD, .SDcols = c(name.cols, "rownames")],
                                    rownames = "rownames"),
                        sparse = TRUE))
}


multilevel_splitmatrixtolist <- function(m, dt.model.data, name.split.by){
  # cran silence
  .I <- NULL

  # get indices of each group
  dt.group.idx <- dt.model.data[, list(g.idx=list(.I)), by=name.split.by]
  # bring to same order as the other lists (shouldnt be required as sorted already)
  dt.group.idx[order(name.split.by), ]

  # Split matrix into groups
  l.groups <- lapply(dt.group.idx$g.idx, function(g.id) {m[g.id, g.id, drop=FALSE]})
  return(l.groups)
}

