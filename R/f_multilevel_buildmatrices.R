multilevel_splittomatrix <- function(dt, name.group, name.by){
  l.groups <- split(dt[, .SD, .SDcols=c("rn",name.group, name.by)],
                    by = name.by, keep.by = FALSE,
                    sorted = FALSE) # keep original ordering
  # take rownames from column rn which also removes this column from the resulting matrix
  l.groups <- lapply(l.groups, as.matrix, rownames =  "rn")
  return(l.groups)
}


multilevel_colstomatrix <- function(dt, name.cols){
  return(Matrix::Matrix(as.matrix(dt[, .SD, .SDcols = c(name.cols, "rn")],
                                  rownames = "rn"),  sparse = TRUE))
}
