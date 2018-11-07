multilevel_splittomatrix <- function(dt, name.group, name.by){
  l.groups <- data.table:::split.data.table(dt[, .SD, .SDcols=c("rn",name.group, name.by)],
                                            by = name.by, keep.by = FALSE,
                                            sorted = FALSE) # keep original ordering
  # take rownames from column rn which also removes this column from the resulting matrix
  l.groups <- lapply(l.groups, data.table:::as.matrix.data.table, rownames =  "rn")
  return(l.groups)
}
