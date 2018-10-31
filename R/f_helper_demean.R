
# Col-wise de-mean helper function
de.mean <- function(x){
  # if(NCOL(x) > 1){
  if(length(dim(x)) > 1){
    # >1 col (data.frame,...).
    # Use sweep contrary to apply because it again returns data.frame
    return(sweep(x = x, MARGIN = 2, STATS = colMeans(x=x, na.rm = T), FUN = "-"))
    # return(apply(x, MARGIN = 2, FUN = function(x){x-mean(x)}))
  }else{
    # vector
    return(x-mean(x))
  }
}


# Extracts the hessian from the optimx result
extract.hessian <- function(res.optimx, names.hessian){
  # If optimx failed, single NA is returned as the hessian. Replace it with correctly-sized
  #   matrix of NAs

  hessian  <- attr(res.optimx, "details")[,"nhatend"][[1]]
  if(length(hessian)==1 & all(is.na(hessian))){
    hessian <- matrix(data = NA_real_, nrow = length(names.hessian), ncol = length(names.hessian))
    warning("Hessian could not be derived. Setting all entries to NA.", immediate. = TRUE)
  }

  rownames(hessian) <- colnames(hessian) <- names.hessian
  return(hessian)
}
