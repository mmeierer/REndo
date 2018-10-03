
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
