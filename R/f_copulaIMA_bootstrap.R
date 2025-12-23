#' @importFrom Formula as.Formula
#'

copulaIMA_bootstrap <- function(F.formula, data, cdf, B){

  n <- nrow(data) #storing the sample size

  estimated <- CopulaIMA_fit(F.formula, data, cdf) #fitting on the full sample
  coef.names <- names(estimated$coefficients)

  #creating a matrix to add bootstrap in each column
  boots <- matrix(NA, nrow = length(coef.names), ncol = B, dimnames = list(coef.names, NULL))

  #looping over the bootstrap replicates
  for(b in seq_len(B)){
    index <- sample.int(n, replace = TRUE) #nonpara bootstrap. n with replacement
    data.b <- data[index, ,drop = FALSE]

    estimated.b <- try(CopulaIMA_fit(F.formula, data.b,cdf), silent = TRUE) # fitting model on the bootstrap sample again

    #storing only successful fits and not NAs
    if (!inherits(estimated.b, "try-error"))
      boots[, b] <- estimated.b$coefficients
  }

  boots
}

