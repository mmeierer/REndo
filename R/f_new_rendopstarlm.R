# Constructor function for consistent creation of return objects
new_rendo_pstar_lm <- function(res.lm, F.formula, cl, data,
                               names.vars.continuous, names.vars.discrete){

  class(res.lm)                <- c("rendo.pstar.lm",class(res.lm))
  res.lm$formula               <- F.formula
  res.lm$call                  <- cl
  res.lm$original.data         <- data
  # add character(0) to surely create the entry in case that the names are NULL
  res.lm$names.vars.continuous <- c(character(0), names.vars.continuous)
  res.lm$names.vars.discrete   <- c(character(0), names.vars.discrete)
  return(res.lm)
}
