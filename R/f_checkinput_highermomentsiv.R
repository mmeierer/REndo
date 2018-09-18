checkinput_highermomentsiv_g <- function(g){
  err.msg <- checkinputhelper_charactervector(vec=g, parameter.name="g",
                                              allowed.inputs=c("x2", "x3", "lnx", "1/x"))
  if(length(err.msg)>0)
    return(err.msg)

  if(length(g) != 1)
    err.msg <- c(err.msg, "Please provide exactly one single element for \'g\'")
  return(err.msg)
}

checkinput_highermomentsiv_iiv <- function(IIV){
  err.msg <- checkinputhelper_charactervector(vec=IIV, parameter.name="IIV",
                                              allowed.inputs=c("g", "gp", "gy", "yp", "p2", "y2"))
  if(length(err.msg)>0)
    return(err.msg)

  if(length(IIV) != length(unique(IIV)))
    err.msg <- c(err.msg, "Please provide every element in \'IIV\' only once.")

  return(err.msg)
}

checkinput_highermomentsiv_interaction_iiv_g <- function(g, IIV){
  # g from c("x2", "x3", "lnx", "1/x")
  # IIV from c("g","gp","gy","yp","p2","y2"))

  # **** TODO
  # only need g if iiv does something with
  if(!any(iiv %in% c("g","gp","gy")))
  # does not contain any g
    return(NULL)

}
