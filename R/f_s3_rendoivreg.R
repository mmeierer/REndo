#' @export
summary.rendo.ivreg <- function(object, ...){
  # Append diagnostics, then call ivreg's summary (the object's next class is ivreg)
  ans              <- NextMethod(object=object, diagnostics = TRUE, ...)
  return(ans)
}
