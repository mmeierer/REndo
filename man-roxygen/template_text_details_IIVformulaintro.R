#' The \code{formula} argument has the following notation:
#' A two-sided formula object describing the MODEL (??), a single endogenous regressor, and
#' the internal instrumental variables to be built, each part separated by a single vertical bar (\code{|}).
#' The MODEL (??) part consists of the response on the left of the \code{~} operator and the term labels
#' on the right-hand side. The sole endogenous regressor is specified in the second right-hand side part
#' of the formula by separating it with a vertical bar from the MODEL(??). The instrumental variables
#' that should be built are specified as (multiple) functions, one for each instrument, and
#' separated from the endogenous regressor by a vertical bar. The function to build the
#' internal variables is \code{IIV} and uses the following arguments:
