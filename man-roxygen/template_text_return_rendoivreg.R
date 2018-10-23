#' @return
#' Returns an object of classes \code{rendo.ivreg} and \code{ivreg}, It extends the object returned from
#' function \code{ivreg} of package \code{AER} and slightly modifies it by adapting the \code{call}
#' and \code{formula} components. The \code{summary} function prints additional diagnostic information as
#' described in documentation for \code{\link[ivreg]{summary.ivreg}}.
#'
#' All generic accessor functions for \code{ivreg} such as \code{anova}, \code{hatalues}, or \code{vcov} are available.
#' @seealso \code{\link[AER]{ivreg}}
