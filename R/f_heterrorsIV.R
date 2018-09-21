#' @importFrom Formula as.Formula
#' @importFrom stats model.frame residuals reformulate update.formula terms
#' @importFrom AER ivreg
#' @export
hetErrorsIV <- function(formula, data){

  cl <- match.call()

  F.formula <- as.Formula(formula)

  # Calculate internal instruments --------------------------------------------------------------------
  # For each exogen. hetero. variables (3rd RHS), derive the instruments:
  #   residuals * [x-mean(x)] where  residuals from
  #     lm(endo P ~ all exogenous(:=RHS2)) and x are the single exo.het.vars

  F.residuals.intinstr <-
    reformulate(response   = attr(terms(formula(F.formula, rhs=1, lhs=0)), "term.labels"),
                termlabels = attr(terms(formula(F.formula, rhs=2, lhs=0)), "term.labels"),
                intercept  = attr(terms(formula(F.formula, rhs=2, lhs=0)), "intercept"))

  # Fit lm and obtain residuals
  intinstr.residuals <- residuals(lm(F.residuals.intinstr, data=data))

  # From RHS3
  df.data.exohetero <- model.frame(formula(F.formula, lhs=0, rhs=3), data = data)

  # **** MAKE MORE STABLE FOR SINGLE COLUMN
  df.data.intinstr <- apply(X = df.data.exohetero, MARGIN = 2, FUN = function(single.exohetero){
    (single.exohetero - mean(single.exohetero)) * intinstr.residuals
    })

  colnames(df.data.intinstr) <- paste0("internal.instrument", seq(NCOL(df.data.intinstr)))

  # Instrumental variable regression ------------------------------------------------------------------

  # Add internal.instruments to data
  df.data.ivreg <- cbind(data, df.data.intinstr)


  # Build IVreg formula: LHS1 ~ RHS1 + RHS2 | RHS2 + instruments (+ RHS4, if present)

  # Build 1st part of ivreg formula: Response ~ RHS1 + RHS2 together
  # (collapse=T->put as one side and update=T->dont keep separated through brackets)
  F.ivreg <- formula(F.formula, lhs=1, rhs=c(1,2), collapse=T, update=T)

  # Build 2nd part of ivreg formula: RHS2 + instruments + anything in RHS4 (if given)
  ivreg.2ndparts   <- if((length(F.formula)[2]) == 4){ c(2,4) }else{ 2 } # include RHS4 only if present
  F.ivreg.2nd.part <- formula(F.formula, lhs=0, rhs=ivreg.2ndparts, collapse=T, update=T)
  # Add instruments to 2nd part
  F.ivreg.2nd.part <- update.formula(F.ivreg.2nd.part,
                                    # include "." to keep stuff already present RHS
                                     reformulate(termlabels = c(".", colnames(df.data.intinstr) )))


  # Add 2nd part to final formula
  F.ivreg <- as.Formula(F.ivreg, F.ivreg.2nd.part)

  # Intercept from where?

  # Fit IVreg
  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  res <- structure(class="rendo.heterrorsiv",
                   list(call          = cl,
                        formula       = F.formula,
                        coefficients  = coef(res.ivreg),
                        res.ivreg     = res.ivreg))
                        # jtest         = as.matrix(unclass(l.res$j.test[[2]])),
                        # ftest         = l.res$f.test.stats))

  return(res)
}


# lm for residuals: RHS1 ~ RHS2
# instruments x for x-mean(x): x from RHS3
# IVreg formula: LHS1 ~ RHS1 + RHS2 | RHS2 + instruments (+ RHS4, if present)
