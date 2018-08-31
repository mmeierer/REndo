#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame update
#' @export
copulaCorrectionContinuous2  <- function(formula, data){
  cl <- match.call()

  # Extract data based on given formula ---------------------------------------------------------------
  F.formula         <- as.Formula(formula)
  df.data.endo      <- model.frame(formula = F.formula, data = data, lhs=0, rhs = 2)

  # P.star --------------------------------------------------------------------------------------------
  p.star            <- copulaCorrectionContinuous_pstar(data.endo = df.data.endo)


  # Fit on copula data --------------------------------------------------------------------------------

  # For copula: all given by user + pstar
  df.data.copula <- cbind(data, p.star)

  # for fitting lm, use the formula first part and also include P.star
  f.lm.relevant <- update(formula(F.formula, lhs=1, rhs=1),
                          paste0(".~.+", paste(colnames(p.star), collapse = "+")))
print(head(df.data.copula))
print(f.lm.relevant)
  # Fit
  res.lm <- lm(formula = f.lm.relevant, data = df.data.copula)


  # Return --------------------------------------------------------------------------------------------
  res.lm.summary <- summary(res.lm)
  lm_stats       <- list(residSE = res.lm.summary$sigma,r2 = res.lm.summary$r.squared,adjr2 = res.lm.summary$adj.r.squared,
                         fstat = res.lm.summary$fstatistic, df=res.lm.summary$df)
  return(structure(list(call         = cl,
                        formula      = formula,
                        coefficients = coef(res.lm),
                        seCoefficients = as.matrix(res.lm.summary$coefficients[,2]),
                        regressors = df.data.copula,
                        lm_stats   = lm_stats,
                        fitted.lm  = res.lm),
              class = "rendo.copulacorrection.continuous2"))
}
