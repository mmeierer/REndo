# Fixtures used in tests

# copula methods  ----------------------------------------------------------------------
fixture_copula_df <- function(n = 100) {
  return(data.frame(
    y = rnorm(n),
    x_num = rnorm(n),
    `x space` = rnorm(n), # non-syntactic name
    x_string = rep(c("a", "b"), length.out = n),
    x_fac = factor(rep(c("a", "b"), length.out = n)),
    x_ord = ordered(rep(c("lo", "hi"), length.out = n), levels = c("lo", "hi")),
    x_lowcard = rep(1:2, length.out = n),
    x_na = c(NA, rnorm(n - 1)),
    check.names = FALSE, # dont need to be syntactically valid
    stringsAsFactors = FALSE
  ))
}


# copulaIMA formula edge cases ---------------------------------------------------------
# Shared by smoke & correctness tests so the edge cases dont diverge.
# Named list of list(formula, data), as used by run_cases()
fixture_ima_formula_edge_cases <- function() {
  data("dataCopIMAContExo", "dataCopIMAMultiEndo", envir = environment())

  df <- dataCopIMAContExo
  df$X_pos <- abs(df$X) + 1
  df$P_pos <- abs(df$P) + 1
  df$`P 1` <- df$P
  df$`X 1` <- df$X

  df.multi <- dataCopIMAMultiEndo
  df.multi$`P 1` <- df.multi$P1
  df.multi$`P 2` <- df.multi$P2
  df.multi$`X 1` <- df.multi$X

  return(list(
    "plain" = list(formula = y ~ X + P | P, data = df),
    # dot: only on data without additional cols
    "dot in rhs1" = list(formula = y ~ . | P, data = dataCopIMAContExo),
    "backticks in endo" = list(formula = y ~ `P 1` + X | `P 1`, data = df),
    "backticks in exo" = list(formula = y ~ P + `X 1` | P, data = df),
    "backticks in endo (multiple) & exo" = list(
      formula = y ~ `P 1` + `P 2` + `X 1` | `P 1` + `P 2`,
      data = df.multi
    ),
    "transformed exo" = list(formula = y ~ log(X_pos) + P | P, data = df),
    "transformed endo" = list(formula = y ~ log(P_pos) + X | log(P_pos), data = df),
    "inline function exo" = list(formula = y ~ I(X^2) + P | P, data = df),
    "duplicate regressor" = list(formula = y ~ X + X + P | P, data = df)
  ))
}

# copula2sCOPEnp formula edge cases ----------------------------------------------------
# only continuous and factor data to run fast (ordered factors are tested separately)
fixture_2scopenp_formula_edge_cases <- function() {
  data("dataCopula2sCOPEnpCont", "dataCopula2sCOPEnpMulti", envir = environment())

  df <- dataCopula2sCOPEnpCont
  df$X_pos <- abs(df$X) + 1
  df$P_pos <- abs(df$P) + 1
  df$`P 1` <- df$P
  df$`X 1` <- df$X
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))

  df.multi <- dataCopula2sCOPEnpMulti
  df.multi$`P 1` <- df.multi$P1

  return(list(
    "plain" = list(formula = y ~ X + P | P, data = df),
    # dot: only on data without additional cols
    "dot in rhs1" = list(formula = y ~ . | P, data = dataCopula2sCOPEnpCont),
    "backticks in endo" = list(formula = y ~ `P 1` + X | `P 1`, data = df),
    "backticks in exo" = list(formula = y ~ P + `X 1` | P, data = df),
    "backticks in endo & exo" = list(formula = y ~ `P 1` + `X 1` | `P 1`, data = df),
    "transformed exo" = list(formula = y ~ log(X_pos) + P | P, data = df),
    "transformed endo" = list(formula = y ~ log(P_pos) + X | log(P_pos), data = df),
    # I() gives class "AsIs" in the model frame which np does not accept
    "inline function exo" = list(formula = y ~ I(X^2) + P | P, data = df),
    "inline function endo" = list(formula = y ~ X + I(P^2) | I(P^2), data = df),
    "inline function factor exo" = list(formula = y ~ X + I(x_fac) + P | P, data = df),
    "duplicate regressor" = list(formula = y ~ X + X + P | P, data = df),
    "factor exo" = list(formula = y ~ X + x_fac + P | P, data = df),
    "multiple endo with backtick" = list(
      # continuous variables only: X1, X2, P1 to run faster
      formula = y ~ X1 + X2 + `P 1` | X2 + `P 1`,
      data = df.multi
    )
  ))
}
