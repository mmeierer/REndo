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


# copula2stage formula edge cases -----------------------------------------------------
# Edge cases shared by all copula2stage methods. Method-specific fixtures append their
# own cases. `data.X.P` has to be single endo data with columns y, X, P
# Returns named list of list(formula, data), as used by run_cases()
.fixture_copula2stage_formula_edge_cases <- function(data.X.P) {
  stopifnot(all(c("y", "X", "P") %in% colnames(data.X.P)))

  df <- data.X.P
  df$X_pos <- abs(df$X) + 1
  df$P_pos <- abs(df$P) + 1
  df$`P 1` <- df$P
  df$`X 1` <- df$X

  return(list(
    "plain" = list(formula = y ~ X + P | P, data = df),
    # dot: only on data without additional cols
    "dot in rhs1" = list(formula = y ~ . | P, data = data.X.P),
    "backticks in endo" = list(formula = y ~ `P 1` + X | `P 1`, data = df),
    "backticks in exo" = list(formula = y ~ P + `X 1` | P, data = df),
    "backticks in endo & exo" = list(formula = y ~ `P 1` + `X 1` | `P 1`, data = df),
    "transformed exo" = list(formula = y ~ log(X_pos) + P | P, data = df),
    "transformed endo" = list(formula = y ~ log(P_pos) + X | log(P_pos), data = df),
    "inline function exo" = list(formula = y ~ I(X^2) + P | P, data = df),
    "duplicate regressor" = list(formula = y ~ X + X + P | P, data = df)
  ))
}


# copulaIMA formula edge cases ---------------------------------------------------------
fixture_ima_formula_edge_cases <- function() {
  data("dataCopIMAContExo", "dataCopIMAMultiEndo", envir = environment())

  df.multi <- dataCopIMAMultiEndo
  df.multi$`P 1` <- df.multi$P1
  df.multi$`P 2` <- df.multi$P2
  df.multi$`X 1` <- df.multi$X

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCopIMAContExo),
    list(
      "multiple endo with backtick" = list(
        formula = y ~ `P 1` + `P 2` + `X 1` | `P 1` + `P 2`,
        data = df.multi
      )
    )
  ))
}

# copula2sCOPE formula edge cases ------------------------------------------------------
fixture_2scope_formula_edge_cases <- function() {
  data("dataCopula2sCOPECase1", envir = environment())

  df <- dataCopula2sCOPECase1
  df$`P 1` <- df$P
  df$P2 <- df$P * df$X # not collinear or P
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCopula2sCOPECase1),
    list(
      "factor exo" = list(formula = y ~ X + x_fac + P | P, data = df),
      "multiple endo with backtick" = list(
        formula = y ~ X + `P 1` + P2 | `P 1` + P2,
        data = df
      ),
      "no exo, single endo" = list(formula = y ~ P | P, data = df),
      "no exo, multiple endo" = list(formula = y ~ P + X | P + X, data = df)
    )
  ))
}


# copulaBMW formula edge cases ---------------------------------------------------------
# - min 1 exo required
# - exo: any class, interactions allowed
fixture_bmw_formula_edge_cases <- function() {
  data("dataCopBMW", "dataCopBMWMultiEndo", envir = environment())

  df <- dataCopBMW
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))
  df$x_char <- rep(c("a", "b", "c"), length.out = nrow(df))
  df$x_bin <- rep(0:1, length.out = nrow(df))

  df.multi <- dataCopBMWMultiEndo
  df.multi$`P 1` <- df.multi$P1

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCopBMW),
    list(
      "factor exo as only" = list(formula = y ~ x_fac + P | P, data = df),
      "factor exo with cont" = list(formula = y ~ X + x_fac + P | P, data = df),
      "character exo" = list(formula = y ~ X + x_char + P | P, data = df),
      "binary exo" = list(formula = y ~ x_bin + P | P, data = df),
      # interactions are allowed: only among exogenous regressors
      "interaction exo" = list(formula = y ~ X * x_bin + P | P, data = df),
      "multiple endo with backtick" = list(
        formula = y ~ X + `P 1` + P2 | `P 1` + P2,
        data = df.multi
      )
    )
  ))
}


# copula2sCOPEnp formula edge cases ----------------------------------------------------
# only continuous and factor data to run fast (ordered factors are tested separately)
fixture_2scopenp_formula_edge_cases <- function() {
  data("dataCopula2sCOPEnpCont", "dataCopula2sCOPEnpMulti", envir = environment())

  df <- dataCopula2sCOPEnpCont
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))

  df.multi <- dataCopula2sCOPEnpMulti
  df.multi$`P 1` <- df.multi$P1

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCopula2sCOPEnpCont),
    list(
      # I() gives class "AsIs" in the model frame which np does not accept
      "inline function endo" = list(formula = y ~ X + I(P^2) | I(P^2), data = df),
      "inline function factor exo" = list(
        formula = y ~ X + I(x_fac) + P | P,
        data = df
      ),
      "factor exo" = list(formula = y ~ X + x_fac + P | P, data = df),
      "multiple endo with backtick" = list(
        # continuous variables only: X1, X2, P1 to run faster
        formula = y ~ X1 + X2 + `P 1` | X2 + `P 1`,
        data = df.multi
      )
    )
  ))
}
