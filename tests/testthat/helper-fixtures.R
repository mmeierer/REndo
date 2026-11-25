# Fixtures used in tests

# copula methods  ----------------------------------------------------------------------
# Contains cols that are broken on purpose (x_na, x_inf, x_const, x_unused): a formula
# with a dot on the whole fixture therefore needs a col subset
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
    # deterministic: no draws to not shift the random number stream
    x_inf = c(Inf, seq_len(n - 1)),
    x_const = rep(1, n),
    x_unused = factor(rep(c("a", "b"), length.out = n), levels = c("a", "b", "c")),
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
  data("dataCop2sCOPECase1", envir = environment())

  df <- dataCop2sCOPECase1
  df$`P 1` <- df$P
  df$P2 <- df$P * df$X # not collinear or P
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCop2sCOPECase1),
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
  data("dataCop2sCOPEnpCont", "dataCop2sCOPEnpMulti", envir = environment())

  df <- dataCop2sCOPEnpCont
  df$x_fac <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))

  df.multi <- dataCop2sCOPEnpMulti
  df.multi$`P 1` <- df.multi$P1

  return(c(
    .fixture_copula2stage_formula_edge_cases(dataCop2sCOPEnpCont),
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


# copulaJAMS formula edge cases --------------------------------------------------------
# - formula has 3 or 4 parts: y ~ model | P | W | Z
# Returns named list of list(formula, data), as used by run_cases()
fixture_jams_formula_edge_cases <- function() {
  data("dataCopJAMS", envir = environment())

  df <- dataCopJAMS
  # non-syntactic names
  df$`P 1` <- df$P1
  df$`W 1` <- df$W
  # second discrete exo
  df$Z2 <- factor(rep(c("a", "b", "c"), length.out = nrow(df)))
  # Z as numeric and not factor
  df$Z_num <- as.numeric(as.character(df$Z))
  # ordered factor
  df$Z_ord <- ordered(df$Z2, levels = c("a", "b", "c"))

  return(list(
    "no W" = list(formula = y ~ P1 + Z | P1 | 0 | Z, data = df),
    "no Z" = list(formula = y ~ P1 + W | P1 | W, data = df),
    "W and Z are 0" = list(formula = y ~ P1 | P1 | 0 | 0, data = df),
    # dot: only on data without additional cols
    "dot in rhs1" = list(formula = y ~ . | P1 + P2 | W | Z, data = dataCopJAMS),
    "transformed in rhs1, plain in rhs2" = list(
      formula = y ~ log(P1) + W | P1 | W,
      data = df
    ),
    "only in interaction in rhs1" = list(formula = y ~ P1 + P1:W | P1 | W, data = df),
    "factor() in rhs4" = list(
      formula = y ~ P1 + W + Z_num | P1 | W | factor(Z_num),
      data = df
    ),
    "multiple strata" = list(
      formula = y ~ P1 + W + Z + Z2 | P1 | W | Z + Z2,
      data = df
    ),
    # Z2 is only in the structural model
    "plain control" = list(formula = y ~ P1 + W + Z + Z2 | P1 | W | Z, data = df),
    "backticks" = list(
      formula = y ~ `P 1` + `W 1` + Z | `P 1` | `W 1` | Z,
      data = df
    ),
    # intercepts are not used in parts 2-4
    "intercept notation in rhs2-4" = list(
      formula = y ~ P1 + W + Z | P1 - 1 | W + 0 | Z + 0,
      data = df
    ),
    "no intercept in rhs1" = list(formula = y ~ P1 + W - 1 | P1 | W, data = df),
    "interactions of endo and strata in rhs1" = list(
      formula = y ~ P1 * P2 + P2:Z + W + Z | P1 + P2 | W | Z,
      data = df
    ),
    "power of endo in rhs1" = list(
      formula = y ~ P1 + I(P1^2) + W | P1 | W,
      data = df
    ),
    "transformed in rhs2 and rhs3" = list(
      formula = y ~ log(P1) + log(W) | log(P1) | log(W),
      data = df
    ),
    "Z is 0" = list(formula = y ~ P1 + W | P1 | W | 0, data = df),
    "W is 0, no Z" = list(formula = y ~ P1 | P1 | 0, data = df),
    "ordered factor in rhs4" = list(
      formula = y ~ P1 + W + Z_ord | P1 | W | Z_ord,
      data = df
    )
  ))
}
