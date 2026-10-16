# dont skip on cran: These are important tests! (and fast)
set.seed(42)

# checkinput_copulashared_data_basics ---------------------------------------
test_that("_data_basics rejects invalid data", {
  df <- fixture_copula_df()

  check_errmsg(
    checkinput_copulashared_data_basics,
    cases = list(
      "NULL" = list(data = NULL),
      "not a df" = list(data = list(1, 2)),
      "zero rows" = list(data = df[0, ]),
      "zero cols" = list(data = df[, 0])
    ),
    regexp = c(
      "must be a data.frame",
      "must be a data.frame",
      "at least one row",
      "at least one column"
    )
  )

  # row + col messages
  expect_length(checkinput_copulashared_data_basics(data = df[0, 0]), 2)
})

test_that("data_basics accepts valid data", {
  # the data.frame itself is valid
  expect_null(checkinput_copulashared_data_basics(data = fixture_copula_df()))
})


# checkinput_copulashared_formula_basics --------------------------------------------
test_that("_formula_basics rejects invalid formulas", {
  check_errmsg(
    fn = checkinput_copulashared_formula_basics,
    cases = list(
      "not a formula" = list(formula = "y ~ x | z"),
      "NULL" = list(formula = NULL),
      "number" = list(formula = 1),
      "one part" = list(formula = y ~ x),
      "more than 2" = list(formula = y ~ x | z | w),
      "multiple responses" = list(formula = y1 + y2 ~ x | z),
      "no response" = list(formula = ~ x | z)
    ),
    regexp = c(
      "must be a formula",
      "must be a formula",
      "must be a formula",
      "two right",
      "two right",
      "exactly one response",
      "one left-hand side"
    )
  )
})

test_that("_formula_basics accepts valid formulas incl all kinds of edge cases", {
  # accepts normal formula, does not require Formula!
  cases <- list(
    "normal " = list(formula = y ~ x | z),
    "with dot" = list(formula = y ~ . | z),
    "backticks" = list(formula = `y space` ~ `x 1` | `z endo`),
    "transformations" = list(formula = log(y) ~ log(x) | exp(z)),
    "inline function" = list(formula = y ~ I(x^2) | z),
    "multi term" = list(formula = y ~ x1 + x2 | z1 + z2)
  )
  check_errmsg_empty(fn = checkinput_copulashared_formula_basics, cases = cases)
})
# checkinput_copulashared_vars_in_data -------------------------------------------
test_that("_vars_in_data reports missing variables", {
  check_errmsg(
    fn = checkinput_copulashared_vars_in_data,
    base.args = list(data = fixture_copula_df()),
    cases = list(
      "missing in rhs1" = list(F.formula = as.Formula(y ~ mis | x_num)),
      "missing in rhs2" = list(F.formula = as.Formula(y ~ x_num | mis)),
      "missing in response" = list(F.formula = as.Formula(mis ~ x_num | x_num)),
      "two missing listed" = list(F.formula = as.Formula(y ~ gone1 | gone2))
    ),
    regexp = c(
      "mis",
      "mis",
      "mis",
      "gone1.*gone2"
    )
  )
})
test_that("_vars_in_data works with valid, non-syntactic, dot", {
  df <- fixture_copula_df()
  check_errmsg_empty(
    fn = checkinput_copulashared_vars_in_data,
    base.args = list(data = df),
    cases = list(
      "non syntactic" = list(F.formula = as.Formula(y ~ `x space` | x_num)),
      "normal" = list(F.formula = as.Formula(y ~ x_num | x_fac)),
      "dot" = list(F.formula = as.Formula(y ~ . | x_num))
    )
  )
})

# checkinput_copulashared_response_not_in_rhs ------------------------------------------
test_that("_response_not_in_rhs fails if response in rhs1", {
  expect_match(
    checkinput_copulashared_response_not_in_rhs(
      F.formula = as.Formula(y ~ y + x_num),
      rhs1.terms = terms(
        as.Formula(y ~ y + x_num),
        lhs = 0,
        rhs = 1,
        data = fixture_copula_df()
      )
    ),
    regexp = "also appear"
  )

  # with transformation
  expect_match(
    checkinput_copulashared_response_not_in_rhs(
      F.formula = as.Formula(y ~ exp(y) + x_num),
      rhs1.terms = terms(
        as.Formula(y ~ log(y)),
        lhs = 0,
        rhs = 1,
        data = fixture_copula_df()
      )
    ),
    regexp = "also appear"
  )
})

test_that("_response_not_in_rhs works when response not in rhs", {
  F.formula <- as.Formula(y ~ x_num + x_fac | x_num)
  rhs.terms <- terms(F.formula, lhs = 0, rhs = 1, data = fixture_copula_df())
  expect_null(
    checkinput_copulashared_response_not_in_rhs(
      F.formula = F.formula,
      rhs1.terms = rhs.terms
    )
  )
})


# label_to_colname -------------------------------------------------------------------
test_that("label_to_colname normalizes labels to col names", {
  # plain
  expect_equal(label_to_colname(lab = "x_num"), "x_num")
  # backticks (space)
  expect_equal(label_to_colname(lab = "`x space`"), "x space")
  # cannot have space in label (requires backticks)
  # expect_equal(label_to_colname(lab = "x space"), "x space")
  # transformation
  expect_equal(label_to_colname(lab = "log(P)"), "log(P)")
})


# checkinput_copulashared_modelframe ---------------------------------------------------
test_that("_modelframe fails on NAs, illegal classes", {
  # NAs from transformation (also produces warning)
  expect_match(
    expect_warning(checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ log(x_num - 1000)),
      data = fixture_copula_df(),
      allowed.classes = list("log(x_num - 1000)" = "numeric"),
      labels.warn.low.card = NULL,
      forbid.constant = FALSE
    )),
    regexp = "missing values"
  )

  df.na.response <- fixture_copula_df()
  df.na.response$y[1] <- NA

  check_errmsg(
    fn = checkinput_copulashared_modelframe,
    base.args = list(data = fixture_copula_df()),
    cases = list(
      "NA already in data" = list(
        F.formula = as.Formula(y ~ x_na),
        allowed.classes = list(x_na = "numeric"),
        labels.warn.low.card = NULL,
        forbid.constant = FALSE
      ),
      "NA in response" = list(
        F.formula = as.Formula(y ~ x_num),
        data = df.na.response,
        allowed.classes = list(x_num = "numeric"),
        labels.warn.low.card = NULL
      ),
      "given wrong class" = list(
        F.formula = as.Formula(y ~ x_fac),
        allowed.classes = list(x_fac = "numeric"),
        labels.warn.low.card = NULL,
        forbid.constant = FALSE
      )
    ),
    regexp = c(
      "missing values",
      "missing values",
      "has class"
    )
  )
})

test_that("_modelframe warns for low-cardinality numerics", {
  df <- fixture_copula_df()

  # warns for given labels
  expect_warning(
    checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ x_lowcard + x_num),
      data = df,
      allowed.classes = list(x_lowcard = "numeric", x_num = "numeric"),
      labels.warn.low.card = "x_lowcard",
      forbid.constant = FALSE
    ),
    regexp = "low cardinality"
  )

  # dont warn if col is not numeric (factors are low card)
  expect_no_warning(
    checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ x_fac),
      data = df,
      allowed.classes = list(x_fac = "factor"),
      labels.warn.low.card = "x_fac",
      forbid.constant = FALSE
    )
  )

  # dont warn if another column is low cardinality (not all columns in mf!)
  expect_no_warning(
    checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ x_lowcard + x_num),
      data = df,
      allowed.classes = list(x_lowcard = "numeric", x_num = "numeric"),
      labels.warn.low.card = "x_num",
      forbid.constant = FALSE
    )
  )

  # works if no low card cols are specified
  expect_no_warning(
    checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ x_lowcard),
      data = df,
      allowed.classes = list(x_lowcard = "numeric"),
      labels.warn.low.card = NULL,
      forbid.constant = FALSE
    )
  )
})

test_that("_modelframe fails on constant cols", {
  df <- fixture_copula_df()
  df$x_const <- 1
  base.args <- list(
    F.formula = as.Formula(y ~ x_num + x_const),
    data = df,
    allowed.classes = list(),
    labels.warn.low.card = NULL
  )

  check_errmsg(
    fn = checkinput_copulashared_modelframe,
    base.args = base.args,
    cases = list("forbidden" = list(forbid.constant = TRUE)),
    regexp = "must not be constant: x_const"
  )
  check_errmsg_empty(
    fn = checkinput_copulashared_modelframe,
    base.args = base.args,
    cases = list("allowed" = list(forbid.constant = FALSE))
  )
})

test_that("_modelframe works for valid data", {
  expect_null(
    checkinput_copulashared_modelframe(
      F.formula = as.Formula(y ~ x_num + x_fac + x_ord + `x space` + exp(x_num)),
      data = fixture_copula_df(),
      allowed.classes = list(
        x_num = "numeric",
        "exp(x_num)" = "numeric",
        "`x space`" = "numeric",
        x_fac = "factor",
        x_ord = "ordered"
      ),
      labels.warn.low.card = NULL,
      forbid.constant = FALSE
    )
  )
})


# checkinput_copulashared_formula_data -----------------------------------------
# check only its intrinsic parts + that building blocks are wired in

formuladata_base_args <- function() {
  df <- fixture_copula_df()
  # to be able to do log()
  df$x_pos <- abs(df$x_num) + 1

  return(list(
    data = df,
    min.num.exo = 0,
    allow.interactions = TRUE,
    allow.constant.regressors = TRUE,
    allowed.classes.endo = NULL,
    allowed.classes.exo = NULL
  ))
}

test_that("_formula_data has building blocks wired in", {
  check_errmsg(
    fn = checkinput_copulashared_formula_data,
    base.args = formuladata_base_args(),
    cases = list(
      "formula_basics" = list(formula = "y ~ x_num | x_num"),
      "vars_in_data" = list(formula = y ~ x_num + mis | x_num),
      "response_not_in_rhs" = list(formula = y ~ y + x_num | x_num),
      "modelframe" = list(formula = y ~ x_na + x_num | x_num)
    ),
    regexp = c(
      "must be a formula",
      "not found",
      "also appear",
      "missing values"
    )
  )
})

test_that("_formula_data rules hardcoded for all methods", {
  base.args <- formuladata_base_args()

  check_errmsg(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "continuous() special" = list(formula = y ~ x_num + x_fac | continuous(x_num)),
      "discrete() special" = list(formula = y ~ x_num + x_fac | discrete(x_fac)),
      "no endo" = list(formula = y ~ x_num | 1),
      "endo not in rhs1" = list(formula = y ~ x_num | x_fac),
      "transformed endo not in rhs1" = list(formula = y ~ x_num + log(x_pos) | x_pos),
      "empty special" = list(formula = y ~ x_num | continuous()),
      "response as endo" = list(formula = y ~ x_num | y),
      "misspelled special" = list(formula = y ~ x_num + x_fac | continous(x_num)),
      "rhs1 and rhs2 swapped" = list(formula = y ~ continuous(x_num) | x_num + x_fac)
    ),
    regexp = c(
      "discrete\\(\\)/continuous\\(\\)",
      "discrete\\(\\)/continuous\\(\\)",
      "At least one endogenous",
      "not in structural model",
      "not in structural model",
      "discrete\\(\\)/continuous\\(\\)",
      "not in structural model",
      # misspelled is not a special: plain term that is not in rhs1
      "not in structural model",
      # stops before building the model frame: endo not in rhs1
      "not in structural model"
    )
  )

  # low cardinality: always warn for endo
  args.lowcard.endo <- c(list(formula = y ~ x_num + x_lowcard | x_lowcard), base.args)
  expect_null(expect_warning(
    do.call(checkinput_copulashared_formula_data, args.lowcard.endo),
    regexp = "low cardinality"
  ))

  # but not for exo
  args.lowcard.exo <- c(list(formula = y ~ x_num + x_lowcard | x_num), base.args)
  expect_no_warning(do.call(checkinput_copulashared_formula_data, args.lowcard.exo))
})

test_that("_formula_data parameters work correctly", {
  base.args <- formuladata_base_args()

  # min.num.exo
  check_errmsg(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "no exo, min 1" = list(formula = y ~ x_num | x_num, min.num.exo = 1),
      "one exo, min 2" = list(formula = y ~ x_num + x_fac | x_num, min.num.exo = 2)
    ),
    regexp = "exogenous regressor"
  )
  check_errmsg_empty(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "no exo, min 0" = list(formula = y ~ x_num | x_num, min.num.exo = 0),
      "one exo, min 1" = list(formula = y ~ x_num + x_fac | x_num, min.num.exo = 1)
    )
  )

  # allow.interactions
  check_errmsg(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "interaction in rhs1" = list(
        formula = y ~ x_num * x_fac | x_num,
        allow.interactions = FALSE
      ),
      "interaction only in rhs2" = list(
        formula = y ~ x_num + x_fac | x_num:x_fac,
        allow.interactions = FALSE
      )
    ),
    regexp = "Interaction terms"
  )
  check_errmsg_empty(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "interaction allowed" = list(
        formula = y ~ x_num * x_fac | x_num,
        allow.interactions = TRUE
      )
    )
  )

  # allowed.classes.endo / allowed.classes.exo
  check_errmsg(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "ordered endo, restricted" = list(
        formula = y ~ x_num + x_ord | x_ord,
        allowed.classes.endo = "numeric"
      ),
      "ordered exo, restricted" = list(
        formula = y ~ x_num + x_ord | x_num,
        allowed.classes.exo = c("numeric", "factor")
      )
    ),
    regexp = "has class"
  )
  check_errmsg_empty(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "ordered endo, any class" = list(
        formula = y ~ x_num + x_ord | x_ord,
        allowed.classes.endo = NULL
      ),
      "ordered exo, any class" = list(
        formula = y ~ x_num + x_ord | x_num,
        allowed.classes.exo = NULL
      ),
      # exo restriction does not apply to endo and vice versa
      "ordered endo, only exo restricted" = list(
        formula = y ~ x_num + x_ord | x_ord,
        allowed.classes.exo = "numeric"
      ),
      "ordered exo, only endo restricted" = list(
        formula = y ~ x_num + x_ord | x_num,
        allowed.classes.endo = "numeric"
      )
    )
  )

  # msg.* returned as-is
  args.msg.exo <- modifyList(
    base.args,
    list(formula = y ~ x_num | x_num, min.num.exo = 1, msg.min.num.exo = "MSG EXO")
  )
  expect_identical(
    do.call(checkinput_copulashared_formula_data, args.msg.exo),
    "MSG EXO"
  )

  args.msg.inter <- modifyList(
    base.args,
    list(
      formula = y ~ x_num * x_fac | x_num,
      allow.interactions = FALSE,
      msg.interactions = "MSG INTERACTIONS"
    )
  )
  expect_identical(
    do.call(checkinput_copulashared_formula_data, args.msg.inter),
    "MSG INTERACTIONS"
  )
})

test_that("_formula_data works with all kinds of formula edge cases", {
  base.args <- formuladata_base_args()

  check_errmsg_empty(
    fn = checkinput_copulashared_formula_data,
    base.args = base.args,
    cases = list(
      "plain" = list(formula = y ~ x_num + x_fac | x_num),
      # limit data to cols that are valid to test dot
      "dot in rhs1" = list(
        formula = y ~ . | x_num,
        data = base.args$data[, c("y", "x_num", "x_fac", "x_ord")]
      ),
      "backticks in exo" = list(formula = y ~ x_num + `x space` | x_num),
      "backticks in endo" = list(formula = y ~ `x space` + x_num | `x space`),
      "transformed response" = list(formula = exp(y) ~ x_num + x_fac | x_num),
      "transformed exo" = list(formula = y ~ log(x_pos) + x_num | x_num),
      "transformed endo" = list(formula = y ~ x_num + log(x_pos) | log(x_pos)),
      "inline function" = list(formula = y ~ x_num + I(x_pos^2) | x_num),
      "no intercept" = list(formula = y ~ x_num + x_fac - 1 | x_num),
      "duplicate regressor" = list(formula = y ~ x_num + x_num + x_fac | x_num),
      "multiple endo" = list(
        formula = y ~ x_num + `x space` + x_fac | x_num + `x space`
      ),
      "character exo" = list(formula = y ~ x_num + x_string | x_num)
    )
  )
})


# checkinput_copulashared_cdf -------------------------------------------------------
test_that("_cdf rejects invalid cdf", {
  check_errmsg(
    fn = checkinput_copulashared_cdf,
    base.args = list(allowed.cdf = c("adj.ecdf", "resc.ecdf", "ecdf", "kde")),
    cases = list(
      "invalid string" = list(cdf = "invalid"),
      "case mismatch" = list(cdf = "ADJ.ECDF"),
      "NULL" = list(cdf = NULL),
      "not character" = list(cdf = 1),
      "non-default vector" = list(cdf = c("ecdf", "kde"))
    ),
    regexp = c(
      "is invalid",
      "is invalid",
      "single character string",
      "single character string",
      "single character string"
    )
  )
})

test_that("_cdf accepts valid cdf incl full default vector", {
  allowed <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")
  check_errmsg_empty(
    fn = checkinput_copulashared_cdf,
    base.args = list(allowed.cdf = allowed),
    cases = list(
      "single allowed" = list(cdf = "ecdf"),
      # default value of the param is the full vector (match.arg() picks first)
      "full default vector" = list(cdf = allowed)
    )
  )
})


# checkinput_copulashared_numboots --------------------------------------------------
test_that("_numboots rejects invalid num.boots", {
  check_errmsg(
    fn = checkinput_copulashared_numboots,
    cases = list(
      "NULL" = list(num.boots = NULL),
      "NA" = list(num.boots = NA_real_),
      "non-numeric" = list(num.boots = "abc"),
      "logical" = list(num.boots = TRUE),
      "multiple" = list(num.boots = c(1000, 2000)),
      "zero" = list(num.boots = 0),
      "negative" = list(num.boots = -1),
      "one (below min of 2)" = list(num.boots = 1),
      "fractional" = list(num.boots = 1000.5)
    ),
    regexp = c(
      "provide a parameter",
      "no NA",
      "numeric parameter",
      "numeric parameter",
      "exactly one single number",
      "greater than",
      "greater than",
      "greater than",
      "whole number"
    )
  )
})

test_that("_numboots accepts valid num.boots, warns if < 1000", {
  expect_null(expect_no_warning(checkinput_copulashared_numboots(num.boots = 1000)))
  expect_null(expect_warning(
    checkinput_copulashared_numboots(num.boots = 2),
    regexp = "recommended to run 1000"
  ))
})


# checkinput_copulashared_verbose ---------------------------------------------------
test_that("_verbose rejects invalid verbose", {
  check_errmsg(
    fn = checkinput_copulashared_verbose,
    cases = list(
      "NULL" = list(verbose = NULL),
      "numeric" = list(verbose = 1),
      "character" = list(verbose = "TRUE"),
      "multiple" = list(verbose = c(TRUE, FALSE)),
      "NA" = list(verbose = NA)
    ),
    regexp = c(
      "single logical",
      "single logical",
      "single logical",
      "single logical",
      "no NA"
    )
  )
})

test_that("_verbose accepts TRUE and FALSE", {
  check_errmsg_empty(
    fn = checkinput_copulashared_verbose,
    cases = list("TRUE" = list(verbose = TRUE), "FALSE" = list(verbose = FALSE))
  )
})
