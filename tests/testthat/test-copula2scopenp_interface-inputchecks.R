# Method-level parameter verification at the public interface
# the input checkers are unit-tested separately. Only check that they are being used

skip_on_cran()
set.seed(42)
data("dataCopula2sCOPEnpCont")


test_that("interface has methods wired in", {
  df.na <- dataCopula2sCOPEnpCont
  df.na$P[1] <- NA

  cases <- list(
    "formula_data" = list(formula = y ~ P + X),
    "data_basics" = list(data = NULL),
    "modelframe" = list(data = df.na),
    "npcdistbwargs" = list(npcdistbw.args = list(xdat = 1)),
    "bws" = list(bws = list(P = 1)),
    # variables differ from the model frame columns (labels.exo passed)
    "bws (variables)" = list(
      bws = list(P = structure(
        list(method = "normal-reference", ynames = "P", xnames = "Z"),
        class = "condbandwidth"
      ))
    ),
    # conflicts with bwmethod in base.args
    "bws_npcdistbwargs" = list(
      bws = list(P = structure(list(method = "cv.ls"), class = "condbandwidth"))
    ),
    "num.boots" = list(num.boots = -1),
    "verbose" = list(verbose = NULL)
  )

  base.args <- list(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont,
    npcdistbw.args = list(bwmethod = "normal-reference"),
    bws = NULL,
    num.boots = 1000,
    verbose = FALSE
  )

  run_cases_error(
    fn = copula2sCOPEnp,
    base.args = base.args,
    cases = cases,
    regexp = "The above errors"
  )
})
