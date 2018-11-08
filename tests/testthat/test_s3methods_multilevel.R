# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")
f.multilevel <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID)

context("S3methods - multilevelIV - S3methods")
# ** TODO: add expect_silent
res.ml <- multilevelIV(formula = f.multilevel, name.endo = "X15", data = dataMultilevelIV, verbose = FALSE)

test_that("Works for every model",{
  expect_silent(summary(res.ml, model = "REF"))
  expect_silent(summary(res.ml, model = "FE_L2"))
  expect_silent(summary(res.ml, model = "FE_L3"))
  expect_silent(summary(res.ml, model = "GMM_L2"))
  expect_silent(summary(res.ml, model = "GMM_L3"))
})

test_that("Has default argument REF",{
  default.arg <- eval(formals(REndo:::summary.rendo.multilevel)[["model"]])
  expect_equal(default.arg, "REF")
})
