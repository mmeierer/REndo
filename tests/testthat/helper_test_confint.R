.test.s3methods.confint <- function(res.model){
  .test.confint(res.model = res.model, names.coefs = names(coef(res.model)))
}

.test.s3methods.confint.multilevel <- function(res.model){
  .test.confint(res.model = res.model, names.coefs = rownames(coef(res.model)))
}

# Verifies that confint exhibits the exact same functionality as confint.lm
.test.confint <- function(res.model, names.coefs){

  test_that("Confint works with different alphas", {
    expect_silent(ci.99 <- confint(res.model, level = 0.99))
    expect_silent(ci.95 <- confint(res.model, level = 0.95))
    expect_silent(ci.90 <- confint(res.model, level = 0.90))
    expect_silent(ci.70 <- confint(res.model, level = 0.70))

    # Level works and provides different values
    expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=FALSE)))
    expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=FALSE)))
    expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=FALSE)))

    # Rightly named, all same
    expect_equal(rownames(ci.99), names.coefs)
    expect_equal(rownames(ci.99), rownames(ci.95))
    expect_equal(rownames(ci.95), rownames(ci.90))
    expect_equal(rownames(ci.90), rownames(ci.70))

    # Also title label correct
    expect_equal(colnames(ci.95), c("2.5 %", "97.5 %"))
    expect_equal(colnames(ci.90), c("5 %", "95 %"))
    expect_equal(colnames(ci.99), c("0.5 %", "99.5 %"))
  })

  test_that("Confint works with character param", {
    # Single
    for(p in names.coefs)
      expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # Multiple
    p <- names.coefs[1:3]
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    p <- names.coefs[1:2]
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # All - excplicitely
    p <- names.coefs
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # All - implicitely (ie none given)
    expect_equal(rownames(confint(res.model)), expected = p)
  })

  test_that("Confint works with integer param", {
    p <- names.coefs
    # Single
    expect_equal(rownames(confint(res.model, parm = 2)), expected = p[2])
    expect_equal(rownames(confint(res.model, parm = 4)), expected = p[4])
    # Sequence
    expect_equal(rownames(confint(res.model, parm = 1:3)), expected = p[1:3])
    expect_equal(rownames(confint(res.model, parm =c(1,2,4))), expected = p[c(1,2,4)])
    # All - excplicitely
    expect_equal(rownames(confint(res.model, parm = seq(length(p)))), expected = p)
    # All - implicitely (ie none given)
    expect_equal(rownames(confint(res.model)), expected = p)

    # Minus removes
    expect_equal(rownames(confint(res.model, parm = -2)), expected = p[-2])
    expect_equal(rownames(confint(res.model, parm = -c(2,4))), expected = p[-c(2,4)])
    # Remove all
    expect_null(rownames(confint(res.model, parm = -seq(length(p)))))
  })
  # same behavior as lm
  # test_that("confint NA if unknown parm", {
  #   # Unknown character
  #   expect_true(all(is.na( confint(res.model, parm = "abc") )))
  #   expect_true(all(is.na( confint(res.model, parm = c("abc", "zcgd")) )))
  #   # Wrong indices
  #   expect_true(all(is.na( confint(res.model, parm = 50:100))))
  #   expect_true(all(is.na( confint(res.model, parm = 99) )))
  #   # Part of it are known
  #   expect_true(all(names.coefs %in%
  #                     rownames(confint(res.model, parm = 1:100))))
  #   expect_true(!all(is.na( confint(res.model, parm = 1:100))))
  # })

}

