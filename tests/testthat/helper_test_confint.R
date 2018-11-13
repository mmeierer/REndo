.test.s3methods.confint <- function(res.model){

  test_that("Confint works with different alphas", {
    expect_silent(ci.99 <- confint(res.model, level = 0.99))
    expect_silent(ci.95 <- confint(res.model, level = 0.95))
    expect_silent(ci.90 <- confint(res.model, level = 0.90))
    expect_silent(ci.70 <- confint(res.model, level = 0.70))

    # Level works and provides different values
    expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=F)))
    expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=F)))
    expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=F)))

    # Rightly named, all same
    expect_equal(rownames(ci.99), names(coef(res.model)))
    expect_equal(rownames(ci.99), rownames(ci.95))
    expect_equal(rownames(ci.95), rownames(ci.90))
    expect_equal(rownames(ci.90), rownames(ci.70))

    # Also title label correct
    expect_equal(colnames(ci.95), c("2.5 %", "97.5 %"))
    expect_equal(colnames(ci.90), c("5 %", "95 %"))
    expect_equal(colnames(ci.99), c("0.5 %", "99.5 %"))

    # CI have to be larger for higher level (with very high probability)
    # expect_gt(ci.99[, 2] - ci.99[, 2], ) ***maybe do as well if time
  })

  test_that("Confint works with character param", {
    # Single
    for(p in names(coef(res.model)))
      expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # Multiple
    p <- names(coef(res.model))[1:3]
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    p <- names(coef(res.model))[1:2]
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # All - excplicitely
    p <- names(coef(res.model))
    expect_equal(rownames(confint(res.model, parm = p)), expected = p)
    # All - implicitely (ie none given)
    expect_equal(rownames(confint(res.model)), expected = p)
  })

  test_that("Confint works with integer param", {
    p <- names(coef(res.model))
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
  test_that("confint NA if unknown parm", {
    # Unknown character
    expect_true(all(is.na( confint(res.model, parm = "abc") )))
    expect_true(all(is.na( confint(res.model, parm = c("abc", "zcgd")) )))
    # Wrong indices
    expect_true(all(is.na( confint(res.model, parm = 50:100))))
    expect_true(all(is.na( confint(res.model, parm = 99) )))
    # Part of it are known
    expect_true(all(names(coef(res.model)) %in%
                      rownames(confint(res.model, parm = 1:100))))
    expect_true(!all(is.na( confint(res.model, parm = 1:100))))
  })

}
