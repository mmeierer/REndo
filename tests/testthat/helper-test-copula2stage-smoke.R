# Smoke tests for the copula2stage methods
# - setup: list(method, fn, data, formula, label)

# Defaults -----------------------------------------------------------------------------
test_smoke_out_of_the_box <- function(setup) {
  test_that(paste0(setup$label, ": Works out of the box"), {
    # verbose = TRUE by default
    expect_message(res <- setup$method(formula = setup$formula, data = setup$data))
    check_clean_fit(res)
  })
}

test_smoke_verbose <- function(setup) {
  test_that(paste0(setup$label, ": verbose controls messages"), {
    expect_message(setup$fn(formula = setup$formula, data = setup$data, verbose = TRUE))
    expect_no_message(setup$fn(
      formula = setup$formula,
      data = setup$data,
      verbose = FALSE
    ))
  })
}

# Specs --------------------------------------------------------------------------------
# spec: named list of list(formula, data)
# cdf: taken from the methods signature
test_smoke_all_cdfs <- function(setup, specs) {
  test_that(paste0(setup$label, ": Works across specs and cdfs"), {
    for (cdf in eval(formals(setup$method)$cdf)) {
      run_cases(
        fn = setup$fn,
        base.args = list(cdf = cdf),
        cases = specs,
        check = check_clean_fit
      )
    }
  })
}

# Formula edge cases, incl print/summary
test_smoke_formula_edge_cases <- function(setup, cases) {
  test_that(
    paste0(setup$label, ": Works for formula edge cases, incl print & summary"),
    {
      results <- run_cases(fn = setup$fn, cases = cases, check = check_clean_fit)
      for (nm in names(results)) {
        check_print_summary(results[[nm]])
      }
    }
  )
}

# Data ---------------------------------------------------------------------------------
test_smoke_na_irrelevant_col <- function(setup) {
  test_that(paste0(setup$label, ": Works with NA in irrelevant column"), {
    df <- setup$data
    df$irrelevant <- rnorm(nrow(df))
    df$irrelevant[1] <- NA
    res <- setup$fn(formula = setup$formula, data = df)
    check_clean_fit(res)
  })
}

test_smoke_small_sample <- function(setup, n = 50) {
  test_that(paste0(setup$label, ": Works on small sample"), {
    res <- setup$fn(formula = setup$formula, data = setup$data[seq_len(n), ])
    check_clean_fit(res)
  })
}
