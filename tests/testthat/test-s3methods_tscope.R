# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataTscope")

context("S3methods - tscope")

# Helper for summary/print/diagnostics is auto-loaded from `helper-s3methods_tscope.R`

# Basic tscope fit for S3 method tests
ts_fit <- tscope(y ~ p + w | p, data = dataTscope, num.boots = 1, verbose = FALSE)
fct.helper.check.tscope.summary(ts_fit)

# Test with multiple endogenous variables
data_multi <- dataTscope
set.seed(123)
data_multi$p2 <- data_multi$p + rnorm(nrow(data_multi), 0, 0.5)
ts_fit_multi <- tscope(y ~ p + w + p2 | p + p2, data = data_multi, num.boots = 1, verbose = FALSE)
fct.helper.check.tscope.summary(ts_fit_multi)
