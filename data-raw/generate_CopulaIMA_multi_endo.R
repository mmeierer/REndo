## Code to prepare `dataCopIMAMultiEndo` dataset

## Generate dataCopIMAMultiEndo - two continuous endogenous regressors correlated with each other and with one exogenous regressor

set.seed(123)

n <- 1000

# Extension of Haschka (2025) Section 4.1 to the multiple endogenous regressor case
# This dataset is designed to exercise the multi-endogenous-regressor code path
# in copulaIMA()

# True parameters
# No intercept, following eq. 4.1 of Haschka (2025)
alpha1 <- 1   # coefficient on endo regressor P1
alpha2 <- 1   # coefficient on endo regressor P2
beta   <- 1

# Latent Gaussian dependence structure
# 4 x 4 matrix, extension of eq. 4.2
# Corr(P1*, P2*) = 0.3 (correlation between the 2 endo regressors)
# Corr(P1*, X*)  = 0.5  (P1 correlated with exogenous regressor same as in Haschka)
# Corr(P2*, X*)  = 0.5   (P2 correlated with exogenous regressor)
# Corr(P1*, xi*) = 0.5  (endogeneity of P1. Same as Haschka's rho = 0.5)
# Corr(P2*, xi*) = 0.5  (endogeneity of P2)
# Corr(X*,  xi*) = 0    X is exogenous therefore uncorrelated with error

Sigma <- matrix(c(1,   0.3, 0.5, 0.5,
                  0.3, 1,   0.5, 0.5,
                  0.5, 0.5, 1,   0,
                  0.5, 0.5, 0,   1),
                nrow = 4, ncol = 4)

latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0, 0), Sigma = Sigma)

P1star <- latent[, 1]
P2star <- latent[, 2]
Xstar  <- latent[, 3]
xistar <- latent[, 4]

# Marginal transformations
# P1 and P2: nonnormal bounded endogenous regressors
# P_t = Phi(P*_t) + 0.5, same transformation as in Haschka (2025) after eq. 4.2
# Values in (0.5, 1.5), ensuring nonnormality for identification
P1 <- pnorm(P1star) + 0.5
P2 <- pnorm(P2star) + 0.5

# X: continuous exogenous regressor, N(1,1)
# Same transformation as Scenario 1 (eq. 4.3) of Haschka (2025)
X  <- Xstar + 1

xi <- xistar #normal

# Outcome equation extension from eq. 4.1 to two endogenous regressors
# Y_t = alpha1 * P1_t + alpha2 * P2_t + beta * X_t + xi_t
# No intercept, following Haschka (2025) Section 4.1
y <- alpha1 * P1 + alpha2 * P2 + beta * X + xi

# Final dataset
dataCopIMAMultiEndo <- data.frame(y  = y, X  = X, P1 = P1, P2 = P2)

usethis::use_data(dataCopIMAMultiEndo, overwrite = TRUE)
