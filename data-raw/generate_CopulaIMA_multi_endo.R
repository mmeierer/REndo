## Code to prepare `dataCopIMAMultiEndo` dataset

## Generate dataCopIMAMultiEndo - two continuous endogenous regressors correlated with each other and with one exogenous regressor

set.seed(123)

n <- 1000

# Extension of Haschka (2025) Section 4.1 to the multiple endogenous regressor case
# This dataset is designed to exercise the multi-endogenous-regressor code path
# in copulaIMA()

# True parameters
# No exogenous regressors but 1 intercept and 2 endogenous regressors
alpha1 <- 1   # coefficient on endo regressor P1
alpha2 <- 1   # coefficient on endo regressor P2
mu <- 10 #true intercept

# Latent Gaussian dependence structure
# 4 x 4 matrix, extension of eq. 4.2
# Corr(P1*, P2*) = 0.3 (correlation between the 2 endo regressors)
# Corr(P1*, xi*) = 0.5  (endogeneity of P1. Same as Haschka's rho = 0.5)
# Corr(P2*, xi*) = 0.5  (endogeneity of P2)

Sigma <- matrix(c(1,   0.3, 0.5,
                  0.3, 1,   0.5,
                  0.5, 0.5, 1),
                nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)

P1star <- latent[, 1]
P2star <- latent[, 2]
xistar <- latent[, 3]

# Marginal transformations
# P1 and P2: nonnormal bounded endogenous regressors
# P_t = Phi(P*_t) + 0.5, same transformation as in Haschka (2025) after eq. 4.2
# Values in (0.5, 1.5), ensuring nonnormality for identification
P1 <- pnorm(P1star) + 0.5
P2 <- pnorm(P2star) + 0.5

xi <- xistar #normal

# Outcome equation extension from eq. 4.1 to two endogenous regressors with intercept
# Y_t = mu + alpha1 * P1_t + alpha2 * P2_t + xi_t
y <- mu + alpha1 * P1 + alpha2 * P2 + xi

# Final dataset
dataCopIMAMultiEndo <- data.frame(y  = y, P1 = P1, P2 = P2)

usethis::use_data(dataCopIMAMultiEndo, overwrite = TRUE)
