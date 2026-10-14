## Code to prepare `dataCopIMAMultiEndo` dataset

## Generate dataCopIMAMultiEndo - two continuous endogenous regressors correlated with each other and with one exogenous regressor

set.seed(1234)

n <- 5000

# Extension of Haschka (2025) Section 4.1 to the multiple endogenous regressor case
# This dataset is designed to exercise the multi-endogenous-regressor code path
# in copulaIMA()

# True parameters
# 1 exogenous regressor, 1 intercept and 2 endo regressors
mu <- 10 #intercept
beta <- 1 #X coefficient
alpha1 <- 1  #coefficient on endo regressor P1
alpha2 <- -1   # coefficient on endo regressor P2


# Latent Gaussian dependence structure
# 4 x 4 matrix, extension of eq. 4.2
# Corr(P1*, P2*) = 0
# Corr(P1*, eps*) = 0.5  (endogeneity of P1. Same as Haschka's rho = 0.5)
# Corr(P2*, eps*) = 0.5  (endogeneity of P2)
# Corr(P1*, X*) = 0.5 P1 correlated with X
# Corr(P2*, X*) = 0.5 P2 correlated with X
# Corr (X*, eps*) = 0 X is exogenous (uncorrelated with error)

#(P1*, P2*, X*, eps*)
Sigma <- matrix(c(1, 0, 0.5,0.5,
                  0, 1, 0.5, 0.5,
                  0.5, 0.5, 1, 0,
                  0.5, 0.5, 0, 1),
                nrow = 4, ncol = 4, byrow = TRUE)

latent <- MASS::mvrnorm(n = n, mu = rep(0,4), Sigma = Sigma)

P1star <- latent[, 1]
P2star <- latent[, 2]
Xstar <- latent[, 3]
epsstar <- latent[, 4]

# Marginal transformations
# P1 and P2: nonnormal bounded endogenous regressors
# P_t = Phi(P*_t) + 0.5, same transformation as in Haschka (2025) after eq. 4.2
# Values in (0.5, 1.5), ensuring nonnormality for identification
P1 <- pnorm(P1star) + 0.5
P2 <- pnorm(P2star) + 0.5
X <- Xstar + 1 # X is cts
eps <- epsstar #normal

# Outcome equation extension from eq. 4.1 to two endogenous regressors, one exogenous regressor with intercept
# Y_t = mu + alpha1 * P1_t + alpha2 * P2_t + beta* X_t + eps_t
y <- mu + beta * X + alpha1 * P1 + alpha2 * P2 + eps

# Final dataset
dataCopIMAMultiEndo <- data.frame(y  = y, P1 = P1, P2 = P2, X = X )

usethis::use_data(dataCopIMAMultiEndo, overwrite = TRUE)
