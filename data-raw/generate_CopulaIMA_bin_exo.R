## code to prepare dataCopIMABinExo dataset
## Generate dataCopIMABinExo ( Section 4.1, eq. 4.4 - binary exogenous regressors)
set.seed(123)
n <- 1000
# True parameters (From Section 4.1)
#beta = alpha =1
beta  <- 1
alpha <- 1
# From eq. 4.2, the latent Gaussian dependence
Sigma <- matrix(c(1,   0.5, 0.5,
                  0.5, 1,   0,
                  0.5, 0,   1),
                nrow = 3, ncol= 3
)
latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)
Pstar  <- latent[, 1]
Xstar  <- latent[, 2]
xistar <- latent[, 3]
# Endogenous regressor (non-normal)
# P_t = Psi(P*_t) + 0.5
P <- pnorm(Pstar) + 0.5
# Scenario 2: binary exogenous regressors from eq. 4.4
X <- as.numeric(Xstar >= 0)
xi <- xistar #normal
#From eq. 4.1 the outcome Y_t = X_t beta + P_t alpha + Xi_t, t = 1,..., T.
#no intercept
y <- beta * X + alpha * P + xi
# Final dataset
dataCopIMABinExo <- data.frame( y = y,
                                X = X,
                                P = P)
usethis::use_data(dataCopIMABinExo, overwrite = TRUE)
