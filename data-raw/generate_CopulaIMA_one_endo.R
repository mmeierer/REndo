## code to prepare `dataCopIMAContExo` dataset goes here

## Generate dataCopIMAContExo (IMA – single endogenous continuous regressor)

#Scenario 1: continuous exogenous, N(1,1)
set.seed(123)

n <- 1000
#page 171 -> sample size does not affect beta in IMA.
# but having a small sample size (100),IMA's estimator has slightly larger bias than PG.
#Bias decreases rapidly when increasing sample size
#The bias in alpha becomes negligible in IMA when sample size is >= 600

# True parameters
#Section 4.1 beta = alpha = 1
beta <- 1
alpha <- 1

# Copula correlation structure
#From Equation 4.2, section 4.1 page 168 of IMA Journal of Mangement Mathematics (2025) 36, 151-180
#Title of paper: Robustness of copula-correction models in Causal Analysis
# Also based on Tran & Tsionas (2022): Eq 25
# r = 0.5 and rho = 0.5, where r = Corr(P star, Xstar) and rho = Corr(Pstar, Xi star)
# P is endo, X is exo, and xi is error term

#Finding sigma
Sigma <- matrix(c(1,  0.5, 0.5,
                  0.5, 1, 0,
                  0.5, 0,   1),
                nrow = 3, ncol = 3)

#Finding Eq 4.2 (Haschka's IMA 2024), components follow a 3-D multivariate normal distribution
latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)

#Obtaining Pstar, Xstar and Xi star
Pstar  <- latent[, 1]
Xstar <- latent[, 2]
xistar <- latent[, 3]

# Marginal transformations
P  <- pnorm(Pstar) + 0.5 # page 168 of Haschka's IMA 2024, P_t = psi(Pstar_t) + .5
#X <- qnorm(pnorm(Xstar, mean = 1)) #Eq 4.3 from Haschka's IMA 2024, scenario 1 : X_t = psi^{-1} [ psi (Xstar_t ) , mu = 1]
# X here shifts distribution back to mean 0 instead of doing X ~N(1,1)

X <- Xstar + 1 # scenario 1, eq. 4.3: X_t = Phi^{-1} [phi (X*_t), mu =1] giving X~N(1,1)
xi <- xistar #xi_t = phi^{-1} [phi(xi*_t)] = xi*_t (identity transofrmation)

# outcome equation from Equation 4.1 page 168 IMA Journal of Management Mathematics (2025) 36, 151-180
#From the equation of the outcome, eq. 4.1, there is no intercept
y <- beta * X + alpha * P + xi

# Final dataset
dataCopIMAContExo <- data.frame(y  = y, X = X, P = P)

usethis::use_data(dataCopIMAContExo, overwrite = TRUE)
