## code to prepare `generate_data_2scope` dataset goes here

## Data for case 1 from chapter simulation study (Yang et al. 2024)

#2sCOPE when nonnormal regressors (both endo (P) and exo (X)) and correlation between P and X

set.seed(123)

n <- 1000

#Yang et al. (2024) see table 5
# T = 1000
# true parameters: mu = 1, alpha = 1 and beta = -1

mu <- 1
alpha <- 1
beta <- -1


# P ~ Gamma(1,1)
# X ~ exp(1)
# xi ~ normal distribution
#rho_ p x = 0.5 which shows correlation between P* and X*
#rho_ p xi = 0.5

#From equation 20 to 23 from Yang et al. (2024)

Sigma <- matrix(c(1, 0.5, 0.5, 0.5, 1, 0, 0.5, 0, 1), nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = n, mu = c(0,0,0), Sigma = Sigma)

Pstar <- latent[,1]
Xstar <- latent[, 2]
xistar <- latent[,3]

#Eq 22 marginal transformation through inverse CDF

P <- qgamma(pnorm(Pstar), shape = 1, rate = 1) # P ~ Gamma(1,1). Nonnormal
X <- qexp(pnorm(Xstar), rate = 1) #non normal
xi <- qnorm(pnorm(xistar)) #normally distributed

#Eq 23

y <-  mu + alpha * P + beta * X + xi

dataCopula2sCOPECase1 <- data.frame(y = y, P = P, X = X)

usethis::use_data(dataCopula2sCOPECase1, overwrite = TRUE)

## Data for case 2 : 2sCOPE - Endo regressors (P) is nonnormal and Exo regressors (X) is normal

set.seed(123)

n <- 1000

#See table 6 of Yang et. al (2024)

# rho_px = 0.5
# rho_pxi = 0.5
# T = 1000

mu <- 1
alpha <- 1
beta <- -1

Sigma <- matrix(c(1, 0.5, 0.5,
                  0.5, 1, 0,
                  0.5, 0, 1), nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = n, mu = c(0,0,0), Sigma = Sigma)

Pstar <- latent[, 1]
Xstar <- latent[, 2]
xistar <- latent[, 3]

P <- qgamma(pnorm(Pstar), shape = 1, rate = 1) #non normal
X <- qnorm(pnorm(Xstar)) #normal
xi <- qnorm(pnorm(xistar)) #normal

y <- mu + alpha * P + beta * X + xi

dataCopula2sCOPECase2 <- data.frame(y = y, P = P, X = X)

usethis::use_data(dataCopula2sCOPECase2, overwrite = TRUE)

#Case 3: endo reg (P) is normal, exo reg (X) is nonnormal

set.seed(123)

n <-1000

#2sCOPE is supposed to stay consistent because X is nonnormal which allow identification through the first stage regression P* and X*

# rho_px = 0.5
# rho_pxi = 0.5
# T = 1000

mu <- 1
alpha <- 1
beta <- -1

Sigma <- matrix(c(1,   0.5, 0.5,
                  0.5, 1,   0,
                  0.5, 0,   1),
                nrow = 3, ncol = 3)


latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)

Pstar  <- latent[, 1]
Xstar  <- latent[, 2]
xistar <- latent[, 3]

P  <- qnorm(pnorm(Pstar)) #normal
X  <- qexp(pnorm(Xstar), rate = 1) #non normal X ~ exp(1)
xi <- qnorm(pnorm(xistar)) #normal

#using same eq. as 23 from case 1

y <- mu + alpha * P + beta * X + xi

dataCopula2sCOPECase3 <- data.frame(y = y, P = P, X = X)

usethis::use_data(dataCopula2sCOPECase3, overwrite = TRUE)


