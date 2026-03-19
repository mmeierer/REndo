## code to prepare `generate_data_2scope` dataset goes here

## Data for case 1 from chapter simulation study (Yang et al. 2024)

#2sCOPE when nonnormal regressors (both endo (P) and exo (W)) and correlation between P and W

set.seed(123)

n <- 1000

#Yang et al. (2024) see table 5
# T = 1000
# true parameters: mu = 1, alpha = 1 and beta = -1

mu <- 1
alpha <- 1
beta <- -1


# P ~ Gamma(1,1)
# W ~ exp(1)
# xi ~ normal distribution
#rho_ p w = 0.5 which shows correlation between P* and W*
#rho_ p xi = 0.5

#From equation 20 to 23 from Yang et al. (2024)

Sigma <- matrix(c(1, 0.5, 0.5, 0.5, 1, 0, 0.5, 0, 1), nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = n, mu = c(0,0,0), Sigma = Sigma)

Pstar <- latent[,1]
Wstar <- latent[, 2]
xistar <- latent[,3]

#Eq 22 marginal transformation through inverse CDF

P <- qgamma(pnorm(Pstar), shape = 1, rate = 1) # P ~ Gamma(1,1). Nonnormal
W <- qexp(pnorm(Wstar), rate = 1) #non normal
xi <- qnorm(pnorm(xistar)) #normally distributed

#Eq 23

y <-  mu + alpha * P + beta * W + xi

dataCopula2sCOPECase1 <- data.frame(y = y, P = P, W = W)

usethis::use_data(dataCopula2sCOPECase1, overwrite = TRUE)

## Data for case 2 : 2sCOPE - Endo regressors (P) is nonnormal and Exo regressors (W) is normal

set.seed(123)

n <- 1000

#See table 6 of Yang et. al (2024)

# rho_pw = 0.5
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
Wstar <- latent[, 2]
xistar <- latent[, 3]

P <- qgamma(pnorm(Pstar), shape = 1, rate = 1) #non normal
W <- qnorm(pnorm(Wstar)) #normal
xi <- qnorm(pnorm(xistar)) #normal

y <- mu + alpha * P + beta * W + xi

dataCopula2sCOPECase2 <- data.frame(y = y, P = P, W = W)

usethis::use_data(dataCopula2sCOPECase2, overwrite = TRUE)

#Case 3: endo reg (P) is normal, exo reg (W) is nonnormal

set.seed(123)

n <-1000

#2sCOPE is supposed to stay consistent because W is nonnormal which allow identification through the first stage regression P* and W*

# rho_pw = 0.5
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
Wstar  <- latent[, 2]
xistar <- latent[, 3]

P  <- qnorm(pnorm(Pstar)) #normal
W  <- qexp(pnorm(Wstar), rate = 1) #non normal W ~ exp(1)
xi <- qnorm(pnorm(xistar)) #normal

#using same eq. as 23 from case 1

y <- mu + alpha * P + beta * W + xi

dataCopula2sCOPECase3 <- data.frame(y = y, P = P, W = W)

usethis::use_data(dataCopula2sCOPECase3, overwrite = TRUE)

#Case 4: Random coefficient linear panel model

set.seed(123)

N <- 500 # no. of individuals

#let t represents the number of occasions per individual

t <- 50

#Table 7 from Yang et al. (2024)

#using the same Gaussian copula DGP as case 1 for P_it, W_it and xi_it

# true parameters: mu_bar = 1, alpha_bar = 1, beta_bar = -1

mu_bar    <- 1
alpha_bar <- 1
beta_bar  <- -1

# rho_pw = 0.7 ( stronnger correlation between P and W compare to case 1 and 2)
# rho_ pxi = 0.5
Sigma <- matrix(c(1,   0.7, 0.5,
                  0.7, 1,   0,
                  0.5, 0,   1),
                nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = N * t, mu = c(0, 0, 0), Sigma = Sigma)

Pstar  <- latent[, 1]
Wstar  <- latent[, 2]
xistar <- latent[, 3]

P_it  <- qgamma(pnorm(Pstar), shape = 1, rate = 1) #nonnormal
W_it  <- qexp(pnorm(Wstar),   rate = 1) #nonnormal
xi_it <- qnorm(pnorm(xistar)) #normal

# Individual random effects [mu_i, a_i, b_i] ~ N(0, I_3)
# this allows individual heterogeneity in intercept and slopes

re   <- MASS::mvrnorm(n = N, mu = c(0, 0, 0), Sigma = diag(3))
mu_i <- re[, 1]
a_i  <- re[, 2]
b_i  <- re[, 3]

# Individual and time indices
id <- rep(seq_len(N),    each  = t)
t_vec  <- rep(seq_len(t), times = N)

# Expanding individual random effects to observation level
mu_i_obs <- mu_i[id]
a_i_obs  <- a_i[id]
b_i_obs  <- b_i[id]


# y_it = mu_bar + mu_i + P_it*(alpha_bar + a_i) + W_it*(beta_bar + b_i) + xi_it
y_it <- mu_bar + mu_i_obs + P_it * (alpha_bar + a_i_obs) + W_it * (beta_bar  + b_i_obs) + xi_it

dataCopula2sCOPECase4 <- data.frame(id = id, t = t_vec, y  = y_it, P  = P_it, W  = W_it)

usethis::use_data(dataCopula2sCOPECase4, overwrite = TRUE)


