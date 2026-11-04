## code to prepare `generate_data_2sCOPEnp` dataset goes here

## Dataset 1: dataCop2sCOPEnpCont - based on case 3 of Hu et al. 2025, continuous P

library(MASS)
library(truncnorm)

#Hu et al. 2025 section 4.3 neither joint Gaussian copula nor mean-dependence model
# This is a situation where (P, X) does not follow a joint Gaussian copula
#and where higher moments of P|X depend on X
#this is 'unique' case compared to other 2sCOPE , where bias is being eliminated better

set.seed(123)
n <- 1000

#true values: mu = 1, alpha =1 , beta = 2
mu <- 1
alpha <- 1
beta <- 2

# X ~ student-t(3) from eq. 34

X <- rt(n, df = 3)

# bivariate normal for (P*, Xi*), rho = 0.5 from eq. 37

Sigma <- matrix(c(1 , 0.5,
                  0.5, 1),
                nrow = 2, ncol = 2)

latent <- MASS:: mvrnorm(n = n , mu = c(0,0), Sigma = Sigma)

Pstar <- latent[,1]
Xistar  <- latent[,2]

# P|X ~ truncated normal with X-dependent bounds from eq. 35
# a = min(0, -2X + 2) , b = max(2, -2X + 2)

a <- pmin(0, -2 *X + 2)
b <- pmax(2, -2 * X + 2)

#P*_i = phi^{-1} (H_{TN(a,b)}(P|X)) from eq. 36

P <- truncnorm::qtruncnorm(pnorm(Pstar), a = a, b =b , mean = 0, sd = 1)

#Xi = Xi* , error is normal eq. 38
Xi <- Xistar

#outcome equation from eq. 39 : Y_i = mu + alpha * P_i + beta * X_i + xi_i
# Y_i = 1 + 1*P_i +2 * X_i + xi_i

y <- mu + alpha * P + beta * X + Xi

dataCop2sCOPEnpCont <- data.frame(y = y, P = P, X = X)
usethis::use_data(dataCop2sCOPEnpCont, overwrite = TRUE)

# Dataset 2: Binary endogenous regressor (from case 5)
# P ~ Bernoulli (binary treatment), X ~ t(3)
# rho_{px} = 0.5, rho_{pxi} = 0.5

#this dataset shows a discrete P. P being binary (0 or 1)

set.seed(123)
n <- 2000 #from paper

#true values: mu = 0, alpha = 1, beta = 2
mu <- 0
alpha <- 1
beta <- 2

#(P*, X* , Xi*) normally distributed eq. 44

Sigma <- matrix(c(1,   0.5, 0.5,
                  0.5, 1,   0,
                  0.5, 0,   1),
                nrow = 3, ncol = 3)

latent <- MASS::mvrnorm(n = n, mu = c(0, 0, 0), Sigma = Sigma)

Pstar <- latent[, 1]
Xstar <- latent[, 2]
Xistar <- latent[,3]

#Binary P: P = I{Phi(P*) > p1} = I{P* > Phi^{-1}(p1)} (eq. 46)
# p1 = 0.5
p1 <- 0.5
P <- as.integer(pnorm(Pstar) > p1) # 0 or 1

# X ~ t(3) : L(X) is student-t CDF eq.47
# X_i = L^{-1} (U_{X,i}) = L^{-1} (psi (X*_i))

X <- qt(pnorm(Xstar), df = 3)

# Xi = Xi* eq 4.5 error is normal
Xi <- Xistar

#outcome equation (Eq. 48): Y_i =  mu + alpha * P_i + beta * X_i + Xi
# Y_i = 0 + 1 * P_i + 2 * X_i + Xi_i

y <-  mu + alpha * P + beta * X + Xi

dataCop2sCOPEnpBi <- data.frame(y = y, P = P, X = X)
usethis::use_data(dataCop2sCOPEnpBi, overwrite = TRUE)

# Dataset 3: dataCop2sCOPEnpMulti
# The following dataset is made up of different types of variable.

#P1 is lognormal (continuous endogenous)
#P2 ordered endogenous (here 4 levels: low, meidum, high and very_high)
#X1~ N(0,1) (continuous exogenous)
#X2 ~N(0,1) (continuous exogenous) and correlated with P2 via latent score
#X3 ordered factor exogenous (here 3 levels: low, medium and high)

set.seed(123)

n <- 5000

#True values: mu = 1, alpha1 = -1 (P1), alpha2 = 1 (P2 per ordered level),
#beta1 = 2 (X1), beta2 = 0.5 (X2) and beta3 = 1 (X3)

mu <- 1
alpha1 <- -1
alpha2 <- 1
beta1 <- 2
beta2 <- 0.5
beta3 <- 1

# Latent Gaussian dependence structure:
# rho(P1, epsilon) = 0.5
# rho(P2_latent, epsilon) = 0.7
# rho(P1, P2_latent) = 0.3
#P1 is correlated with X1
#P2_latent is correlated with X3

#joint latent normal for epsilon, P1_latent and P2_latent
Sigma <- matrix(c(1, 0.5, 0.7,
                  0.5, 1, 0.3,
                  0.7, 0.3, 1),
                nrow = 3, ncol= 3, byrow = TRUE)

latent <- MASS::mvrnorm(n = n, mu = c(0,0,0), Sigma = Sigma)
epsilon <- latent[,1] #structural error ~N(0,1)
P1_latent <- latent[,2] #latent score for continuous P1
P2_latent <- latent[,3] #latent score for discrete ordered P2

#Exogenous regressors
X1 <- rnorm(n)

#X2 ~ N(0,1) correlated with P2_latent and avoiding extreme outliers
X2 <- P2_latent * 0.5 + rnorm(n, sd = sqrt(1-0.25)) #corr(X2, P2_latent) = 0.5

#X3 ordered factor with 3 levels correlated with P2 through latent scores
X3_score <- P2_latent + rnorm(n, sd = 1.5)
X3_cuts  <- quantile(X3_score, probs = c(1/3, 2/3))
X3_index   <- findInterval(X3_score, X3_cuts) + 1L  # 1, 2, or 3
X3 <- ordered( c("low", "medium", "high")[X3_index], levels = c("low", "medium", "high")
)
#P1 continuous lognorma and correlated with X1 through latent scores
P1_score <- P1_latent + 0.5 *X1
P1 <- exp(P1_score) #lognormal which is strictly positive.

#P2 ordered factor with 4 levels and obtained from P2_latent + X3 contribution.
#X3 contribution creates P2 -X3 correlation (which is the endo-exo correlation)
P2_score <- P2_latent + 0.5 * X2
P2_cuts <- quantile(P2_score, probs = c(0.25, 0.5, 0.75))
P2_index   <- findInterval(P2_score, P2_cuts) + 1L  # 1, 2, 3, or 4
P2 <- ordered( c("low", "medium", "high", "very_high")[P2_index], levels = c("low", "medium", "high", "very_high")
)
#representing outcome equation numerically
X3_n <- as.numeric(X3) # 1=low, 2=medium, 3=high
P2_n <- as.numeric(P2) # 1=low, 2=medium, 3=high, 4=very_high

#outcome equation: Y_i = mu + alpha1*P1_i + alpha2*P2_i + beta1*X1_i + beta2*X2 + beta3 * X3_n
y <- mu + alpha1 * P1 + alpha2 * P2_n + beta1 * X1 + beta2 * X2 +  beta3 * X3_n + epsilon

dataCop2sCOPEnpMulti <- data.frame(y = y, P1 = P1, P2 = P2, X1 = X1, X2 = X2, X3 = X3)

usethis::use_data(dataCop2sCOPEnpMulti, overwrite = TRUE)



