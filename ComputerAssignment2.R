install.packages("BSDA")
library(BSDA)
install.packages("pwr")
library(pwr)


#8.38

#Problem gives that a normal probability plot of the data shows 
#an acceptable pattern in light of the reasonably large sample size, so data is n.d

sampleData1 <- c(1.10, 5.09, 0.97, 1.59, 0.14, 4.47, 1.20, 3.50, 3.98, 3.17, 3.03, 2.21, 0.76, 1.17, 1.57, 2.62, 
                 4.60, 0.32, 5.02, 4.67, 0.69, 4.47, 1.66, 2.05, 0.55, 1.45, 5.22, 2.69, 3.31, 1.17)
#Part A

#getting different data information
mean1 <- mean(sampleData1)
print(mean1)
n <- length(sampleData1)
print(n)
sigma1 <- 1.616
alpha1 <- 0.10 #so 90% confidence level

#test statistic
z1 <- z.test(sampleData1, sigma.x = sigma1, mu = 3, alternative = "two.sided", conf.level = 0.90)
print(z1)

#CV Method to make conclusion
z_criticalVal <- qnorm(0.05)
print(z_criticalVal)

#trying with alpha1 = 0.05, which means we do a 95% confidence level
z1 <- z.test(sampleData1, sigma.x = sigma1, mu = 3, alternative = "two.sided", conf.level = 0.95)
print(z1)

z_criticalVal <- qnorm(0.975)
print(z_criticalVal)

#Part B

mu_o <- 3
mu_prime <- 5
sigma1
n
alpha1 #given in the problem to be 0.10

z_alpha1div2 <- qnorm(1 - (alpha1/2))
print(z_alpha1div2)

x <- ((mu_o - mu_prime)/(sigma1 / sqrt(n)))
print(x)

phi_1 <- (z_alpha1div2 + x)
print(phi_1)
phi_2 <- ((-1 *z_alpha1div2) + x)
print(phi_2)

beta <- pnorm(phi_1) - pnorm(phi_2)
print(beta)

#Part C

alpha1c <- 0.01 #so 99% confidence level
beta_new <- 0.1 #so 90% confidence level

z_alpha_div2 <- qnorm(0.995)
print(z_alpha_div2)
z_beta_new <- qnorm(1 - 0.1)
print(z_beta_new)

sigma1
mu_o
mu_prime

#find n for two tailed test, according to part A of problem

n_calc <- (((sigma1)*(z_alpha_div2 + z_beta_new))/(3-5))^2
ceiling(n_calc)

########################################################################################################

#8.62

#Problem says to assume that the heat-flux distribution is approximately normal, so sample is n.d

sampleData2 <- c(34.7, 35.4, 34.7, 37.7, 32.5, 28.0, 18.4, 24.9)

#getting different data information
mu <- 29.0
alpha2 <- 0.05 #95% confidence level
n <- length(sampleData2)
print(n)
sample_mean <- mean(sampleData2)
print(sample_mean)
s <- sd(sampleData2)
print(s)

#getting test statistic and p value
t <- t.test(sampleData2, mu = 29, alternative = "greater")
print(t)

########################################################################################################

#8.64

sampleData3 <- c(1.03, 1.23, 1.10, 1.64, 1.30, 1.27, 1.25, 0.78, 1.05, 0.64, 
                 0.94, 2.86, 1.05, 0.75, 0.09, 0.79, 1.61, 1.26, 0.93, 0.84)

#Problem gives that A normal probability plot of the 
#20 values shows a very pronounced linear pattern, so it is n.d

#getting different data information
s <- sd(sampleData3)
print(s)
n <- length(sampleData3)
print(n)
alpha <- 0.02 #98% confidence level
sigma <- 0.6
df <- n-1

#test statistic (x^2)
chi_square_statistic <- ((n-1)*s^2)/(sigma^2)
print(chi_square_statistic)

#CV method
chi2_critical_right <- qchisq(0.99, df)
print(chi2_critical_right)
chi2_critical_left <- qchisq(0.01, df)
print(chi2_critical_left)

########################################################################################################

#8.66

sampleData4 <- c(9.85, 9.93, 9.75, 9.77, 9.67, 9.87, 9.67, 9.94, 9.85, 9.75, 
                 9.83, 9.92, 9.74, 9.99, 9.88, 9.95, 9.95, 9.93, 9.92, 9.89)


#getting different data information
n <- length(sampleData4)
print(n)
x <- length(subset(sampleData4, sampleData4 > 9.7))
print(x)

#Part A

#finding proportion of times in the sample that exceed 9.7 -> p hat
p_hat <- x/n
print(p_hat)

#Part B

(n * p_hat)
(n*(1-p_hat))

#test statistic
z <- binom.test(x = x, n = n, p = 0.8, alternative = "two.sided")
print(z)

#0.05 significance level most common when alpha is not specified, so we'll be using that
alpha <- 0.05

#Part C

true_proportion <- 0.7
test_proportion <- 0.5

power <- power.prop.test(p1 = true_proportion, p2 = test_proportion, n = n, sig.level = alpha, alternative = "two.sided")
print(power$power)

beta <- (1 - power$power)
print(beta)

#Part D

alph_div2a <- 0.05 /2
beta <- 0.01

Po <- 0.8
Qo <- 0.2
P_prime <- 0.7
Q_prime <- 0.3

z_alpha_div2 <- qnorm(1 - 0.025)
print(z_alpha_div2)

z_beta <- qnorm(1 -0.01)
print(z_beta)

partOne <- z_alpha_div2 * sqrt(Po * Qo)
partTwo <- z_beta * sqrt(P_prime * Q_prime)
ans <- (partOne + partTwo) / (P_prime - Po)
print(ceiling(ans^2))
