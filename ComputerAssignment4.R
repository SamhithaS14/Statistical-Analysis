#10.42

#Part A

Group1<- c(26.8, 27.9, 23.7, 25.0, 26.3, 24.8, 25.7, 24.5)
Group2<- c(26.4, 24.2, 28.0, 26.9, 29.1)
Group3<- c(25.7, 27.2, 29.9, 28.5, 29.4, 28.3)

data <- data.frame(
  value = c(Group1, Group2, Group3),
  group = c(rep("Group1", length(Group1)), rep("Group2", length(Group2)), rep("Group3", length(Group3)))
)

model <- aov(value ~ group, data = data)
anova_result <- summary(model)
print(anova_result)

#Part B

tukey_result <- TukeyHSD(model)
print(tukey_result)

################################################################################
#12.16

#Part A
x <- c(5, 12, 14, 17, 23, 30, 40, 47, 55, 67, 72, 81, 96, 112, 127)
y <- c(4, 10, 13, 15, 15, 25, 27, 46, 38, 46, 53, 70, 82, 99, 100)

plot(x, y, main = "Scatterplot of Rainfull Volume and Runoff Volume",
     xlab = "x", ylab = "y", pch = 16, col = "purple")

abline(lm(y ~ x), col = "black")
grid()
legend("topleft", legend = "Regression line", col = "black", lty = 1, cex = 0.8)

#Part B
x_bar <- mean(x)
y_bar <- mean(y)

estimatedSlope <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
print(paste("Slope estimate:", estimatedSlope))

estimatedIntercept <- y_bar - estimatedSlope * x_bar
print(paste("Intercept estimate:", estimatedIntercept))

#Part C
y_hat <- estimatedIntercept + estimatedSlope * 50
print(paste("The calculated point estimate of the true average runoff volume when rainfall volume is 50 is ", y_hat))

#Part D
y_hat <- estimatedIntercept + estimatedSlope * x
RSE <- sqrt(sum((y - y_hat)^2) / (length(y) - 2)) #residual SE
print(paste("The calculated point estimate of the standard deviation sigma is ", RSE))

#Part E
SS_regression <- sum((y_hat - mean(y))^2)
SS_total <- sum((y - mean(y))^2)
R_squared <- SS_regression / SS_total #proportion of variation explained by the regression model
print(paste
      ("The proportion of observed variation in runoff volume attributed to the simple linear regression relationship between runoff and rainfall is ", R_squared))

################################################################################
#12.31

#Part A
#given
n <- 13
sum_x <- 10576
sum_y <- 894
sum_x_squared <- 8741264
sum_y_squared <- 66224
sum_xy <- 703192

Sxx <- sum_x_squared - (sum_x^2) / n
Sxx <- Sxx / n

Sxy <- sum_xy - (sum_x * sum_y) / n
Sxy <- Sxy / n

mean_x <- sum_x / n
mean_y <- sum_y / n

b1 <- Sxy / Sxx
b0 <- mean_y - b1 * mean_x

SST <- sum_y_squared - (sum_y^2) / n
SSE <- sum_y_squared - b0 * sum_y - b1 * sum_xy
R_squared <- 1 - SSE / SST
print(paste("Proportion of observed variation in stress explained by the linear relationship (R-squared):", R_squared))

#Part B
#given
SSE <- 509.4675
n <- 13
Sxx <- 137281.2308

s <- sqrt(SSE / (n - 2))
beta1_s <- s / sqrt(Sxx)
print(paste("Estimated standard deviation of beta1 (S_beta1):", beta1_s))

#Part C
#given
n <- 13
b1 <- -0.175635
beta1_s <- 0.01837
t_value <- qt(0.025, df = n - 2)

margin_of_error <- t_value * beta1_s

lower_bound <- b1 - margin_of_error
upper_bound <- b1 + margin_of_error

print(paste("A 95% confidence interval for the expected change in stress associated with a 1 MPa increase in strength is ", round(lower_bound, 3), ",", round(upper_bound, 3)))
################################################################################

#12.52

#Part A
#given
x <- c(1.5, 1.5, 2.0, 2.5, 2.5, 3.0, 3.5, 3.5, 4.0)
y <- c(23.0, 24.5, 25.0, 30.0, 33.5, 40.0, 40.5, 47.0, 49.0)

b_hat_0 <- 6.448718
b_hat_1 <- 10.602564
SE_regression <- 6.806  
n <- length(x) 

SSE <- sum((y - (b_hat_0 + b_hat_1 * x))^2)
s <- sqrt(SSE / (n - 2))
Sxx <- sum(x^2) - (sum(x)^2 / n)

s_b_hat_1 <- s / sqrt(Sxx)

t_statistic <- (b_hat_1 - 0) / s_b_hat_1
p_value <- 2 * pt(abs(t_statistic), df = n - 2, lower.tail = FALSE)

print(paste("T Test Statistic:", t_statistic))
print(paste("P-value:", p_value))

#Part B
n <- 9  
alpha <- 0.05 
b_hat_1 <- 10.602564
s_b_hat_1 <- 0.9985  # estimated sd of  slope
t_value <- qt(1 - alpha / 2, df = n - 2)  

CI_lower <- b_hat_1 - t_value * s_b_hat_1
CI_upper <- b_hat_1 + t_value * s_b_hat_1

print(paste("A 95% confidence interval for the true average change in etch rate associated with a 1-SCCM increase in flow rate is ", round(CI_lower, 2), ",", round(CI_upper, 2)))

#Part C
#given
b_hat_0 <- 6.448718
b_hat_1 <- 10.602564
s_b_hat_1 <- 0.9985

flow_rate <- 3.0

y_hat <- b_hat_0 + b_hat_1 * flow_rate

n <- 9 
alpha <- 0.05 
t_value <- qt(1 - alpha / 2, df = n - 2) 

S_xx <- 6.5  
y_hat_s <- s * sqrt(1/n + (flow_rate-mean(x))^2/S_xx) 

CI_lower <- y_hat - t_value * y_hat_s
CI_upper <- y_hat + t_value * y_hat_s

print(paste("A 95% confidence interval for the true average etch rate when flow = 3.0 is ", round(CI_lower, 3), ",", round(CI_upper, 3)))

#Part D
# Given data
b_hat_0 <- 6.448718
b_hat_1 <- 10.602564
s_b_hat_1 <- 0.9985

flow_rate <- 3.0  

y_hat <- b_hat_0 + b_hat_1 * flow_rate

n <- 9  
alpha <- 0.05  
t_value <- qt(1 - alpha / 2, df = n - 2) 

PI_lower <- y_hat - t_value * sqrt(t_value^2+y_hat_s^2)
PI_upper <- y_hat + t_value * sqrt(t_value^2+y_hat_s^2)

# Print prediction interval
print(paste("A 95% prediction interval for a single future observation on etch rate when flow = 3.0 is ", round(PI_lower, 3), ",", round(PI_upper, 3)))

#Part E
mean(x)
################################################################################
#12.64

#Part A
Sxx <- 25.5224
Syy <- 5593.0588
Sxy <- 264.4882
corr_coef <- Sxy/sqrt(Sxx*Syy)
print(paste("The sample correlation coefficient is ", corr_coef))

#Part B
r_prop <- corr_coef^2
print(paste("The proportion of observed variation in maximum prevalence explained by the model relationship is", r_prop))

#Part D
n <- 17
t_val <- (corr_coef - 0.5) / sqrt((1 - 0.5^2) / (n - 2))
t_val

df <- n - 2
p_val <- 2 * pt(-abs(t_val), df)
print(paste("The obtained p value for the hypothesis test is ", p_val))

