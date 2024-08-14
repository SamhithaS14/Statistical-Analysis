sampleData <- c(62, 50, 53, 57, 41, 53, 55, 61, 59, 64, 50, 53, 64, 62, 
                50, 68, 54, 55, 57, 50, 55, 50, 56, 55, 46, 55, 53, 54, 
                52, 47, 47, 55, 57, 48, 63, 57, 57, 55, 53, 59, 53, 52, 
                50, 55, 60, 50, 56, 58)

#creating boxplot of sample data
boxplot(sampleData, col = "lightblue",
         main = "Boxplot of Sample Breakdown Voltage Values",
         ylab = "Breakdown Voltage Values")
text(y = boxplot.stats(sampleData)$stats, labels = boxplot.stats(sampleData)$stats, x = 1.25)

#creating normal probability plot to show n.d since we don't have sigma -- need to use t-dist
qqnorm(sampleData, main = "Normal Probability Plot of Sample Values", col = "blue")
qqline(sampleData)
 
#different data statistics
n <- length(sampleData)
xBar <- mean(sampleData)
s <- sd(sampleData)
df = (n - 1)

#get 95% confidence interval
t.test(sampleData,conf.level=0.95)
 
#getting the sample size for CI of width 2 kV, estimating population sd
z_value <- round(qnorm(0.975), 2)
sigmaEstimated <- ((70-40)/4)
E <- (2/2)
sampleSize <- ceiling(((z_value * sigmaEstimated)/E)^2)
print(sampleSize)

######################################################################################

sampleData2 <- c(33.2, 41.8, 37.3, 40.2, 36.7, 39.1, 36.2, 41.8, 36.0, 35.2, 36.7, 38.9, 35.8, 35.2, 40.1)
 
#creating normal probability plot to see if sample could have been selected from normal pop dist.
qqnorm(sampleData2, main = "Normal Probability Plot of Sample Values",col = "purple")
qqline(sampleData2)

#estimating the true average bond strength
n <- length(sampleData2)
s <- sd(sampleData2)
df = (n-1)

#getting the upper bound with confidence level 95% for pop sd
chi2_critical_right <- qchisq(0.025, df)
print(chi2_critical_right)
chi2_critical_left <- qchisq(0.975, df)
print(chi2_critical_left)

lower_bound <- sqrt((df * s^2) / chi2_critical_left)
print(lower_bound)
upper_bound <- sqrt((df * s^2) / chi2_critical_right)
print(upper_bound)

######################################################################################
sampleData3 <- c(11.5, 12.1, 9.9, 9.3, 7.8, 6.2, 6.6, 7.0, 13.4, 17.1,
                  9.3, 5.6, 5.7, 5.4, 5.2, 5.1, 4.9, 10.7, 15.2, 8.5,
                  4.2, 4.0, 3.9, 3.8, 3.6, 3.4, 20.6, 25.5, 13.8, 12.6,
                  13.1, 8.9, 8.2, 10.7, 14.2, 7.6, 5.2, 5.5, 5.1, 5.0,
                  5.2, 4.8, 4.1, 3.8, 3.7, 3.6, 3.6, 3.6)

#different data statistics
n <- length(sampleData3)
xBar <- mean(sampleData3)
sd <- sd(sampleData3)
df = (n-1)

#creating normal probability plot to show n.d -- need to use t-dist
qqnorm(sampleData3, main = "Normal Probability Plot of Sample Values", col = "darkred")
qqline(sampleData3)

#estimating the true average bond strength, 95%
t.test(sampleData3,conf.level=0.95)

#95% CI for the proportion of all such bonds whose strength values exceed 10
sampleData3New <- subset(sampleData3, sampleData3 > 10)

#creating normal probability plot to show n.d -- need to use t-dist
qqnorm(sampleData3New, main = "Normal Probability Plot of Sample Values", col = "darkred")
qqline(sampleData3New)

nNew <- length(sampleData3New)
 
prop.test(nNew,n,0.5,correct=FALSE,conf.level = 0.95)

######################################################################################

sampleData4 <- c(4.7, 5.1, 5.2, 5.3, 5.6, 5.8, 6.3, 6.7, 7.2, 7.4, 7.7, 8.5, 8.9, 9.3, 10.1, 11.2)

#problem states the data follows a normal distribution, so no need to create NPP

#different data statistics
n <- length(sampleData4)
xBar <- mean(sampleData4)
sd <- sd(sampleData4)
df = (n-1)
alpha <- 0.01

#creating 99% confidence interval for true average crack initiation depth
t.test(sampleData4,conf.level=0.99) 

#creating a 99% prediction interval for true average crack initiation depth -- don't have a mu for t formula, find t value with CI
t_value = qt(0.99 + (alpha /2), df) 
print(t_value)

ME = (t_value)*sd*sqrt(1 + (1/n))
print(ME)

lowerLimit = xBar - ME
upperLimit = xBar + ME
print(lowerLimit)
print(upperLimit)

######################################################################################

sampleData5 <- c(159, 120, 480, 149, 270, 547, 340, 43, 228, 202, 240, 218)

#problem states the data follows a normal distribution, so no need to create NPP

#different data statistics
n <- length(sampleData5)
xBar <- mean(sampleData5)
sd <- sd(sampleData5)
df = (n-1)
alpha <- 0.05
mu <- 200 #assume Ho true

#find p-value...
t.test(sampleData5,mu = mu,conf.level=0.95, alternative="greater") #right tailed

#OR find critical value --right tailed
critical_value = qt((1-alpha), df)
print(critical_value)

######################################################################################

sampleData6 <- c(112.3, 97.0, 92.7, 86.0, 102.0, 99.2, 95.8, 103.5, 89.0, 86.7)

#creating normal probability plot to show n.d and -- t-dist can be used
qqnorm(sampleData6, main = "Normal Probability Plot to Show n.d", col = "orange")
qqline(sampleData6)

#different data statistics
n <- length(sampleData6)
xBar <- mean(sampleData6)
sd <- sd(sampleData6)
df = (n-1)
alpha <- 0.05
mu <- 100 #assume Ho true

#find p-value...
t.test(sampleData6,mu = mu,conf.level=0.95, alternative="less") #left tailed

#OR critical value left tailed
critical_value <- qt(alpha, df)
print(critical_value)


### ALTERNATIVE WITHOUT t.test(): get t statistic this way & compare to CV / probability this way ###
t_statistic <- (mean(sampleData6) - mu) / (sd(sampleData6) / sqrt(n))
print(t_statistic)

probability <- pt(t_statistic, df)
print(probability)
