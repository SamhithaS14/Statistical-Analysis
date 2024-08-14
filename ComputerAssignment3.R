#9.23
#PART A

H <- c(1.2, 0.9, 1.9, 1.3, 0.8, 2.0, 0.7, 1.0, 1.7, 1.7, 1.1, 0.9, 1.7, 2.1, 1.6, 1.8, 1.4, 1.3, 1.9, 1.6, 1.7, 1.6, 2.3, 2.0)
P <- c(1.6, 1.5, 1.1, 2.1, 1.5, 1.3, 1.0, 2.6)

qqnorm(H, main = "Normal Probability Plot of High Quality Sample Values", col = "blue")
qqline(H)

qqnorm(P, main = "Normal Probability Plot of Poor Quality Sample Values", col = "red")
qqline(P)

#PART B

par(mfrow=c(1,2))
boxplot(H)
boxplot(P)
par(mfrow=c(1,1))
boxplot(H,P,
        main = "High Quality vs Low Quality",
        ylab = "Extensibility Percent",
        names = c("High Quality", "Low Quality"))

#PART C
H_xbar <- 1.508
P_xbar <- 1.588
H_s <- .444
P_s <- .530

#write null and alternate hypothesis
t_test_result <- t.test(H, P, var.equal = FALSE)
print(t_test_result)

critical_val <- qt(0.025, 10.482) #2 sided (this gives 1 side)
print(critical_val)
#######################################################################
#9.36

U <- c(36.4, 55.0, 51.5, 38.7, 43.2, 48.8, 25.6, 49.8)
A <- c(28.5, 20.0, 46.0, 34.5, 36.5, 52.5, 26.5, 46.5)

n <- length(U)
df <- n - 1

t_test_result <- t.test(U, A, paired = TRUE, alternative = "greater")
print(t_test_result)
#######################################################################
#9.54

x1 <- 18
n1 <- 56

x2 <- 12
n2 <- 51

phat1 <- x1/n1
qhat1 <- (1 - phat1)

phat2 <- x2/n2
qhat2 <- (1 - phat2)

phat_p <- (x1 + x2) / (n1 + n2)
qhat_p <- (1 - phat_p)

z_test_statistic = (phat1 - phat2) / (sqrt(phat_p * qhat_p) * sqrt((1/n1) + (1/n2)))
print(z_test_statistic)

criticalVal_right <- qnorm(1 - 0.05)
print(criticalVal_right)
criticalVal_left = (-1 * criticalVal_right)
print(criticalVal_left)
#######################################################################
#9.64
#given that NPP supports assumption that the population distributions are normal

Energizer <- c(8.65, 8.74, 8.91, 8.72, 8.85, 8.52, 8.62, 8.68, 8.86)
Ultracell <- c(8.76, 8.81, 8.81, 8.70, 8.73, 8.76, 8.68, 8.64, 8.79)

f_test_result <- var.test(Energizer, Ultracell)
print(f_test_result)

sd(Energizer)
sd(Ultracell)
#######################################################################
#9.74
##given that NPP supports assumption that the population distributions are normal

commutator <- c(211, 273, 305, 258, 270, 209, 223, 288, 296, 233, 262, 291, 278, 275, 210, 272, 264)
pinion <- c(226, 278, 259, 244, 273, 236, 290, 287, 315, 242, 288, 242, 278, 208, 281, 274, 268)

mean_difference <- mean(commutator) - mean(pinion)
se <- sqrt( (sd(commutator)^2/length(commutator)) + (sd(pinion)^2/length(pinion) ))

df <- ((((sd(commutator)^2 / length(commutator)) + (sd(pinion)^2 / length(pinion)))^2) / 
         ((((sd(commutator)^2 / length(commutator))^2) / (length(commutator) - 1)) + 
          (((sd(pinion)^2 / length(pinion))^2) / (length(pinion) - 1))))

t_value <- qt(1 - 0.05, df = df) #positive t value, 2 tailed

marginOfError <- t_value * se

lower_bound <- mean_difference - marginOfError
upper_bound <- mean_difference + marginOfError
print(lower_bound)
print(upper_bound)
########################################################################
#10.6

#checking if each group should be nd, var should be around the same, SRS

Group1 <- c(20.5, 28.1, 27.8, 27.0, 28.0, 25.2, 25.3, 27.1, 20.5, 31.3)
Group2 <- c(26.3, 24.0, 26.2, 20.2, 23.7, 34.0, 17.1, 26.8, 23.7, 24.9)
Group3 <- c(29.5, 34.0, 27.5, 29.4, 27.9, 26.2, 29.9, 29.5, 30.0, 35.6)
Group4 <- c(36.5, 44.2, 34.1, 30.3, 31.4, 33.1, 34.1, 32.9, 36.3, 25.5)

qqnorm(Group1, main = "Normal Probability Plot of High Quality Sample Values", col = "blue")
qqline(Group1)
qqnorm(Group2, main = "Normal Probability Plot of High Quality Sample Values", col = "purple")
qqline(Group2)
qqnorm(Group3, main = "Normal Probability Plot of High Quality Sample Values", col = "green")
qqline(Group3)
qqnorm(Group4, main = "Normal Probability Plot of High Quality Sample Values", col = "orange")
qqline(Group4)

combined <- data.frame(cbind(Group1, Group2, Group3, Group4))
stacked <- stack(combined)
ANOVA <- aov(values~ind, data = stacked)
summary(ANOVA) #get the table

#Tukey's multiple comparison test
tukey <- TukeyHSD(ANOVA) #gives pairwise comparisons
print(tukey$'ind') #at the 0.01 sig level

#to see which p adj values are less than my sig level (the means are diff then)
significant_comparisons <- tukey$ind[tukey$ind[, "p adj"] < 0.01, ]
print(significant_comparisons)



