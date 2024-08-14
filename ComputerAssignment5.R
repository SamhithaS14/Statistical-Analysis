#11.10

data <- data.frame(
  Batch = rep(1:10, each = 3),
  Method = rep(c("A", "B", "C"), times = 10),
  Strength = c(
    30.7, 33.7, 30.5,
    29.1, 30.6, 32.6,
    30.0, 32.2, 30.5,
    31.9, 34.6, 33.3,
    30.5, 33.0, 32.4,
    26.9, 29.3, 27.8,
    28.2, 28.4, 30.7,
    32.4, 32.6, 33.6,
    26.6, 29.5, 29.2,
    28.6, 29.4, 33.2
  )
)

# ANOVA
model_method <- lm(Strength ~ Method, data = data)
model_batch <- lm(Strength ~ Batch, data = data)

# SS for Method
ss_method <- sum((predict(model_method) - mean(data$Strength))^2)

# SS for Batch
ss_batch <- nrow(data) * sum((tapply(data$Strength, data$Batch, mean) - mean(data$Strength))^2) / 10

# SS for Total
ss_total <- sum((data$Strength - mean(data$Strength))^2)

# Residual SS
ss_residual <- ss_total - ss_method - ss_batch

# MS for Method
ms_method <- ss_method / (length(unique(data$Method)) - 1)

# MS for Batch
ms_batch <- ss_batch / (length(unique(data$Batch)) - 1)

# df
df_method <- length(unique(data$Method)) - 1
df_batch <- length(unique(data$Batch)) - 1
df_residual <- nrow(data) - df_method - df_batch - 1

# ANOVA table
ANOVA_table <- data.frame(
  Sources = c("Method", "Batch", "Residuals", "Total"),
  DF = c(df_method, df_batch, df_residual, nrow(data) - 1),
  SS = c(ss_method, ss_batch, ss_residual, ss_total),
  MS = c(ms_method, ms_batch, ss_residual / df_residual, NA),
  F_value = c(ms_method / (ss_residual / df_residual), ms_batch / (ss_residual / df_residual), NA, NA),
  p_value = c(pf(ms_method / (ss_residual / df_residual), df_method, df_residual, lower.tail = FALSE), 
              pf(ms_batch / (ss_residual / df_residual), df_batch, df_residual))
)

print(ANOVA_table)

# Q CV
q_value <- qtukey(1 - 0.05, 3, 18)
w_test = q_value*sqrt((ss_residual/df_residual)/10)
print(q_value)
print(w_test)
###########################################################################################################

#11.22
data <- data.frame(
  writing_Surface = rep(1:3, each = 4),
  Brand_of_Pen = rep(1:4, times = 3),
  Lifetime = c(
    c(709, 659), c(713, 726), c(660, 645),
    c(668, 685), c(722, 740), c(692, 720),
    c(659, 685), c(666, 684), c(678, 750),
    c(698, 650), c(704, 666), c(686, 733)
  )
)

xi.. = c(4112, 4227, 4122, 4137)
xbari.. = xi../3*2
xbari..
x.j. = c(5413, 5621, 5564)
xbar.j. = x.j../4*2
xbar... <- sum(data$Lifetime) / length(data$Lifetime)

# Define the total sum of squares (SS) and the degrees of freedom (DF)
sst <- sum((data$Lifetime - xbar...)^2)
sst
total_DF <- 3*4*2-1

# Define the sum of squares (SS) and the degrees of freedom (DF) for each factor
ss_pen <- (3*2)*sum((xbari..-xbar...)^2)
ss_pen
#1382.5
DF_Pen <- 3

ss_surface <- (4*2)*sum((xbar.j. - xbar...)^2)
ss_surface
#2888.1
DF_Surface <- 2

mean_values <- numeric()
# Loop through each pair of consecutive indices in the sequence 1 to the length of Lifetime with a step of 2
for (i in seq(1, length(data$Lifetime), by = 2)) {
  # Calculate the mean value for the current pair of indices and append it to the mean_values vector
  mean_values <- c(mean_values, mean(data$Lifetime[i:(i+1)]))
}

mean_values

ss_error <- numeric(length(data$Lifetime))

# Iterate through each data point
for (i in seq_along(data$Lifetime)) {
  mean_index <- ceiling(i / 2)
  
  # Calculate the squared difference between the data point and the corresponding mean cell value
  ss_error[i] <- (data$Lifetime[i] - mean_values[mean_index])^2
}

ss_error <- sum(ss_error)
print(ss_error)
DF_Error <- 12

ss_pen_surface <- sst-ss_pen-ss_surface-ss_error
ss_pen_surface
DF_Interaction <- 6

# Calculate the mean squares (MS) for each factor and their interaction
MS_Pen <- ss_pen / DF_Pen
MS_Surface <- ss_surface / DF_Surface
MS_Interaction <- ss_pen_surface / DF_Interaction
MS_Error <- ss_error / DF_Error

# Print the corrected ANOVA table
ANOVA_table <- data.frame(
  Source = c("Pen", "Surface", "Interaction", "Error", "Total"),
  DF = c(DF_Pen, DF_Surface, DF_Interaction, DF_Error, total_DF),
  Sum_Sq = c(ss_pen, ss_surface, ss_pen_surface, ss_error, sst),
  Mean_Sq = c(MS_Pen, MS_Surface, MS_Interaction, MS_Error, NA),
  F_value = c(MS_Pen / MS_Interaction, MS_Surface / MS_Interaction, MS_Interaction / MS_Error, NA, NA),
  Pr_F = c(pf(MS_Pen / MS_Interaction, DF_Pen, DF_Error, lower.tail = FALSE),
           pf(MS_Surface / MS_Interaction, DF_Surface, DF_Error, lower.tail = FALSE),
           pf(MS_Interaction / MS_Error, DF_Interaction, DF_Error, lower.tail = FALSE),
           NA, NA)
)

print(ANOVA_table)
###########################################################################################################

#13.78

# Part C
Sy = 0.00286
x2 = 25
x3 = 1.2
yhat = -0.005315 - 0.0004968*x2+0.102204*x3
test_t = (yhat-0.1)/Sy
print(round(test_t, 3))
#Critical t = 2.021

# Part D
SSEl = 0.011862
SSEk = 0.003579
n = 49
k = 9
l = 3
num = (SSEl - SSEk)/(k-1)
denom = SSEk/(n-k+1)
test_f = num/denom
print(round(test_f, 3))
pf(test_f, 6, 30, lower.tail = FALSE) #is it close to 0? yes
###########################################################################################################

# 14.4
observations = c(342, 180, 164, 155, 86, 65, 54, 47, 56)
expected <- round(1000 * log10((1 + 1:9) / 1:9))
chisq = chisq.test(observations, p = expected/sum(expected))
print(chisq)
###########################################################################################################

# 14.29
observations = matrix(c(56, 162, 198, 211, 56, 223, 243, 239, 109, 164, 74, 28), nrow = 3, byrow = TRUE)
homogeneityTest = chisq.test(observations)
print(homogeneityTest)
###########################################################################################################

# 15.4
data = c(30.6, 30.1, 15.6, 26.7, 27.1, 25.4, 35.0, 30.8, 31.9, 53.2, 12.5, 23.2, 8.8, 24.9, 30.2)
wilcoxons = wilcox.test(data, mu = 30, alternative = "less", conf.level = 0.90)
print(wilcoxons)
###########################################################################################################

# 15.24
data_matrix <- matrix(c(
  4.079, 4.859, 3.540, 5.047, 3.298, 4.679, 2.870, 4.648, 3.847,
  4.368, 5.668, 3.752, 5.848, 3.802, 4.844, 3.578, 5.393, 4.374,
  4.169, 5.709, 4.416, 5.666, 4.123, 5.059, 4.403, 4.496, 4.688,
  4.928, 5.608, 4.940, 5.291, 4.674, 5.038, 4.905, 5.208, 4.806
), nrow = 4, ncol = 9, byrow = TRUE)

# Calculate the ranks for each group
ranks <- rank(data_matrix)
ranks_reshaped <- matrix(ranks, nrow = ncol(data_matrix), byrow = TRUE)
# Transpose the ranks output matrix
transposed_ranks_output <- t(ranks_reshaped)
rowSum_transpose <- rowSums(transposed_ranks_output)
transposed_ranks_output <- cbind(transposed_ranks_output, rowSum_transpose)
# Print the transposed matrix
print(transposed_ranks_output)

k_test <- (12 / (36 * (36 + 1))) * (sum((rowSum_transpose^2) / 9)) - 3*(36 + 1)
print(k_test)
# critical value is 7.81 at 10% from chart

###########################################################################################################

# 15.32 (a)
lateral <- c(0.86, 1.31, 1.64, 1.51, 1.53, 1.39, 1.09)
diagonal <- c(1.27, 1.82, 1.66, 0.85, 1.45, 1.24)

combined_data <- c(lateral, diagonal)
combined_data
combined_ranks <- rank(combined_data)
print(combined_ranks)
sum_ranks_d <- sum(combined_ranks[8:13])

w_test <- sum_ranks_d
w_test
n1<-length(diagonal)
n1
n2<-length(lateral)
n2

#Use chart for cv
Rcv <- 54
Lcv <- n1*(n1+n2+1)-Rcv
cv_range <- cbind(Lcv, Rcv)
cv_range

