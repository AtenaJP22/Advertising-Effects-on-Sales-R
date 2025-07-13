#Authors: Atena Jafari Parsa 2101183, Ava Arabi 2104906
# Set the working directory
setwd("/Users/atenaparsa/Downloads")
# Import the whole dataset
advertising_data <- read.csv("advertising.csv", header = TRUE)

# Check the structure of the dataset
str(advertising_data)

#To check the relationship between TV ads and sales and see if it is linear or it needs transformations
#we need to remove the Radio and Newspaper columns as we do not need them for now.

# Identify and remove the columns
columns_to_remove <- c("Radio", "Newspaper")
TV_vs_Sales <- advertising_data[, -which(colnames(advertising_data) %in% columns_to_remove)]

summary(advertising_data)

# Save the modified dataset as a new file
write.csv(TV_vs_Sales, "TV_vs_Sales.csv", row.names = FALSE)

# Create a scatter plot to visualize the relationship between TV Advertisements and sales.
plot(TV_vs_Sales$TV, TV_vs_Sales$Sales, xlab = "TV Advertisements", ylab = "Sales", main = "Scatter Plot")

# Fit a linear regression model
lm_model <- lm(Sales ~ TV, data = TV_vs_Sales)

# Create a scatter plot
plot(TV_vs_Sales$TV, TV_vs_Sales$Sales,
     xlab = "TV Advertisements", ylab = "Sales",
     main = "Scatter Plot with Linear Regression Line")
# Add the regression line to the scatter plot
abline(lm_model, col = "red", lwd = 2)

#To have a general idea of the data, we would also use some other types of visualization.

# Create a histogram
hist(TV_vs_Sales$TV, main = "Histogram", xlab = "TV Advertisements", ylab = "Sales")

# Create a box plot
boxplot(TV_vs_Sales$TV, main = "Box Plot", ylab = "Sales")

#Now, to see the overall relation between Radio and Sales, we repeat the same process except, we keep Radio column.

# Identify and remove the columns
columns_to_remove_Radio_Version <- c("TV", "Newspaper")
Radio_vs_Sales <- advertising_data[, -which(colnames(advertising_data) %in% columns_to_remove_Radio_Version)]

# Save the modified dataset as a new file
write.csv(Radio_vs_Sales, "Radio_vs_Sales.csv", row.names = FALSE)

# Create a scatter plot to visualize the relationship between Radio ads and sales
plot(Radio_vs_Sales$Radio, Radio_vs_Sales$Sales, xlab = "Radio Advertisements", ylab = "Sales", main = "Scatter Plot")

# Fit a linear regression model
lm_model <- lm(Sales ~ Radio, data = Radio_vs_Sales)

# Create a scatter plot
plot(Radio_vs_Sales$Radio, Radio_vs_Sales$Sales,
     xlab = "Radio Advertisements", ylab = "Sales",
     main = "Scatter Plot with Linear Regression Line")
# Add the regression line to the scatter plot
abline(lm_model, col = "red", lwd = 2)

#Now, to see the overall relation between Newspaper and Sales, we repeat the same process except, we keep Newspaper column.

# Identify and remove the columns
columns_to_remove_Newspaper_Version <- c("TV", "Radio")
Newspaper_vs_Sales <- advertising_data[, -which(colnames(advertising_data) %in% columns_to_remove_Newspaper_Version)]

# Save the modified dataset as a new file
write.csv(Newspaper_vs_Sales, "Newspaper_vs_Sales.csv", row.names = FALSE)

# Create a scatter plot to visualize the relationship between Newspaper ads and sales
plot(Newspaper_vs_Sales$Newspaper, Newspaper_vs_Sales$Sales, xlab = "Newspaper Advertisements", ylab = "Sales", main = "Scatter Plot")

# Fit a linear regression model
lm_model <- lm(Sales ~ Newspaper, data = Newspaper_vs_Sales)

# Create a scatter plot
plot(Newspaper_vs_Sales$Newspaper, Newspaper_vs_Sales$Sales,
     xlab = "Newspaper Advertisements", ylab = "Sales",
     main = "Scatter Plot with Linear Regression Line")
# Add the regression line to the scatter plot
abline(lm_model, col = "red", lwd = 2)


#To apply the normality test on the TV ads we store the TV column in a vector. We should repeat the same process for sales.

#To pick a random sample we need to store the data in a vector
# Extract a column as a vector using the $ operator
TVvector <- advertising_data$TV

# Extract a column as a vector using the [ ] operator
TVvector <- advertising_data["TV"]

# Extract a column as a vector using the [ ] operator
Salesvector <- advertising_data$Sales

# Extract a column as a vector using the [ ] operator
Salesvector <- advertising_data["Sales"]

# Extract a column as a vector using the [ ] operator
Radiovector <- advertising_data$Radio

# Extract a column as a vector using the [ ] operator
Radiovector <- advertising_data["Radio"]

#Store the transposed version of Radiovector for later use
RadiovectorTransposed <- t(Radiovector)

#Now we can use the sample() function to pick 30 random integers from 1-200

#Generate 30 random numbers between 1 and 200
rand_nums <- sample(1:200, 30, replace = FALSE)

# View the generated random numbers
rand_nums

# Create the data frame
SampleTV <- data.frame(TV = c(44.7, 248.8, 76.4, 134.3, 222.4, 187.8, 283.6, 100.4, 121.0, 109.8, 87.2, 7.3, 50.0, 184.9, 280.7, 228.3, 262.9, 199.1, 95.7, 78.2, 26.8, 0.7, 89.7, 135.2, 175.1, 220.3, 234.5, 224.0, 75.1, 248.4))
# Transpose the data frame
SampleTV <- t(SampleTV)

# Create the data frame for sales (the other dimension)
SampleSales <- data.frame(Sales = c(20.7, 18.9, 9.4,  14.0, 16.7, 20.6, 25.5, 10.7, 11.6, 12.4,  10.6,  5.5, 8.4,  20.7, 16.1, 20.5,  17.0, 18.3, 11.9,  14.6, 8.8, 1.6, 10.6, 17.2, 16.1, 24.7, 16.9, 16.6, 12.6, 20.2))
# Transpose the data frame
SampleSales <- t(SampleSales)

#To view the summary of the sample data, we must transpose it and store it first.
SampleTVForSummary <- t(SampleTV)
summary(SampleTVForSummary)

#Select a random sample for radio

#Now we can use the sample() function to pick 20 random integers from 1-200
#Generate 20 random numbers between 1 and 200
rand_nums <- sample(1:200, 20, replace = FALSE)

# View the generated random numbers
rand_nums

# Create the data frame
SampleRadio <- data.frame(Radio = c(1.9, 16.7, 33.2, 8.6, 222.4, 43.7, 4.1, 14.3, 17.0, 20.1, 33.5, 34.6, 0.3, 2.4, 10.8, 27.5, 36.3, 20.5, 47.0, 26.7, 36.9))
# Transpose the data frame
SampleRadio <- t(SampleRadio)

#Select a new sample for TV with size 20 (So that it is not normally distributed by CLT.)
# Create the data frame
SampleTVNew <- data.frame(TV = c(44.7, 248.8, 76.4, 134.3, 222.4, 187.8, 283.6, 100.4, 121.0, 109.8, 87.2, 7.3, 50.0, 184.9, 280.7, 228.3, 262.9, 199.1, 95.7, 78.2))
# Transpose the data frame
SampleTVNew <- t(SampleTVNew)

#Select a new sample for TV with size 20 

#Generate 20 random numbers between 1 and 200
rand_nums <- sample(1:200, 20, replace = FALSE)

# View the generated random numbers
rand_nums
SampleTV2 <- data.frame(TV = c(193.7, 107.4, 18.8, 123.1, 164.5, 296.4, 43.0, 112.9, 281.4, 166.8, 69.0, 234.5, 135.2, 284.3, 8.4, 206.9, 139.3, 184.9, 139.5, 229.5))
# Transpose the data frame
SampleTV2 <- t(SampleTV2)

#View the sample statistics (Sales wrt TV ads)

# Create a scatter plot
plot(SampleTV, SampleSales,
     xlab = "Sample TV Advertisements", ylab = "Sample Sales",
     main = "Sample Scatter Plot")

#Transpose the samples and store them to turn them into columns before combining them.
SampleSalesForVisual <- t(SampleSales)
SampleTVForVisual <- t(SampleTV)

# Connect vectors as columns using cbind()
sampleData <- cbind(SampleTVForVisual, SampleSalesForVisual)

# Create a histogram
hist(SampleTV, main = "Histogram", xlab = "Sample TV Advertisements", ylab = "Sales")

# Shapiro-Wilk normality test for the sample(TV ads)
shapiro.test(SampleTV)

#Shapiro-Wilk test to see whether or not the Radio and the TV population is normally distributed.
# Shapiro-Wilk normality test for the population(TV)
TVvectorTransposed <- t(TVvector)
shapiro.test(TVvectorTransposed)

# Shapiro-Wilk normality test for the population(Radio)
RadiovectorTransposed <- t(Radiovector)
shapiro.test(RadiovectorTransposed)

#Examples of Normal Distribution 

# Shapiro-Wilk normality test for the population(Sales)
SalesvectorTransposed <- t(Salesvector)
shapiro.test(SalesvectorTransposed)


#We need the mean and the standard deviation of the population

summary(Salesvector)

# Calculate and store the standard deviation
sales_sd <- sd(SalesvectorTransposed)

#View the standard deviation: 5.283892
sales_sd

# Store the mean
salesMean <- 15.13

#ggplot2 library is installed for depicting the graphs of the questions.
install.packages("ggplot2")

library(ggplot2)

#Question 1
# a. Calculate the value of x for P(X > x) = 0.6
p1 <- 0.6
x1 <- qnorm(1 - p1, salesMean, sales_sd)
cat("x1 =", x1, "\n")

#Graph for 1.a
# Set the significance level
alpha <- 0.05
# Calculate the critical value
critical_value <- qnorm(alpha, salesMean, sales_sd)
# Generate the x-axis values
x <- seq(salesMean - 3*sales_sd, salesMean, length.out = 100)
# Generate the y-axis values (left-tailed probabilities)
y <- pnorm(x, salesMean, sales_sd)
# Plot the graph
plot(x, y, type = "l", xlab = "x", ylab = "P(X < x)", main = "Left-Tailed Hypothesis Test")
abline(v = critical_value, col = "red")

# Set the significance level
alpha <- 0.05
# Calculate the critical value
critical_value <- qnorm(alpha, salesMean, sales_sd)
# Generate the x-axis values
x <- seq(salesMean - 3*sales_sd, salesMean, length.out = 100)
# Generate the y-axis values (left-tailed probabilities)
y <- pnorm(x, salesMean, sales_sd)
# Plot the graph
plot(x, y, type = "l", xlab = "x", ylab = "P(X < x)", main = "Left-Tailed Hypothesis Test", col = "skyblue")
# Color the area before the critical value
polygon(c(x[1], x[x <= critical_value], critical_value), c(0, y[x <= critical_value], 0), col = "skyblue", border = NA)
abline(v = critical_value, col = "red")

# b. Calculate the value of x for P(X < x) = 0.85
p2 <- 0.85
x2 <- qnorm(p2, salesMean, sales_sd)
cat("x2 =", x2, "\n")

#Graph for 1.b

# c. Calculate the value of x for P(Z < x) = 0.04
p3 <- 0.04
x3 <- qnorm(p3, salesMean, sales_sd)
x3 <- x3 * sales_sd + salesMean
cat("x3 =", x3)

#Question 2
# Calculate the probability P(X < 20)
x <- 20
p <- pnorm(x, salesMean, sales_sd)
cat("P(X < 20) =", p)

#Point Estimation Questions

#Question 3: Estimating the mean of the population using the sample
# Calculate the sample mean
sample_mean <- mean(SampleSales)
# Print the sample mean
cat("Sample Mean:", sample_mean)

#Question 4: Estimating the sd of the population using the sample
# Calculate the sample standard deviation
sample_std <- sd(SampleSales)
# Print the estimated population standard deviation
cat("Estimated population standard deviation:", sample_std, "\n")

#Confidence Intervals

#We already have the sample mean and the sample standard deviation stored so we don't calculate them again.

#Question 5: Confidence Interval for the Mean (95% confidence level)
# Set the confidence level (e.g., 95%)
confidence_level <- 0.95
# Calculate the standard error
standard_error <- sample_std / sqrt(length(SampleSales))
# Calculate the margin of error
margin_of_error <- qnorm(1 - (1 - confidence_level) / 2) * standard_error
# Calculate the lower and upper bounds of the confidence interval
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error
# Print the confidence interval
cat("Confidence Interval for the Mean (95% confidence level): [", lower_bound, ",", upper_bound, "]\n")

#Question 6: Confidence Interval for the Standard Deviation (90% confidence level) using chi-square test
# Set the confidence level
confidence_level <- 0.90
# Set the degrees of freedom
df <- length(SampleSales) - 1
# Calculate the lower and upper bounds of the confidence interval
lower_bound <- sqrt(df * sample_std^2 / qchisq((1 + confidence_level) / 2, df))
upper_bound <- sqrt(df * sample_std^2 / qchisq((1 - confidence_level) / 2, df))
# Print the confidence interval
cat("Confidence Interval for the Standard Deviation (95% confidence level): [", lower_bound, ",", upper_bound, "]\n")

#1Hypothesis Test

#Calculate the mean and standard deviation of the Radio population and the sample
#Radio Population
Radio_mean <- mean(RadiovectorTransposed) #23.264
Radio_sd <- sd(RadiovectorTransposed) #14.84681
#Radio Sample
RadioSampleMean <- mean(t(SampleRadio)) #31.35714
RadioSampleSd <- sd(t(SampleRadio)) #45.99748

#Calculate the mean and standard deviation of the TV population and the sample
#TV Population
TV_mean <- mean(TVvectorTransposed) #147.0425
TV_sd <- sd(TVvectorTransposed) #85.85424
#TV Sample
SampleTVNewMean <- mean(t(SampleTVNew)) #31.35714
SampleTVNewSd <- sd(t(SampleTVNew)) #85.20829

#Question 7: Claim: The population mean of Radio is greater than 23.
# Set the significance level (alpha)
alpha <- 0.05
# Null hypothesis (H0): The mean value of Radio budgets is equal to 23
# Alternative hypothesis (Ha): The mean value of Radio budgets is greater than 23
# Perform one-sample t-test
t_test_result <- t.test(SampleRadio, mu = 23, alternative = "greater")
# Extract the p-value from the test result
p_value <- t_test_result$p.value
# Compare p-value with significance level to make a decision
if (p_value <= alpha) {
  cat("Reject the null hypothesis (H0), p-value = ", p_value)
} else {
  cat("Fail to reject the null hypothesis (H0), p-value = ", p_value)
}

#Graph for Question 7
# Create a data frame for plotting
df <- data.frame(Hypothesis = c("H0: μ = 23", "Ha: μ > 23"),
                 P_Value = c(1 - p_value, p_value))
# Create a bar plot
ggplot(df, aes(x = Hypothesis, y = P_Value, fill = Hypothesis)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Hypothesis", y = "P-Value", fill = "Hypothesis") +
  scale_fill_manual(values = c("grey", "blue")) +
  theme_minimal()

#Question 8: Claim: The mean weight of a population is not equal to 15.13 
# Set the significance level (alpha)
alpha <- 0.05
# Null hypothesis (H0): The mean value of the Sales population is equal to 15.13
# Alternative hypothesis (Ha): The mean weight of a population is not equal to 15.13 

# Calculate the test statistic (Z-score)
z_score <- (sample_mean - salesMean) / (sales_sd / sqrt(length(SampleSales)))
# Calculate the critical value for a two-tailed test
critical_value <- qnorm(1 - alpha/2)
# Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z_score)))
# Compare the test statistic with the critical value and p-value to make a decision
if (p_value > critical_value) {
  cat("Reject the null hypothesis (H0)")
} else {
  cat("Fail to reject the null hypothesis (H0)")
}
cat("\n")
cat("p-value:", p_value, ", critical value: ", critical_value)

#Graph for Question 8
# Set the significance level (alpha)
alpha <- 0.05
# Null hypothesis (H0): The mean value of the Sales population is equal to 15.13
# Alternative hypothesis (Ha): The mean weight of a population is not equal to 15.13
# Calculate the test statistic (Z-score)
z_score <- (sample_mean - salesMean) / (sales_sd / sqrt(length(SampleSales)))
# Calculate the critical value for a two-tailed test
critical_value <- qnorm(1 - alpha/2)
# Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z_score)))
# Create a data frame for plotting
df <- data.frame(x = c(-4, 4))
# Create a ggplot object and add layers for the two tails
ggplot(df, aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), n = 100, geom = "area",
                xlim = c(-4, -2), fill = "red", alpha = 0.2) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), n = 100, geom = "area",
                xlim = c(2, 4), fill = "red", alpha = 0.2) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), n = 100, geom = "line") +
  geom_vline(xintercept = z_score, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = -z_score, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = critical_value, color = "red", linetype = "dashed") +
  geom_vline(xintercept = -critical_value, color = "red", linetype = "dashed") +
  annotate("text", x = z_score + 0.1, y = 0.1, label = "Test Statistic") +
  annotate("text", x = critical_value + 0.1, y = 0.1, label = "Critical Value") +
  annotate("text", x = -critical_value - 1.3, y = 0.1, label = "Critical Value") +
  labs(x = "Z-score", y = "Density") +
  theme_minimal()

#2 Hypothesis Test

#Question 9: Claim: The means of the two samples (Radio sample and TV sample) are significantly different.

# Hypothesis Test
# Step 1: Set the significance level
alpha <- 0.05
# Step 2: Calculate the test statistic (t-value) and p-value
result <- t.test(SampleRadio, SampleTVNew)
# Step 3: Compare the p-value to the significance level
if (result$p.value <= alpha) {
  cat("Reject the null hypothesis. The means of the two samples are significantly different. , P-value = ", result$p.value)
} else {
  cat("Fail to reject the null hypothesis. The means of the two samples are not significantly different.")
}

#Question 10

# Hypothesis Test 1
# Step 1: Set the significance level
alpha <- 0.05
# Step 2: Calculate the test statistic (t-value) and p-value
result_1 <- t.test(SampleTVNew, SampleTV2, paired = TRUE)
# Step 3: Compare the p-value to the significance level
if (result_1$p.value <= alpha) {
  cat("Reject the null hypothesis for Hypothesis Test 1. The means of the two samples are significantly different. , p-value = ", result_1$p.value)
} else {
  cat("Fail to reject the null hypothesis for Hypothesis Test 1. The means of the two samples are not significantly different. , p-value = ", result_1$p.value)
}
# Hypothesis Test 2
# Step 1: Set the significance level
alpha <- 0.05
# Step 2: Calculate the test statistic (t-value) and p-value
result_2 <- t.test(SampleTVNew, SampleTV2, alternative = "less", paired = TRUE)
# Step 3: Compare the p-value to the significance level
if (result_2$p.value <= alpha) {
  cat("Reject the null hypothesis for Hypothesis Test 2. The mean of sample 1 is significantly less than the mean of sample 2. , p-value = ", result_2$p.value)
} else {
  cat("Fail to reject the null hypothesis for Hypothesis Test 2. The mean of sample 1 is not significantly less than the mean of sample 2., p-value = ", result_2$p.value)
}

#Goodness of Fits Tests and other Checks for Detecting the Distribution

#Question 11: Goodness-of-Fit Test (Chi-Squared Test):
# Perform a chi-squared goodness-of-fit test
# Assume you want to test if the observed variable follows a normal distribution
# Perform the chi-squared test
chi_squared_test <- chisq.test(TVvector)
# Print the test result
print(chi_squared_test)

#Question 12: Use Chi-squared test to test two variables independence

# Create a contingency table of two categorical variables
# The two variables: Radio and Sales
# Create a data frame with the two variables
data <- data.frame(advertising_data$Sales, advertising_data$Radio)
# Create a contingency table
contingency_table <- table(data)
# Perform the chi-square test of independence
chi_square_test <- chisq.test(contingency_table)
# Print the test result
print(chi_square_test)

#Linear Regression Model Design

# Fit a linear regression model
lm_model <- lm(Sales ~ TV, data = TV_vs_Sales)

# Create a scatter plot
plot(TV_vs_Sales$TV, TV_vs_Sales$Sales,
     xlab = "TV Advertisements", ylab = "Sales",
     main = "Scatter Plot with Linear Regression Line")
# Add the regression line to the scatter plot
abline(lm_model, col = "red", lwd = 2)

# Fit a linear regression model
lm_model <- lm(advertising_data$Sales ~ advertising_data$TV + advertising_data$Radio + advertising_data$Newspaper, data = advertising_data)

#Question 13: Predict the Sales by using TV, Radio and Newspaper advertising budgets, calculate the residual error, p-value and F-statistic

# Print the model summary
summary(lm_model)

#Question 14: Print all the predicted values. Ad budgets, TV: $230.1K, Radio: $37.8K, Newspaper: $69.2K residual error of the predicted sales?

# Specify the values for the three variables
TV_value <- 230.1 
Radio_value <- 37.8
Newspaper_value <- 69.2
# Create a new data frame with the values for prediction
new_data <- data.frame(TV = TV_value, Radio = Radio_value, Newspaper = Newspaper_value)
# Predict the outcome variable for the new data using the linear regression model
predicted_value <- predict(lm_model, newdata = new_data)#21.22097
# Print the predicted value
cat("Predicted value:", predicted_value, "\n")
# Calculate the residual error
observed_value <- 22.1 # Specify the observed value
residual <- observed_value - predicted_value
# Print the residual error
cat("Residual:", residual, "\n") #0.8790279

#Question 15: Claim: There is a significant difference in the mean values of the 3 types of advertisements.

#install dplyr library
install.packages("dplyr")
# Load the necessary packages
library(dplyr)
# Perform one-way ANOVA
anova_result <- aov(advertising_data$Sales ~ advertising_data$TV * advertising_data$Radio * advertising_data$Newspaper, data = advertising_data )
# Check the ANOVA table
summary(anova_result)
# Check the p-value
p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
# Compare p-value with significance level to make a decision
alpha <- 0.05
if (p_value <= alpha) {
  cat("There is a significant difference in the mean values of the groups.")
} else {
  cat("There is no significant difference in the mean values of the groups.")
}

#Question 16

# Load the necessary packages
library(dplyr)
# Perform ANOVA with multiple numerical variables
anova_result <- aov(advertising_data$Sales ~ advertising_data$TV + advertising_data$Radio + advertising_data$Newspaper, data = advertising_data)
# Check the ANOVA table
summary(anova_result)
# Check the p-value
p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
# Compare p-value with significance level to make a decision
alpha <- 0.05
if (p_value <= alpha) {
  cat("There is a significant difference in the means of the groups based on the numerical variables.")
} else {
  cat("There is no significant difference in the means of the groups based on the numerical variables.")
}

# Perform two-way ANOVA
anova_result <- aov(advertising_data$Sales ~ advertising_data$TV * advertising_data$Radio * advertising_data$Newspaper, data = advertising_data)
# Check the ANOVA table
summary(anova_result)
# Check the p-value
p_value <- summary(anova_result)[[1]]$"Pr(>F)"
# Compare p-value with significance level to make a decision
alpha <- 0.05
if (any(p_value <= alpha)) {
  cat("There is a significant effect of the TV advertisement Budget or the Radio advertisement Budget, or the Newspaper advertisement Budget or their interaction on sales.")
} else {
  cat("There is no significant effect of the TV advertisement Budget or the Radio advertisement Budget, or the Newspaper advertisement Budget or their interaction on sales.")
}

# Applications of Nonparametric Tests

#Question 17: Wilcoxon Signed-Rank Test Question: Is there a significant difference in the type of advertisements before and after a certain intervention?
#install stats library
install.packages("stats")
# Load the necessary packages
library(stats)
# Perform Wilcoxon signed-rank test
wilcox_result <- wilcox.test(advertising_data$Radio, advertising_data$TV, paired = TRUE)
# Check the test statistic and p-value
test_statistic <- wilcox_result$statistic
p_value <- wilcox_result$p.value
# Compare p-value with significance level to make a decision
alpha <- 0.05
if (p_value <= alpha) {
  cat("There is a significant difference in the sales before and after the intervention.")
} else {
  cat("There is no significant difference in the sales before and after the intervention.")
}

#Question 18: Single Sample Sign Test: Is there a significant difference in the sample median from the hypothesized value?

# Load the necessary packages

library(stats)
# Perform Single Sample Sign Test
sign_test_result <- binom.test(sum(data$Signs == "+"), n = length(SampleSales), p = 0.5, alternative = "two.sided")
# Check the test statistic and p-value
test_statistic <- sign_test_result$statistic
p_value <- sign_test_result$p.value
# Compare p-value with significance level to make a decision
alpha <- 0.05
if (p_value <= alpha) {
  cat("There is a significant difference in the sample median from the hypothesized value.")
} else {
  cat("There is no significant difference in the sample median from the hypothesized value.")
}




