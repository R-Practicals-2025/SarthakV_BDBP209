## EX I

# Question 1
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)

# Create the barplot and store midpoints of bars
bar_positions <- barplot(
  means,
  names.arg = c("A", "B", "C"),
  col = "grey",
  ylim = c(0, max(means + stderr) + 2),
  main = "Errors on bar plot"
)

# Add error bars using arrows()
arrows(
  x0 = bar_positions, y0 = means + stderr,
  x1 = bar_positions, y1 = means - stderr,
  angle = 90, code = 3, length = 0.06, col = "red"
)
# This line draws vertical error bars centered on each bar, extending from the
# mean + stderr to mean - stderr, with horizontal caps at both ends, all in red.
# Perfect for showing uncertainty in measurements!


# Question 2
x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Create the barplot and store midpoints of bars
plot(x,y,pch=16,xlab ="concentration",ylab = "optical activity",main = "Error bars on data points",ylim = c(min(y - errors) - 1, max(y + errors) + 1))

# Add vertical error bars using arrows()
arrows(
  x0 = x, y0 = y + errors,
  x1 = x, y1 = y - errors,
  angle = 90, code = 3, length = 0.05, col = "blue"
)


# Question 3
x = c(10,20,30,40,50,60,70,80,90,100)
y = c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x,y)
cor(x,y)
cor(longley) #multivariate data


## Ex II
 
## Q1) One sample Z-test
 
##a)
##custom function

one_sample_Ztest <- function(x,sigma,muzero,alpha,null){
  
  #compute z-stat
  n <- length(x)
  z_stat <- (mean(x) - muzero) / (sigma / sqrt(n))
  # print(z_stat) #2.659793
  
  #compute p-value based on the null hypothesis type
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(z_stat))) #Total p-value 
    conclusion <- ifelse(p_val < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(z_stat)
    conclusion <- ifelse(p_val < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else if (null == "more_than_or_equal") {
    p_val <- pnorm(z_stat)
    conclusion <- ifelse(p_val < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  } else {
    stop("Invalid 'null' argument. Must be 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  
  # return p value corresponding to alpha = 0.025 i.e. half of the 0.05   
  # return z-value, p-value, and conclusion 
  return(list(z_value = z_stat, p_value = p_val, conclusion = conclusion))
}



##b)
##inputs
x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
      137.4, 145.6, 135.6, 135.4, 121.5)
sigma <- 14.5
muzero <- 124.6
alpha <- 0.05 #significance level
null <- c("equal", "less_than_or_equal", "more_than_or_equal")

#expected output
#p-value, z-value and stat conclusion

#check if the data comes from a normal distribution
qqnorm(x)
qqline(x)

#result -> the data points lie close to the line, therefore the points comes
#from a normal distribution






## Q2) One sample t-test
#a)
# One-sample t-test custom function
one_sample_t_test <- function(x, muzero, alpha, null) {
  
  n <- length(x)               # sample size
  df <- n - 1                  # degrees of freedom
  sample_mean <- mean(x)       # sample mean
  sample_sd <- sd(x)           # sample standard deviation
  
  # T-statistic formula
  T_stat <- (sample_mean - muzero) / (sample_sd / sqrt(n))
  
  # Calculate p-value based on the alternative hypothesis
  if (null == "equal") {
    p_val <- 2 * (1 - pt(abs(T_stat), df))  # two-tailed
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pt(T_stat, df)             # right-tailed
  } else if (null == "more_than_or_equal") {
    p_val <- pt(T_stat, df)                 # left-tailed
  } else {
    stop("Invalid 'null' argument. Must be one of: 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Conclusion
  conclusion <- ifelse(p_val < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  
  # Return results
  return(list(
    t_value = round(T_stat, 4),
    p_value = round(p_val, 4),
    conclusion = conclusion
  ))
}


#b)
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

# Hypothesis values
muzero <- 100     # Null hypothesis mean
alpha <- 0.05     # Significance level
null <- "equal"   # Testing H0: mu = muzero (two-tailed)

# Check normality (visually)
qqnorm(x)
qqline(x)

# Run t-test
result <- one_sample_t_test(x, muzero, alpha, null)

# Print result
print(result)

# Run t-test
result <- one_sample_t_test(x, muzero, alpha, "equal")

# Print result
print(result)


## Q3) One sample proportion test

# binom.test(x,n,p,alternative)
# prop.test(x,n,p,alternative,correct)


#data
x = 710
n = 2600
p = 0.25 #H0
alternative = "greater"
#H1 (alternative hypothesis): true proportion > 0.25 #right tailed test

#binom.test()
results <- binom.test(x,n,p,alternative)
print("Binomial Test Results:")
print(results)



#prop.test()
correct <- FALSE
results <- prop.test(x,n,p,alternative,correct,conf.level=0.95)
print("proportion test")
print(result)


##Q4
#a)
# Custom function to perform the one-sample variance test
one_sample_variance_test <- function(x, test_sigma, alpha) {
  # Sample size
  n <- length(x)
  
  # Compute sample variance
  s2 <- var(x)
  
  # Compute the Chi-squared test statistic
  chi_squared_stat <- (n - 1) * s2 / (test_sigma^2)
  
  # Compute the critical values for Chi-squared distribution
  lower_limit <- qchisq(alpha / 2, df = n - 1)  # Lower critical value
  upper_limit <- qchisq(1 - alpha / 2, df = n - 1)  # Upper critical value
  
  # Statistical conclusion
  conclusion <- ifelse(chi_squared_stat < lower_limit | chi_squared_stat > upper_limit, 
                       "Reject null hypothesis", 
                       "Fail to reject null hypothesis")
  
  # Return results
  return(list(chi_squared_stat = chi_squared_stat, 
              p_value = 2 * min(pchisq(chi_squared_stat, df = n - 1), 
                                1 - pchisq(chi_squared_stat, df = n - 1)), 
              conclusion = conclusion))
}

# Data 
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Hypothesized population standard deviation
test_sigma <- 29

# Significance level
alpha <- 0.05

# Perform the test
result <- one_sample_variance_test(x, test_sigma, alpha)

# Print the results
print(result)


##Q5
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Perform one-sided Wilcoxon signed-rank test (H0: median >= 160, HA: median < 160)
wilcox_result <- wilcox.test(x,
                             alternative = "less",
                             mu = 160,
                             conf.int = TRUE,
                             conf.level = 0.95,
                             correct = FALSE)

print("Wilocox result")
print(wilcox_result)



##Exercise III

##Question 1
#a)
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null = "greater_than_or_equal") {
  # Sample sizes
  n1 <- length(x1)
  n2 <- length(x2)
  
  # Sample means
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z-statistic
  Z <- (mean1 - mean2) / se
  
  # Compute p-value and conclusion
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(Z)))
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 = μ2", "Fail to reject H0: μ1 = μ2")
  } else if (null == "greater_than_or_equal") {
    p_val <- pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≥ μ2", "Fail to reject H0: μ1 ≥ μ2")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≤ μ2", "Fail to reject H0: μ1 ≤ μ2")
  } else {
    stop("Invalid 'null' argument. Use 'equal', 'greater_than_or_equal', or 'less_than_or_equal'.")
  }
  
  return(list(
    mean1 = mean1,
    mean2 = mean2,
    z_value = Z,
    p_value = p_val,
    conclusion = conclusion
  ))
}

#b)
# Read the data (assuming two columns)
# data <- read.table("~/R/two-sample.dat", header = FALSE)
x1 = c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
        246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
        231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
        218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)


x2 = c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
        206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
        178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
        267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)


# Known population standard deviations
sigma_x1 <- 24.6
sigma_x2 <- 27.8
alpha <- 0.05

# Perform the test
result <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null = "greater_than_or_equal")
print(result)


##question2

#a)
# Data
Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

# Welch's two-sample t-test (default in R)
welch_test <- t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

# Output
print(welch_test)


#b)
# Data
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Paired t-test
paired_test <- t.test(data_before, data_after, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# Output
print(paired_test)



#Question3
#a)
# Number of successes (people who use antibiotics)
x <- c(520, 550)

# Number of trials (total surveyed)
n <- c(600, 600)

# Perform the two-sample proportion test
output <- prop.test(x, n, alternative = "two.sided", correct = TRUE)

print(output)


#b)
# Create a 2x2 contingency matrix
x <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = FALSE)

# Perform Fisher's exact test
output <- fisher.test(x, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)
print(output)


##Question4 --- Two Sample variance test

#a)

two_sample_variance_test <- function(x,y,alpha){
  var_x <- var(x)
  var_y <- var(y)
  n_x <- length(x)
  n_y <- length(y)
  
  # F-statistic (larger variance / smaller variance)
  F_statistic <- var_x / var_y
  
  df1 <- n_x - 1
  df2 <- n_y - 1
  
  # p-value for two-tailed test
  if (F_statistic > 1) {
    p_value <- 2 * (1 - pf(F_statistic, df1, df2))
  } else {
    p_value <- 2 * pf(F_statistic, df1, df2)
  }
  
  # Decision
  if (p_value < alpha) {
    conclusion <- "Reject H0: Variances are significantly different."
  } else {
    conclusion <- "Fail to reject H0: Variances are not significantly different."
  }
  
  list(F_statistic = F_statistic, p_value = p_value, conclusion = conclusion)
}

#b)
x <- c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8,
       1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5, 1011.1, 991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

output <- two_sample_variance_test(x, y, alpha = 0.05)
print(output)


##Question 5

Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

output <- wilcox.test(Pre_therapy, Post_therapy,
            alternative = "greater", 
            conf.level = 0.95, 
            paired = TRUE)

print(output)


##Question 6
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

output <- wilcox.test(placebo, drug,
            alternative = "less", 
            conf.level = 0.95, 
            paired = FALSE)

print(output)


##Question 7 

Group1 <- c(220, 214, 203, 184, 186, 200, 165)
Group2 <- c(262, 193, 225, 200, 164, 266, 179)
Group3 <- c(272, 192, 190, 208, 231, 235, 141)
Group4 <- c(190, 255, 247, 278, 230, 269, 289)

# Combine all the values as x
x <- c(Group1, Group2, Group3, Group4)

# Create the group labels as y
y <- factor(rep(1:4, each = 7))
# print(y)

# Perform Kruskal-Wallis test
output <- kruskal.test(x ~ y)
print(output)


##Question 8

#custom function
chi_square_gof_test <- function(observed, expected, alpha) {
  test_statistic <- sum((observed - expected)^2 / expected)
  
  df <- length(observed) - 1
  
  critical_value <- qchisq(1 - alpha, df)
  p_value <- 1 - pchisq(test_statistic, df)
  
  if (test_statistic > critical_value) {
    conclusion <- "Reject H0: Observed frequencies differ from expected."
  } else {
    conclusion <- "Fail to reject H0: Observed frequencies match expected."
  }
  
  list(Chi_square_statistic = test_statistic, 
       p_value = p_value, 
       conclusion = conclusion)
}

Observed <- c(32, 82, 77, 49)
Expected <- c(40, 80, 80, 40)

output <- chi_square_gof_test(Observed, Expected, alpha = 0.05)
print(output)


#EXERCISE 4

##Question 1
##"Anova test on people on the Titanic ship"

##a)
#load the data
titanicdata <- read.csv("~/R/R-dataset/titanic.csv")
print(nrow(Titanicdata))
print(colnames(Titanicdata))

# Plot histograms

par(mfrow = c(3,1))

# Histogram for 1st Class
hist(titanicdata$age[titanicdata$passenger_class == '1st'],
     main = "Histogram of Age: 1st Class",
     xlab = "Age",
     breaks = 50,
     col = "lightblue")

# Histogram for 2nd Class
hist(titanicdata$age[titanicdata$passenger_class == '2nd'],
     main = "Histogram of Age: 2nd Class",
     xlab = "Age",
     breaks = 50,
     col = "lightgreen")

# Histogram for 3rd Class
hist(titanicdata$age[titanicdata$passenger_class == '3rd'],
     main = "Histogram of Age: 3rd Class",
     xlab = "Age",
     breaks = 50,
     col = "lightpink")


#b)
# Mean age by passenger_class
group_mean <- tapply(titanicdata$age, titanicdata$passenger_class, mean, na.rm = TRUE)

# Standard deviation of age by passenger_class
group_sd <- tapply(titanicdata$age, titanicdata$passenger_class, sd, na.rm = TRUE)

# Display results
data.frame(passenger_class = names(group_mean),
           group_mean = group_mean,
           group_sd = group_sd)

#c)
# Fit linear model
lmresults <- lm(age ~ passenger_class, data = titanicdata)


# Perform ANOVA
anova(lmresults)

#interpretation
# Since the p-value is much smaller than 0.05, we reject the null hypothesis.
# Conclusion: There is a significant difference in the mean ages among the three passenger classes.


#D) Tukey-Kramer's test

TukeyHSD(aov(lmresults))

# 
# The Tukey-Kramer test shows that the mean age is significantly different between all pairs of passenger classes.
# In particular:
#   
# 1st class passengers are older than 2nd and 3rd class.
# 
# 2nd class passengers are older than 3rd class passengers.


#E) Kruskal - Wallis Test

kruskal.test(age~passenger_class, data=titanicdata)
# 
# The Kruskal-Wallis test also gives a very small p-value (< 0.05), so we again reject the null hypothesis.
# Conclusion: There are significant differences in the age distributions across the passenger classes.
#            




### Question 2

# a)

# Load the data

cuckooData <- read.csv("~/R/R-dataset/cuckooeggs.csv")
str(cuckooData)
dim(cuckooData)
head(cuckooData)
len_of_unique<-length(unique(cuckooData$host_species))
# Plot histograms for each host species
par(mfrow = c(len_of_unique/2, 2))  # Set layout

for (species in unique(cuckooData$host_species)) {
  hist(cuckooData$egg_length[cuckooData$host_species == species],
       main = species, xlab = "Egg Length", col = "lightblue",breaks=10)
}

par(mfrow = c(1,1)) 

# (b)

# Group the data by host species
group_cuckoo_byspecies <- group_by(cuckooData, host_species)

# Summarize mean and standard deviation by species
summary_stats <- summarise(group_cuckoo_byspecies, 
                           group_mean = mean(egg_length, na.rm = TRUE),
                           group_sd = sd(egg_length, na.rm = TRUE))

# Print the summary statistics
print(summary_stats)

# Comment about variances
cat("The standard deviations across host species classes are similar.\n")
cat("Thus, the assumption of equal variance needed for ANOVA is satisfied.\n")

# (c)
# Fit ANOVA model
anova_model <- aov(egg_length ~ host_species, data = cuckooData)
summary(anova_model)
# Conclusion:
# If p-value < 0.05 → Means are different across species.

# (d)
# Perform Tukey's HSD (Honestly Significant Difference) test
tukey_result <- TukeyHSD(anova_model)
# Print results
print(tukey_result)


# Significant differences (p < 0.05):
# Meadow Pipit vs Hedge Sparrow (p = 0.043) → Significant
# Wren vs Hedge Sparrow (p ≈ 0.0000006) → Highly Significant
# Tree Pipit vs Meadow Pipit (p ≈ 0.047) → Significant
# Wren vs Meadow Pipit (p ≈ 0.000486) → Highly Significant
# Wren vs Pied Wagtail (p ≈ 0.0000070) → Highly Significant
# Wren vs Robin (p ≈ 0.000318) → Highly Significant
# Wren vs Tree Pipit (p ≈ 0.0000006) → Highly Significant

# Not significant (p > 0.05):
# Pied Wagtail vs Hedge Sparrow
# Robin vs Hedge Sparrow
# Tree Pipit vs Hedge Sparrow
# Pied Wagtail vs Meadow Pipit
# Robin vs Meadow Pipit
# Robin vs Pied Wagtail
# Tree Pipit vs Pied Wagtail
# Tree Pipit vs Robin

# Question 3
# Read the data
malariaData <- read.csv("/home/ibab/R/Lab14/malaria vs maize.csv")

# Quick look
print(head(malariaData))

# (a) Multiple histograms for original incidence rates
# Subset data
low <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "Low"]
medium <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "Medium"]
high <- malariaData$incidence_rate_per_ten_thousand[malariaData$maize_yield == "High"]

# Plot histograms
par(mfrow = c(1, 3))  # 1 row, 3 columns
hist(low, 
     main = "Low Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightblue", 
     breaks = 10)

hist(medium, 
     main = "Medium Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightgreen", 
     breaks = 10)

hist(high, 
     main = "High Maize Yield", 
     xlab = "Malaria Incidence Rate", 
     col = "lightpink", 
     breaks = 10)

# Reset layout
par(mfrow = c(1,1))

# (b) Calculate standard deviations
sd_low <- sd(low)
sd_medium <- sd(medium)
sd_high <- sd(high)

cat("Standard deviations (original data):\n")
cat("Low:", sd_low, "\n")
cat("Medium:", sd_medium, "\n")
cat("High:", sd_high, "\n")

# (b) Check ANOVA assumptions
# If SDs are very different, variance homogeneity assumption is violated!

# (c) Log-transform incidence rates
malariaData$log_incidence <- log(malariaData$incidence_rate_per_ten_thousand)

# Subset log data
low_log <- malariaData$log_incidence[malariaData$maize_yield == "Low"]
medium_log <- malariaData$log_incidence[malariaData$maize_yield == "Medium"]
high_log <- malariaData$log_incidence[malariaData$maize_yield == "High"]

# Plot histograms for log-transformed data
par(mfrow = c(1, 3))
hist(low_log, 
     main = "Low Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightblue", 
     breaks = 10)

hist(medium_log, 
     main = "Medium Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightgreen", 
     breaks = 10)

hist(high_log, 
     main = "High Maize Yield (Log)", 
     xlab = "Log Malaria Incidence Rate", 
     col = "lightpink", 
     breaks = 10)

# Reset layout
par(mfrow = c(1,1))

# (c) Calculate standard deviations of log data
sd_low_log <- sd(low_log)
sd_medium_log <- sd(medium_log)
sd_high_log <- sd(high_log)

cat("Standard deviations (log-transformed data):\n")
cat("Low (log):", sd_low_log, "\n")
cat("Medium (log):", sd_medium_log, "\n")
cat("High (log):", sd_high_log, "\n")

# (d) Test association
# ANOVA on log-transformed data
anova_result <- aov(log_incidence ~ maize_yield, data = malariaData)
summary(anova_result)


# Question 4

# (a)

# Load the data
circadianData <- read.csv("/home/ibab/R/Lab14/circadian mutant health.csv")
# Subset data for each genotype
tim01_data <- circadianData$days_to_death[circadianData$genotype == "tim01"]
rescued_data <- circadianData$days_to_death[circadianData$genotype == "tim01 (rescued)"]
wildtype_data <- circadianData$days_to_death[circadianData$genotype == "wild type"]

# Set up 1 row 3 columns layout for plots
par(mfrow = c(1, 3))

# Plot histograms
hist(tim01_data, 
     main = "tim01 Mutant", 
     xlab = "Days to Death", 
     col = "red", 
     breaks = 10, 
     xlim = c(0, 22))

hist(rescued_data, 
     main = "tim01 Rescued", 
     xlab = "Days to Death", 
     col = "blue", 
     breaks = 10, 
     xlim = c(0, 22))

hist(wildtype_data, 
     main = "Wild Type", 
     xlab = "Days to Death", 
     col = "green", 
     breaks = 10, 
     xlim = c(0, 22))

# Reset layout
par(mfrow = c(1,1))


# (b)
kruskal.test(days_to_death ~ genotype, data = circadianData)
# If the p-value is < 0.05, it means there is a statistically significant difference in median survival times between at least two groups.




