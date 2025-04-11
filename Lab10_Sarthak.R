#LAB10

###EXERCISE I
##Sampling, permutations and combinations

#1) Sampling from a vector

x<-seq(1,100)
s <- sample(x,10) #sampling without replacement, therefore all the values will be unique.
print(s)

t <- sample(x,10,replace=TRUE) #sampling with replacement, values can repeat in 't' itself
print(t)

#using gtools pkg for permutation and combinations

install.packages("gtools")
library(gtools)

#permutations
x <- c("A","B","C","D")

per_rep_on <- permutations(n=length(x), r=3, v=x, repeats.allowed=TRUE) #with repeats of element in the row eg:AAA
#v in syntax stands for vector to be used for permutation
per_rep_off <- permutations(n=length(x), r=3, v=x, repeats.allowed=FALSE) #without repeats of element in the row itself
print(per_rep_on)
print(per_rep_off)

#combinations

comb_rep <- combinations(n=length(x), r=3, v=x,repeats.allowed = TRUE) #with repeats
comb_rep_off <- combinations(n=length(x), r=3, v=x,repeats.allowed = FALSE) #without repeats
print(comb_rep)
print(comb_rep_off)


####EXERCISE II

###DISTRIBUTIONS

##1) BINOMIAL DISTRIBUTION

# 1) Binomial distribution parameters
n <- 10   # Total number of trials
p <- 0.4  # Probability of success in a single trial
m <- 3    # Number of successes

# (a) Probability of exactly 3 successes
pdf_bin <- dbinom(m, n, p)
print(paste("P(X = 3):", pdf_bin))

# (b) Cumulative probability P(X ≤ 3)
cpdf_bin <- pbinom(m, n, p)
print(paste("P(X ≤ 3):", cpdf_bin))

# (c) Find m where cumulative probability = 0.8
cum_prob <- 0.8
m_val <- qbinom(cum_prob, n, p)
print(paste("m corresponding to P(X ≤ m) ≥ 0.8:", m_val))

# (d) 5 random samples from Binomial(n=10, p=0.4)
random_samples <- rbinom(5, n, p)
print("Random samples from Binomial(n=10, p=0.4):")
print(random_samples)

# (e) Plot Probability Density Function (PDF) for p=0.4 and p=0.7
p2 <- 0.7  # Second probability value for comparison
x <- 0:n  # Possible outcomes from 0 to n

# Compute binomial probabilities
pdf_p1 <- dbinom(x, n, p)
pdf_p2 <- dbinom(x, n, p2)

# Plot the PDFs
plot(x, pdf_p1, type = "b", col = "blue", pch = 19, lty = 1, ylim = c(0, max(pdf_p1, pdf_p2)), 
     xlab = "Number of Successes", ylab = "Probability", main = "Binomial PDFs for p=0.4 and p=0.7")
lines(x, pdf_p2, type = "b", col = "red", pch = 19, lty = 2)

# Add legend
legend("topright", legend = c("p = 0.4", "p = 0.7"), col = c("blue", "red"), pch = 19, lty = c(1, 2))


# (f) Generate 100 and 10,000 points from Binomial(n=10, p=0.4)
set.seed(123)  # Ensure reproducibility
sample_100 <- rbinom(100, n, p)
sample_10000 <- rbinom(10000, n, p)
print(sample_100)

# Create frequency tables
freq_100 <- table(sample_100)
freq_10000 <- table(sample_10000)
print(freq_100)

# Set up a 2x1 grid for bar plots
par(mfrow = c(2, 1))

# Plot frequency distributions
barplot(freq_100, col = "skyblue", main = "Frequency Distribution (100 Samples)", 
        xlab = "Number of Successes", ylab = "Frequency")
barplot(freq_10000, col = "brown", main = "Frequency Distribution (10,000 Samples)", 
        xlab = "Number of Successes", ylab = "Frequency")




##2) HYPERGEOMETRIC DISTRIBUTION


#a) histogram type plot of hypergeometric pdf

# Set parameters
N <- 100  # Total population size
K <- 70   # Total number of success states in population
n <- 12   # Number of draws
k_values <- 0:n  # Possible values of k (successes in draws)

# Compute the probability mass function (PMF)
pdf_hyper <- dhyper(k_values, K, N - K, n)  

# Plot the histogram-type bar plot
barplot(pdf_hyper, names.arg = k_values, col = "skyblue", border = "black",
        main = "Hypergeometric Distribution (N=100, K=70, n=12)",
        xlab = "Number of Successes (k)", ylab = "Probability Mass Function")

# Add text within the plot window
text(5, max(pdf_hyper) * 0.8, 
     labels = paste("N =", N, "\nK =", K, "\nn =", n), 
     col = "brown", cex = 1)


#b) Compute the cumulative probability up to x=10 and print the result after rounding
# off to 3 decimal places.


# Compute cumulative probability P(X ≤ 10)
cum_prob <- phyper(x, K, N - K, n)

# Round to 3 decimal places
cum_prob_rounded <- round(cum_prob, 3)

# Print result
print(cum_prob_rounded)


#c) Obtain the x value corresponding to a cumulative probability value of 0.9.

cum_prob <- 0.9  # Cumulative probability

# Find the x value for which P(X ≤ x) = 0.9
x_val <- qhyper(cum_prob, K, N - K, n)

# Print result
print(x_val)

#d) Sample 5 points randomly from this distribution and print these with two significant
# digits.

# Sample 5 random values
random_samples <- rhyper(5, K, N - K, n)

# Print with two significant digits
print(signif(random_samples, digits = 2))




## 3) Geometric Distribution

# (a) Plot 2 probability mass functions (PMFs) in a 1x2 grid

# Define probabilities
p1 <- 0.3
p2 <- 0.8

# Define trial numbers (m values)
m <- 1:15  # First success occurs between 1 and 15 trials

# Compute PMF values
pmf_p1 <- dgeom(m - 1, prob = p1)  # Adjusting for R's zero-based index
pmf_p2 <- dgeom(m - 1, prob = p2)

# Set up a 1x2 grid for plotting
par(mfrow = c(1, 2))  
par(mar = c(4, 4, 2, 1))  # Adjust margins (bottom, left, top, right)

# Plot for p=0.3
barplot(pmf_p1, names.arg = m, col = "skyblue", main = "Geometric PMF (p=0.3)",
        xlab = "Trial Number", ylab = "Probability")

# Plot for p=0.8
barplot(pmf_p2, names.arg = m, col = "brown", main = "Geometric PMF (p=0.8)",
        xlab = "Trial Number", ylab = "Probability")

# (b) Compute cumulative probability P(X ≤ 4)
cum_prob_4 <- pgeom(4 - 1, prob = 0.3)  # Adjusting for zero-based indexing
print(paste("Cumulative probability P(X ≤ 4) =", round(cum_prob_4, 3)))

# (c) Compute the trial number where CDF = 0.2
m_val <- qgeom(0.2, prob = 0.3) + 1  # Convert from zero-based to one-based indexing
print(paste("Trial number where cumulative probability is 0.2:", m_val))

# (d) Generate 6 random deviates from Geometric(p=0.4)
set.seed(123)  # Ensure reproducibility
random_samples <- rgeom(6, prob = 0.4) + 1  # Convert to one-based indexing
print(paste("Random samples from Geometric(0.4):", toString(random_samples)))



## (4) Negative Binomial Distribution

# (a) Compute P(Y=5) for Negative Binomial with r=3, p=0.3
p <- 0.3  # Probability of success
r <- 3    # Number of successes needed
y <- 5    # Failures before achieving r successes

prob_nb <- dnbinom(y, size=r, prob=p)  # Negative binomial PMF
print(paste("P(Y=5) =", round(prob_nb, 4)))

# (b) Compute cumulative probability P(Y ≤ 5)
cum_prob_5 <- pnbinom(y, size=r, prob=p)
print(paste("P(Y ≤ 5) =", round(cum_prob_5, 4)))

# (c) Find the median (y value where CDF = 0.5)
median_y <- qnbinom(0.5, size=r, prob=p)
print(paste("Median y value:", median_y))

# (d) Generate 4 random samples from Negative Binomial(r=3, p=0.3)
set.seed(123)  # Ensure reproducibility
random_samples_nb <- rnbinom(4, size=r, prob=p)
print(paste("Random samples:", toString(random_samples_nb)))

# (e) Plot the Negative Binomial distribution (r=10, p=0.3)
r <- 10  # Change r for the plot
y_values <- 0:30  # Range of failures before r successes
nb_pmf <- dnbinom(y_values, size=r, prob=p)

barplot(nb_pmf, names.arg=y_values, col="blue", main="Negative Binomial Distribution (r=10, p=0.3)",
        xlab="Number of Failures (y)", ylab="Probability")

# (f) Generate a histogram of 10,000 random deviates
set.seed(123)
sample_nb <- rnbinom(10000, size=r, prob=p)
hist(sample_nb, breaks=30, col="orange", main="Histogram of Negative Binomial Deviates (r=10, p=0.3)",
     xlab="Number of Failures (y)", ylab="Frequency", probability=TRUE)



## (5) Poisson Distribution

# # (a) Compute P(m=7) for Poisson(λ=10)
# lambda <- 10  # Mean number of events
# m <- 7        # Observed count
# 
# poisson_prob <- dpois(m, lambda)
# print(paste("P(m=7) =", round(poisson_prob, 4)))
# 
# # (b) Compute cumulative probability P(m ≤ 7)
# cum_prob_pois <- ppois(m, lambda)
# print(paste("P(m ≤ 7) =", round(cum_prob_pois, 4)))
# 
# # (c) Compare Binomial and Poisson Distributions
# n <- 200:400
# p <- 0.3
# lambda_binom <- n * p  # λ = np
# 
# # Generate Binomial and Poisson distributions
# m_vals <- 0:1000
# binom_pmf <- dbinom(m_vals, n, p)
# poisson_pmf <- dpois(m_vals, lambda_binom)
# 
# # Plotting
# # par(mfrow = c(1, 2))  # 1x2 grid
# # barplot(binom_pmf, names.arg=m_vals, col="skyblue", main="Binomial Distribution (n=1000, p=0.3)",
# #         xlab="m (Success Count)", ylab="Probability")
# # lines(poisson_pmf, type='h',names.arg=m_vals, col="red", main="Poisson Distribution (λ=300)",
# #         xlab="m (Event Count)", ylab="Probability")
# # par(mfrow = c(1, 1)) 
# 
# par(mfrow = c(1, 2))  # 1x2 grid
# plot(binom_pmf,type="h", names.arg=m_vals, col="skyblue", main="Binomial Distribution (n=1000, p=0.3)",
#         xlab="m (Success Count)", ylab="Probability")
# lines(poisson_pmf, type='h',names.arg=m_vals, col="red", main="Poisson Distribution (λ=300)",
#       xlab="m (Event Count)", ylab="Probability")
# # par(mfrow = c(1, 1)) 

lambda <- 10  # Mean number of events
m <- 7        # Observed count

# (a) Compute P(m=7) for Poisson(λ=10)
poisson_prob <- dpois(m, lambda)
print(paste("P(m=7) =", round(poisson_prob, 4)))

# (b) Compute cumulative probability P(m ≤ 7)
cum_prob_pois <- ppois(m, lambda)
print(paste("P(m ≤ 7) =", round(cum_prob_pois, 4)))

# (c) Compare Binomial and Poisson Distributions
n <- 10000  # Large n for binomial
# p <- 0.3
p <- 0.01
lambda_binom <- n * p  # λ = np

# Define m values (x values)
m_vals <- 0:n

# Generate Binomial and Poisson distributions
binom_pmf <- dbinom(m_vals, n, p)
poisson_pmf <- dpois(m_vals, lambda_binom)

# Plot Binomial PMF
plot(m_vals, binom_pmf, type="h", col="skyblue", lwd=2,
     main="Comparison of Binomial & Poisson Distributions",
     xlab="m (Event Count)", ylab="Probability",ylim =range(c(binom_pmf,poisson_pmf)),xlim=range(c(0,200)))

#Overlay Poisson PMF
lines(m_vals, poisson_pmf, type="h", col="red", lwd=2)

# #legend
# legend("topright", legend=c("Binomial (n=1000, p=0.3)", "Poisson (λ=300)"),
#        col=c("skyblue", "red"), lwd=2)


# (d) Find quantile for cumulative probability = 0.22 (λ = 10)
quantile_pois <- qpois(0.22, lambda)
print(paste("Quantile (0.22 CDF):", quantile_pois))

# (e) Generate histogram of 10,000 Poisson deviates (λ=9)
set.seed(123)
sample_pois <- rpois(10000, lambda=9)
hist(sample_pois, breaks=30, col="purple", main="Histogram of Poisson Deviates (λ=9)",
     xlab="Event Count (m)", ylab="Frequency", probability=TRUE)

## 6) Gaussian Distribution

# (a) Compute and print the unit normal PDF value for µ = 12 and σ = 2.

mu = 12
sigma = 2
x <- mu

pdf_value <- dnorm(x,mean = mu,sd=sigma)
print(paste("PDF value at X=",mu,":",round(pdf_value,4)))

# (b) Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-
#     CPDF(Z=-2)?

z <- 2
cpdf_z <- pnorm(z)
cpdf_neg_z <- pnorm(-z)

print(paste("CPDF at z=",cpdf_z,":",round(cpdf_z,4)))
print(paste("CPDF at negative z=",cpdf_neg_z,":",round(cpdf_neg_z,4)))
print(paste("Checking if CPDF and neg CPDF is same or not :",cpdf_z == (1 - cpdf_neg_z)))

# (c) Plot a unit normal curve for the above parameters with X range of ±4σ and add a
# text box to the plot showing the parameter symbols and their values.


# (c) Plot a normal curve for μ = 12, σ = 2 with range ±4σ
x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length.out=100)
y_vals <- dnorm(x_vals, mean=mu, sd=sigma)

plot(x_vals, y_vals, type="l", col="blue", lwd=2, main="Normal Distribution",
     xlab="X", ylab="Density")

text(mu, max(y_vals) * 0.9, labels=paste("μ =", mu, "\nσ =", sigma), col="red")

# (d) Compute the 75th quantile
q_75 <- qnorm(0.75, mean=mu, sd=sigma)
print(paste("75th Quantile:", round(q_75, 4)))

# (e) Generate 10,000 random deviates and plot histogram with normal curve overlay
set.seed(42)
samples <- rnorm(10000, mean=mu, sd=sigma)

hist(samples, breaks=50, probability=TRUE, col="lightgray", main="Histogram of Random Deviates")
curve(dnorm(x, mean=mu, sd=sigma), col="red", lwd=2, add=TRUE)

# Set parameters
n <- 100  # Increase number of trials for better normal approximation
p <- 0.5
num_samples <- 10000  # More samples give a smoother histogram

# Generate binomial samples
m <- rbinom(num_samples, n, p)

# Standardize to get W
W <- (m - n*p) / sqrt(n*p*(1-p))

# Plot histogram with density scaling
hist(W, breaks=50, probability=TRUE, col="lightblue", border="black",
     main="Normalised Binomial Histogram", xlab="W", ylab="Density")

# Overlay standard normal curve
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)



###############
# Set seed for reproducibility
#set.seed(42)

# Parameters
n <- 100
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
num_samples <- 10000

# Generate binomial samples and normalize
binom_samples <- rbinom(num_samples, n, p)
W <- (binom_samples - mu) / sigma

# Plot histogram with proper density scaling
hist(W, breaks = 50, probability = TRUE, col = "skyblue", border = "gray",
     main = "Normalized Binomial Histogram", xlab = "W", ylab = "Density")

# Add standard normal curve
curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)




#f) Normalised Binomial vs Standard Binomial
# Parameters
n <- 100 #no of trials
p <- 0.5 #probability
mu <- n * p #mean
sigma <- sqrt(n * p * (1 - p)) #std deviation

# Simulate binomial data
num_samples <- 10000 
m <- rbinom(num_samples, size = n, prob = p) #no of success in n trials

# Normalize
W <- (m - mu) / sigma 

# Plot histogram
hist(W, probability = TRUE, breaks = 30, col = "skyblue",
     main = "Normalized Binomial vs Standard Normal",
     xlab = "W", xlim = c(-4, 4))

# Overlay standard normal density
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

# Add legend
#legend("topright", legend = c("Normalized Binomial", "Standard Normal N(0,1)"),
      # col = c("skyblue", "red"), lwd = 2, fill = c("skyblue", NA), border = NA)


#g) 
lambda_vals <- c(1,10,100,1000)
par(mfrow=c(2,2))

for (lambda in lambda_vals){
  m_vals  <- 0:ceiling(lambda + 4 * sqrt(lambda)) #dynamic version to compute the event values
  poisson_pmf <- dpois(m_vals,lambda)
  z_vals <- (m_vals - lambda) / sqrt(lambda)
  normal_pmf <- dnorm(z_vals)
  
  plot(m_vals, poisson_pmf, type = 'h',col="blue",main=paste("Poisson lambda =", lambda),
       xlab="m", ylab="Probability")
  lines(m_vals,normal_pmf,col="red",lwd=2)

}

par(mfrow=c(1,1))

#interesting question, look into the ways to make the plots agree.

# Set lambda values to compare different Poisson distributions
lambda_vals <- c(1, 10, 100, 1000)

# Create a 2x2 grid of plots and adjust margins for better spacing
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Loop over each lambda value
for (lambda in lambda_vals) {
  
  # Define range of m values around the mean to capture ~99.99% of probability mass
  m_vals <- 0:ceiling(lambda + 4 * sqrt(lambda))
  print(m_vals)
  
  # Calculate the Poisson probability mass function (PMF)
  poisson_pmf <- dpois(m_vals, lambda)
  
  # Transform m-values to z-scores (standard normal units)
  z_vals <- (m_vals - lambda) / sqrt(lambda)
  
  # Convert PMF to density for fair comparison with continuous normal PDF
  poisson_density <- poisson_pmf / (1 / sqrt(lambda))  # = poisson_pmf * sqrt(lambda)
  
  # Standard normal probability density function (PDF)
  normal_pdf <- dnorm(z_vals)
  
  # Plot the normalized Poisson distribution as vertical lines ("h" type)
  plot(z_vals, poisson_density, type = "h", col = "blue",
       main = paste("Poisson vs Normal (λ =", lambda, ")"),
       xlab = "Z = (m - λ) / √λ", ylab = "Density",
       xlim = range(z_vals),
       ylim = range(c(poisson_density, normal_pdf)),
       cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
  
  # Overlay the standard normal curve
  lines(z_vals, normal_pdf, col = "red", lwd = 2)
}

# Reset plotting layout to default
par(mfrow = c(1, 1))



## (h) Correlated Normal Distributions using MASS
library(MASS)
xy <- mvrnorm(1000, mu=c(50,60), Sigma=matrix(c(4,3.7,3.7,9),2,2))
print("Variance-Covariance Matrix:")
print(var(xy))

# Extract x and y
x <- xy[,1]
y <- xy[,2]

# Scatter plot of x and y
plot(x, y, main="Scatter Plot of Correlated Variables", xlab="X", ylab="Y", col="blue")
print(paste("Var(X) =", var(x)))
print(paste("Var(Y) =", var(y)))

# Check independence: sum of variances vs variance of sum
var_sum <- var(x) + var(y)
var_combined <- var(x + y)
print(paste("Sum of Individual Variances =", round(var_sum, 4)))
print(paste("Variance of (X+Y) =", round(var_combined, 4)))
print(paste("Are they independent?", round(var_sum, 4) == round(var_combined, 4)))

# Compute covariance using correlation coefficient
cov_computed <- cor(x, y) * sqrt(var(x) * var(y))
cov_reported <- var(xy)[1,2]

print(paste("Computed Covariance =", round(cov_computed, 4)))
print(paste("Reported Covariance =", round(cov_reported, 4)))
print(paste("Do they match?", round(cov_computed, 4) == round(cov_reported, 4)))



##7) UNIFORM DISTRIBUTION

# (a) Generate 5 uniform random numbers between 0 and 1
uniform_nums <- runif(5, min = 0, max = 1)
print(uniform_nums)

# (b) Generate 5 uniform random numbers between 50 and 100
set.seed(123)
uniform_nums <- runif(5, min = 50, max = 100)
print(uniform_nums)


# (c) Generate 10,000 uniform random numbers between 1 and 2
uniform_data <- runif(10000, min = 1, max = 2)

# Plot histogram
hist(uniform_data, 
     breaks = 50,             # more breaks = smoother histogram
     col = "skyblue", 
     main = "Histogram of 10,000 Uniform(1,2) Samples", 
     xlab = "Value", 
     xlim = c(1, 2), 
     probability = TRUE)      # scales y-axis to show density

# added the theoretical density line
abline(h = 1, col = "red", lwd = 2, lty = 2)


##(8) Exponential distribution

# (a)
x <- 3
lambda <- 2
density <- dexp(x, rate = lambda)
print(density)


# (b)
p <- 0.995
quantile_val <- qexp(p, rate = lambda)
print(quantile_val)

# (c)
x_vals <- seq(0, 2, length.out = 500)
plot(x_vals, pexp(x_vals, rate = 2), type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "Cumulative Probability",
     main = "Exponential CDFs for λ = 2, 10, 100")
lines(x_vals, pexp(x_vals, rate = 10), col = "red", lwd = 2)
lines(x_vals, pexp(x_vals, rate = 100), col = "green", lwd = 2)
legend("bottomright", legend = c("λ = 2", "λ = 10", "λ = 100"),
       col = c("blue", "red", "green"), lwd = 2)


# (d)
set.seed(123)  # optional, for reproducibility
random_vals <- rexp(4, rate = 3)
print(random_vals)















##(9) Gamma Distribution

#) a) 
par(mfrow = c(1, 2))  # Set up 1 row, 2 column grid for plots
x_vals <- seq(0, 40, length.out = 500)

# Left plot: Vary α (shape), fix θ = 4
theta <- 4
alpha_vals <- c(1, 2, 3, 4)
colors <- c("black", "blue", "red", "magenta")

plot(x_vals, dgamma(x_vals, shape = alpha_vals[1], scale = theta), type = "l", col = colors[1],
     ylim = c(0, 0.12), xlab = "x", ylab = "Density", main = expression(paste("Varying ", alpha, ", ", theta, " = 4")))

for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha_vals[i], scale = theta), col = colors[i])
}
legend("topright", legend = paste("α =", alpha_vals), col = colors, lwd = 2, cex = 0.8)

# Right plot: Vary θ (scale), fix α = 4
alpha <- 4
theta_vals <- c(1, 2, 3, 4)

plot(x_vals, dgamma(x_vals, shape = alpha, scale = theta_vals[1]), type = "l", col = colors[1],
     ylim = c(0, 0.4), xlab = "x", ylab = "Density", main = expression(paste("Varying ", theta, ", ", alpha, " = 4")))

for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha, scale = theta_vals[i]), col = colors[i])
}
legend("topright", legend = paste("θ =", theta_vals), col = colors, lwd = 2)

par(mfrow = c(1, 1))  # Reset layout


#b)

pdf_value <- dgamma(6, shape = 4, scale = 1)
print(pdf_value)

#c) 

cdf_value <- pgamma(6, shape = 4, scale = 1)
print(cdf_value)

#d) 

quantile_95 <- qgamma(0.95, shape = 4, scale = 1)
print(quantile_95)

#e) 

set.seed(123)
gamma_samples <- rgamma(10000, shape = 4, scale = 1)

hist(gamma_samples, breaks = 50, col = "skyblue",
     main = "Histogram of 10,000 Gamma(α=4, θ=1) Samples",
     xlab = "x", probability = TRUE)

# Overlay the theoretical PDF
curve(dgamma(x, shape = 4, scale = 1), add = TRUE, col = "red", lwd = 2)


# Ex 10 - Chi square distribution 

# (a) Plot χ² distribution for df = 2, 3, 5, 10
x_vals <- seq(0, 30, length.out = 1000)
df_vals <- c(2, 3, 5, 10)
colors <- c("black", "blue", "red", "magenta")

plot(x_vals, dchisq(x_vals, df = 2), type = "l", col = colors[1],
     main = "Chi-square PDFs", xlab = "x", ylab = "Density", ylim = c(0, 0.3))

for (i in 2:4) {
  lines(x_vals, dchisq(x_vals, df = df_vals[i]), col = colors[i])
}

legend("topright", legend = paste("df =", df_vals), col = colors, lwd = 2)

# (b) Probability density at x = 6, df = 5
pdf_val <- dchisq(6, df = 5)
print(paste("PDF at x = 6, df = 5:", round(pdf_val, 5)))

# (c) Cumulative probability up to x = 6, df = 10
cum_val <- pchisq(6, df = 10)
print(paste("Cumulative probability up to x = 6, df = 10:", round(cum_val, 5)))

# (d) 85th quantile for df = 6
quantile_85 <- qchisq(0.85, df = 6)
print(paste("85th percentile for df = 6:", round(quantile_85, 5)))

# (e) Histogram of 10,000 random deviates, df = 6
set.seed(42)
chisq_sample <- rchisq(10000, df = 6)
hist(chisq_sample, breaks = 30, col = "red", border = "white", 
     main = "Chi-square r = 6", xlab = "Value")
text(20, 1000, "r = 6", col = "black", cex = 1.5)

# (f) Z² = ((x - μ)^2) / σ² and χ²(1) overlay
mu <- 2
sigma <- 1
x_vals <- seq(-5, 10, length.out = 1000)
z_sq_vals <- ((x_vals - mu)^2) / sigma^2
chisq_pdf <- dchisq(z_sq_vals, df = 1)

plot(z_sq_vals, chisq_pdf, type = "l", col = "darkgreen", lwd = 2,
     main = expression(paste("Chi-square PDF with ", df == 1)),
     xlab = expression(Z^2), ylab = "Density")


# Ex 11 - CLT
# Set seed for reproducibility
set.seed(42)

# (1) CLT with Uniform Distribution

# i) Generate 10,000 samples of size 5 from Uniform(0,10)
sample_means <- replicate(10000, mean(runif(5, min=0, max=10)))

# ii) Plot histogram of sample means
hist(sample_means, breaks=30, prob=FALSE, col="lightblue", main="Histogram of Sample Means (Uniform Distribution)", xlab="Sample Mean", border="black")

# Compute mean and std deviation of sample means
mean_sample <- mean(sample_means)
sd_sample <- sd(sample_means)
print(paste("Mean of sample means:", mean_sample))
print(paste("Standard deviation of sample means:", sd_sample))

#hist(mean_sample, breaks=30, prob=TRUE, col="lightblue", main="Histogram of Sample Means (Uniform Distribution)", xlab="Sample Mean", border="black")

# iii) Generate normal PDF with calculated mean and SD
x_seq <- seq(0, 10, by=0.1)
normal_pdf <- dnorm(x_seq, mean=mean_sample, sd=sd_sample)

# iv) Scaling of normal PDF to match histogram height
scaling_factor <- 10000 * 0.5  # Adjusted for histogram bin width
scaled_pdf <- normal_pdf * scaling_factor

# v) Overlay normal curve
lines(x_seq, scaled_pdf, col="red", lwd=2)




####
# Ex 11 - CLT
# Set seed for reproducibility
set.seed(42)

# (i) Generate 10,000 samples of size 5 from Uniform(0,10)
sample_matrix <- replicate(10000, runif(5, min=0, max=10))  # matrix with 5 rows × 10000 columns
sample_means <- colMeans(sample_matrix)  # get mean of each sample

# (ii) Plot histogram of sample means
hist_obj <- hist(sample_means, breaks=30, col="lightblue", 
                 main="Histogram of Sample Means (Uniform Distribution)", 
                 xlab="Sample Mean", border="black")

# Calculate mean and standard deviation of sample means
mean_sample <- mean(sample_means)
sd_sample <- sd(sample_means)
print(paste("Mean of sample means:", round(mean_sample, 4)))
print(paste("Standard deviation of sample means:", round(sd_sample, 4)))

# (iii) Generate sequence for normal PDF
x_seq <- seq(0, 10, by=0.1)
normal_pdf <- dnorm(x_seq, mean=mean_sample, sd=sd_sample)

# (iv) Scale normal PDF using bin width × total frequency
#bin_width <- hist_obj$breaks[2] - hist_obj$breaks[1]  # should be ~0.5
scaling_factor <- 10000 * bin_width
scaled_pdf <- normal_pdf * scaling_factor

# (v) Overlay normal curve
lines(x_seq, scaled_pdf, col="red", lwd=2)




# (2) CLT with Dice Rolls
# ------------------------

# Step (i): Single dice throw (10,000 rolls)
a <- sample(1:6, size=10000, replace=TRUE)
hist(a, breaks=6, prob=TRUE, col="lightblue", main="Single Dice Roll", xlab="Dice Value", border="black")

# Step (ii): Two dice (sum of two rolls)
b <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE)
hist(b, breaks=11, prob=TRUE, col="lightblue", main="Sum of Two Dice Rolls", xlab="Sum", border="black")

# Step (iii): Three dice (sum of three rolls)
c <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE)
hist(c, breaks=16, prob=TRUE, col="lightblue", main="Sum of Three Dice Rolls", xlab="Sum", border="black")

# Step (iv): Five dice (sum of five rolls)
d <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) +
  sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) +
  sample(1:6, size=10000, replace=TRUE)

# Compute mean and standard deviation for normal curve overlay
mean_d <- mean(d)
sd_d <- sd(d)

# Generate normal PDF with calculated mean and SD
x_vals <- seq(min(d), max(d), length=100)
normal_curve <- dnorm(x_vals, mean=mean_d, sd=sd_d)

# Scale the normal PDF
scaled_curve <- normal_curve * 10000 * 0.5  # Adjusting for histogram scaling

# Plot histogram with normal curve overlay
hist(d, breaks=30, prob=TRUE, col="lightblue", main="Sum of Five Dice Rolls", xlab="Sum", border="black")
lines(x_vals, scaled_curve, col="red", lwd=2)


# Ex 12 - ROC

# Load required library
library(pROC)

# Read white wine dataset
wine_data <- read.csv("/home/ibab/R/R-dataset/winequality-white.csv", sep = ";")

# Define threshold values and assign different colors
thresholds <- c(6, 7, 8, 9, 10)
colors <- c("black", "blue", "red", "magenta", "darkgreen")

# Initialize an empty list to store ROC curves
roc_list <- list()

# Create ROC curves for each threshold
for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  
  # Convert quality score to binary: Good (1) if quality >= threshold
  wine_data$quality_binary <- ifelse(wine_data$quality >= t, 1, 0)
  
  # Check that both 0 and 1 are present
  if (length(unique(wine_data$quality_binary)) == 2) {
    wine_data$quality_binary <- factor(wine_data$quality_binary, levels = c(0, 1))
    roc_list[[i]] <- roc(wine_data$quality_binary, wine_data$alcohol)
  } else {
    message(sprintf("Threshold %d skipped: only one class present.", t))
    roc_list[[i]] <- NULL
  }
}

# Filter out NULL entries (i.e., skipped thresholds)
valid_indices <- which(!sapply(roc_list, is.null))

# Check if we have at least one valid ROC curve
if (length(valid_indices) > 0) {
  # Plot first valid ROC
  plot.roc(roc_list[[valid_indices[1]]], col = colors[valid_indices[1]], 
           main = "ROC Curves for Various Quality Thresholds",
           legacy.axes = TRUE, ci = FALSE, print.auc = FALSE)
  
  # Add the rest of the ROC curves
  for (j in valid_indices[-1]) {
    plot.roc(roc_list[[j]], col = colors[j], add = TRUE)
  }
  
  # Add legend
  legend("bottomright", legend = paste("Threshold =", thresholds[valid_indices]),
         col = colors[valid_indices], lwd = 2)
} else {
  message("No valid ROC curves to plot.")
}





