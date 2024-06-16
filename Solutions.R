# Function to find the largest prime factor of a number
largest_prime_factor <- function(n) {
  
  # Initialize variables
  largest_factor <- 1
  i <- 2
  
  # Loop to find prime factors
  while (i <= n/i) {
    if (n %% i == 0) {
      largest_factor <- i
      while (n %% i == 0) {
        n <- n / i
      }
    }
    i <- i + 1
  }
  
  # If the remaining number is a prime greater than 1
  if (n > 1) {
    largest_factor <- n
  }
  
  return(largest_factor)
}

# Calculate the largest prime factor of 600851475143
number <- 600851475143
result <- largest_prime_factor(number)

# Print the result
cat("The largest prime factor of", number, "is:", result, "\n")

# Function to find the largest prime factor of a number
largest_prime_factor <- function(n) {
  
  # Initialize variables
  largest_factor <- 1
  i <- 2
  
  # Loop to find prime factors
  while (i <= n / i) {
    if (n %% i == 0) {
      largest_factor <- i
      while (n %% i == 0) {
        n <- n / i
      }
    }
    i <- i + 1
  }
  
  # If the remaining number is a prime greater than 1
  if (n > 1) {
    largest_factor <- n
  }
  
  return(largest_factor)
}

# Calculate the largest prime factor of 600851475143
number <- 600851475143
result <- largest_prime_factor(number)

# Print the result
cat("The largest prime factor of", number, "is:", result, "\n")

# Function to calculate the sum of multiples of 3 or 5 below a given number
sum_of_multiples <- function(limit) {
  sum <- 0
  for (i in 1:(limit - 1)) {
    if (i %% 3 == 0 || i %% 5 == 0) {
      sum <- sum + i
    }
  }
  return(sum)
}

# Calculate the sum of multiples of 3 or 5 below 1000
result <- sum_of_multiples(1000)

# Print the result
cat("The sum of all multiples of 3 or 5 below 1000 is:", result, "\n")


# Load the ggplot2 package
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate random data
n <- 1000

# Binomial distribution
binom_data <- rbinom(n, size = 10, prob = 0.5)

# Poisson distribution
pois_data <- rpois(n, lambda = 5)

# Normal distribution
norm_data <- rnorm(n, mean = 0, sd = 1)

# Create data frames
binom_df <- data.frame(value = binom_data)
pois_df <- data.frame(value = pois_data)
norm_df <- data.frame(value = norm_data)

# Plot Binomial distribution
binom_plot <- ggplot(binom_df, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Binomial Distribution") +
  xlab("Value") +
  ylab("Frequency")

# Plot Poisson distribution
pois_plot <- ggplot(pois_df, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  ggtitle("Poisson Distribution") +
  xlab("Value") +
  ylab("Frequency")

# Plot Normal distribution
norm_plot <- ggplot(norm_df, aes(x = value)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  ggtitle("Normal Distribution") +
  xlab("Value") +
  ylab("Frequency")

# Print the plots
print(binom_plot)
print(pois_plot)
print(norm_plot)

# Save the plots to files
ggsave("binom_plot.png", plot = binom_plot)
ggsave("pois_plot.png", plot = pois_plot)
ggsave("norm_plot.png", plot = norm_plot)



# Set seed for reproducibility
set.seed(123)

# Function to generate binomial and normal data and plot histograms
generate_and_plot <- function(N, p, num_samples) {
  # Generate binomial data
  binom_data <- rbinom(num_samples, size = N, prob = p)
  
  # Calculate mean and standard deviation for the normal distribution
  mean <- N * p
  sd <- sqrt(N * p * (1 - p))
  
  # Generate normal data
  norm_data <- rnorm(num_samples, mean = mean, sd = sd)
  
  # Create data frames
  binom_df <- data.frame(value = binom_data, distribution = 'Binomial')
  norm_df <- data.frame(value = norm_data, distribution = 'Normal')
  
  # Combine data frames
  combined_df <- rbind(binom_df, norm_df)
  
  # Plot histograms
  plot <- ggplot(combined_df, aes(x = value, fill = distribution)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 30) +
    facet_wrap(~ distribution) +
    ggtitle(paste("Binomial vs Normal Distribution (N =", N, ", p =", p, ")")) +
    xlab("Value") +
    ylab("Density") +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
  # Save the plot
  ggsave(paste("binom_norm_N", N, ".png", sep=""), plot = plot)
}

# Parameters
p <- 0.5
num_samples <- 10000

# Generate and plot for different values of N
generate_and_plot(10, p, num_samples)
generate_and_plot(50, p, num_samples)
generate_and_plot(100, p, num_samples)
generate_and_plot(500, p, num_samples)
generate_and_plot(1000, p, num_samples)

