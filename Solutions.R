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
