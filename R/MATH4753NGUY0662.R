#' Calculate Total Sum of Squares (TSS)
#'
#' This function calculates the Total Sum of Squares (TSS) for the variable `Height` in a given data frame.
#' The TSS measures the total variability in the `Height` variable around its mean.
#'
#' @param x A data frame that contains a numeric `Height` column.
#' @return A numeric value representing the Total Sum of Squares (TSS).
#' @examples
#' # Example using a sample data frame with a Height column:
#' sample_df <- data.frame(Height = c(150, 160, 170, 180, 190))
#' TSS(sample_df)
#'
#' @export
TSS <- function(x) {
  with(x, sum((Height - mean(Height)) ^ 2))
}

#' Binomial Simulation
#'
#' Simulates binomial trials and generates a barplot of the relative frequencies of successes
#' over multiple iterations. This function takes a specified number of iterations and trials
#' and visualizes the distribution of successes in a barplot.
#'
#' @param iter The number of iterations (simulations) to perform. Default is 100.
#' @param n The number of trials in each iteration. Default is 10.
#' @param p The probability of success on each trial. Default is 0.5.
#' @return A table showing the proportion of successes across all iterations.
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @examples
#' # Run the binomial simulation with 100 iterations, 10 trials, and success probability of 0.7:
#' mybin(iter=100, n=10, p=0.7)
#'
#' @export
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA, nrow=n, ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}


#' Plot and Calculate Normal Curve with Shaded Area
#'
#' This function plots a normal distribution curve with specified mean and standard deviation,
#' shades the area under the curve between -∞ and a given x-value, and calculates the probability
#' of P(X <= a).
#'
#' @param mu The mean of the normal distribution.
#' @param sigma The standard deviation of the normal distribution.
#' @param a The x-value up to which the area under the curve is shaded and the probability is calculated.
#' @return A list containing the mean (mu), standard deviation (sigma), and the probability P(X <= a).
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @examples
#' # Example usage of myncurve function:
#' myncurve(mu = 0, sigma = 1, a = 1.5)
#'
#' @export
myncurve = function(mu, sigma, a) {
  x <- NULL
  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        xlab = "x", ylab = "Density", main = paste("Normal Curve: mu =", mu, ", sigma =", sigma))

  # Define the range for the shaded area (from -∞ to a)
  xcurve = seq(mu - 3 * sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)

  # Shade the area under the curve from -∞ to a
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "lightblue")

  # Calculate the area (probability) P(X <= a)
  prob = pnorm(a, mean = mu, sd = sigma)

  # Print the calculated probability to the command-line
  print(paste("P(X <=", a, ") =", round(prob, 4)))

  # Return a list with mu, sigma, and the calculated probability
  return(list(mu = mu, sigma = sigma, prob = round(prob, 4)))
}

#' Optimal Number of Tickets Calculation
#'
#' This function calculates the optimal number of tickets to sell for an event
#' given the event capacity, risk tolerance (`gamma`), and probability of
#' attendance (`p`). It computes both discrete and continuous approximations
#' and plots the results for comparison.
#'
#' @param N The number of available seats or capacity of the event. Must be a positive integer.
#' @param gamma The allowable risk of overbooking. A probability value between 0 and 1.
#' @param p The probability that a ticket holder attends the event. A value between 0 and 1.
#' @return A list with the optimal number of tickets to sell for both discrete (`nd`) and continuous (`nc`) cases.
#' @importFrom stats pbinom pnorm
#' @importFrom graphics plot points lines abline layout
#' @examples
#' # Example usage of ntickets function:
#' result <- ntickets(N = 100, gamma = 0.02, p = 0.95)
#' print(result)
#' @export
ntickets <- function(N, gamma, p) {
  # Discrete calculation
  n <- seq(N, floor(N + N/10), by = 1)
  obj_discrete <- 1 - gamma - pbinom(N, size = n, prob = p)
  ind_discrete <- which.min(abs(obj_discrete))
  nd <- n[ind_discrete]

  # Continuous calculation
  n_continuous <- seq(N, floor(N + N/10), by = 0.001)
  obj_continuous <- 1 - gamma - pnorm(N + 0.5, mean = n_continuous * p, sd = sqrt(n_continuous * p * (1 - p)))
  ind_continuous <- which.min(abs(obj_continuous))
  nc <- n_continuous[ind_continuous]

  # Plot the objective function for both discrete and continuous cases
  layout(matrix(1:2, nrow = 2, byrow = TRUE))

  # Plot the discrete calculation
  plot(n, obj_discrete, type = "n", ylab = "Objective", pch = 16,
       col = ifelse(n == nd, "red", "black"),
       main = paste("Objective vs n to find optimal tickets sold (", nd, ")\n",
                    "gamma = ", gamma, " N = ", N, " discrete"))
  points(n, obj_discrete, pch = 16, cex = 0.75, col = ifelse(n == nd, "red", "black"))
  lines(n, obj_discrete, col = ifelse(n == nd, "red", "black"), lwd = .75)
  abline(h = obj_discrete[ind_discrete], v = nd, col = "red")

  # Plot the continuous calculation
  plot(n_continuous, obj_continuous, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective vs n to find optimal tickets sold (", round(nc, 2), ")\n",
                    "gamma = ", gamma, " N = ", N, " continuous"))
  abline(h = obj_continuous[ind_continuous], v = nc, col = "blue")

  # Return a named list
  return(list(nd = nd, nc = round(nc, 2), N = N, p = p, gamma = gamma))
}
#' Poisson Sampling Simulation
#'
#' This function simulates random samples from a Poisson distribution, generates a histogram of the sample means, and adds a theoretical normal curve. It also displays a bar plot of the sampled data and the probability function of the Poisson distribution.
#'
#' @param n The number of samples per iteration (sample size). Default is 10.
#' @param iter The number of iterations (simulations). Default is 100.
#' @param lambda The rate parameter for the Poisson distribution. Default is 10.
#' @param ... Additional graphical parameters to pass to the histogram plot.
#' @return A plot showing the histogram of the sample means, a density curve, a theoretical normal curve, a barplot of the sampled data, and the Poisson probability mass function.
#' @importFrom graphics hist curve barplot plot layout
#' @importFrom grDevices rainbow
#' @importFrom stats rpois dnorm dpois
#' @examples
#' # Example usage of mycltp function:
#' mycltp(n=20, iter=10000, lambda=10)
#'
#' @export
mycltp = function(n, iter, lambda=10, ...) {
  ## Generate random samples from the Poisson distribution
  y = rpois(n * iter, lambda = lambda)
  
  ## Place these numbers into a matrix (nrow = n, ncol = iter)
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  
  ## Compute the mean of each column (sample means)
  w = apply(data, 2, mean)
  
  ## Create a histogram of the sample means without plotting it yet
  param = hist(w, plot = FALSE)
  
  ## Set the y-axis max value (with a 10% buffer)
  ymax = 1.1 * max(param$density)
  
  ## Set up layout for multiple plots
  layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))
  
  ## Plot the histogram of sample means
  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)),
       main = paste("Histogram of sample mean", "\n", "Sample size = ", n, " Iterations = ", iter, " Lambda = ", lambda, sep = ""),
       xlab = "Sample mean", ...)
  
  ## Add a theoretical normal curve
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)), add = TRUE, col = "Red", lty = 2, lwd = 3)
  
  ## Create a barplot of the sampled data (y)
  barplot(table(y) / (n * iter), col = rainbow(max(y)), main = "Barplot of sampled y", ylab = "Relative Frequency", xlab = "y")
  
  ## Plot the Poisson probability function
  x = 0:max(y)
  plot(x, dpois(x, lambda = lambda), type = "h", lwd = 5, col = rainbow(max(y)),
       main = "Poisson Probability Function", ylab = "Probability", xlab = "y")
}



# FIREDAM.CSV
fire <- read.csv("inst/extdata/FIREDAM.csv")
