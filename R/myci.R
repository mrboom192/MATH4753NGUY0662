#' Confidence Interval for the Mean
#'
#' This function calculates a 95% confidence interval for the population mean (\eqn{\mu}) 
#' based on a single sample using a t-distribution. The function takes a numeric vector 
#' as input, computes the sample mean and standard error, and returns the lower and upper 
#' bounds of the confidence interval.
#'
#' @param x A numeric vector representing the sample data from which the confidence interval 
#' will be calculated.
#'
#' @return A numeric vector of length two, representing the lower and upper bounds of the 
#' 95% confidence interval for the mean of the population from which the sample was drawn.
#'
#' @details
#' The confidence interval is calculated using the formula:
#' \deqn{\bar{x} \pm t_{\alpha/2, n-1} \times \frac{s}{\sqrt{n}}}
#' where \eqn{\bar{x}} is the sample mean, \eqn{t_{\alpha/2, n-1}} is the critical value of 
#' the t-distribution with \eqn{n-1} degrees of freedom, \eqn{s} is the sample standard deviation, 
#' and \eqn{n} is the sample size.
#'
#' The t-distribution is used to account for the uncertainty in the estimate of the population 
#' standard deviation from the sample.
#'
#' @importFrom stats qt sd
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' sample_data <- rnorm(30, mean = 50, sd = 10)
#' myci(sample_data)
#'
#' @export
myci <- function(x) {
  # Sample size
  n <- length(x)
  
  # Sample mean
  sample_mean <- mean(x)
  
  # Sample standard deviation
  sample_sd <- sd(x)
  
  # Critical value for 95% confidence (two-tailed t-distribution)
  t_critical <- qt(0.975, df = n - 1)
  
  # Standard error of the mean
  se <- sample_sd / sqrt(n)
  
  # Margin of error
  margin_of_error <- t_critical * se
  
  # Confidence interval
  ci_lower <- sample_mean - margin_of_error
  ci_upper <- sample_mean + margin_of_error
  
  # Return the confidence interval
  return(c(ci_lower, ci_upper))
}
