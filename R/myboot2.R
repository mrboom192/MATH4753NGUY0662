#' Bootstrap Confidence Interval
#'
#' This function performs a bootstrap simulation to generate confidence intervals
#' for a specified statistic (mean by default) from a given sample. It also
#' plots a histogram of the bootstrap sample statistics with the confidence interval
#' visualized on the plot.
#'
#' @param iter The number of bootstrap iterations. Default is 10,000.
#' @param x A numeric vector representing the sample data.
#' @param fun A function to apply to the bootstrap samples (e.g., "mean", "median", "var"). Default is "mean".
#' @param alpha The significance level for the confidence interval (e.g., 0.05 for a 95% CI). Default is 0.05.
#' @param cx A numeric expansion factor for the size of the confidence interval annotations. Default is 1.5.
#' @param ... Additional graphical parameters to pass to the histogram plot.
#' @return A list containing the confidence interval (`ci`), the function used (`fun`), and the original data (`x`).
#' @examples
#' set.seed(123)
#' sample_data <- rnorm(20, mean = 10, sd = 2)
#' result <- myboot2(iter = 10000, x = sample_data, fun = "mean", alpha = 0.05)
#' print(result$ci)  # Display the confidence interval
#' @importFrom graphics hist abline segments text
#' @importFrom stats quantile
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n = length(x)  # sample size
  
  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun)  # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2))  # Confidence interval
  
  # Plot the histogram of bootstrap statistics
  para = hist(xstat, freq = FALSE, las = 1,
              main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha =", alpha, " iter =", iter, sep = ""),
              ...)
  
  # Matrix that contains the data
  mat = matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  
  # Point estimate (using the specified function)
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")  # Vertical line at the point estimate
  segments(ci[1], 0, ci[2], 0, lwd = 4)  # Confidence interval segment
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  
  # Plot the point estimate half-way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  
  # Return the confidence interval, function used, and original data
  invisible(list(ci = ci, fun = fun, x = x))
}

