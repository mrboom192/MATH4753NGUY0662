
test_that("TSS function calculates correct total sum of squares", {
  # Create a sample data frame
  sample_df <- data.frame(Height = c(150, 160, 170, 180, 190))

  # Calculate expected TSS manually
  expected_TSS <- sum((sample_df$Height - mean(sample_df$Height))^2)

  # Test if the TSS function returns the correct value
  expect_equal(TSS(sample_df), expected_TSS)
})

test_that("mybin function returns a valid proportion table", {
  # Run the mybin function
  result <- mybin(iter = 100, n = 10, p = 0.5)

  # Check if the result is a table with the correct length (0 to n inclusive, so n+1)
  expect_equal(length(result), 11)

  # Check if the sum of proportions is approximately 1 (due to floating-point precision)
  expect_equal(sum(result), 1, tolerance = 0.01)
})

test_that("myncurve function returns correct probability and list structure", {
  # Run the myncurve function with a known value
  result <- myncurve(mu = 0, sigma = 1, a = 1.5)

  # Check if the result is a list using expect_type
  expect_type(result, "list")

  # Check if the list contains mu, sigma, and prob
  expect_named(result, c("mu", "sigma", "prob"))

  # Check if the mean and sigma are correct
  expect_equal(result$mu, 0)
  expect_equal(result$sigma, 1)

  # Check if the probability is correctly calculated using pnorm
  expected_prob <- pnorm(1.5, mean = 0, sd = 1)
  expect_equal(result$prob, round(expected_prob, 4))
})

