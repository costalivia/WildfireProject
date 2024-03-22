library(testthat)

# Test 1 for Correctness
test_that("risk_score function calculates risk score correctly", {
  # Define input values for annual data
  T <- 25  # Mean temperature in Celsius for the year
  p <- 300  # Total precipitation in mm for the year
  h <- 70  # Mean humidity in percentage for the year

  # Expected risk score (you should replace this with your expected result)
  expected_score <- 0.5 * T^2 + 0.2 * (400 - p) + 0.3 * (100 - h)

  # Calculate risk score using the function
  calculated_score <- risk_score(T, p, h)

  # Check if the calculated score matches the expected score
  expect_equal(calculated_score, expected_score)
})

# Test 2 for Robustness
test_that("risk_score function handles extreme input gracefully", {
  # Define extreme input values
  T <- 40  # Very high mean temperature in Celsius for the year
  p <- 0  # Very low total precipitation in mm for the year
  h <- 10  # Very low mean humidity in percentage for the year

  # Calculate risk score using the function
  calculated_score <- risk_score(T, p, h)

  # Check if the calculated score is not NaN or infinite
  expect_true(!is.nan(calculated_score) & !is.infinite(calculated_score))
})

# Test 3 for Missing Values
#test_that("risk_score function handles missing values gracefully", {
  # Define input values with missing data
 # T <- 25  # Mean temperature in Celsius for the year
 # p <- NA  # Missing total precipitation value
 # h <- 70  # Mean humidity in percentage for the year

  # Calculate risk score using the function
 # calculated_score <- risk_score(T, p, h)

  # Check if the calculated score is NaN (since the function returns NaN when there are missing values)
 # expect_true(is.nan(calculated_score))
# })

#' @export
